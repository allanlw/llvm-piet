/* Copyright (c) 2012, Allan Wirth <allan@allanwirth.com>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *  * Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *
 *  * Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 *  * Neither the name of this software, nor the names of its
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <llvm/Pass.h>
#include <llvm/Support/CFG.h>
#include <llvm/Function.h>
#include <llvm/Instructions.h>
#include <llvm/BasicBlock.h>
#include <llvm/PassManager.h>
#include <llvm/Module.h>
#include <llvm/Operator.h>
#include <llvm/Support/IRBuilder.h>
#include <llvm/Support/CallSite.h>
#include <llvm/PassManagers.h>
#include <llvm/Constants.h>
#include <llvm/Use.h>

#include <iostream>
#include <sstream>
#include <list>
#include <vector>
#include <set>
#include <deque>

using namespace llvm;

namespace {

static bool isText(std::string t) {
  return (t=="printf"||t=="puts"||t=="putc"||t=="debug"||t=="scanf"||
      t=="llvm.lifetime.begin"||t=="llvm.lifetime.end");
}

// This pass does basic 'peephole' optimization of basic blocks
// merging obviously redundant stack operations and text operations
struct PushPopMergePass : public BasicBlockPass {
  static char ID;
  PushPopMergePass() : BasicBlockPass(ID) {}

  virtual bool runOnBasicBlock(BasicBlock &BB) {
    return this->_runOnBasicBlock(BB);
  }

  static bool _runOnBasicBlock(BasicBlock &BB, bool ret = false) {
    if (BB.getParent()->getName().str().find("codel") != 0) {
      return false;
    }
    BasicBlock::iterator end = BB.end();
    CallInst *a, *b;
    for (BasicBlock::iterator it = BB.begin(); it != end; ++it) {
      if (!(a = dyn_cast<CallInst>(it))) {
        continue;
      }
      std::string name1 = a->getCalledFunction()->getName().str();
      if ((name1 == "roll" && unrollRoll(a)) ||
          (name1 == "printf" && reducePrintf(a))) {
        return _runOnBasicBlock(BB, true);
      } else if (name1 == "peek" && a->use_empty()) {
        a->eraseFromParent();
        return _runOnBasicBlock(BB, true);
      } else if (name1 != "push" && name1 != "pop" && name1 != "debug" &&
          name1 != "roll" && name1 != "printf" && name1 != "peek") {
        continue;
      }
      BasicBlock::iterator it2(it); ++it2;
      for (; it2 != end; ++it2) {
        if (!(b = dyn_cast<CallInst>(it2))) {
          ReturnInst* d;
          // useless instructions before returns can be dumped
          if ((d = dyn_cast<ReturnInst>(it2)) && (name1 == "push" ||
              name1 == "roll" || ((name1 == "pop" || name1 == "peek") &&
              a->use_empty()))) {
            a->eraseFromParent();
            return _runOnBasicBlock(BB, true);
          }
          continue;
        }
        std::string name2 = b->getCalledFunction()->getName().str();
        if (mergeTwoCalls(a, b)) {
          return _runOnBasicBlock(BB, true);
        } else if ((isText(name1) && isText(name2)) || !isText(name2)) {
          break;
        }
      }
    }
    return ret;
  }

private:
  // tries to unroll a 'roll' instruction if possible
  // return true if it did, false if it didn't
  static bool unrollRoll(CallInst *a) {
    BasicBlock& BB = *a->getParent();
    ConstantInt *depth_c, *times_c;
    if (!(depth_c = dyn_cast<ConstantInt>(a->getArgOperand(0))) ||
        !(times_c = dyn_cast<ConstantInt>(a->getArgOperand(1)))) {
      return false;
    }
    APInt depth = depth_c->getValue();
    APInt times = times_c->getValue();
    if (depth.isNegative() || !depth.getBoolValue()) return false;
    while (times.isNegative()) { times += depth; }
    std::list<Value*> pops;
    Instruction* next = a->getNextNode();

    APInt depth2(depth);
    while (depth2.getBoolValue()) {
      CallInst* ci = CallInst::Create(BB.getParent()->getParent()
          ->getFunction("pop"));
      ci->insertBefore(next);
      pops.push_front(ci);
      --depth2;
    }
    APInt times2(times);
    while (times2.getBoolValue()) {
      pops.push_front(pops.back());
      pops.pop_back();
      --times2;
    }
    std::list<Value*>::iterator f = pops.begin();
    std::list<Value*>::iterator b = pops.end();
    for (; f != b; ++f) {
      CallInst::Create(BB.getParent()->getParent()
         ->getFunction("push"), *f, "", next);
    }
    a->eraseFromParent();
    return true;
  }

  // tries to reduce a printf that has only constant arguments
  // DON'T TRY THIS AT HOME FOLKS
  // return true if it did, false if it didn't
  static bool reducePrintf(CallInst* a) {
    BasicBlock& BB = *a->getParent();
    if (a->getNumArgOperands() == 1) return false;
    GEPOperator* gepo = cast_or_null<GEPOperator>(a->getArgOperand(0));
    GlobalVariable* gv = cast_or_null<GlobalVariable>(gepo->
        getPointerOperand());
    ConstantArray* ca = cast_or_null<ConstantArray>(gv->getInitializer());
    if (!ca) return false;
    std::string format(ca->getAsString());
    format.resize(format.size()-1);
    size_t start = 0;
    std::vector<Value*> args;
    bool operated = false;
    for(size_t i = 1; i < a->getNumArgOperands(); i++) {
      Value* arg = a->getArgOperand(i);
      size_t j = format.find("%", start);
      std::string* replacement = NULL;
      if (isa<ConstantInt>(arg) && format[j+1] == 'c') {
        replacement = new std::string(1, (char)cast<ConstantInt>(arg)->
            getValue().getSExtValue());
      } else if (isa<ConstantInt>(arg) && format[j+1] == 'd') {
        replacement = new std::string(cast<ConstantInt>(arg)->getValue().
            toString(10, true));
      }
      if (replacement && replacement->find("%") == std::string::npos) {
        format.replace(j, 2, *replacement);
        start = j + replacement->size();
        delete replacement;
        operated = true;
      } else {
        start = j + 2;
        args.push_back(arg);
      }
    }
    if (operated) {
      IRBuilder<> bu(&BB, a);
      args.insert(args.begin(), bu.CreateGlobalStringPtr(format));
      bu.CreateCall(a->getCalledFunction(), args);
      a->eraseFromParent();
    }
    return operated;
  }

  // tries to merge two printf statements (or debug() calls)
  // together. Return true on success, false on failure
  static bool mergePrintfs(CallInst *a, CallInst *b) {
    BasicBlock& BB = *a->getParent();
    GEPOperator *s1, *s2;
    if (!(s1 = dyn_cast<GEPOperator>(a->getArgOperand(0))) ||
        !(s2 = dyn_cast<GEPOperator>(b->getArgOperand(0)))) {
       return false;
    }
    GlobalVariable *f1, *f2;
    if (!(f1 = dyn_cast<GlobalVariable>(s1->getPointerOperand())) ||
        !(f2 = dyn_cast<GlobalVariable>(s2->getPointerOperand()))) {
      return false;
    }
    ConstantArray *a1, *a2;
    if (!(a1 = dyn_cast<ConstantArray>(f1->getInitializer())) ||
        !(a2 = dyn_cast<ConstantArray>(f2->getInitializer()))) {
      return false;
    }
    std::string nc = a1->getAsString();
    nc.resize(nc.size()-1); // chop off null terminator
    if (a->getCalledFunction()->getName().str() == "debug") nc.append("+");
    nc.append(a2->getAsString());
    nc.resize(nc.size()-1); // chop off null terminator

    IRBuilder<> bu(&BB, b);
    Value* st = bu.CreateGlobalStringPtr(nc);
    std::vector<Value*> arguments;
    arguments.push_back(st);
    size_t i;
    for (i = 1; i < a->getNumArgOperands(); i++) {
      arguments.push_back(a->getArgOperand(i));
    }
    for (i = 1; i < b->getNumArgOperands(); i++) {
      arguments.push_back(b->getArgOperand(i));
    }
    bu.CreateCall(a->getCalledFunction(), arguments);
    a->eraseFromParent();
    b->eraseFromParent();
    return true;
  }

  // tries to merge two adjacent calls (a is first, b is second)
  // return true on success, false on failure
  static bool mergeTwoCalls(CallInst *a, CallInst *b) {
    BasicBlock& BB = *a->getParent();
    std::string name1 = a->getCalledFunction()->getName().str();
    std::string name2 = b->getCalledFunction()->getName().str();
    if (name1 == "push") {
      if (name2 == "pop") {
        b->replaceAllUsesWith(a->getArgOperand(0));
        b->eraseFromParent();
        a->eraseFromParent();
        return true;
      } else if (name2 == "peek") {
        b->replaceAllUsesWith(a->getArgOperand(0));
        b->eraseFromParent();
        return true;
      }
    } else if (name1 == "pop") {
      if (name2 == "push" && a == b->getArgOperand(0)) {
        a->setCalledFunction(BB.getParent()->getParent()
           ->getFunction("peek"));
        b->eraseFromParent();
        return true;
      }
    } else if (name1 == "peek") {
      if (name2 == "pop") {
        b->moveBefore(a);
        a->replaceAllUsesWith(b);
        a->eraseFromParent();
        return true;
      } else if (name2 == "peek") {
        b->replaceAllUsesWith(a);
        b->eraseFromParent();
        return true;
      }
    } else if ((name1 == "printf" && name2 == "printf") ||
        (name1 == "debug" && name2 == "debug")) {
      if (mergePrintfs(a, b)) {
        return true;
      }
    }
    return false;
  }
};

// If most call sites for a function are preceded by pushes, we
// move the pushes into the entry block of the function and pass
// the values that would have been pushes as parameters.
struct InterFuncMovePass : public ModulePass {
  static char ID;
  InterFuncMovePass() : ModulePass(ID) {}

  virtual bool runOnModule(Module& m) {
    return this->_runOnModule(m);
  }

private:
  bool _runOnModule(Module& m, bool ret = false) {
    Module::iterator b = m.begin(), e = m.end();
    for (; b != e; ++b) {
      if (b->getName().str().find("codel") != 0) {
        continue;
      }
      if (mostCallSitesPreceededByPush(b) ||
          firstCallInBlock(&b->getEntryBlock(), "pop") ||
          firstCallInBlock(&b->getEntryBlock(), "peek")) {
        Function* NF = addFunctionParamWithCall(*b, m.getFunction("pop"));
        CallInst::Create(m.getFunction("push"), &*(--NF->arg_end()),
            "", NF->getEntryBlock().getFirstNonPHI());
        return _runOnModule(m, true);
      }
    }
    return ret;
  }

  static bool firstCallInBlock(BasicBlock* b, std::string a) {
    for (BasicBlock::iterator i = b->begin(), e = b->end(); i != e; ++i) {
      CallInst *c;
      if (!(c = dyn_cast<CallInst>(i))) continue;
      std::string cicn = c->getCalledFunction()->getName().str();
      if (cicn == a) return true;
      else if (cicn == "pop" || cicn == "roll" || cicn=="peek" ||
          cicn == "push") return false;
    }
    return false;
  }

  static bool mostCallSitesPreceededByPush(Function* b) {
    Function::use_iterator ui(b->use_begin()), ue(b->use_end());
    // number of call sites that are preceded by a push
    unsigned numFound = 0;
    // number of call sites that are safe to add a pop before but do not have
    // a push. we assume that if there was a peek or a roll then the execution
    // would have already terminated if the stack was empty
    unsigned numNotFound = 0;
    for (; ui != ue; ++ui) {
      CallSite CS(*ui);
      Instruction* Call = CS.getInstruction();
      BasicBlock::iterator ic(Call), ib = Call->getParent()->begin();
      if (Call->getParent()->getParent()->getName().str().find("codel")
          != 0) {
        return false;
      }
      if (ic == Call->getParent()->begin()) {
        return false;
      }
      bool found = false;
      do {
        --ic;
        CallInst* cic;
        if (!(cic = dyn_cast<CallInst>(ic))) continue;
        std::string cicn = cic->getCalledFunction()->getName().str();
        if (cicn == "push") {
          found = true;
          break;
        } else if (cicn == "roll" || cicn == "peek") {
          break;
        } else if (cicn == "pop") return false;
      } while (ic != ib);
      if (found == true) ++numFound;
      else ++numNotFound;
    }
    return numFound >= numNotFound + 1;
  }

  static Function* addFunctionParamWithCall(Function& f, Function* newCall) {
    Module* m = f.getParent();
    const FunctionType* Fty = f.getFunctionType();

    std::vector<Type*> params(Fty->param_begin(), Fty->param_end());
    params.push_back(newCall->getReturnType());
    FunctionType* NFty = FunctionType::get(Fty->getReturnType(),
       params, false);

    Function *NF = Function::Create(NFty, f.getLinkage());

    NF->copyAttributesFrom(&f);
    NF->takeName(&f);

    m->getFunctionList().insert(&f, NF);

    while (!f.use_empty()) {
      CallSite CS(f.use_back());
      Instruction* call = CS.getInstruction();
      std::vector<Value*> args(CS.arg_begin(), CS.arg_end());

      CallInst* p = CallInst::Create(newCall);
      p->insertBefore(call);
      args.push_back(p);

      CallInst* N = CallInst::Create(NF, args);
      N->setCallingConv(CS.getCallingConv());
      N->setTailCall(cast<CallInst>(CS.getInstruction())->isTailCall());
      N->insertBefore(call);

      call->replaceAllUsesWith(N);
      call->eraseFromParent();
      PushPopMergePass::_runOnBasicBlock(*N->getParent());
    }
    NF->getBasicBlockList().splice(NF->begin(), f.getBasicBlockList());

    Function::arg_iterator in = NF->arg_begin(), io = f.arg_begin(),
        eo = f.arg_end();
    for(; io != eo; ++io, ++in) {
      io->replaceAllUsesWith(in);
    }
    f.eraseFromParent();
    return NF;
  }
};

// this function find predecessor basic blocks with pushes
// and moves them into itself using phi nodes.
struct InterBlockMergePass : public FunctionPass {
  static char ID;
  InterBlockMergePass() : FunctionPass(ID) {}

  virtual bool runOnFunction(Function& fn) {
    return this->_runOnFunction(fn);
  }

private:
  // returns true if this basic block is a candidate for cross-block
  // call merging using phis
  bool basicBlockIsOperable(BasicBlock& bb) {
    bool flag = true;
    for (BasicBlock::iterator ia = bb.begin(), ie = bb.end(); ia != ie; ++ia) {
      if (!isa<CallInst>(*&ia)) {
        continue;
      }
      std::string name = cast<CallInst>(ia)->getCalledFunction()->getName().str();
      if (name == "peek" || name == "pop" || name == "roll") {
        flag = false;
        break;
      } else if (name == "push") return false;
    }
    if (flag) return false;
    pred_iterator end3 = pred_end(&bb), it3=pred_begin(&bb);
    if (it3 == end3) {
      return false;
    }
    size_t numNoPush = 0;
    size_t numPush = 0;
    for (; it3 != end3; ++it3) {
      Instruction* ti = (*it3)->getTerminator();
      if (ti == NULL || &(*it3)->front() == ti) {
        numNoPush++;
        continue;
      }
      bool flag = false;
      // loop to see if the last call site is a push
      BasicBlock::iterator tii(ti), tib = (*it3)->begin();
      for (; ; --tii) {
        CallInst *b;
        if ((b = dyn_cast<CallInst>(tii))) {
          std::string name2 = b->getCalledFunction()->getName().str();
          if (name2 == "push") {
            flag = true;
            break;
          } else if (!isText(name2)) break;
        }
        if (tii == tib) break;
      }
      if (flag) numPush++;
      else numNoPush++;
    }
    // return true if this will have a net gain or be neutral
    return (numPush+1 >= numNoPush && numPush != 0);
  }

  bool _runOnFunction(Function &fn, bool ret = false) {
    if (fn.getName().str().find("codel") != 0) {
      return false;
    }
    Function::iterator end = fn.end();
    for (Function::iterator it = fn.begin(); it != end; ++it) {
      if (!basicBlockIsOperable(*it)) continue;
      // at this point we have found mergable pop or peek
      PHINode* p = PHINode::Create(fn.getParent()->getFunction("pop")->
          getReturnType(), 0);
      // find all the call sites for the pushes
      // and remove them, adding new basic blocks on other successors
      // to this predecessor with pushes
      std::set<BasicBlock*> processed;
      pred_iterator end3 = pred_end(it), it3=pred_begin(it);
      for (; it3 != end3; ++it3) {
        if (processed.count(*it3)) {
          int i = p->getBasicBlockIndex(*it3);
          p->addIncoming(p->getIncomingValue(i), *it3);
          continue;
        } else {
          processed.insert(*it3);
        }
        TerminatorInst* ti = (*it3)->getTerminator();
        CallInst* push_f = NULL;
        // walk to the first call site
        BasicBlock::iterator tii(ti), tib = (*it3)->begin();
        for (; ; --tii) {
          if ((push_f = dyn_cast<CallInst>(tii))) {
            std::string name2 = push_f->getCalledFunction()->getName().str();
            if (name2 == "push") break;
            else {
              push_f = NULL;
              if (!isText(name2)) break;
            }
          }
          if (tii == tib) break;
        }
        if (push_f == NULL) {
          CallInst* ci = CallInst::Create(fn.getParent()->getFunction("pop"));
          ci->insertBefore(ti);
          p->addIncoming(ci, *it3);
        } else {
          p->addIncoming(push_f->getArgOperand(0), (*it3));
          push_f->eraseFromParent();
        }
        // if the predecessor has other successors we make intermediate nodes
        succ_iterator suc_b = succ_begin(*it3), suc_e = succ_end(*it3);
        for (; suc_b != suc_e; ++suc_b) {
          // if we're going into the block we're making the phi in we're fine.
          if (*suc_b == &*it) {
            continue;
          }
          BasicBlock* newInter = BasicBlock::Create(it->getContext(), "", &fn);
          IRBuilder<> bu(newInter);
          bu.CreateCall(fn.getParent()->getFunction("push"),
              p->getIncomingValue(p->getBasicBlockIndex(*it3)));
          bu.CreateBr(*suc_b);
          // replace phi uses
          // taken from BasicBlock.cpp
          for (BasicBlock::iterator II = (*suc_b)->begin(),
              IE = (*suc_b)->end(); II != IE; ++II) {
            PHINode *PN = dyn_cast<PHINode>(II);
            if (!PN)
              break;
            int i;
            // only replace ONE
            if ((i = PN->getBasicBlockIndex(*it3)) >= 0)
              PN->setIncomingBlock(i, newInter);
          }
          ti->setSuccessor(suc_b.getSuccessorIndex(), newInter);
        }
      }
      Instruction* it2 = it->getFirstNonPHI();
      p->insertBefore(it2);
      CallInst::Create(fn.getParent()->getFunction("push"),
        p, "", it2);
      // Reduce the new site right away before some other pass tries to
      // move it back
      PushPopMergePass::_runOnBasicBlock(*p->getParent());
      return _runOnFunction(fn, true);
    }
    return ret;
  }
};

// this pass finds peeks and pops that are the first stack operation in a
// basic block and attempts to prove that they have only one value.
// If it can prove that, then the peek or pop's uses will be substituted with
// those of the precursor value.
struct InterBlockMergePass2 : public FunctionPass {
  static char ID;
  InterBlockMergePass2() : FunctionPass(ID) {}

  virtual bool runOnFunction(Function& fn) {
    Function::iterator fi = fn.begin(), fe = fn.end();
    bool res = false;
    for (; fi != fe; ++fi) {
      BasicBlock::iterator bi = fi->begin(), be = fi->end();
      for (; bi != be; ++bi) {
        CallInst *ci;
        if (!(ci = dyn_cast<CallInst>(bi))) continue;
        std::string cin = ci->getCalledFunction()->getName().str();
        if (cin == "push" || cin == "roll") break;
        else if (cin != "peek" && cin != "pop") continue;
        res |= proveValue(ci);
        break;
      }
    }
    return res;
  }

  // prove a peek/pop can only have one value and if so replace it with
  // that value (in the case of a pop there must still be a pop left after
  // which will have to be cleaned up later, if possible)
  // precondition: this must be the first stack operation in the basic block!
  // return true on success and false on failure (uncertainty)
  static bool proveValue(CallInst* ci) {
    std::deque<BasicBlock*> preds;
    std::set<BasicBlock*> scanned;
    // doesn't work on entry blocks
    if (&ci->getParent()->getParent()->getEntryBlock() == ci->getParent()) {
      return false;
    }
    pred_iterator _pi= pred_begin(ci->getParent()),
        _pe = pred_end(ci->getParent());
    for (; _pi != _pe; ++_pi) {
      if (scanned.count(*_pi)) continue;
      scanned.insert(*_pi);
      preds.push_back(*_pi);
    }
    Value* possible_value = NULL;
    while (!preds.empty()) {
      BasicBlock* bb = preds.front();
      preds.pop_front();
      BasicBlock::iterator bi = bb->end(), bf = bb->begin();
      bool terminated = false;
      do {
        --bi;
        CallInst *ci2;
        if (!(ci2 = dyn_cast<CallInst>(bi))) continue;
        std::string cin = ci2->getCalledFunction()->getName().str();
        // if we've "wrapped around" and we're a peek we still might
        // have a chance (pops have different values each time)
        if (cin == "peek" && ci2 == ci) {
          terminated = true;
        // can't do anything across stack ops that aren't push
        } else if (cin=="roll" || cin=="pop") return false;
        else if (cin == "push") {
          if (possible_value == NULL) {
            possible_value = ci2->getArgOperand(0);
            terminated = true;
          } else if (possible_value == ci2->getArgOperand(0)) {
            terminated = true;
          } else {
            ConstantInt *cia, *cib;
            if (!(cia = dyn_cast<ConstantInt>(possible_value)) ||
                !(cib = dyn_cast<ConstantInt>(ci2->getArgOperand(0)))) {
              return false;
            }
            if (cia->getValue() == cib->getValue()) {
              terminated = true;
            } else {
              return false;
            }
          }
        }
      } while (!terminated && bi != bf);
      if (!terminated) {
        if (&bb->getParent()->getEntryBlock() == bb) {
          return false;
        }
        pred_iterator pi = pred_begin(bb), pe = pred_end(bb);
        for(; pi != pe; ++pi) {
          if (scanned.count(*pi)) continue;
          scanned.insert(*pi);
          preds.push_back(*pi);
        }
      }
    }
    if (possible_value == NULL) return false;
    ci->replaceAllUsesWith(possible_value);
    return true;
  }
};

// this is a generic pass which cleans up extra cruft from the module
//   - If the results of none of the calls to pop and peek are used,
//     then we delete all calls to them.
//   - If there are no pops and peeks called in the function,
//     then all push and roll calls are deleted.
//   - We internalize all functions that are not main
//   - We remove noinline from all functions that are not main
struct ModCleanupPass : public ModulePass {
  static char ID;
  ModCleanupPass() : ModulePass(ID) { }

  virtual bool runOnModule(Module& m) {
    unsigned peeks = countFunctionUses(m, "peek"),
        pops = countFunctionUses(m, "pop"),
        pops_used = countFunctionReturnUses(m, "pop"),
        peeks_used = countFunctionReturnUses(m, "peek");
    bool res = false;
    // WARNING THIS COULD GET RID OF WARNING MESSAGES so this is unsafe
    if (pops_used == 0 && peeks_used == 0) {
      deleteAllCalls(m, "pop");
      deleteAllCalls(m, "peek");
      peeks = 0;
      pops = 0;
      res = true;
    }
    if (peeks == 0 && pops == 0) {
      deleteAllCalls(m, "push");
      deleteAllCalls(m, "roll");
      res = true;
    }
    Module::iterator b = m.begin(), e = m.end();
    for (; b != e; ++b) {
      if (b->getName().str() != "main" && !b->isDeclaration()) {
        b->setLinkage(Function::InternalLinkage);
        res |= true;
        if (b->getName().str().find("codel") == 0) {
          b->removeFnAttr(Attribute::NoInline);
        }
      }
    }
    return res;
  }

  static unsigned countFunctionUses(Module& m, std::string n) {
    Function* f = m.getFunction(n);
    return f->getNumUses();
  }

  static unsigned countFunctionReturnUses(Module& m, std::string n) {
    Function *f = m.getFunction(n);
    Function::use_iterator ui(f->use_begin()), ue(f->use_end());
    unsigned count = 0;
    for (; ui != ue; ++ui) {
      CallSite CS(*ui);
      Instruction* i = CS.getInstruction();
      if (!i->use_empty()) count++;
    }
    return count;
  }

  static void deleteAllCalls(Module& m, std::string n) {
    Function* f = m.getFunction(n);
    while (!f->use_empty()) {
      CallSite CS(f->use_back());
      CS.getInstruction()->eraseFromParent();
    }
  }
};

}

char PushPopMergePass::ID = 0;
char InterBlockMergePass::ID = 0;
char InterBlockMergePass2::ID = 0;
char InterFuncMovePass::ID = 0;
char ModCleanupPass::ID = 0;

static RegisterPass<PushPopMergePass> X("pushpopmerge", "Piet PushPopMerge Pass", false, false);
static RegisterPass<InterBlockMergePass> Y("interblockmerge", "Piet InterBlock merge pass", false, false);
static RegisterPass<InterBlockMergePass2> W("interblockmerge2", "Piet InterBlock merge pass two", false, false);
static RegisterPass<InterFuncMovePass> Z("interfuncmove", "Piet interfunc move pass", false, false);
static RegisterPass<ModCleanupPass> A("modcleanup", "Piet module cleanup", false, false);
