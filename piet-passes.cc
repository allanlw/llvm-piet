// Python includes
#include <Python.h>

// LLVM includes
#include <llvm/Pass.h>
#include <llvm/Support/CFG.h>
#include <llvm/Function.h>
#include <llvm/Instructions.h>
#include <llvm/BasicBlock.h>
#include <llvm/PassManager.h>
#include <llvm/Support/IRBuilder.h>
#include <llvm/Support/CallSite.h>
#include <llvm/PassManagers.h>
#include <llvm/Constants.h>

// LLVM-C includes
#include <llvm-c/Core.h>

#include <iostream>
#include <sstream>
#include <list>
#include <vector>
#include <set>
#include <deque>

using namespace llvm;

static bool isText(std::string t) {
  return (t=="printf"||t=="puts"||t=="putc"||t=="debug"||t=="scanf");
}

// This pass finds pops and peeks in the entry block of functions
// and moves them to the call site with the result being passed as a parameter
// to the function
struct InterFuncMovePass : public ModulePass {
  static char ID;
  InterFuncMovePass() : ModulePass(ID) {}

  virtual bool runOnModule(Module& m) {
    return this->_runOnModule(m);
  }

  void getAnalysisUsage(AnalysisUsage &info) const {
  }

private:
  bool _runOnModule(Module& m, bool ret = false) {
    Module::iterator b = m.begin(), e = m.end();
    for (; b != e; ++b) {
      if (b->getName().str().find("codel") != 0) {
        continue;
      }
      BasicBlock& bb = b->getEntryBlock();
      BasicBlock::iterator bb_i = bb.begin(), bb_e = bb.end();
      for (; bb_i != bb_e; ++bb_i) {
        CallInst *a;
        if (!(a = dyn_cast<CallInst>(bb_i))) {
          continue;
        }
        std::string name = a->getCalledFunction()->getName().str();
        // heavily inspired by DeadArgElimination.cpp
        if (name == "peek" || name == "pop") {
          return this->_doMove(b, a);
        } else if (name == "roll" || name == "push") {
          break;
        }
      }
    }
    return ret;
  }

  bool _doMove(Function* b, CallInst* a) {
    Module* m = b->getParent();
    std::string name = a->getCalledFunction()->getName().str();
    const FunctionType* Fty = b->getFunctionType();

    std::vector<const Type*> params(Fty->param_begin(), Fty->param_end());
    params.push_back(a->getType());
    FunctionType* NFty = FunctionType::get(Fty->getReturnType(),
       params, false);

    Function *NF = Function::Create(NFty, b->getLinkage());

    NF->copyAttributesFrom(b);
    NF->takeName(b);

    m->getFunctionList().insert(b, NF);

    while (!b->use_empty()) {
      CallSite CS(b->use_back());
      Instruction* call = CS.getInstruction();
      std::vector<Value*> args(CS.arg_begin(), CS.arg_end());

      CallInst* p = CallInst::Create(m->getFunction(name));
      p->insertBefore(call);
      args.push_back(p);

      CallInst* N = CallInst::Create(NF, args.begin(), args.end());
      N->setCallingConv(CS.getCallingConv());
      N->setTailCall(cast<CallInst>(CS.getInstruction())->isTailCall());
      N->insertBefore(call);

      call->replaceAllUsesWith(N);
      call->eraseFromParent();
    }
    NF->getBasicBlockList().splice(NF->begin(), b->getBasicBlockList());

    Function::arg_iterator in = NF->arg_begin(), io = b->arg_begin(),
        eo = b->arg_end();
    for(; io != eo; ++io, ++in) {
      io->replaceAllUsesWith(in);
    }
    a->replaceAllUsesWith(in);
    a->eraseFromParent();

    b->eraseFromParent();
    return this->_runOnModule(*m, true);
  }
};

// This pass does basic 'peephole' optimization of basic blocks
// merging obviously redundant stack operations and text operations
struct PushPopMergePass : public BasicBlockPass {
  static char ID;
  PushPopMergePass() : BasicBlockPass(ID) {}

  virtual bool runOnBasicBlock(BasicBlock &BB) {
    return this->_runOnBasicBlock(BB);
  }

  void getAnalysisUsage(AnalysisUsage &info) const {
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
      if (name1 == "roll") {
        if (unrollRoll(a)) {
          return _runOnBasicBlock(BB, true);
        } else {
           continue;
        }
      } else if (name1 == "printf") {
        if (reducePrintf(a)) {
          return _runOnBasicBlock(BB, true);
        }
        /* explicit fall through...*/
      } else if (name1 == "peek") {
        if (a->use_empty()) {
          a->eraseFromParent();
          return _runOnBasicBlock(BB, true);
        }
        /* explicit fall through...*/
      } else if (name1 != "push" && name1 != "pop" && name1 != "debug")
        continue;
      BasicBlock::iterator it2(it); ++it2;
      for (; it2 != end; ++it2) {
        if (!(b = dyn_cast<CallInst>(it2))) {
          ReturnInst* d;
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
        } else if ((isText(name2) && isText(name1)) || !isText(name2)) {
          break;
        }
      }
    }
    return ret;
  }

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
  // return true if it did, false if it didn't
  static bool reducePrintf(CallInst* a) {
    BasicBlock& BB = *a->getParent();
    size_t i;
    bool flag = (a->getNumArgOperands()>1);
    for (i = 1; i < a->getNumArgOperands(); i++) {
      flag &= isa<ConstantInt>(a->getArgOperand(i));
    }
    if (!flag) return false;
    GEPOperator* gepo = cast_or_null<GEPOperator>(a->getArgOperand(0));
    GlobalVariable* gv = cast_or_null<GlobalVariable>(gepo->
        getPointerOperand());
    ConstantArray* ca = cast_or_null<ConstantArray>(gv->getInitializer());
    if (!ca) return false;
    std::string format(ca->getAsString());
    format.resize(format.size()-1);
    for(i = 1; i < a->getNumArgOperands(); i++) {
      ConstantInt* d = dyn_cast<ConstantInt>(a->getArgOperand(i));
      size_t i = format.find("%");
      if (format[i+1] == 'c') {
        format.replace(i, 2, std::string(1,
           (char)d->getValue().getSExtValue()));
      } else {
        format.replace(i, 2, d->getValue().toString(10, true));
      }
    }
    IRBuilder<> bu(&BB, a);
    Value* v1 = bu.CreateGlobalStringPtr(format.c_str());
    bu.CreateCall(a->getCalledFunction(), v1);
    a->eraseFromParent();
    return true;
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
    Value* st = bu.CreateGlobalStringPtr(nc.c_str());
    std::vector<Value*> arguments;
    arguments.push_back(st);
    size_t i;
    for (i = 1; i < a->getNumArgOperands(); i++) {
      arguments.push_back(a->getArgOperand(i));
    }
    for (i = 1; i < b->getNumArgOperands(); i++) {
      arguments.push_back(b->getArgOperand(i));
    }
    bu.CreateCall(a->getCalledFunction(), arguments.begin(), arguments.end());
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

// this function find predecessors with pushes and moves them into itself
struct InterBlockMergePass : public FunctionPass {
  static char ID;
  InterBlockMergePass() : FunctionPass(ID) {}

  virtual bool runOnFunction(Function& fn) {
    return this->_runOnFunction(fn);
  }

  void getAnalysisUsage(AnalysisUsage &info) const {
//    info.addRequired<InterFuncMovePass>();
  }

private:
  // returns true if this basic block is a candidate for cross-block
  // call merging using phis
  bool basicBlockIsOperable(BasicBlock& bb) {
    CallInst *a;
    if (!(a = dyn_cast<CallInst>(bb.getFirstNonPHI()))) {
      return false;
    }
    std::string name = a->getCalledFunction()->getName().str();
    if (name != "peek" && name != "pop" && name != "roll") {
      return false;
    }
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
      // loop to see if the first call site is a push
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
          getReturnType());
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
        // if the predecessor has other successors we make a intermediate node.
        succ_iterator suc_b = succ_begin(*it3), suc_e = succ_end(*it3);
        for (; suc_b != suc_e; ++suc_b) {
          if (*suc_b == &*it) {
            continue;
          }
          BasicBlock* newInter = BasicBlock::Create(it->getContext(), "", &fn);
          IRBuilder<> bu(newInter);
          bu.CreateCall(fn.getParent()->getFunction("push"),
              p->getIncomingValue(p->getBasicBlockIndex(*it3)));
          bu.CreateBr(*suc_b);
          ti->setSuccessor(suc_b.getSuccessorIndex(), newInter);
          // replace phi uses
          // taken from BasicBlock.cpp
          for (BasicBlock::iterator II = suc_b->begin(),
              IE = suc_b->end(); II != IE; ++II) {
            PHINode *PN = dyn_cast<PHINode>(II);
            if (!PN)
              break;
            int i;
            while ((i = PN->getBasicBlockIndex(*it3)) >= 0)
              PN->setIncomingBlock(i, newInter);
          }
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
    bool isPeek = (ci->getCalledFunction()->getName().str()=="peek");
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
        if (ci == ci2) {
          if (isPeek) terminated = true;
          else return false;
        // can't do anything across stack ops that aren't push
        } else if (cin=="roll" || cin=="pop" || cin=="peek") return false;
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

struct ModCleanupPass : public ModulePass {
  static char ID;
  ModCleanupPass() : ModulePass(ID) { }

  virtual bool runOnModule(Module& m) {
    unsigned peeks = countFunctionUses(m, "peek"),
        pops = countFunctionUses(m, "pop"),
        pushes = countFunctionUses(m, "push"),
        rolls = countFunctionUses(m, "roll");
    bool res = false;
    if (peeks == 0 && pops == 0 && rolls == 0) {
      deleteAllCalls(m, "push");
      res = true;
    }
    return res;
  }

  static unsigned countFunctionUses(Module& m, std::string n) {
    Function* f = m.getFunction(n);
    return f->getNumUses();
  }

  static void deleteAllCalls(Module& m, std::string n) {
    Function* f = m.getFunction(n);
    while (!f->use_empty()) {
      CallSite CS(f->use_back());
      CS.getInstruction()->eraseFromParent();
    }
  }
};

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

extern "C" void AddPushPopMergePass(PyObject* o) {
  LLVMPassManagerRef arg1 = (LLVMPassManagerRef)PyCObject_AsVoidPtr(o);
  PassManager *pmp = reinterpret_cast<llvm::PassManager*>(arg1);

  pmp->add(new PushPopMergePass());
  pmp->add(new InterBlockMergePass());
  pmp->add(new InterBlockMergePass2());
  pmp->add(new InterFuncMovePass());
  pmp->add(new ModCleanupPass());
}
