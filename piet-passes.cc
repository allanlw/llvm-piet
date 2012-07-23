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

using namespace llvm;

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
          return this->_doMove(&*b, a);
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
    NF->takeName(&*b);

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

    Function::arg_iterator in = NF->arg_begin(),
        io = b->arg_begin(), eo = b->arg_end();
    for(; io != eo; ++io, ++in) {
      io->replaceAllUsesWith(in);
    }
    a->replaceAllUsesWith(in);
    a->eraseFromParent();

    b->eraseFromParent();
    return this->_runOnModule(*m, true);
  }
};

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
  bool _runOnFunction(Function &fn, bool ret = false) {
    if (fn.getName().str().find("codel") != 0) {
      return false;
    }
    Function::iterator end = fn.end();
    /* Find basic blocks in this function that start with a pop or peek. */
    for (Function::iterator it = fn.begin(); it != end; ++it) {
      BasicBlock::iterator it2 = it->begin();
      CallInst *a;
      if (!(a = dyn_cast<CallInst>(it2))) {
        continue;
      }
      std::string name = a->getCalledFunction()->getName().str();
      if (name != "pop" && name != "peek") {
        continue;
      }
      pred_iterator end3 = pred_end(it), it3=pred_begin(it);
      if (it3 == end3) {
        continue;
      }
      bool flag = false;
      for (; it3 != end3; ++it3) {
        Instruction* ti = (*it3)->getTerminator();
        if (ti == 0 || &(*it3)->front() == ti) {
          flag = true;
          break;
        }
        Instruction* bti = ti->getPrevNode();
        CallInst *b;
        if (bti == 0 || !(b = dyn_cast<CallInst>(bti))) {
          flag = true;
          break;
        }
        std::string name2 = b->getCalledFunction()->getName().str();
        if (name2 != "push") {
          flag = true;
          break;
        }
      }
      if (flag) {
        continue;
      }
      PHINode* p = PHINode::Create(a->getType());
      for (it3 = pred_begin(it); it3 != end3; ++it3) {
        TerminatorInst* ti = (*it3)->getTerminator();
        CallInst *push_f = cast<CallInst>(ti->getPrevNode());
        p->addIncoming(push_f->getArgOperand(0), push_f->getParent());
        // if the predecessor has other successors we make a intermediate node.
        succ_iterator suc_b = succ_begin(*it3), suc_e = succ_end(*it3);
        for (; suc_b != suc_e; ++suc_b) {
          if (*suc_b == &*it) {
            continue;
          }
          BasicBlock* newInter = BasicBlock::Create(it->getContext(), "", &fn);
          IRBuilder<> bu(newInter);
          PHINode* interPHI = bu.CreatePHI(p->getType());
          interPHI->addIncoming(push_f->getArgOperand(0), *it3);
          bu.CreateCall(push_f->getCalledFunction(), interPHI);
          bu.CreateBr(*suc_b);
          ti->setSuccessor(suc_b.getSuccessorIndex(), newInter);
        }
        push_f->eraseFromParent();
      }
      p->insertBefore(a);
      a->replaceAllUsesWith(p);
      a->eraseFromParent();
      if (name == "peek") {
        CallInst::Create(fn.getParent()->getFunction("push"),
            p, "", p->getNextNode());
      }
      return _runOnFunction(fn, true);
    }
    return ret;
  }
};

struct PushPopMergePass : public BasicBlockPass {
  static char ID;
  PushPopMergePass() : BasicBlockPass(ID) {}

  virtual bool runOnBasicBlock(BasicBlock &BB) {
    return this->_runOnBasicBlock(BB);
  }

  void getAnalysisUsage(AnalysisUsage &info) const {
//    info.setPreservesCFG();
//    info.addRequired<InterBlockMergePass>();
  }

private:
  bool _runOnBasicBlock(BasicBlock &BB, bool ret = false) {
    if (BB.getParent()->getName().str().find("codel") != 0) {
      return false;
    }
    BasicBlock::iterator end = BB.end();
    CallInst *a, *b;
    for (BasicBlock::iterator it = BB.begin(); it != end; ++it) {
      if (!(a = dyn_cast<CallInst>(it))) {
/*
        CastInst* c = dyn_cast<CastInst>(it);
        if (!c) {
          continue;
        }
        if (!(c->isIntegerCast() &&
            cast<IntegerType>(c->getSrcTy())->getBitWidth() == 1)) {
          continue;
        }
        Value* vals[] = {ConstantInt::get(c->getType(), 0),
            ConstantInt::get(c->getType(), 2)};
        c->setMetadata("range", MDNode::get(c->getContext(), vals, 2));
*/
        continue;
      }
      std::string name1 = a->getCalledFunction()->getName().str();
      if (name1 == "roll") {
        ConstantInt *depth_c, *times_c;
        if (!(depth_c = dyn_cast<ConstantInt>(a->getArgOperand(0))) ||
            !(times_c = dyn_cast<ConstantInt>(a->getArgOperand(1)))) {
          continue;
        }
        APInt depth = depth_c->getValue();
        APInt times = times_c->getValue();
        if (depth.isNegative() || !depth.getBoolValue()) continue;
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
        return _runOnBasicBlock(BB, true);
      } else if (name1 == "printf") {
        size_t i;
        bool flag = (a->getNumArgOperands()>1);
        for (i = 1; i < a->getNumArgOperands(); i++) {
          flag &= isa<ConstantInt>(a->getArgOperand(i));
        }
        if (flag) {
          GEPOperator* gepo = cast_or_null<GEPOperator>(a->getArgOperand(0));
          GlobalVariable* gv = cast_or_null<GlobalVariable>(gepo->
              getPointerOperand());
          ConstantArray* ca = cast_or_null<ConstantArray>(gv->getInitializer());
          if (ca) {
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
            bu.CreateCall(BB.getParent()->getParent()->getFunction("printf"),
                v1);
            a->eraseFromParent();
            return _runOnBasicBlock(BB, true);
          }
        }
        /* explicit fall through...*/
      } else if (name1 != "push" && name1 != "pop" && name1 != "peek"
          && name1 != "debug") continue;
      BasicBlock::iterator it2(it); ++it2;
      for (; it2 != end; ++it2) {
        if (!(b = dyn_cast<CallInst>(it2))) continue;
        std::string name2 = b->getCalledFunction()->getName().str();
        if (name1 == "push") {
          if (name2 == "pop") {
            b->replaceAllUsesWith(a->getArgOperand(0));
            b->eraseFromParent();
            a->eraseFromParent();
            return _runOnBasicBlock(BB, true);
          } else if (name2 == "peek") {
            b->replaceAllUsesWith(a->getArgOperand(0));
            b->eraseFromParent();
            return _runOnBasicBlock(BB, true);
          }
        } else if (name1 == "pop") {
          if (name2 == "push" && a == b->getArgOperand(0)) {
            a->setCalledFunction(BB.getParent()->getParent()->getFunction("peek"));
            b->eraseFromParent();
            return _runOnBasicBlock(BB, true);
          }
        } else if (name1 == "peek") {
          if (name2 == "pop") {
            b->moveBefore(a);
            a->replaceAllUsesWith(b);
            a->eraseFromParent();
            return _runOnBasicBlock(BB, true);
          } else if (name2 == "peek") {
            b->replaceAllUsesWith(a);
            b->eraseFromParent();
            return _runOnBasicBlock(BB, true);
          }
        } else if ((name1 == "printf" && name2 == "printf") ||
            (name1 == "debug" && name2 == "debug")) {
          GEPOperator *s1, *s2;
          if (!(s1 = dyn_cast<GEPOperator>(a->getArgOperand(0))) ||
              !(s2 = dyn_cast<GEPOperator>(b->getArgOperand(0)))) {
            break;
          }
          GlobalVariable *f1, *f2;
          if (!(f1 = dyn_cast<GlobalVariable>(s1->getPointerOperand())) ||
              !(f2 = dyn_cast<GlobalVariable>(s2->getPointerOperand()))) {
            break;
          }
          ConstantArray *a1, *a2;
          if (!(a1 = dyn_cast<ConstantArray>(f1->getInitializer())) ||
              !(a2 = dyn_cast<ConstantArray>(f2->getInitializer()))) {
            break;
          }
          std::string nc = a1->getAsString();
          nc.resize(nc.size()-1);
          if (name1 == "debug") nc.append("+");
          nc.append(a2->getAsString());
          nc.resize(nc.size()-1);
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
          bu.CreateCall(BB.getParent()->getParent()->getFunction(name1),
              arguments.begin(), arguments.end());
          a->eraseFromParent();
          b->eraseFromParent();
          return _runOnBasicBlock(BB, true);
        } else if (name1 == "debug" && name2 != "debug") {
          break;
        }
        if (name2 != "printf" && name2 != "scanf" && name2 != "debug") {
          break;
        }
      }
    }
    return ret;
  }
};

char PushPopMergePass::ID = 0;
char InterBlockMergePass::ID = 0;
char InterFuncMovePass::ID = 0;

static RegisterPass<PushPopMergePass> X("pushpopmerge", "Piet PushPopMerge Pass", false, false);
static RegisterPass<InterBlockMergePass> Y("interblockmerge", "Piet InterBlock merge pass", false, false);
static RegisterPass<InterFuncMovePass> Z("interfuncmove", "Piet interfunc move pass", false, false);

extern "C" void AddPushPopMergePass(PyObject* o) {
  LLVMPassManagerRef arg1 = (LLVMPassManagerRef)PyCObject_AsVoidPtr(o);
  PassManager *pmp = reinterpret_cast<llvm::PassManager*>(arg1);

  pmp->add(new PushPopMergePass());
  pmp->add(new InterBlockMergePass());
  pmp->add(new InterFuncMovePass());
}
