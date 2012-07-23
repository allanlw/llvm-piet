import os

import llvm
import llvm.core
from llvm.core import Builder, Type, Constant

import colors

_externs = [
#  (Type.function(Type.int(), [Type.int()]), "putchar"),
  (Type.function(Type.int(), [Type.pointer(Type.int(8))],True), "printf"),
#  (Type.function(Type.int(), []), "getchar"),
  (Type.function(Type.int(), [Type.pointer(Type.int(8))], True), "scanf")
]

# stack type
stack = Type.struct([Type.int(), Type.pointer(Type.int())])

def str_const(mod, s, n):
  v = mod.add_global_variable(Type.array(Type.int(8), len(s)+1), n)
  v.initializer = Constant.stringz(s)
  v.linkage = llvm.core.LINKAGE_INTERNAL
  return v

def printf(mod, bu, s, *args):
  v = mod.get_global_variable_named(s)
  pf = mod.get_function_named("printf")
  t = bu.gep(v, [Constant.int(Type.int(), 0), Constant.int(Type.int(), 0)])
  bu.call(pf, (t,)+(args))

def debug(mod, bu, inst, codel):
  s = "{0}(X:{1} Y:{2} E:{3} OC:{5} NC:{6} S:{4})".format(inst, *codel)
  v = str_const(mod, s, s)
  d = mod.get_function_named("debug")
  t = bu.gep(v, [Constant.int(Type.int(), 0), Constant.int(Type.int(), 0)])
  bu.call(d, [t])

def scanf(mod, bu, s):
  v = mod.get_global_variable_named(s)
  sf = mod.get_function_named("scanf")

  t1 = bu.alloca(Type.int())
  t2 = bu.gep(v, [Constant.int(Type.int(), 0), Constant.int(Type.int(), 0)])
  bu.call(sf, [t2, t1])
  return bu.load(t1)

def setup_module():
  p = os.path.join(os.path.split(os.path.realpath(__file__))[0], "pushpop.ll")
  mod = llvm.core.Module.from_assembly(open(p, "r"))
  for function in mod.functions:
    if function.is_declaration:
      continue
    function.linkage = llvm.core.LINKAGE_INTERNAL
    function.add_attribute(llvm.core.ATTR_NO_INLINE)
#    function.calling_convention = llvm.core.CC_FASTCALL
#  for g in mod.global_variables:
#    g.linkage = llvm.core.LINKAGE_PRIVATE
  for e in _externs:
    mod.add_function(*e)
  str_const(mod, "%d", "pd")
  str_const(mod, "%c", "pc")
  for x in colors.Ops:
    globals()[x](mod)
  return mod

def noret(pfn):
  return lambda m : pfn(m, Type.void())

def intret(pfn):
  return lambda m : pfn(m, llvm.core.Type.int())

def boolret(pfn):
  return lambda m: pfn(m, llvm.core.Type.int(1))

def noarg(pfn):
  return lambda m, t : pfn(m, t, [])

def intarg(pfn):
  return lambda m, t: pfn(m, t, [llvm.core.Type.int()])

def boolarg(pfn):
  return lambda m, t : pfn(m, t, [llvm.core.Type.int(1)])

def build(pfn):
  def wrapper(m, r, a):
    t = llvm.core.Type.function(r, a)
    fn = m.add_function(t, pfn.__name__)
    fn.add_attribute(llvm.core.ATTR_ALWAYS_INLINE)
#    fn.add_attribute(llvm.core.ATTR_INLINE_HINT)
#    fn.add_attribute(llvm.core.ATTR_NO_INLINE)
    fn.linkage = llvm.core.LINKAGE_INTERNAL
#    fn.calling_convention = llvm.core.CC_FASTCALL
    bb = fn.append_basic_block("entry")
    builder = Builder.new(bb)
    pfn(m, fn, bb, builder)
  return wrapper

def pushpop(pfn):
  def wrapper(m, fn, bb, bu):
    pop = m.get_function_named("pop")
    push = m.get_function_named("push")
    pfn(m, fn, bb, bu, push, pop)
  wrapper.__name__ = pfn.__name__
  return wrapper

def binaryop(pfn):
  def wrapper(m, fn, bb, bu, pu, po):
    t1 = bu.call(po, [])
    t2 = bu.call(po, [])
    t3 = pfn(m, fn, bb, bu, t1, t2)
    bu.call(pu, [t3])
    bu.ret_void()
  wrapper.__name__ = pfn.__name__
  return noret(noarg(build(pushpop(wrapper))))

def simple(pfn):
  def wrapper(m, fn, bb, bu, pu, po):
    pfn(m, fn, bb, bu, pu, po)
    bu.ret_void()
  wrapper.__name__ = pfn.__name__
  return noret(noarg(build(pushpop(wrapper))))

@noret
@noarg
@build
def NOP(m, fn, bb, bu):
  bu.ret_void()

@noret
@noarg
@build
def EXIT(m, fn, bb, bu):
  bu.ret_void()

@binaryop
def ADD(m, fn, bb, bu, t1, t2):
  return bu.add(t1, t2, "t3")

@binaryop
def SUB(m, fn, bb, bu, t1, t2):
  return bu.sub(t2, t1, "t3")

@binaryop
def MUL(m, fn, bb, bu, t1, t2):
  return bu.mul(t1, t2, "t3")

@binaryop
def DIV(m, fn, bb, bu, t1, t2):
  return bu.sdiv(t2, t1, "t3")

@binaryop
def MOD(m, fn, bb, bu, t1, t2):
  return bu.srem(t2, t1, "t3")

@binaryop
def GT(m, fn, bb, bu, t1, t2):
  t3 = bu.icmp(llvm.core.ICMP_SGT, t2, t1, "t3")
  t4 = bu.zext(t3, llvm.core.Type.int())
#  t4 = bu.select(t3, llvm.core.Constant.int(t1.type, 1),
#      llvm.core.Constant.int(t1.type, 0))
  return t4

@simple
def DUP(m, fn, bb, bu, pu, po):
#  t1 = bu.call(po, [], "t1")
#  bu.call(pu, [t1])
  t1 = bu.call(m.get_function_named("peek"), [])
  bu.call(pu, [t1])

@simple
def NOT(m, fn, bb, bu, pu, po):
  t1 = bu.call(po, [], "t1")
  t2 = bu.icmp(llvm.core.ICMP_EQ, t1, llvm.core.Constant.int(t1.type, 0))
  t4 = bu.zext(t2, t1.type)
#  t4 = bu.select(t2, llvm.core.Constant.int(t1.type, 1),
#      llvm.core.Constant.int(t1.type, 0))
  bu.call(pu, [t4])

@intret
@noarg
@build
def POP(m, fn, bb, bu):
  po = m.get_function_named("pop")
  t = bu.call(po, [])
  bu.ret(t)

@noret
@intarg
@build
def PUSH(m, fn, bb, bu):
  pu = m.get_function_named("push")
  bu.call(pu, [fn.args[0]])
  bu.ret_void()

@simple
def OUN(m, fn, bb, bu, pu, po):
  printf(m, bu, "pd", bu.call(po, []))

@simple
def OUC(m, fn, bb, bu, pu, po):
  printf(m, bu, "pc", bu.call(po, []))

@simple
def INN(m, fn, bb, bu, pu, po):
  bu.call(pu, [scanf(m, bu, "pd")])

@simple
def INC(m, fn, bb, bu, pu, po):
  bu.call(pu, [scanf(m, bu, "pc")])

@simple
def ROL(m, fn, bb, bu, pu, po):
  a = bu.call(po, [])
  b = bu.call(po, [])
  ro = m.get_function_named("roll")
  bu.call(ro, [b, a])

@intret
@noarg
@build
@pushpop
def DP(m, fn, bb, bu, pu, po):
  t1 = bu.call(po, [])
  t2 = bu.srem(t1, llvm.core.Constant.int(t1.type, 4))
  bu.ret(t2)

@boolret
@boolarg
@build
@pushpop
def SW(m, fn, bb, bu, pu, po):
  t1 = bu.call(po, [])
  t4 = bu.zext(fn.args[0], t1.type)
#  t4 = bu.select(fn.args[0], llvm.core.Constant.int(t1.type, 1),
#      llvm.core.Constant.int(t1.type, 0))
  t2 = bu.add(t1, t4)
  t3 = bu.srem(t2, llvm.core.Constant.int(t2.type,2))
  t5 = bu.trunc(t3, llvm.core.Type.int(1))
  bu.ret(t5)
