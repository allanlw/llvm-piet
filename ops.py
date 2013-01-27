# Copyright (c) 2012, Allan Wirth <allan@allanwirth.com>
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
#  * Redistributions of source code must retain the above copyright notice,
#    this list of conditions and the following disclaimer.
#
#  * Redistributions in binary form must reproduce the above copyright notice,
#    this list of conditions and the following disclaimer in the documentation
#    and/or other materials provided with the distribution.
#
#  * Neither the name of this software, nor the names of its
#    contributors may be used to endorse or promote products derived from
#    this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

"""This python file generates the llvm for the intrinsic piet operations. We
use it instead of a (e.g. a C file) because we want to use special llvm types.

In retrospect I probably should have just written llvm IR assembly by hand
but I didn't so I'm including this generation file.

It is a must that these functions get inlined for optimization."""

import os

import llvm
import llvm.core
from llvm.core import Builder, Type, Constant, GlobalVariable

import colors

_externs = [
  (Type.function(Type.int(), [Type.pointer(Type.int(8))],True), "printf"),
  (Type.function(Type.int(), [Type.pointer(Type.int(8))],True), "scanf")
]

def setup_module():
  """Perform a very crude linking step to get boilerplate code."""
  p = os.path.join(os.path.split(os.path.realpath(__file__))[0], "pushpop.ll")
  mod = llvm.core.Module.from_assembly(open(p, "r"))
  for function in mod.functions:
    if function.is_declaration:
      continue
    function.linkage = llvm.core.LINKAGE_EXTERNAL
    function.add_attribute(llvm.core.ATTR_NO_INLINE)
  for e in _externs:
    mod.add_function(*e).linkage = llvm.core.LINKAGE_EXTERNAL
  str_const(mod, "%d", "pd")
  str_const(mod, "%c", "pc")
  for x in colors.Ops:
    globals()[x](mod)
  return mod

# decorators for function generation

def noret(pfn):
  return lambda m : pfn(m, Type.void())

def intret(pfn):
  return lambda m : pfn(m, llvm.core.Type.int())

def boolret(pfn):
  return lambda m: pfn(m, llvm.core.Type.int(1))

def quadret(pfn):
  return lambda m: pfn(m, llvm.core.Type.int(2))

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
    fn.linkage = llvm.core.LINKAGE_INTERNAL
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

def noinline(pfn):
  def wrapper(m, fn, *args):
    fn.remove_attribute(llvm.core.ATTR_ALWAYS_INLINE)
    pfn(m, fn, *args)
  wrapper.__name__ = pfn.__name__
  return wrapper


# definitions for actual piet function intrinsics

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
  return t4

@simple
def DUP(m, fn, bb, bu, pu, po):
  t1 = bu.call(m.get_function_named("peek"), [])
  bu.call(pu, [t1])

@simple
def NOT(m, fn, bb, bu, pu, po):
  t1 = bu.call(po, [], "t1")
  t2 = bu.icmp(llvm.core.ICMP_EQ, t1, llvm.core.Constant.int(t1.type, 0))
  t4 = bu.zext(t2, t1.type)
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
@noinline
def INN(m, fn, bb, bu, pu, po):
  bu.call(pu, [scanf(m, bu, "pd")])

@simple
@noinline
def INC(m, fn, bb, bu, pu, po):
  bu.call(pu, [scanf(m, bu, "pc")])

@simple
def ROL(m, fn, bb, bu, pu, po):
  a = bu.call(po, [])
  b = bu.call(po, [])
  ro = m.get_function_named("roll")
  bu.call(ro, [b, a])

@quadret
@noarg
@build
@pushpop
def DP(m, fn, bb, bu, pu, po):
  t1 = bu.call(po, [])
  t2 = bu.srem(t1, llvm.core.Constant.int(t1.type, 4))
  t3 = bu.trunc(t2, llvm.core.Type.int(2))
  bu.ret(t3)

@boolret
@boolarg
@build
@pushpop
def SW(m, fn, bb, bu, pu, po):
  t1 = bu.call(po, [])
  t4 = bu.zext(fn.args[0], t1.type)
  boolt = llvm.core.Type.int(1)
  t4 = bu.select(fn.args[0], llvm.core.Constant.int(boolt, 1),
      llvm.core.Constant.int(boolt, 0))
  bu.ret(t4)


# Basically just helper macros

def str_const(mod, s, n):
  c = Constant.stringz(s)
  v = GlobalVariable.new(mod, c.type, n)
  v.initializer = c
  v.linkage = llvm.core.LINKAGE_INTERNAL
  v.global_constant = True
  return v

def printf(mod, bu, s, *args):
  v = mod.get_global_variable_named(s)
  pf = mod.get_function_named("printf")
  t = bu.gep(v, [Constant.int(Type.int(), 0), Constant.int(Type.int(), 0)])
  bu.call(pf, (t,)+(args))

def debug(mod, bu, inst, codel):
  s = "{0}(X:{1} Y:{2} E:{3})".format(inst, *codel)
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

if __name__ == "__main__":
  print setup_module()
