#!/usr/bin/python2
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

import sys
import operator
import os

import llvm
import llvm.core
from PIL import Image
import PIL.ImageColor
import numpy
import argparse
import subprocess

from colors import *
import ops

LINKAGE = llvm.core.LINKAGE_INTERNAL
CALL_CONV = llvm.core.CC_FASTCALL

PASSES = os.path.join(os.path.split(os.path.realpath(__file__))[0],
    "piet-passes.so")

_dirs_rev = {
  "L" : "R",
  "R" : "L",
  "U" : "D",
  "D" : "U"
}

_dirs_off = {
  "L" : (-1,0),
  "R" : (1, 0),
  "U" : (0, -1),
  "D" : (0, 1)
}

_dirs_comps = {
  "L" : (0, operator.lt),
  "R" : (0, operator.gt),
  "U" : (1, operator.lt),
  "D" : (1, operator.gt)
}

_dirs_cw = ["R", "D", "L", "U"]

class PietBlock:
  def __init__(self, im, pal, x, y):
    self.image = im
    self.codels = []
    self.color = pal[x][y]
    self._calculate(pal, x, y)
  def _calculate(self, pal, x, y):
    queue = [(x, y)]
    hit = set(queue)
    hit.add(None)
    while len(queue)>0:
      k = queue.pop(0)
      self.image.blocks[k] = self
      self.codels.append(k)
      for d in _dirs_rev.keys():
        l = self.image.getrel(d, k[0], k[1])
        if not l in hit:
          hit.add(l)
          if pal[l] == self.color:
            queue.append(l)
  def get_exit(self, dp, cc):
    best = None
    comp = _dirs_comps[dp]
    ccomp = _dirs_comps[self.image.rotcw(dp, 1 if cc else 3)]
    for c in self.codels:
      if best is None:
        best = c
        continue
      # if this codel is more so in the dp direction
      if comp[1](c[comp[0]], best[comp[0]]):
        best = c
      # if this codel is the same in the dp direction
      elif c[comp[0]] == best[comp[0]]:
        # if this codel is better in the cc direction
        if ccomp[1](c[ccomp[0]], best[ccomp[0]]):
          best = c
    return best
  def get_real_exit_loc(self, dp, cc):
    dp_ = dp
    cc_ = cc
    for i in xrange(0, 8):
      ex = self.get_exit(dp_, cc_)
      n = self.image.getrel(dp_, *ex)
      if n is not None and self.image.blocks[n].color != len(Colors)-2:
        return n + (_dirs_rev[dp_],cc_)
      if i % 2 == 0:
        cc_ = (cc_ + 1) % 2
      else:
        dp_ = self.image.rotcw(dp_, 1)      
    return None
    
class PietImage:
  def __init__(self, im):
    self.image = im
    self.make_piet()
  def make_piet(self):
    pixels = self.image.convert("RGB").load()
    pal = numpy.empty(self.image.size, numpy.int8)
    pal.fill(-1)
    self.blocks = numpy.zeros(self.image.size, numpy.object_)
    for x in xrange(self.image.size[0]):
      for y in xrange(self.image.size[1]):
        pal[x,y] = Colors.index(pixels[x, y])
    count = 0
    for x in xrange(self.image.size[0]):
      for y in xrange(self.image.size[1]):
        if not self.blocks[x, y]:
          PietBlock(self, pal, x, y)
          count += 1
  def rotcw(self, d, i):
    return _dirs_cw[(_dirs_cw.index(d)+i)%4]
  def getrel(self, d, x, y):
    r1 = _dirs_off[d]
    r = r1[0]+x, r1[1]+y
    if (r[0] < 0 or r[0] >= self.image.size[0] or
        r[1] < 0 or r[1] >= self.image.size[1]):
      return None
    else:
      return r

  def compile(self, debug):
    ty_int = llvm.core.Type.int()
    ty_cfunc = llvm.core.Type.function(llvm.core.Type.void(),
      [llvm.core.Type.int(1)])
    mod = ops.setup_module()
    didnt_bother = set()
    double_check = {}
    generated = set()

    codel_queue = [(0, 0, "L", None)]

    top = mod.add_function(ty_cfunc, self.get_codel_name(*codel_queue[0][0:3]))
    top.linkage = LINKAGE
    top.add_attribute(llvm.core.ATTR_NO_INLINE)
    top.calling_convention = CALL_CONV

    main = mod.add_function(llvm.core.Type.function(llvm.core.Type.int(),
        []), "main")
    mbb = main.append_basic_block("entry")
    mb = llvm.core.Builder.new(mbb)
    e = mb.call(top, [llvm.core.Constant.int(llvm.core.Type.int(1), 0)])
    e.calling_convention = CALL_CONV
    mb.ret(llvm.core.Constant.int(llvm.core.Type.int(), 0))

    while len(codel_queue):
      codel = codel_queue.pop(0)
      block = self.blocks[codel[0:2]]
      cname = self.get_codel_name(*codel[0:3])

      generated.add(codel[0:3])

      f = mod.get_function_named(cname)
      cc = f.args[0]
      builder = llvm.core.Builder.new(f.append_basic_block("entry"))

      other = self.getrel(codel[2], codel[0], codel[1])
      saved = codel[3]

      bc = block.color
      if other is None:
        if codel == (0, 0, "L", None):
          op = "NOP"
        else:
          print "WHAT THE FUCK ERROR"
          return
        oc = None
      else:
        oc = self.blocks[other].color
        op = get_op(oc, bc)

      if debug:
        ops.debug(mod, builder, op, codel + (oc, bc))

      n_saved = None
      if op in ("NOP", "DUP"):
        n_saved = saved
      if op == "PUSH":
        n_saved = len(self.blocks[other].codels)
        temp = builder.call(mod.get_function_named(op),
            [llvm.core.Constant.int(ty_int, n_saved)])
      elif op == "SW":
        temp = builder.call(mod.get_function_named(op),[cc])
        cc = temp
      else:
        temp = builder.call(mod.get_function_named(op), [])

      type, next = self.get_next_coords(codel[0], codel[1], codel[2], op)
      dont_bother = set()
      blocs = {}
      if saved is not None and saved in range(4) and type == "D":
        do_bother = set()
        for i in range(4):
          for ncodel in next[i][1]:
            if i == saved:
              do_bother.add(ncodel[0:3])
            else:
              # functions called by branches I hope not to take don't need
              # to be generated
              dont_bother.add(ncodel[0:3])
        dont_bother -= do_bother
        # can't do anything if I call myself
        if codel[0:3] in do_bother:
          dont_bother = set()
        elif len(dont_bother):
          # if we renter this function w/o the predicted value
          # we need to generate all those functions
          double_check[codel[0:3]] = [saved, dont_bother]
          didnt_bother |= dont_bother

      if type != "D":
        nexts = next
      else:
        nexts = []
        for x in next:
          nexts.extend(x[1])
      for i,n in enumerate(nexts):
        # if this is the only target, don't make a block
        # if this is a duplicate target, don't remake a block
        if len(next) != 1:
          if n in blocs:
            continue
          blocs[n] = f.append_basic_block("out_{0}".format(i))
          bu = llvm.core.Builder.new(blocs[n])
        else:
          bu = builder
        if n is not None:
          tcname = self.get_codel_name(*n[0:3])
          try:
            to = mod.get_function_named(tcname)
          except llvm.LLVMException:
            to = mod.add_function(ty_cfunc, tcname)
            to.linkage = LINKAGE
            to.calling_convention = CALL_CONV
            # I'm the first codel to want this target, but I don't actually
            # want it so it's just a dummy node
            if n[0:3] not in dont_bother:
              codel_queue.append(n[0:3] + (n_saved,))
          else:
            # if we actually might get there
            if n[0:3] not in dont_bother:
              # if we're going into a function that we made an assumption about
              # that no longer holds true, we need to generate the other places
              # it might go
              # note this can never be me because I can't be in double_check
              # if I am a target
              if n[0:3] in double_check and double_check[n[0:3]][0] != n_saved:
                for x in double_check[n[0:3]][1]:
                  if x not in didnt_bother:
                    continue
                  didnt_bother.remove(x)
                  codel_queue.append(x+(None,))
                del double_check[n[0:3]]
              # if we're going into a function we assumed wouldn't be called
              # we need to generate it
              elif n[0:3] in didnt_bother:
                didnt_bother.remove(n[0:3])
                codel_queue.append(n[0:3]+(None,))
              # this is currently queued for generation so we have to check
              # the generation queue and make sure that the predicted
              # value matches. if it doesn't, then we invalidate both.
              elif n[0:3] not in generated:
                for i in range(len(codel_queue)):
                  if codel_queue[i][0:3] == n[0:3]:
                    if codel_queue[i][3] != n_saved:
                      codel_queue[i] = n[0:3] + (None,)
                    break
          if n[3] == -1:
            ncc = cc
          elif n[3] == -2:
            ncc = bu.add(cc, llvm.core.Constant.int(llvm.core.Type.int(1), 1))
          else:
            ncc = llvm.core.Constant.int(llvm.core.Type.int(1), n[3])
          d = bu.call(to, [ncc])
          d.calling_convention = CALL_CONV
          llvm.core._core.LLVMSetTailCall(d.ptr, True)
        else:
          bu.call(mod.get_function_named("EXIT"), [])
        bu.ret_void()

      if type in ("J", "K"):
        f.add_attribute(llvm.core.ATTR_INLINE_HINT)
      elif type == "D":
        d_cases = []
        for i, (d_type, d_case) in enumerate(next):
          if d_type in ("J", "K"):
            d_cases.append(blocs[d_case[0]])
          else:
            bb2 = f.append_basic_block("case_"+str(i))
            llvm.core.Builder.new(bb2).cbranch(cc, blocs[d_case[1]],
                blocs[d_case[0]])
            d_cases.append(bb2)
        conds = [f.append_basic_block("cond_"+self.rotcw(_dirs_rev[codel[2]], i)) for i in range(3)]
        for i in range(3):
          bu = llvm.core.Builder.new(conds[i])
          t = bu.icmp(llvm.core.ICMP_EQ, temp,
              llvm.core.Constant.int(temp.type, i))
          bu.cbranch(t, d_cases[i], conds[i+1] if i < 2 else d_cases[i+1])
        builder.branch(conds[0])
      elif type == "C":
        builder.cbranch(cc, blocs[nexts[1]], blocs[nexts[0]])
        f.add_attribute(llvm.core.ATTR_INLINE_HINT)
      elif type == "E":
        builder.call(mod.get_function_named("EXIT"), [])
        builder.ret_void()
        f.add_attribute(llvm.core.ATTR_INLINE_HINT)

    for codel in didnt_bother:
      f = mod.get_function_named(self.get_codel_name(*codel))
      bb3 = f.append_basic_block("entry")
      llvm.core.Builder.new(bb3).ret_void()
    return mod
  def get_codel_name(self, x, y, d):
    return "codel_{0}_{1}_{2}".format(x, y, d)
  # Determine where to go after this block
  # return codes: "J" unconditional jump
  # "C" cc conditional jump (anytime)
  # "K" cc relative unconditional (-2 toggle, -1 same)
  # "E" program termination
  # "D" dp+cc conditional exit (after toggle)
  def get_next_coords(self, x, y, d, op):
    if self.blocks[x,y].color == len(Colors)-1: #entered white
      dp = _dirs_rev[d]
      c = x,y
      route = set()
      cc_ = 0
      while True:
        if c + (dp,) in route:
          return "E", [] # program termination
        route.add(c + (dp,))
        n = self.getrel(dp, *c)
        if n is None or self.blocks[n].color == len(Colors)-2:
          dp = self.rotcw(dp, 1)
          cc_ = (cc_+1)%2
        elif self.blocks[n].color == len(Colors)-1:
          c = n
        else:
          return "K", (n + (_dirs_rev[dp], -1 - cc_),)
    elif op != "DP": # ONLY cc dependent
      res = [self.blocks[x,y].get_real_exit_loc(_dirs_rev[d], cc) for cc in (0, 1)]
      # very simple CFG simplification
      if res[0] == res[1]:
        return "J", (res[0],)
      elif res[0][0:3] == res[1][0:3]:
        if res[0][3]==0 and res[1][3] == 1:
          return "K", (res[0][0:3]+(-1,),)
        else:
          return "K", (res[0][0:3]+(-2,),)
      else:
        return "C", tuple(res)
    else:
      res = []
      for dp in xrange(4):
        res.append(self.get_next_coords(x, y, self.rotcw(d, dp), "NOP"))
      return "D", tuple(res)

def str_call(args, input):
  p = subprocess.Popen(args, stdout=subprocess.PIPE, stdin=subprocess.PIPE)
  p.stdin.write(input)
  return p.communicate()[0]

# min < 0 means run exactly that many times, min > 0 means run at least
def run_opt(mod, pm, name, verbose, min = 1):
  last = str(mod).count("\n")
  if verbose:
    print "Starting runs of pass '{0}' (len: {1})".format(name, last)
  i = 0
  while True:
    if verbose:
      print "Running passes '"+name+"'..."
    mod = str_call(["opt-3.0", "-load="+PASSES, "-S"] + pm, mod)
    now = str(mod).count("\n")
    i += 1
    if verbose:
      print "Done running passes '{0}'... (len: {1})".format(name, now)
    if ((min < 0 and i*-1 == min) or
        (now == last and i >= min)):
      break
    last = now
  if verbose:
    print "Done running pass '{0}' (len: {1})".format(name, now)
  return mod

def main():
  parser = argparse.ArgumentParser(description="Piet to LLVM Compiler")
  parser.add_argument("file", metavar="F", type=str)
  parser.add_argument("-O", dest="opt", action="store_const", const=True,
      default=False, help="Optimize LLVM IR")
  parser.add_argument("-v", dest="verbose", action="store_const", const=True,
      default=False, help="Enable verbose output.")
  parser.add_argument("-d", dest="debug", action="store_const", const=True,
      default=False, help="Enable debug output.")
  parser.add_argument("-o", dest="out", type=argparse.FileType("w", 0),
      default="-")
  parser.add_argument("--lowerswitch", dest="lowerswitch",
      action="store_const", const=True, default=False,
      help="Enable lower switch optimization (used for old versions of LLVM"
      " that do not optimize switch statements after zext correctly.")
  parser.add_argument("--no-simplify-libcalls", dest="libcalls",
      action="store_const", const=False, default=True,
      help="Disable simplify lib calls (used for old versions of LLVM"
      " that do not generate unnammed_addr attribute for puts.")
  parser.add_argument("--no-tailcallelim", dest="tailcallelim",
      action="store_const", const=False, default=True,
      help="Dsiable tail call elimination (used if LLVM is having trouble"
      " with nested + inlined eliminated tail calls)")
  args = parser.parse_args()
  
  if len(sys.argv) == 1:
    print "Specify image as argument"
    return
  if args.verbose:
    print "Loading image..."
  im = PietImage(Image.open(args.file))
  if args.verbose:
    print "Done loading image..."
    print "compiling..."
  mod = im.compile(args.debug)
  if args.verbose:
    print "Done compiling..."

  # after this point we now only use llvm-3.0 tools so we transform the mod
  # into a string

  mod = str(mod)

  if args.opt:

    # simple, easy passes that offer a lot of benefit
    # can reduce coe size by as much as a third
    reduce = ["-instcombine", "-reassociate", "-gvn", "-sccp",
        "-ipsccp", "-adce", "-simplifycfg", "-constmerge"]

    always_inline = ["-always-inline"]

    piet_spec = reduce + ["-pushpopmerge", "-interblockmerge",
        "-interblockmerge2", "-interfuncmove"]

    inline = reduce + ["-inline"]

    # unfortunately, llvm poorly optimizes switches on things that have
    # just been zext'd, so we use lower-switch to fix this issue.
    general = (["-loop-rotate", "-loop-simplify", "-indvars", "-loop-unroll"]
        + piet_spec + reduce)
    if args.tailcallelim:
      general += ["-tailcallelim"]
    if args.lowerswitch:
      general += ["-lowerswitch"]
    general += ["-inline", "-jump-threading"] + reduce

    cleanup = ["-modcleanup"]
    if not args.tailcallelim:
      cleanup += ["-tailcallelim"]
    cleanup += ["-inline"] + reduce
    if args.libcalls:
      cleanup += ["-simplify-libcalls"]
    cleanup += ["-globaldce", "-block-placement", "-strip-dead-prototypes",
        "-deadargelim", "-functionattrs"]

    mod = run_opt(mod, always_inline, "always inline", args.verbose, -1)

    for i in range(3):
      mod = run_opt(mod, inline, "inline", args.verbose)

      mod = run_opt(mod, piet_spec, "piet specific", args.verbose, -1)

    mod = run_opt(mod, reduce, "reduce", args.verbose, -2)

    mod = run_opt(mod, general, "general", args.verbose, 4)

    mod = run_opt(mod, cleanup, "cleanup", args.verbose, -1)

  args.out.write(mod)

if __name__ == "__main__":
  main()
