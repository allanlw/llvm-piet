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

import PIL.ImageColor

Colors = [
  PIL.ImageColor.getrgb(x) for x in (
    "#F00", "#C00000", "#FFC0C0",
    "#FF0", "#C0C000", "#FFFFC0",
    "#0F0", "#00C000", "#C0FFC0",
    "#0FF", "#00C0C0", "#C0FFFF",
    "#00F", "#0000C0", "#C0C0FF",
    "#F0F", "#C000C0", "#FFC0FF",
    "#000000", "#FFFFFF"
  )
]

Ops = [
  "NOP", "PUSH","POP",
  "ADD", "SUB", "MUL",
  "DIV", "MOD", "NOT",
  "GT", "DP", "SW",
  "DUP", "ROL", "INN",
  "INC", "OUN", "OUC",
  "EXIT" # program termination pseudo instruction for debugging
]

def get_op(pre, now):
  if len(Colors)-1 in (pre, now):
    return Ops[0]
  else:
    lig = (now - pre) % 3 # how much darker?
    hue = ((now/3) - (pre/3))% 6 # how much hue change?
    return Ops[lig+hue*3]
