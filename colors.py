import PIL.ImageColor

Colors = [
  PIL.ImageColor.getrgb(x) for x in (
    "#F00",
    "#C00000",
    "#FFC0C0",
    "#FF0",
    "#C0C000",
    "#FFFFC0",
    "#0F0",
    "#00C000",
    "#C0FFC0",
    "#0FF",
    "#00C0C0",
    "#C0FFFF",
    "#00F",
    "#0000C0",
    "#C0C0FF",
    "#F0F",
    "#C000C0",
    "#FFC0FF",
    "#000000",
    "#FFFFFF"
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
