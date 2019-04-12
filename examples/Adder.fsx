#I __SOURCE_DIRECTORY__
#r "../FluidAnnotations/bin/Debug/net46/FluidAnnotations.dll"

open FluidTypes.Annotations

[<Refined("(x: {v:int|true} -> {v:int|v>=7})")>]
let add7 x
  = x + 7

let add7_anno (x: int)
  = x + 7
