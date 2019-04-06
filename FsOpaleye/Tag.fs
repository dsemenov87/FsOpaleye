module internal InternalTag

type Tag = UnsafeTag of int32

let start : Tag = UnsafeTag 1
let unsafeUnTag (UnsafeTag i) = i
let tagWith (t : Tag) (s : string) : string = sprintf "%s_%O" s (unsafeUnTag t)
let next : Tag -> Tag = unsafeUnTag >> ((+) 1) >> UnsafeTag

