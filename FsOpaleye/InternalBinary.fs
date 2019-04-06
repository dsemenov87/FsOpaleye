module internal InternalBinary

open InternalColumn
module T = InternalTag
module PM = PackMap
module HPQ = InternalPrimQuery

open FSharpPlus
open FSharpPlus.Data
open FsControl

let extractBinaryFields : T.Tag -> HPQ.PrimExpr * HPQ.PrimExpr
                    -> PM.PM<(HPQ.Symbol * (HPQ.PrimExpr * HPQ.PrimExpr)) list, HPQ.PrimExpr> =
                    PM.extractAttr "binary"
//
//type Binaryspec<'cols1, 'cols2'> =
//  Binaryspec of (PM.PackMap (HPQ.PrimExpr, HPQ.PrimExpr) HPQ.PrimExpr
//                         (columns, columns) columns')
