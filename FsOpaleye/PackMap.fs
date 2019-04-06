module internal PackMap

module T = InternalTag
module HPQ = InternalPrimQuery

open FSharpPlus
open FSharpPlus.Data
open FsControl.Internals

/// A helpful monad for writing columns in the AST
type PM<'a, 'b> = State<('a * int32), 'b>

let create<'a> : PM<'a, string> =
  State.get
  |> State.bind (fun (a, i) ->
      State.put (a, i + 1)
      |> State.map (fun _ -> string i)
  )

let write (a: 'a) : PM<'a list, unit> =
  State.get
  |> State.bind (fun (as', i) ->
      State.put (as' @ [a], i)
  )

let run (m: PM<'a list, 'r>) : ('r * 'a list) =
  let (r, (as', _)) = State.run m ([], 0)
  in (r, as')

// General functions for writing columns in the AST
//
// Make a fresh name for an input value (the variable @primExpr@
// type is typically actually a 'HPQ.PrimExpr') based on the supplied
// function and the unique 'T.Tag' that is used as part of our
// @QueryArr@.
//
// Add the fresh name and the input value it refers to to the list in
// the state parameter.
let extractAttrPE (mkName: 'primExpr -> string -> string) (t: T.Tag) (pe: 'primExpr)
               : PM<(HPQ.Symbol * 'primExpr) list, HPQ.PrimExpr> =
  create
  |> State.bind (fun i ->
      let s = HPQ.Symbol (mkName pe i, t)
      write (s, pe)
      |> State.map (fun _ -> HPQ.AttrExpr s)
  )

/// As 'extractAttrPE' but ignores the 'primExpr' when making the
/// fresh column name and just uses the supplied 'String' and 'T.Tag'.
let extractAttr (s: string) : T.Tag -> 'primExpr
               -> PM<(HPQ.Symbol * 'primExpr) list, HPQ.PrimExpr> =
  extractAttrPE (fun _ sa -> (s + sa))
    
//let eitherFunction (f: 'a -> 'f b)
//               -> (a' -> f b')
//               -> Either a a'
//               -> f (Either b b')
//let eitherFunction f g =
//  let (+++!) : p a b -> p a' b' -> p (Either a a') (Either b b')
//  map (either (map Choice1Of2) (map Choice2Of2)) (f PP.+++! g)