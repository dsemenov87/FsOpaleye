module internal InternalPrimaryQuery

//import           Prelude hiding (product)

type NEL<'a> =  FSharpx.Collections.NonEmptyList<'a>
module NEL =  FSharpx.Collections.NonEmptyList
module HPQ = InternalPrimQuery
type Symbol = InternalPrimQuery.Symbol

type LimitOp =
  | LimitOp of int32
  | OffsetOp of int32
  | LimitOffsetOp of int32 * int32

type BinOp = Except | Union | UnionAll

type JoinType = LeftJoin

// In the future it may make sense to introduce this datatype
// type Bindings a = [(Symbol, a)]

/// We use a 'NEL.NonEmpty' for Product because otherwise we'd have to check
/// for emptiness explicity in the SQL generation phase.
type PrimQuery =
  | Unit
  | BaseTable of string * (Symbol * HPQ.PrimExpr) list
  | Product of NEL<PrimQuery> * HPQ.PrimExpr list
  | Aggregate of (Symbol * (HPQ.AggrOp option * HPQ.PrimExpr)) list * PrimQuery
  | Order of HPQ.OrderExpr list * PrimQuery
  | Limit of LimitOp * PrimQuery
  | Join of JoinType * HPQ.PrimExpr * PrimQuery * PrimQuery
  | Values of Symbol list * HPQ.PrimExpr list list
  | Binary of BinOp * (Symbol * (HPQ.PrimExpr * HPQ.PrimExpr)) list * (PrimQuery * PrimQuery)

type PrimQueryFold<'p> = 'p
                       * (string -> (Symbol * HPQ.PrimExpr) list -> 'p)
                       * (NEL<'p> -> HPQ.PrimExpr list -> 'p)
                       * ((Symbol * (HPQ.AggrOp option * HPQ.PrimExpr)) list -> 'p -> 'p)
                       * (HPQ.OrderExpr list -> 'p -> 'p)
                       * (LimitOp -> 'p -> 'p)
                       * (JoinType -> HPQ.PrimExpr -> 'p -> 'p -> 'p)
                       * (Symbol list -> HPQ.PrimExpr list list -> 'p)
                       * (BinOp -> (Symbol * (HPQ.PrimExpr * HPQ.PrimExpr)) list -> ('p * 'p) -> 'p)

let foldPrimQuery
  ((unit', baseTable, product, aggregate, order, limit, join, values, binary) : PrimQueryFold<'p>)
  : PrimQuery -> 'p =
  let rec fold primQ =
    match primQ with
    | Unit                       -> unit'
    | BaseTable (n, s)           -> baseTable n s
    | Product (pqs, pes)         -> product (NEL.map fold pqs) pes
    | Aggregate (aggrs, pq)      -> aggregate aggrs (fold pq)
    | Order (pes, pq)            -> order pes (fold pq)
    | Limit (op, pq)             -> limit op (fold pq)
    | Join (j, cond, q1, q2)     -> join j cond (fold q1) (fold q2)
    | Values (ss, pes)           -> values ss pes
    | Binary (binop, pes, (pq, pq'))
                                  -> binary binop pes (fold pq, fold pq')  
  in
   fold

let times (q: PrimQuery) (q': PrimQuery) : PrimQuery = Product (NEL.create q [q'], [])

let restrict (cond: HPQ.PrimExpr) (primQ: PrimQuery) = Product (NEL.singleton primQ, [cond])

let isUnit : PrimQuery -> bool = function Unit -> true | _ -> false

