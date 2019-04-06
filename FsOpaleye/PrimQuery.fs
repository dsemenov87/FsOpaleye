module internal InternalPrimQuery

module InternalTag = InternalTag

type ByteString = FSharpx.Collections.ByteString

type TableName  = string
type Attribute  = string
type Name       = string
type Scheme     = Attribute list
type Assoc      = (Attribute * PrimExpr) list

and Symbol = Symbol of string * InternalTag.Tag

and PrimExpr =
  | AttrExpr of Symbol
  | BaseTableAttrExpr of Attribute
  | BinExpr   of BinOp * PrimExpr * PrimExpr
  | UnExpr    of UnOp * PrimExpr
  | AggrExpr  of AggrOp * PrimExpr
  | ConstExpr of Literal
  | CaseExpr  of (PrimExpr * PrimExpr) list * PrimExpr
  | ListExpr  of PrimExpr list
  | ParamExpr of (Name option) * PrimExpr
  | FunExpr   of Name * (PrimExpr list)
  | CastExpr  of Name * PrimExpr // ^ Cast an expression to a given type.
  | DefaultInsertExpr // Indicate that we want to insert the
                      // default value into a column.
                      // TODO: I'm not sure this belongs
                      // here.  Perhaps a special type is
                      // needed for insert expressions.

and Literal =
  | NullLit
  | DefaultLit            // ^ represents a default value
  | BoolLit of bool
  | StringLit of string
  | ByteStringLit of ByteString
  | IntegerLit of int32
  | DoubleLit of double
  | OtherLit of string       // ^ used for hacking in custom SQL

and BinOp =
  | OpEq  | OpLt    | OpLtEq  | OpGt  | OpGtEq | OpNotEq
  | OpAnd | OpOr    | OpLike  | OpIn  | OpOther of string
  | OpCat | OpPlus  | OpMinus | OpMul | OpDiv | OpMod
  | OpBitNot | OpBitAnd | OpBitOr | OpBitXor | OpAsg

and UnOp =
  | OpNot
  | OpIsNull
  | OpIsNotNull
  | OpLength
  | OpAbs
  | OpNegate
  | OpLower
  | OpUpper
  | UnOpOther of string

and AggrOp =
  | AggrCount | AggrSum | AggrAvg | AggrMin | AggrMax
  | AggrStdDev | AggrStdDevP | AggrVar | AggrVarP
  | AggrBoolOr | AggrBoolAnd | AggrArr | AggrStringAggr of PrimExpr
  | AggrOther of string

and OrderExpr = OrderExpr of OrderOp * PrimExpr

and OrderNulls = NullsFirst | NullsLast

and OrderDirection = OpAsc | OpDesc

and OrderOp = { orderDirection : OrderDirection
                orderNulls     : OrderNulls }


