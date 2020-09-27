signature AST =
sig

    datatype PreType =
        Nat
      | TypVarName of string
      | TypVar of int
      | Arr of PreType * PreType
      | All of string * PreType (* binds *)
      | Prod of PreType * PreType
    datatype Qual = Un | Lin
    datatype Typ = Type of Qual * PreType

    datatype Exp =
        Zero of Qual
      | Var of string * int (* idx into ctx *)
      | Succ of Exp
      | Lam of Qual * string * Typ (*argType*) * Exp (*funcBody*)
      | Let of string * Typ (*vartype*) * Exp (*varval*) * Exp (*varscope*)
      | App of Exp * Exp
      | Fix of string (*x*) * Typ (*: t*) * Exp (*x's scope*)
      | TypAbs of string * Exp (* binds type variable *)
      | Ifz of Exp * Exp * string * Exp
      | TypApp of Typ * Exp
      | Tuple of Qual * Exp * Exp
      | Split of Exp * string * string * Exp
      | ProdLeft of Exp
      | ProdRight of Exp

end

structure Ast :> AST =
struct
    datatype PreType =
        Nat
      | TypVarName of string
      | TypVar of int
      | Arr of PreType * PreType
      | All of string * PreType (* binds *)
      | Prod of PreType * PreType
    datatype Qual = Un | Lin
    datatype Typ = Type of Qual * PreType

    datatype Exp =
        Zero of Qual
      | Var of string * int (* idx into ctx *)
      | Succ of Exp
      | Lam of Qual * string * Typ (*argType*) * Exp (*funcBody*)
      | Let of string * Typ (*vartype*) * Exp (*varval*) * Exp (*varscope*)
      | App of Exp * Exp
      | Fix of string (*x*) * Typ (*: t*) * Exp (*x's scope*)
      | TypAbs of string * Exp (* binds type variable *)
      | Ifz of Exp * Exp * string * Exp
      | TypApp of Typ * Exp
      | Tuple of Qual * Exp * Exp
      | Split of Exp * string * string * Exp
      | ProdLeft of Exp
      | ProdRight of Exp

end
