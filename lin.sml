(* lin - a small functional language *)
structure Lin : sig
                   val test : unit -> unit
                   val parse : string -> Ast.Exp
                   val parseFile : string -> Ast.Exp
                   val typeof : A.Exp -> A.Typ
                   val eval : A.Exp -> A.Exp
                   val isval : A.Exp -> bool
                   val step : A.Exp -> A.Exp
                   val subst : A.Exp -> A.Exp -> A.Exp
                   val run : string -> A.Exp
                   val runFile : string -> A.Exp
                 end =
struct

exception IllTyped
exception IllTypedMsg of string
exception No
exception VarNotInContext
exception VarWithNegativeDeBruijinIndex of string * int

fun get ctx i =
    case (List.findi (fn (j, _) => j = i) ctx) of
        NONE => (print (Int.toString i); raise VarNotInContext)
      | SOME (j,v) => v

fun istype typeCtx t = true

fun substType' src dst bindingDepth = dst

fun substType src dst = substType' src dst 0

fun decrDeBruijinIndices t = t

fun substTypeInExp' srcType dstExp bindingDepth =
    case dstExp
     of  A.Zero qual => A.Zero qual
       | A.Var (name, i) => A.Var (name, i)
       | A.Succ e2 =>
         A.Succ (substTypeInExp' srcType e2 bindingDepth)
       | A.ProdLeft e =>
         A.ProdLeft (substTypeInExp' srcType e bindingDepth)
       | A.ProdRight e =>
         A.ProdRight (substTypeInExp' srcType e bindingDepth)
       | A.Lam (qual, argName, argType, funcBody) =>
         A.Lam(qual, argName, (substType' srcType argType bindingDepth),
               substTypeInExp' srcType funcBody bindingDepth)
       | A.Let (varname, vartype, varval, varscope) =>
         A.Let(varname, (substType' srcType vartype bindingDepth),
               substTypeInExp' srcType varval bindingDepth,
               substTypeInExp' srcType varscope bindingDepth
              )
       | A.Split(e, lname, rname, scope) =>
         A.Split(e, lname, rname,
                 substTypeInExp' srcType scope bindingDepth)
       | A.App (f, n) =>
         A.App (substTypeInExp' srcType f bindingDepth,
                substTypeInExp' srcType n bindingDepth)
       | A.Ifz (i, t, prev, e) =>
         A.Ifz(substTypeInExp' srcType i bindingDepth,
               substTypeInExp' srcType t bindingDepth,
               prev,
               substTypeInExp' srcType e bindingDepth)
       | A.Tuple (qual, l, r) =>
         A.Tuple (qual, substTypeInExp' srcType l bindingDepth,
                  substTypeInExp' srcType r bindingDepth)
       | A.Fix (name, t, e) =>
         A.Fix(name,
               substType' srcType t bindingDepth,
               substTypeInExp' srcType e bindingDepth)

fun setDeBruijnIndexInType t varnames typnames = t

fun setDeBruijnIndex e varnames typnames =
    let fun find name names =
        (case List.findi (fn (_, n : string) => n = name) names
         of NONE => NONE
          | SOME (i, _) => SOME i)
    in
    case e
     of  A.Zero qual => e
       | A.Var (n, i) =>
         (case find n varnames of
             NONE => (print ("unknown var: "^ n); raise VarNotInContext)
           | SOME i => A.Var (n, i))
       | A.Lam(qual, name, argType, funcBody) =>
         A.Lam(qual, name,
               setDeBruijnIndexInType argType varnames typnames,
               setDeBruijnIndex funcBody (name::varnames) typnames)
       | A.Let (varname, vartype, varval, varscope) =>
         A.Let(varname,
               setDeBruijnIndexInType vartype varnames typnames,
               (setDeBruijnIndex varval (varnames) typnames),
               setDeBruijnIndex varscope (varname::varnames) typnames)
       | A.Split(e, lname, rname, scope) =>
         A.Split(e, lname, rname,
                 setDeBruijnIndex scope (rname::(lname::varnames)) typnames)
       | A.Succ e2 => A.Succ (setDeBruijnIndex e2 varnames typnames)
       | A.ProdLeft e => A.ProdLeft (setDeBruijnIndex e varnames typnames)
       | A.ProdRight e => A.ProdRight (setDeBruijnIndex e varnames typnames)
       | A.App (f, n) => A.App (setDeBruijnIndex f varnames typnames,
                                setDeBruijnIndex n varnames typnames)
       | A.Ifz (i, t, prev, e) => A.Ifz (setDeBruijnIndex i varnames typnames,
                                   setDeBruijnIndex t varnames typnames,
                                   prev,
                                   setDeBruijnIndex e (prev::varnames) typnames)
       | A.Tuple (qual, l, r) => A.Tuple (qual, setDeBruijnIndex l varnames typnames,
                                    setDeBruijnIndex r varnames typnames)
       | A.Fix(name, t, e) =>
         A.Fix(name,
               setDeBruijnIndexInType t varnames typnames,
               setDeBruijnIndex e (name::varnames) typnames)
end


fun substTypeInExp srcType dstExp = substTypeInExp' srcType dstExp 0

fun typeEq (t : A.Typ) (t' : A.Typ) = (t = t')

fun typeof' ctx typCtx e = A.Type (A.Un, A.Nat)
fun typeof e = typeof' [] [] e

fun isval e =
    case e of
        A.Zero qual => true
      | A.Succ(n) => isval n
      | A.Lam(_, _, _, _) => true
      | A.Let(_, _, _, _) => false
      | A.Tuple(q, l, r) => (isval l) andalso (isval r)
      | _ => false

fun subst' src dst bindingDepth =
    case dst
     of  A.Zero qual => A.Zero qual
       | A.Var (name, n)  => if n = bindingDepth then src else
                   if n > bindingDepth then A.Var(name, n-1) else
                   A.Var(name, n)
       | A.Succ e2 => A.Succ (subst' src e2 bindingDepth)
       | A.ProdLeft e => A.ProdLeft (subst' src e bindingDepth)
       | A.ProdRight e => A.ProdRight (subst' src e bindingDepth)
       | A.Lam (qual, argName, t, f) => A.Lam(qual, argName, t, (subst' src f (bindingDepth+1)))
       | A.Let (varname, vartype, varval, varscope) =>
            A.Let(varname,
                  vartype,
                  (subst' src varval (bindingDepth)),
                  (subst' src varscope (bindingDepth+1)))
       | A.Split(e, lname, rname, scope) =>
         A.Split(subst' src e bindingDepth, lname, rname,
                 subst' src scope (bindingDepth+2))
       | A.App (f, n) => A.App((subst' src f bindingDepth), (subst' src n bindingDepth))
       | A.Ifz (i, t, prev, e) => A.Ifz(subst' src i bindingDepth,
                                  subst' src t bindingDepth,
                                  prev,
                                  subst' src e (bindingDepth+1)) (* binds *)
       | A.Fix (name, t, e) =>
         A.Fix(name, t, subst' src e (bindingDepth+1)) (* binds *)
       | A.Tuple (qual, l, r) => A.Tuple (qual, subst' src l bindingDepth, subst' src r bindingDepth)


fun subst src dst = subst' src dst 0


fun step e =
    let val _ = typeof' [] [] e in
    if isval e then e else
    case e of
        A.Succ(n) => if not (isval n) then A.Succ(step n) else e
      | A.ProdLeft n  => if not (isval n) then A.ProdLeft(step n) else
                   let val A.Tuple(qual, l, r) = n in l end
      | A.ProdRight n  => if not (isval n) then A.ProdRight(step n) else
                    let val A.Tuple(qual, l, r) = n in r end
      | A.Tuple(qual, l, r) => if not (isval l) then A.Tuple(qual, step l, r) else
                       if not (isval r) then A.Tuple(qual, l, step r) else
                       e
      | A.App(f, n) => if not (isval f) then A.App(step f, n)
                     else (if not (isval n) then A.App(f, step n)
                           else let val A.Lam(qual, argName, t, f') = f
                           in
                               (* plug `n` into `f'` *)
                               subst n f'
                           end
                          )
      | A.Ifz(i, t, prev, e) =>
            if not (isval i) then A.Ifz(step i, t, prev, e)
            else (case i of
                      A.Zero qual => t
                    | A.Succ i' => subst i' e
                    | _ => raise IllTypedMsg "ifz conditional must be an integer")
      | A.Let (varname, vartype, varval, varscope) => subst varval varscope
      | A.Var (name, x) => (if x < 0 then raise VarNotInContext else A.Var (name, x))
      | A.Fix(name, t, body) => subst e body
      | _ => if (isval e) then e else raise No
    end


fun parse s =
    let val ast : A.Exp = Parse.parse s
    in
        setDeBruijnIndex ast [] []
    end

fun parseFile filename =
    let val ast : A.Exp = Parse.parseFile filename
    in
        setDeBruijnIndex ast [] []
    end

fun eval e = if isval e then e else eval (step e)
fun run s = let val e = parse s in if isval e then e else eval (step e) end
fun runFile s = let val e = parseFile s in if isval e then e else eval (step e) end

fun test() = let
open A;
val Zero Lin = parse "lin Z";
val Zero Un = parse "un Z";

val Tuple (Lin,Zero Un,Zero Un) : Ast.Exp = parse "lin (un Z, un Z)";
val Tuple (Un,Zero Un,Zero Un) : Ast.Exp = parse "un (un Z, un Z)";

val Split (Tuple (Lin,Zero Un,Zero Un),"x","y",Var ("x",1))
    = parse "split lin (un Z, un Z) as x, y in x"

val Split (Tuple (Lin,Zero Un,Zero Un),"x","y",Var ("y",0))
    = parse "split lin (un Z, un Z) as x, y in y"

val Let ("x",Type (Un,Nat),Zero Un,Var ("x",0)) : Ast.Exp =
    parse "let x : un nat = un Z in x";

val Let ("x",Type (Lin,Nat),Zero Lin,Var ("x",0)) : Ast.Exp =
    parse "let x : lin nat = lin Z in x";

val Lam (Lin,"x",Type (Lin,Nat),Var ("x",0)) : Ast.Exp =
    parse "lin \\ x : lin nat = x";

in () end

end (* structure Lin *)
