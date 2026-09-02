(* Programming language concepts for software developers, 2010-08-28 *)

(* Evaluating simple expressions with variables *)

module Intro2

(* Association lists map object language variables to their values *)

let env = [("a", 3); ("c", 78); ("baf", 666); ("b", 111)];;

let emptyenv = []; (* the empty environment *)

let rec lookup env x =
    match env with
    | []        -> failwith (x + " not found")
    | (y, v)::r -> if x=y then v else lookup r x;;

let cvalue = lookup env "c";;


(* Object language expressions with variables *)

type expr =
  | CstI of int
  | Var of string
  | Prim of string * expr * expr
  | If of expr * expr * expr;;

let e1 = CstI 17;;

let e2 = Prim("+", CstI 3, Var "a");;

let e3 = Prim("+", Prim("*", Var "b", CstI 9), Var "a");;


(* Evaluation within an environment *)

(* Here is changes for the Assignment *)
let rec eval e (env : (string * int) list) : int =
    match e with
    | CstI i            -> i
    | Var x             -> lookup env x
    | Prim(ope, e1, e2) ->
        let i1 = eval e1 env
        let i2 = eval e2 env
        match ope with
        | "+" -> i1 + i2
        | "*" -> i1 * i2
        | "-" -> i1 - i2 
        | "max" -> if i1 > i2 then i1 else i2
        | "min" -> if i1 < i2 then i1 else i2
        | "==" -> if i1 = i2 then 1 else 0
        | _ -> failwith "unknown operator"
    | Prim _            -> failwith "unknown primitive"
    | If (e1, e2, e3) ->
         if eval e1 env <> 0 then eval e2 env else eval e3 env                    
  

let e1v  = eval e1 env;;
let e2v1 = eval e2 env;;
let e2v2 = eval e2 [("a", 314)];;
let e3v  = eval e3 env;;

(* Here is changes for the Assignment *)
let exampleExpression1 = eval (Prim ("max", CstI 42, CstI 22)) env;;
let exampleExpression2 = eval (Prim ("min", CstI 42, CstI 21)) env;;
let exampleExpression3 = eval (Prim ("==", CstI 67, CstI 42)) env;;
let exampleExpression4 = eval (Prim ("==", CstI 67, CstI 67)) env;;

(* Here is changes for the Assignment *)
type aexpr =
  | CstI of int
  | Var of string
  | Add of aexpr * aexpr
  | Mul of aexpr * aexpr
  | Sub of aexpr * aexpr  

(* Here is changes for the Assignment *)
let rec fmt a =
    match a with
    | CstI x -> string x
    | Var v -> string v
    | Add (e1,e2) -> "( " + (fmt e1) + " + " + (fmt e2) + " )"
    | Mul (e1,e2) -> "( " + (fmt e1) + " * " + (fmt e2) + " )"
    | Sub (e1,e2) -> "( " + (fmt e1) + " * " + (fmt e2) + " )"

(* Here is changes for the Assignment *)    
let rec simplify a =
    match a with
    | CstI n -> CstI n
    | Var v -> Var v
    | Add (e, CstI 0)  -> simplify e
    | Add (CstI 0, e) -> simplify e
    | Sub (e, CstI 0) -> simplify e
    | Mul (CstI 1, e) -> simplify e
    | Mul (e, CstI 1) -> simplify e
    | Mul (CstI 0, _) -> CstI 0
    | Mul (_, CstI 0) -> CstI 0
    | Sub (e1, e2) when e1 = e2  -> CstI 0
    | Mul (e1, e2) -> (Mul (simplify e1, simplify e2))
    | Add (e1, e2) -> (Add (simplify e1, simplify e2))
    | Sub (e1, e2) -> (Sub (simplify e1, simplify e2))
    | _ -> a

(* Here is changes for the Assignment *)
let rec diff a x =
    match a with
    | CstI _ -> CstI 0
    | Var y -> if y = x then CstI 1 else CstI 0
    | Add (e1,e2) -> Add (diff e1 x, diff e2 x)
    | Sub (e1,e2) -> Sub (diff e1 x, diff e2 x)
    | Mul (e1,e2) -> Add (Mul (diff e1 x, e2), Mul (e1, diff e2 x))

(* Here is changes for the Assignment *)
let aExampleExpression1 = Sub (Var "v", Add (Var "w", Var "z"))
let aExampleExpression2 = Mul (CstI 2, Sub (Var "v", Add (Var "w", Var "z")))
let aExampleExpression3 = Add (Var "x", Add (Var "y", Add (Var "z", Var "v")))
