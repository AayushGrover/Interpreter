exception TypeError of string
exception StackError

type myStack = 
  | EmptyStack
  | NonEmptyStack of int * myStack

type myMap = 
  | Map of (int, string) Hashtbl.t
  
(* Expression - begin *)
type expr =
  | MapConst of myMap
  | StackConst of myStack
  | Id        of string
  | IntConst  of int
  | Add       of expr * expr
  | Sub       of expr * expr
  | If        of expr * expr * expr
  | Let       of string * expr * expr
  | BoolConst of bool
  | Not       of expr
  | Or        of expr * expr
  | And       of expr * expr
  | Equals    of expr * expr
  | Closure   of string * expr * env
  | FunDef    of string * expr
  | FunApp    of expr * expr
  | CreateStack of string(*The name of the list*)
  | PushToStack of string * int(*The list identifier and the value to add (int for now)*)
  | TopOfStack of string
  | PopFromStack of string
  | CreateMap of string
  | AddMapping of string * int * string
  | GetMapValue of string * int
  | EndProgram
  (* Expression - end *)
  (* Environment - begin *)
  and env =
  EmptyEnv
  | NonEmptyEnv of (string * expr) * env
  (* Environment - end *)
  
type exprResult = 
{
  mutable res_expr : expr;
  mutable res_env : env
}

let rec string_of_stack l = 
  match l with
  EmptyStack -> "(end of stack)\n"
| NonEmptyStack(e, l') -> string_of_int e ^ ", " ^ string_of_stack l'

let string_of_map m = 
  let f key value init = "(" ^ (string_of_int key) ^ " - " ^ value ^ ")" ^ init in
    (match m with
      Map(tbl) -> Hashtbl.fold f tbl "\n")

let rec string_of_expr = function
  | MapConst(m)           -> string_of_map m
  | StackConst(l)         -> string_of_stack l
  | Id(vname)             -> vname
  | IntConst(n)           -> string_of_int n
  | Add(e1, e2)           -> (string_of_expr e1) ^ " + " ^ (string_of_expr e2)
  | Sub(e1, e2)           -> (string_of_expr e1) ^ " - " ^ (string_of_expr e2)
  | If(b, e1, e2)         -> "if (" ^ (string_of_expr b) ^ ") then (" ^ (string_of_expr e1) ^ ") else (" ^ (string_of_expr e2) ^ ")"
  | Let(vname, e1, e2)    -> "let " ^ vname ^ " = (" ^ (string_of_expr e1) ^ ") in (" ^ (string_of_expr e2) ^ ")"
  | BoolConst(b)          -> string_of_bool b
  | Not(e)                -> "not(" ^ string_of_expr e ^ ")"
  | Or(e1, e2)            -> "(" ^ (string_of_expr e1) ^ ") or (" ^ (string_of_expr e2) ^ ")"
  | And(e1, e2)           -> "(" ^ (string_of_expr e1) ^ ") and (" ^ (string_of_expr e2) ^ ")"
  | Equals(e1, e2)        -> "(" ^ (string_of_expr e1) ^ ") = (" ^ (string_of_expr e2) ^ ")"
  | Closure(vname, e, env) ->  "(fun " ^ vname ^ "->" ^ (string_of_expr e) ^") " ^ (string_of_env env)(*failwith "no string representation for closure." *) 
  | FunDef(vname, e)      -> "fun " ^ vname ^ " -> " ^ (string_of_expr e)
  | FunApp(e1, e2)        -> "(" ^ (string_of_expr e1) ^ ") (" ^ (string_of_expr e2) ^ ")"
  | CreateStack(id)        -> "Stack (" ^ id ^ ")"
  | PushToStack(id,v)      -> "Stack push (" ^ id ^ ") (" ^  (string_of_int v) ^ ")"
  | TopOfStack(id)         -> "Stack top (" ^ id ^ ")"
  | PopFromStack(id)       -> "Stack pop (" ^ id ^ ")"
  | CreateMap(id)              -> "Map (" ^ id ^ ")"
  | AddMapping(id, key, value) -> "Map add (" ^ string_of_int key ^ " - " ^ value ^ ")"
  | GetMapValue(id, key)   -> "Map get Value (" ^ string_of_int key ^ ")"
  | EndProgram          -> "(End of program)"
and string_of_env env =
  let rec iter = function
      EmptyEnv -> ""
    | NonEmptyEnv((vname, value), env') -> "(" ^ vname ^ "=" ^ (string_of_expr value) ^ "); " ^ (string_of_env env')
  in
  "[" ^ (iter env) ^ "]" 
let emptyEnv () = EmptyEnv

let addBinding x v env =
  NonEmptyEnv((x, v), env)

let rec apply x env =
  match env with
    EmptyEnv -> raise Not_found
  | NonEmptyEnv((vname, value), env') ->
    if x = vname then value
    else (apply x env')

let rec addElementToStack e id env =
  match env with 
    EmptyEnv -> raise Not_found
  | NonEmptyEnv((vname,value), env') ->
      (
        match value with
            StackConst(ml) ->
              if vname = id then NonEmptyEnv((vname, StackConst(NonEmptyStack(e, ml))), env')
              else NonEmptyEnv((vname, value), (addElementToStack e id env'))
          | _ -> raise (TypeError "Expected value of type myStack")
      )

let rec topOfStack id env = 
  match env with
    EmptyEnv -> raise Not_found
  | NonEmptyEnv((vname, value), env') -> 
      if vname = id then
        (
          match value with
            StackConst(ml) ->
              (
              match ml with
                EmptyStack -> raise (StackError) 
              | NonEmptyStack(e, _) -> IntConst e
              )
          | _ -> raise (TypeError "Expected value of type myStack")
        )
      else topOfStack id env'

let rec popFromStack id env = 
  match env with
	| EmptyEnv -> raise Not_found
	| NonEmptyEnv((vname,value), env') ->
		(
			match value with 
			| StackConst(ml) ->
				(if vname = id then 
				(
					match ml with
					| NonEmptyStack(hd, ml') -> NonEmptyEnv((vname, StackConst(ml')), env')
					| EmptyStack -> raise (TypeError "Empty List")
				)
				else NonEmptyEnv((vname, value), (popFromStack vname env')))
			| _ -> raise (TypeError "Expected value of type myList")
    )
    
let rec addMapping key s id env= 
    match env with
      EmptyEnv -> raise Not_found
    | NonEmptyEnv((vname, value), env') ->
        if vname = id then 
          (
            match value with
              MapConst(m) -> 
                (
                  match m with
                    Map(tbl) -> 
                    (
                      Hashtbl.add tbl key s;
                      env
                    )
                )
            | _ -> raise (TypeError "Expected value of type Map")
          )
        else NonEmptyEnv ((vname, value), addMapping key s id env')

let rec getValueFromKey key id env = 
  match env with
    EmptyEnv -> raise Not_found
  | NonEmptyEnv((vname, value), env') ->
    if vname = id then
    (
      match value with
        MapConst(m) -> 
        (
          match m with
            Map(tbl) -> Id (Hashtbl.find tbl key)
        )
      | _ -> raise (TypeError "Expected value of type Map")
    )
    else getValueFromKey key id env'