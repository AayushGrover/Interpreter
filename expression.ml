exception TypeError of string
exception StackError

type myStack = 
  | EmptyStack
  | NonEmptyStack of int * myStack

type myMap = 
  | Map of (int, string) Hashtbl.t

type myTree = 
  | EmptyTree
  | NonEmptyTree of int * myTree * myTree * int 
  
(* Expression - begin *)
type expr =
  | TreeConst of myTree
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
  | CreateTree of string(*The name of tree*)
  | AddElementTree of string * int (*Tree identifier and the value to add*)
  | DeleteElementTree of string * int(*Tree identifier and the value to delete*)
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

      let rec string_of_tree t =
        match t with
        | EmptyTree -> " (NULL) "
        | NonEmptyTree(e, lt, rt, h) -> 
          (
            let s = string_of_int e ^ ": "
             in
            (
              match lt, rt with
              | EmptyTree, EmptyTree -> s ^ " (NULL) " ^ ", (NULL) \n"
              | NonEmptyTree(e', _, _, _), EmptyTree -> s ^ string_of_int e' ^ ", (NULL) \n" ^ (string_of_tree lt) 
              | EmptyTree, NonEmptyTree(e', _, _, _) -> s ^ " (NULL), " ^ string_of_int e' ^ "\n" ^ (string_of_tree rt)
              | NonEmptyTree(e', _, _, _), NonEmptyTree(e'', _, _, _) -> s ^ string_of_int e' ^ ", " ^string_of_int e'' ^ "\n" 
                                            ^ (string_of_tree lt) ^ (string_of_tree rt)
            )
          )       

let rec string_of_expr = function
  | TreeConst(t)		  -> string_of_tree t
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
  | CreateTree(id)      -> "Tree (" ^ id ^ ")"
  | AddElementTree(id,v)    -> "Tree add (" ^ id ^ ") (" ^  (string_of_int v) ^ ")"
  | DeleteElementTree(id,v)   -> "Tree remove (" ^ id ^ ") (" ^ (string_of_int v) ^ ")"
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

let height t = 
  match t with
  | EmptyTree -> 0
  | NonEmptyTree(x, lt, rt, ht) -> ht  

let newNode x lt rt = NonEmptyTree(x, lt, rt, 1 + (max (height lt) (height rt)))

let rotRight t = 
  match t with
  | EmptyTree -> raise (TypeError "No tree to rotate")
  | NonEmptyTree(y, NonEmptyTree(x, t1, t2, h1), t3, h2) -> (newNode x t1 (newNode y t2 t3))
  | NonEmptyTree(y, EmptyTree, t3, h2) -> raise (TypeError "No rotation needed") 

let rotLeft t = 
  match t with
  | EmptyTree -> raise (TypeError "No tree to rotate")
  | NonEmptyTree(y, t1, NonEmptyTree(x, t2, t3, h1), h2) -> (newNode x (newNode y t1 t2) t3)
  | NonEmptyTree(y, t1, EmptyTree, h2) -> raise (TypeError "No rotation needed")

let rec addNode x t = 
  match t with
  | EmptyTree -> NonEmptyTree(x, EmptyTree, EmptyTree, 1)
  | NonEmptyTree(v, lt, rt, h) -> 
    if x > v then 
    (
      let rt = (addNode x rt)
      in
      match rt with 
      | EmptyTree -> raise (TypeError "Not Possible")
      | NonEmptyTree(v', lt', rt', h') -> 
        if (abs(h' - (height lt)) <= 1) then (newNode v lt rt)
        else 
        (
          let rt = if ((height lt') > (height rt')) then (rotRight rt) else rt
          in 
          rotLeft (newNode v lt rt) 
        ) 
    )
    else 
    (
      if x < v then 
        (
          let lt = (addNode x lt)
          in
          match lt with
          | EmptyTree -> raise (TypeError "Not Possible")
          | NonEmptyTree(v', lt', rt', h') ->
            if(abs(h' - (height rt)) <= 1) then (newNode v lt rt)
            else
            (
              let lt = if ((height rt') > (height lt')) then (rotLeft lt) else lt
              in 
              rotRight (newNode v lt rt)
            ) 
        )
      else NonEmptyTree(v, lt, rt, h)
    ) 

let rec addElementToTree id v env =
  match env with 
  | EmptyEnv -> raise Not_found
  | NonEmptyEnv((vname,value), env') ->
    (
      match value with
      | TreeConst(mt) ->
        if vname = id then NonEmptyEnv((vname, TreeConst(addNode v mt)), env')
        else NonEmptyEnv((vname, value), (addElementToTree id v env')) 
      | _ -> raise (TypeError "Expected value of type myTree")
    )

let rec treeToList t = 
  match t with 
  | EmptyTree -> []
  | NonEmptyTree(v, lt, rt, h) -> (v :: (treeToList lt)) @ (treeToList rt)

let rec rmNodeFromList x l = 
  match l with 
  | [] -> []
  | h::t -> if (h <> x) then h::(rmNodeFromList x t) else (rmNodeFromList x t)

let rec listToTree l = 
  match l with 
  | [] -> EmptyTree
  | h::t -> (addNode h (listToTree t))

let rmNode x tr = 
  match tr with
  | EmptyTree -> raise (TypeError "Node not found")
  | NonEmptyTree(v, lt, rt, h) -> 
  (
    let l = treeToList tr in
    let l' = rmNodeFromList x l in
    if ((List.length l) = (List.length l')) then raise Not_found 
    else (listToTree l')
  )


let rec removeElementFromTree id v env = 
  match env with 
  | EmptyEnv -> raise Not_found
  | NonEmptyEnv((vname,value), env') ->
    (
      match value with
      | TreeConst(mt) ->
        if vname = id then NonEmptyEnv((vname, TreeConst(rmNode v mt)), env')
        else NonEmptyEnv((vname, value), (removeElementFromTree id v env')) 
      | _ -> raise (TypeError "Expected value of type myTree")
    )	