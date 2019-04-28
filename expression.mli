type myStack = 
  EmptyStack
| NonEmptyStack of int * myStack

type myMap = 
  | Map of (int, string) Hashtbl.t

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
  (* Corresponds to the underlying function closure. Note that it is a normal form as IntConst and BoolConst. *)
  | FunDef    of string * expr
  (* The abstract syntactic structure. This doesn't have the information about the environment. *)
  | FunApp    of expr * expr
  (* Application of function *)
  | CreateStack of string(*The name of the list*)
  | PushToStack of string * int(*The list identifier and the value to add (int for now)*)
  | TopOfStack of string
  | PopFromStack of string
  | CreateMap of string
  | AddMapping of string * int * string
  | GetMapValue of string * int
  | EndProgram
  and env =
  EmptyEnv
  | NonEmptyEnv of (string * expr) * env

type exprResult = 
{
  mutable res_expr : expr;
  mutable res_env : env
}
val string_of_expr : expr -> string
  
val string_of_stack : myStack -> string

val emptyEnv : unit -> env

val addBinding : string -> expr -> env -> env

val apply : string -> env -> expr

val addElementToStack : int -> string -> env -> env

val topOfStack : string -> env -> expr

val popFromStack : string -> env -> env

val string_of_map : myMap -> string

val addMapping : int -> string -> string -> env -> env

val getValueFromKey : int -> string -> env -> expr