type myListType = 
    EmptyList
  | NonEmptyList of int * myListType

type myList = 
{
  size : int;
  listID : string;
  body : myListType;
}


let makeList () = 
  EmptyList

let addElement e l = 
  NonEmptyList(e, l)

let deleteElement l = 
  match l with
      EmptyList -> raise Not_found
    | NonEmptyList(e, l') -> l'