type myList = 
  EmptyList
| NonEmptyList of int * myList

val makeList : unit -> myList

val addElement : int -> myList -> myList

val deleteElement : myList -> myList