(* 2 : Find the last but one element of a list *)

let rec butLast (lst : 'a list) : 'a =
  if List.length lst == 2 then List.hd lst
  else butLast (List.tl lst)


