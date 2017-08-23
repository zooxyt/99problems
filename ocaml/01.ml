(* 1 : Find the last element of a list *)

let rec last (lst : 'a list) : 'a =
  if List.length lst == 1 then List.hd lst
  else last (List.tl lst)
