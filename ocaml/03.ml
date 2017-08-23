(* 3 : Find the K'th element of a list : The first element in the list is number 1 *)

let rec nth (lst : 'a list) (n : int) : 'a =
  if n == 1 then List.hd lst
  else nth (List.tl lst) (n - 1)
  
