(* 5 : Reverse a list : *)

let rec reverse (lst : 'a list) : 'a list =
  if List.length lst == 0 then []
  else List.append (reverse (List.tl lst)) [(List.hd lst)]
