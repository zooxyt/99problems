(* 6 : Find out whether a list is a palindrome : A palindrome can be read forward or backward; e.g : (x a m a x)  *)

let rec palindome (s : string) : bool =
  if String.length s <= 1 then true
  else
    if s.[0] == s.[String.length s - 1] then 
      palindome (String.sub s 1 (String.length s - 2))
    else
      false
                   
    
