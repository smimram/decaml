module List = struct
  include List

  let index p l =
    let rec aux n = function
      | x::l -> if p x then n else aux (n+1) l
      | [] -> raise Not_found
    in
    aux 0 l
end
