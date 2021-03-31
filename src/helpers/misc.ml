let rec zip a b = match a, b with
| [], [] -> []
| x::xs, y::ys -> (x, y)::(zip xs ys)
| _ -> failwith "Bad lenght"