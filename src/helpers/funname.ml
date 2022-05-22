class fun_name (name : string) =
  object
    val name = name
    val mutable i = 0

    method generate =
      let new_name = name ^ string_of_int i in
      i <- i + 1;
      new_name
  end
