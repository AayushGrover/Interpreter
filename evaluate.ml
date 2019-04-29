let evaluate () =
  let result = {Expression.res_expr = Expression.Id("Start"); Expression.res_env = Expression.EmptyEnv} in
    try
      let cin =
        if Array.length Sys.argv > 1 then
          open_in Sys.argv.(1)
        else
          stdin
      in
        let lexbuf = Lexing.from_channel cin in
        while true do
          let result_parse = Parser.main Lexer.scan lexbuf
          in
            print_string ("\n" ^ (Expression.string_of_expr result_parse) ^ "\n");
            let res = (Interpreter.eval result_parse (result).Expression.res_env) in
              let _ = (result.Expression.res_expr <- res.Expression.res_expr;
                            result.Expression.res_env <- res.Expression.res_env) in
              match (result).Expression.res_expr with
              | Expression.Id(s) -> Printf.printf "\n\t = %s\n" s
              | Expression.IntConst(n) -> Printf.printf "\n\t = %d\n" n
              | Expression.BoolConst(b) -> Printf.printf "\n\t = %b\n" b
              | Expression.Closure(_, _, _) -> Printf.printf "\n\t = %s\n" (Expression.string_of_expr (result).Expression.res_expr)
              | Expression.StackConst(l) -> Printf.printf "\n\t = %s\n" (Expression.string_of_stack l)
              | Expression.MapConst(m) -> Printf.printf "\n\t = %s\n"  (Expression.string_of_map m)
              | Expression.TreeConst(t) -> Printf.printf "\n\t = %s\n" (Expression.string_of_tree t) 
              | Expression.EndProgram -> exit 0
              | _ -> Printf.printf "Empty\n"
        done
    with End_of_file -> exit 0
    

    let _ = evaluate ()
