let usage_msg = "olox [script] || olex (for REPL)"
let script = ref false
let repl = ref false
let input_files = ref []
let anon_fun filename = input_files := filename :: !input_files

let speclist =
  [
    ("-s", Arg.Set script, "script path e.g. ./script.ox");
    ("-repl", Arg.Set repl, "run repl");
  ]

let () = Arg.parse speclist anon_fun usage_msg

let _report (line : int) (where : string) (message : string) =
  Printf.eprintf "[line %d] Error: %s : %s" line where message;
  true

let run_file (filename : string) =
  let file_content = In_channel.with_open_text filename In_channel.input_all in
  print_endline file_content

let run (input : string) = print_string input

let repl () =
  while true do
    print_string "> ";
    flush stdout;
    let line = read_line () in
    match line with "" -> exit 0 | _ -> run line
  done

let () =
  match Array.to_list Sys.argv with
  | _ :: "-s" :: filename :: _ -> run_file filename
  | _ :: "-repl" :: _ -> repl ()
  | [ _ ] -> Arg.usage speclist usage_msg
  | _ -> print_endline "Something went wrong"
