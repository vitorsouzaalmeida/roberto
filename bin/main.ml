let usage_msg = "Roberto's CLI"
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

(* I've splitted report and error to separate the code that generates
the errors from the code that reports them. *)
let report (line : int) (where : string) (message : string) =
  Printf.eprintf "[line %d] Error: %s : %s" line where message;
  true

let _error (line : int) message = report line "" message

let run_file (filename : string) =
  let file_content = In_channel.with_open_text filename In_channel.input_all in
  print_endline file_content

let repl () =
  while true do
    print_string "> ";
    flush stdout;
    let line = read_line () in
    let lexbuf = Sedlexing.Utf8.from_string line in
    let lexer = Sedlexing.with_tokenizer Roberto_lib.Scanner.token lexbuf in
    let parser =
      MenhirLib.Convert.Simplified.traditional2revised Roberto_lib.Parser.main
    in
    try
      let result = parser lexer in
      print_float result;
      print_newline ();
      flush stdout
    with
    | Failure msg -> Printf.printf "Error: %s\n" msg
    | _ -> Printf.eprintf "Parse error\n"
  done

let () =
  match Array.to_list Sys.argv with
  | _ :: "-s" :: filename :: _ -> run_file filename
  | _ :: "-repl" :: _ -> repl ()
  | [ _ ] -> Arg.usage speclist usage_msg
  | _ -> print_endline "Something went wrong"
