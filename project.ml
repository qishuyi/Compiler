let is_matching = ref false
let arglist = ref []

let main =
begin
let speclist = [("-length", Arg.Set is_matching, "prints the lengths of each of the arguments");]
in let usage_msg = "Usage: my-project [flags] [args]"
in Arg.parse speclist (fun anon -> arglist :=  anon :: !arglist) usage_msg;
end

let _ = if is_matching = ref true
then List.iter (fun _ -> print_endline (string_of_int (List.length !arglist))) !arglist
else List.iter print_endline (List.rev(!arglist));;
