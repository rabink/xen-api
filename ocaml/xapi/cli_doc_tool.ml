open Cli_frontend
open Cli_cmdtable
open Printf

module D = Debug.Debugger(struct let name = "cli_doc_tool" end)
    
let cmdtable : (string, cmd_spec) Hashtbl.t =
  Hashtbl.create 50
    
let get_plain_args cmd_spec = 
    let rec add_args reqd_args_list =
        match reqd_args_list with
        | [] -> ""
        | hd::tail -> sprintf "<arg choice=\"plain\">%s=<replaceable>placeholder</replaceable></arg>\n%s" hd (add_args tail)
    in add_args cmd_spec.reqd

let get_opt_args cmd_spec =    
    let rec add_args optn_args_list =
        match optn_args_list with
        | [] -> ""
        | hd::tail -> sprintf "<arg choice=\"opt\">%s=<replaceable>placeholder</replaceable></arg>\n%s" hd (add_args tail)
    in add_args cmd_spec.optn

let esc_pcdata data =
	let buf = Buffer.create (String.length data + 10) in
	for i = 0 to String.length data - 1
	do
		let s = match data.[i] with
		| '>' -> "&gt;";
		| '<' -> "&lt;";
		| '&' -> "&amp;";
		| '"' -> "&quot;";
		| c when (c >= '\x20' && c <= '\xff')
			|| c = '\x09' || c = '\x0a' || c = '\x0d'
				-> String.make 1 c
		| _ -> ""
		in
		Buffer.add_string buf s
	done;
	Buffer.contents buf

(* sample command section
   <section>
      <title>appliance-assert-can-be-recovered</title>
      <cmdsynopsis>
        <command>appliance-assert-can-be-recovered</command>
        <arg choice="plain">
            uuid=<replaceable>appliance-uuid</replaceable>
        </arg>
        <arg choice="plain">
            database:vdi-uuid=<replaceable>vdi-uuid</replaceable>
        </arg>
      </cmdsynopsis>
      <para>Tests whether storage is available to recover this VM appliance/vApp.</para>
    </section>
*)
let print_cmd_xml_section out_channel command command_spec = 
	Printf.fprintf out_channel
			"<section>
				<title>%s</title>
				<cmdsynopsis>
					<command>%s</command>
					%s
					%s
				</cmdsynopsis>
				<para>%s</para>
			</section>\n" command command (get_plain_args command_spec) (get_opt_args command_spec) (esc_pcdata command_spec.help)

let print_all_commands () = 
    List.iter(fun (n,c) -> printf "%s\n" n ) Cli_frontend.cmdtable_data
    
let load_all_commands () = List.iter
	(fun (n,c)-> Hashtbl.add cmdtable n c)
	(Cli_frontend.cmdtable_data)
    
let missing_commands : (string, cmd_spec) Hashtbl.t = 
    Hashtbl.create 50
    
let _ =
	let xml_file = ref "" in
	let out_file = ref "" in
	Arg.parse (Arg.align [
			"-xml", Arg.Set_string xml_file, "       Name of CLI Reference guild docbook xml";
			"-out", Arg.Set_string out_file, "       Output file. If no file is provided, stdout is used";
			])
			(fun _ -> failwith "Invalid argument. (try -help for help)")
			"Usage: cli_doc_tool [-xml cli_guide_xml] [-out output_file <optional>]";
    if !xml_file = "" then failwith "Must provide CLI Reference guide xml path. (try -help to see usage)";
	ignore(load_all_commands());
	
	let out_ch = if !out_file = "" then stdout else (open_out !out_file) in
	let lb = Lexing.from_channel (open_in !xml_file) in
	let documented_cmds = Cli_doc_lexer.commands lb in	
	Hashtbl.iter (fun n c -> if not (List.mem n documented_cmds) then Hashtbl.add missing_commands n c) cmdtable;
	Hashtbl.iter (fun n c -> if not (List.mem Hidden c.flags) then print_cmd_xml_section out_ch n c) missing_commands;
    