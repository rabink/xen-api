{
	let ext_ent_ht = Hashtbl.create 2
}

let ws = [' ' '\t' '\n' '\r']

let data = [^'<' '>']* 
let name = [^'<' '>' '\n' ' ' '\t' '\r']
let identchar =  ['A'-'Z' 'a'-'z' '_' '0'-'9' ':' '-']
let id = ws* "id" ws* "=" ws*

rule commands = parse
	| eof { raise End_of_file }
	| "<!DOCTYPE" { process_dtd lexbuf }
	| "<section" id "\"cli-xe-commands\">" { cli_cmd_section 0 lexbuf }
	| _ { commands lexbuf }
	
and process_dtd = parse
	| '[' { process_data lexbuf }
	| '>' { commands lexbuf }
	| _ { process_dtd lexbuf }

and process_data = parse
	| "<!ENTITY" 
		{ 
			let ent_name = entity_name lexbuf in
			let ent_value = entity_value lexbuf in
			Hashtbl.add ext_ent_ht ent_name ent_value;
			process_data lexbuf
		}
	| ']' { process_dtd lexbuf }
	| _ { process_data lexbuf }
	
and entity_name = parse
	| '%'?
	| ws+ { entity_name lexbuf }
	| identchar+ { Lexing.lexeme lexbuf }
	
and entity_value = parse
	| ws+
	| "SYSTEM" { entity_value lexbuf }
	| "\"" ((identchar | '/' | '.')+ as ent_val) "\"" { ent_val }
	
and cli_cmd_section sec_level = parse
	| "<cmdsynopsis" data  ">" 
		{
			ignore_spaces lexbuf;
			let cmd = cmdsynopsis lexbuf in
			cmd :: (cli_cmd_section sec_level lexbuf)
		}
	| "<section" data ">" { cli_cmd_section (sec_level + 1) lexbuf }
	| "</section>"
		{ 
			if sec_level = 0 then []
			else cli_cmd_section (sec_level - 1) lexbuf 
		}	
	| _  { cli_cmd_section sec_level lexbuf }
	
and cmdsynopsis = parse
	| "</cmdsynopsis>" { "" }
	| "<command" data ">" ( data as command ) "</command>" { command }
	| _ { cmdsynopsis lexbuf }
	
and ignore_spaces = parse
	| ws+ { ignore_spaces lexbuf }
	| "" { }

{	
}		