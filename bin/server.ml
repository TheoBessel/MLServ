(* A web server written from scratch in OCaml *)

let verbose : bool = false;;

let addr : Unix.sockaddr = Unix.ADDR_INET(Unix.inet_addr_of_string "127.0.0.1", 8000);;

let dom : Unix.socket_domain = Unix.domain_of_sockaddr addr;;

let typ : Unix.socket_type = Unix.SOCK_STREAM;;

(*let packet : bytes = Bytes.of_string "Server response";;*)

let flags : Unix.msg_flag list = [Unix.MSG_DONTROUTE];;

let packet_size : int = 182;;


let print_info(s : string) =
	if verbose then
		print_string s
	else ();;

let print_int_info(i : int) =
	if verbose then
		print_int i
	else ();;

let print_line_info(s : string) =
	if verbose then
		print_endline s
	else ();;

let print_addr (a : Unix.sockaddr) =
	match a with
		| Unix.ADDR_UNIX s -> print_string s
		| Unix.ADDR_INET (i,p) -> let s = Unix.string_of_inet_addr i in
				if s = "127.0.0.1" then print_string "localhost" else print_string s;
				print_string "."; print_int p;;

let print_thread () : unit =
	if verbose then
		begin
			print_string "[t"; print_int ((Domain.self ()) :> int); print_string "] - ";
		end
	else ();;



let rec handle_messages (socket : Unix.file_descr) (addr : Unix.sockaddr) =
	let buffer = Bytes.create packet_size in
	print_thread (); print_line_info "[5/5] Waiting for a client message ...";
	let _ = Unix.recv socket buffer 0 packet_size flags in
	print_thread (); print_line_info "[5/5] Recieved client message !";
	print_thread (); print_line_info "------------------------------";
	print_thread (); print_line_info "Message is :";
	
	if verbose then
		(print_thread (); print_bytes buffer; print_line_info "")
	else
		(print_string "["; print_addr addr; print_string "] : "; print_bytes buffer; print_endline "");

	print_thread (); print_line_info "------------------------------";
	match (String.sub (Bytes.to_string buffer) 0 13) with
		| "end connexion" -> ()
		| _ -> handle_messages socket addr;;



let connect_client (socket : Unix.file_descr) (addr : Unix.sockaddr) : unit =
	print_info "[4/5] New thread created : t"; print_int_info ((Domain.self ()) :> int); print_line_info " !";

	Domain.at_exit (fun () -> print_info "[5/5] Exited thread : t"; print_int_info ((Domain.self ()) :> int); print_line_info " !";);

	handle_messages socket addr;

	Unix.shutdown socket Unix.SHUTDOWN_RECEIVE;;



let rec handle_clients (server : Unix.file_descr) =
	print_line_info "[3/5] Accepting entering connexion ...";
	let (s,a) = Unix.accept server in
	print_line_info "[3/5] Entering connexion accepted !";
	
	print_line_info "[4/5] Creating a new thread ...";
	let _ = Domain.spawn (fun () -> connect_client s a) in
	handle_clients server;;



let main () : unit =
	print_line_info "[1/5] Creating web socket ...";
	let server : Unix.file_descr = Unix.socket dom typ 0 in
	print_line_info "[1/5] Web socket sucessfully created !";

	print_line_info "[2/5] Binding socket to address ...";
	Unix.bind server addr;
	Unix.listen server 1;
	print_line_info "[2/5] Sucessfully bound socket to address !";
	
	let t :  unit Domain.t = Domain.spawn (fun () -> handle_clients server) in
	Domain.join t;;
	
let _ = main ();