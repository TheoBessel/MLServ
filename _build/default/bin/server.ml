(* A web server written from scratch in OCaml *)

let addr : Unix.sockaddr = Unix.ADDR_INET(Unix.inet_addr_of_string "192.168.1.99", 8000);;

let dom : Unix.socket_domain = Unix.domain_of_sockaddr addr;;

let typ : Unix.socket_type = Unix.SOCK_STREAM;;

(*let packet : bytes = Bytes.of_string "Server response";;*)

let flags : Unix.msg_flag list = [Unix.MSG_DONTROUTE];;

let packet_size : int = 182;;



let print_thread () : unit =
	print_string "[t";
	print_int ((Domain.self ()) :> int);
	print_string "] - ";;



let rec handle_messages socket =
	let buffer = Bytes.create packet_size in
	(*print_thread (); print_endline "[5/5] Waiting for a client message ...";*)
	let _ = Unix.recv socket buffer 0 packet_size flags in
	(*print_thread (); print_endline "[5/5] Recieved client message !";
	print_thread (); print_endline "------------------------------";
	print_thread (); print_endline "Message is :";*)
	print_thread (); print_bytes buffer; print_endline "";
	(*print_thread (); print_endline "------------------------------";*)
	match (String.sub (Bytes.to_string buffer) 0 13) with
		| "end connexion" -> ()
		| _ -> handle_messages socket;;



let connect_client (socket : Unix.file_descr) : unit =
	print_string "[4/5] New thread created : t"; print_int ((Domain.self ()) :> int); print_endline " !";

	Domain.at_exit (fun () -> print_string "[5/5] Exited thread : t"; print_int ((Domain.self ()) :> int); print_endline " !";);

	handle_messages socket;

	Unix.shutdown socket Unix.SHUTDOWN_RECEIVE;;



let rec handle_clients (server : Unix.file_descr) =
	print_endline "[3/5] Accepting entering connexion ...";
	let (s,_) = Unix.accept server in
	print_endline "[3/5] Entering connexion accepted !";
	
	print_endline "[4/5] Creating a new thread ...";
	let _ = Domain.spawn (fun () -> connect_client s) in
	handle_clients server;;



let main () : unit =
	print_endline "[1/5] Creating web socket ...";
	let server : Unix.file_descr = Unix.socket dom typ 0 in
	print_endline "[1/5] Web socket sucessfully created !";

	print_endline "[2/5] Binding socket to address ...";
	let _ = Unix.bind server addr in
	let _ = Unix.listen server 1 in (* 10 simultaneous clients allowed *)
	print_endline "[2/5] Sucessfully bound socket to address !";
	
	let t :  unit Domain.t = Domain.spawn (fun () -> handle_clients server) in
	Domain.join t;;
	
let _ = main ();