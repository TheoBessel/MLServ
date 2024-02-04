(* A web client written from scratch in OCaml *)

let addr : Unix.sockaddr = Unix.ADDR_INET(Unix.inet_addr_of_string "127.0.0.1", 8000)

let dom : Unix.socket_domain = Unix.domain_of_sockaddr addr

let typ : Unix.socket_type = Unix.SOCK_STREAM

let flags : Unix.msg_flag list = [Unix.MSG_DONTROUTE];;

let () =
	print_endline "[1/4] Creating web socket ...";
	let client : Unix.file_descr = Unix.socket dom typ 0 in
	print_endline "[1/4] Web socket sucessfully created !";

	print_endline "[2/4] Connecting to server ...";
	Unix.connect client addr;
	print_endline "[2/4] Sucessfully connected to server !";

	let maintain_connexion : bool ref = ref(true) in

	while !maintain_connexion do
		print_endline "Enter your message : ";
		let packet : bytes = Bytes.of_string (read_line ()) in

		print_endline "[3/4] Sending message to server !";
		let _ = Unix.send client packet 0 (Bytes.length packet) flags in
		print_endline "[3/4] Message sent to server !";

		maintain_connexion := not ((Bytes.to_string packet) = "end connexion");
	done;
	print_endline "[4/4] Connexion ended !";