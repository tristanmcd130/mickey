open Common

let rec assemble instructions =
	(* print_endline "ASSEMBLE";
	let labels = Hashtbl.create 10 in
	let rec resolve_labels labels pc = function
	| [] -> ()
	| Instruction.ILabel l :: is ->
		Hashtbl.replace labels l pc;
		resolve_labels labels pc is
	| _ :: is -> resolve_labels labels (pc + 1) is in
	resolve_labels labels 0 instructions; *)
	let object' = Object.create () in
	let arg_to_int = function
	| Instruction.Int i -> i
	| Label l ->
		Object.use_label object' l;
		0 in
		(* match Hashtbl.find_opt labels l with
		| None -> failwith ("Undefined label " ^ l)
		| Some i -> i in *)
	let assemble_instruction = function
	| Instruction.ILodd a -> Object.add_word object' (arg_to_int a)
	| IStod a -> Object.add_word object' (1 lsl 12 + arg_to_int a)
	| IAddd a -> Object.add_word object' (2 lsl 12 + arg_to_int a)
	| ISubd a -> Object.add_word object' (3 lsl 12 + arg_to_int a)
	| IJpos a -> Object.add_word object' (4 lsl 12 + arg_to_int a)
	| IJzer a -> Object.add_word object' (5 lsl 12 + arg_to_int a)
	| IJump a -> Object.add_word object' (6 lsl 12 + arg_to_int a)
	| ILoco a -> Object.add_word object' (7 lsl 12 + arg_to_int a)
	| ILodl a -> Object.add_word object' (8 lsl 12 + a)
	| IStol a -> Object.add_word object' (9 lsl 12 + a)
	| IAddl a -> Object.add_word object' (10 lsl 12 + a)
	| ISubl a -> Object.add_word object' (11 lsl 12 + a)
	| IJneg a -> Object.add_word object' (12 lsl 12 + arg_to_int a)
	| IJnze a -> Object.add_word object' (13 lsl 12 + arg_to_int a)
	| ICall a -> Object.add_word object' (14 lsl 12 + arg_to_int a)
	| IPshi -> Object.add_word object' 0b1111000000000000
	| IPopi -> Object.add_word object' 0b1111001000000000
	| IPush -> Object.add_word object' 0b1111010000000000
	| IPop -> Object.add_word object' 0b1111011000000000
	| IRetn -> Object.add_word object' 0b1111100000000000
	| ISwap -> Object.add_word object' 0b1111101000000000
	| IInsp a -> Object.add_word object' (0b1111110000000000 + a)
	| IDesp a -> Object.add_word object' (0b1111111000000000 + a)
	| IHalt -> Object.add_word object' 0b1111111100000000
	| ILabel l -> Object.add_label object' l
	| IInt i -> Object.add_word object' i in
	List.iter assemble_instruction instructions;
	object'