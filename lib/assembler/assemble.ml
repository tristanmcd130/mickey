open Common

let assemble instructions: Object.t =
	let buffer = Buffer.create 100 in
	let labels = Hashtbl.create 10 in
	let relocations = Hashtbl.create 10 in
	let arg_to_int = function
	| Instruction.Int i ->
		if i < 0 || i > 4095 then
			failwith "Address out of range";
		i
	| Label l ->
		Hashtbl.replace relocations (Buffer.length buffer / 2) l;
		0 in
	let local_arg int =
		if int < 0 || int > 255 then
			failwith "Argument must be in 0-255"
		else
			int in
	let assemble_instruction = function
	| Instruction.ILodd a -> Buffer.add_uint16_be buffer (arg_to_int a)
	| IStod a -> Buffer.add_uint16_be buffer (1 lsl 12 + arg_to_int a)
	| IAddd a -> Buffer.add_uint16_be buffer (2 lsl 12 + arg_to_int a)
	| ISubd a -> Buffer.add_uint16_be buffer (3 lsl 12 + arg_to_int a)
	| IJpos a -> Buffer.add_uint16_be buffer (4 lsl 12 + arg_to_int a)
	| IJzer a -> Buffer.add_uint16_be buffer (5 lsl 12 + arg_to_int a)
	| IJump a -> Buffer.add_uint16_be buffer (6 lsl 12 + arg_to_int a)
	| ILoco a -> Buffer.add_uint16_be buffer (7 lsl 12 + arg_to_int a)
	| ILodl a -> Buffer.add_uint16_be buffer (8 lsl 12 + local_arg a)
	| IStol a -> Buffer.add_uint16_be buffer (9 lsl 12 + local_arg a)
	| IAddl a -> Buffer.add_uint16_be buffer (10 lsl 12 + local_arg a)
	| ISubl a -> Buffer.add_uint16_be buffer (11 lsl 12 + local_arg a)
	| IJneg a -> Buffer.add_uint16_be buffer (12 lsl 12 + arg_to_int a)
	| IJnze a -> Buffer.add_uint16_be buffer (13 lsl 12 + arg_to_int a)
	| ICall a -> Buffer.add_uint16_be buffer (14 lsl 12 + arg_to_int a)
	| IPshi -> Buffer.add_uint16_be buffer 0b1111000000000000
	| IPopi -> Buffer.add_uint16_be buffer 0b1111001000000000
	| IPush -> Buffer.add_uint16_be buffer 0b1111010000000000
	| IPop -> Buffer.add_uint16_be buffer 0b1111011000000000
	| IRetn -> Buffer.add_uint16_be buffer 0b1111100000000000
	| ISwap -> Buffer.add_uint16_be buffer 0b1111101000000000
	| IInsp a -> Buffer.add_uint16_be buffer (0b1111110000000000 + local_arg a)
	| IDesp a -> Buffer.add_uint16_be buffer (0b1111111000000000 + local_arg a)
	| IHalt -> Buffer.add_uint16_be buffer 0b1111111100000000
	| ILabel l ->
		if Hashtbl.mem labels l then
			failwith (Printf.sprintf "Label %s already defined at %x" l (Hashtbl.find labels l));
		Hashtbl.replace labels l (Buffer.length buffer / 2)
	| IInt i -> Buffer.add_uint16_be buffer i
	| IString s -> String.iter (fun c -> Buffer.add_uint16_be buffer (Char.code c)) s in
	List.iter assemble_instruction instructions;
	{labels; relocations; code = Buffer.to_bytes buffer}