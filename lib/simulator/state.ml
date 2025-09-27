type t = {
  mutable ac: int;
  mutable pc: int;
  mutable sp: int;
  memory: Bytes.t;
  read_callbacks: (int, t -> unit) Hashtbl.t;
  write_callbacks: (int, t -> unit) Hashtbl.t;
}

let create ?(read_callbacks = Hashtbl.create 0) ?(write_callbacks = Hashtbl.create 0) program = {
  ac = 0;
  pc = 0;
  sp = 3968;
  memory =
    if Bytes.length program > 8192 then
      failwith "Program too big"
    else
      Bytes.extend program 0 (8192 - Bytes.length program);
  read_callbacks;
  write_callbacks;
}
let read state addr =
  (* Printf.printf "READ %x\n" addr; *)
  (match Hashtbl.find_opt state.read_callbacks addr with
  | None -> ()
  | Some c -> c state);
  Bytes.get_uint16_be state.memory (addr * 2)
let write state addr value =
  (* Printf.printf "WRITE %x %x\n" addr value; *)
  Bytes.set_uint16_be state.memory (addr * 2) value;
  match Hashtbl.find_opt state.write_callbacks addr with
  | None -> ()
  | Some c -> c state
let (mod) x y =
  let result = x mod y in
  if result < 0 then
    result + y
  else
    result
let push state value =
  if state.sp <= 0 then
    failwith "Stack overflow";
  state.sp <- state.sp - 1;
  write state state.sp value
let pop state =
  if state.sp >= 4095 then
    failwith "Stack underflow";
  let result = read state state.sp in
  state.sp <- state.sp + 1;
  result
let step state =
  let instruction = read state (let pc = state.pc in state.pc <- state.pc + 1; pc) in
  (* Printf.printf "STEP %d: %x\n" (state.pc - 1) instruction; *)
  let addr = instruction land 4095 in
  let constant = instruction land 255 in
  (match instruction lsr 12 with
  | 0 -> state.ac <- read state addr
  | 1 -> write state addr state.ac
  | 2 -> state.ac <- (state.ac + read state addr) mod 65536
  | 3 -> state.ac <- (state.ac - read state addr) mod 65536
  | 4 ->
    if state.ac lsr 15 = 0 then
      state.pc <- addr
  | 5 ->
    if state.ac = 0 then
      state.pc <- addr
  | 6 -> state.pc <- addr
  | 7 -> state.ac <- addr
  | 8 -> state.ac <- read state (state.sp + addr)
  | 9 -> write state (state.sp + addr) state.ac
  | 10 -> state.ac <- (state.ac + read state (state.sp + addr)) mod 65536
  | 11 -> state.ac <- (state.ac - read state (state.sp + addr)) mod 65536
  | 12 -> 
    if state.ac lsr 15 = 1 then
      state.pc <- addr
  | 13 ->
    if state.ac <> 0 then
      state.pc <- addr
  | 14 ->
    push state state.pc;
    state.pc <- addr
  | 15 ->
    (match (instruction lsr 8) land 15 with
    | 0 -> push state (read state state.ac)
    | 2 -> write state state.ac (pop state)
    | 4 -> push state state.ac
    | 6 -> state.ac <- pop state
    | 8 -> state.pc <- pop state
    | 10 ->
      let tmp = state.ac in
      state.ac <- state.sp;
      state.sp <- tmp
    | 12 ->
      state.sp <- state.sp + constant;
      if state.sp > 4095 then
        failwith "Stack underflow"
    | 14 ->
      state.sp <- state.sp - constant;
      if state.sp < 0 then
        failwith "Stack overflow"
    | _ -> ())
  | _ -> ());
  instruction lsr 8 <> 255
let run state =
  while step state do
    ()
  done