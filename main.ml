module SubStr = struct
	type t = {
		s: string;
		i: int;
		len: int;
	}

	let (@) (t : t) (i : int) = String.get t.s (i + t.i)

	let of_string (s : string) : t = {s=s; i=0; len=(String.length s)}

	let to_string (t : t) : string = String.sub t.s t.i t.len

	let uncons (t : t) : (char * t) option =
		if t.len > 0 then
			Some (t @ 0, {t with i=(t.i + 1); len=(t.len - 1)})
		else
			None
	
	let skip (t : t) (c : char) : t =
		let rec aux t =
			match uncons t with
			| Some (c', tail) when c' = c -> aux tail
			| _ -> t
		in aux t
	
	let partition (t : t) (c : char) : (t * t) =
		let rec aux t =
			match uncons t with
			| Some (c', tail) -> if c' = c then
				t
			else
				aux tail
			| _ -> t
		in
		let t2 = aux t in
		({t with len=(t2.i - t.i)}, t2)
end

module Sexp = struct
	type t = {
		kind: string;
		range: (int * int);
		children: (string * t) list;
	}

	let make kind range children = {kind; range; children}

	let empty = make "" (0, 0) []

	let from_string (s : string) : t =
		let module S = SubStr in
		let ios s = S.to_string s |> int_of_string in
		let skip_ws s = S.skip s ' ' in
		let parse_kind s = match S.uncons s with
			| Some ('(', tail) ->
				let (kind, tail) = S.partition tail ' ' in (S.to_string kind, tail)
			| _ -> assert false
		in
		let parse_range s = match S.uncons s with
			| Some ('[', tail) ->
				let (start_byte, tail) = S.partition tail ':' in
				let tail = S.skip tail ':' in
				let (end_byte, tail) = S.partition tail ']' in
				let tail = S.skip tail ']' in
				(ios start_byte, ios end_byte, tail)
			| _ -> assert false
		in
		let parse_field s =
			let (field, tail) = S.partition s ':' in
			let tail = S.skip tail ':' in
			(S.to_string field, tail)
		in

		let rec make_sexp (s : S.t) : (t * S.t) =
			let s = skip_ws s in
			let (kind, s) = parse_kind s in 
			let s = skip_ws s in
			let (start_byte, end_byte, s) = parse_range s in

			let make_children (s : S.t) : ((string * t) list * S.t) =
				let rec aux s field lst =
					let s = skip_ws s in
					match S.uncons s with
					| Some ('(', _) -> (* anon child *)
						let (child, tail) = make_sexp s in
						aux tail "" ((field, child)::lst)
					| Some (')', tail) -> (* end *)
						(lst, tail)
					| Some _ -> (* field *)
						let (field', tail) = parse_field s in
						aux tail field' lst
					| None -> assert false (* invalid *)
				in
				aux s "" []
			in

			make_children s |> fun (children, s) ->
				let expr = (make kind (start_byte, end_byte) children) in
				(expr, s)
		in

		make_sexp (S.of_string s) |> fst

	let from_file (path : string) : t =
		let ch = open_in path in
		let s = input_line ch in
		close_in ch; from_string s
end

let (let+) a b = if a = None then raise Exit else Option.get a |> b

let sexp =
	let+ path = match Sys.argv with
		| [|_; path|] -> Some path
		| _ -> None
	in

	Sexp.from_file path

