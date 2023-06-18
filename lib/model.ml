(*
   The difference of the tiled model and the overlapping model
   is explained:

   https://twitter.com/exppad/status/1267045331004469248
*)

(* let list_sum = List.fold_left (+.) 0.0 *)
let list_sum_int = List.fold_left (+) 0
let array_sum = Array.fold_left (+.) 0.0
let array_sum_int = Array.fold_left (+) 0

module Dir = struct
  type t = N | E | W | S

  let dxy = function
    | N -> (0, -1)
    | E -> (1, 0)
    | W -> (-1, 0)
    | S -> (0, 1)

  let iter f = f N; f E; f W; f S

  let to_string = function
    | N -> "N"
    | E -> "E"
    | W -> "W"
    | S -> "S"
end

type dir = Dir.t
open Dir

module DirArray : sig
  type 'a t
  val of_array : 'a array -> 'a t
  val of_news : ('a * 'a * 'a * 'a) -> 'a t
  val get : 'a t -> dir -> 'a
  val set : 'a t -> dir -> 'a -> unit
  val update : 'a t -> dir -> ('a -> 'a) -> 'a
  val create : (dir -> 'a) -> 'a t
  val iter : (dir -> 'a -> unit) -> 'a t -> unit
  val zeros : int t
end = struct
  type 'a t = { mutable n : 'a; mutable e : 'a; mutable w : 'a; mutable s : 'a }

  let of_array xs =
    assert (Array.length xs = 4);
    { n= xs.(0); e= xs.(1); w= xs.(2); s= xs.(3) }

  let of_news (n,e,w,s) = { n; e; w; s }
  let zeros = of_news (0,0,0,0)

  let create f = of_news (f N, f E, f W, f S)

  let get a = function
    | N -> a.n
    | E -> a.e
    | W -> a.w
    | S -> a.s

  let set a d v =
    match d with
    | N -> a.n <- v
    | E -> a.e <- v
    | W -> a.w <- v
    | S -> a.s <- v

  let update a d f =
    match d with
    | N -> let v = f a.n in a.n <- f a.n; v
    | E -> let v = f a.e in a.e <- f a.e; v
    | W -> let v = f a.w in a.w <- f a.w; v
    | S -> let v = f a.s in a.s <- f a.s; v

  let iter f t =
    f N t.n;
    f E t.e;
    f W t.w;
    f S t.s
end

type tile_info =
  { (* how frequently the tile appears *)
    weight : int ;

    (* possible adjacent tiles *)
    propagator : int list DirArray.t
  }

type config = {
  width : int;
  height : int;

  (* width * height *)
  length : int;

  (* world border buffer size ? *)
  n : int;

  (* loops at the border or not *)
  periodic : bool;

  scanline : bool;
  use_entropy : bool;
  ground : bool;

  (* number of possible tiles *)
  ntiles : int;

  (* tile seleciton weights.  #weights = ntiles *)
  weights : int array;

  propagator : int list DirArray.t array;

  (* Σ w *)
  sumOfWeights : int;

  (* Σ w * log w *)
  sumOfWeightLogWeights : float;

  (* log2 Σw - Σ (w * log2 w) /Σw *)
  startingEntropy : float;
}

let log2 x = log x /. log 2.0

(*  - Σ (p * log2 p)
    = - Σ (w/Σw * log2 (w/Σw))
    = - (Σ (w * log2 (w/Σw))) /Σw
    = - (Σ (w * (log2 w - log2 Σw))) /Σw
    = (Σw * log2 Σw - Σ (w * log2 w)) /Σw
    = Σw * log2 Σw / Σw - Σ (w * log2 w) /Σw
    = log2 Σw - Σ (w * log2 w) /Σw
*)
let _entropy weights =
  let ntiles = Array.length weights in
  (* Σ w *)
  let sumOfWeights = array_sum weights in
  (* w * log2 w *)
  let weightLogWeights =
    Array.init ntiles (fun i -> weights.(i) *. log2 weights.(i))
  in
  (* Σ w * log2 w *)
  let sumOfWeightLogWeights = array_sum weightLogWeights in
  (* log2(Σ w) - Σ (w * log2 w) / Σ w *)
  log2 sumOfWeights -. sumOfWeightLogWeights /. sumOfWeights

let init ~width ~height ~n ~periodic ~use_entropy ~scanline ~ground ~tiles =
  let length = width * height in
  let ntiles = List.length tiles in
  let weights = Array.of_list @@ List.map (fun {weight; _} -> weight) tiles in
  let propagator = Array.of_list @@ List.map (fun ({propagator; _} : tile_info) -> propagator) tiles in
  (* Σ w *)
  let sumOfWeights = array_sum_int weights in
  (* w * log2 w *)
  let weightLogWeights =
    Array.init ntiles (fun i -> float weights.(i) *. log2 (float weights.(i)))
  in
  (* Σ w * log2 w *)
  let sumOfWeightLogWeights = array_sum weightLogWeights in
  (* log2(Σ w) - Σ (w * log2 w) / Σ w *)
  let startingEntropy =
    log2 (float sumOfWeights) -. sumOfWeightLogWeights /. float sumOfWeights
  in
  { length; width; height; n; periodic; ground;
    ntiles; weights; propagator;
    sumOfWeights; sumOfWeightLogWeights; startingEntropy;
    scanline; use_entropy
  }

type state = {
  (* possible tiles *)
  wave : int list array;

  compatible : int DirArray.t array array;

  (* List.length wave.(xy) *)
  sumsOfOnes : int array;

  (* sum (List.map (fun t -> weights.(t)) wave.(xy))
     To update [entropies] fast
  *)
  sumsOfWeights : int array;

  (* sum (List.map (fun t -> let w = weights.(t) in w * log2 w) wave.(xy))
     To update [entropies] fast
  *)
  sumsOfWeightLogWeights : float array;

  (* log2 sumOfWeights.(xy) - sumOfWeightLogWeights.(xy) / sumOfWeights.(xy)
     Updated fast using [sumsOfWeights] and [sumsOfWeightLogWeights]
  *)
  entropies : float array;

  (* banned node with the tile *)
  mutable stack : (int * int) list;

  mutable observedSoFar : int; (* Only for `Scanline. *)
}

let coords config ~xy = (xy mod config.width, xy / config.width)

let nextUnobservedNode rng config state =
  match config.scanline with
  | true ->
      (* Deterministic order from the bottom to the top *)
      let rec loop xy =
        let (x,y) = coords config ~xy in
        if xy = config.length then None
        else if
          (* skip the border area *)
          not config.periodic
          && (x + config.n > config.width
              || y + config.n > config.height)
        then loop (xy+1)
        else if state.sumsOfOnes.(xy) > 1 then begin
          state.observedSoFar <- xy + 1;
          Some xy
        end else loop (xy+1)
      in
      loop state.observedSoFar
  | false ->
      (* Find a node with the lowest entropy, with some noise *)
      let rec loop xy currentMin found =
        let continue () = loop (xy+1) currentMin found in
        if xy = config.length then found
        else if
          not config.periodic
          && (xy mod config.width + config.n > config.width
              || xy / config.width + config.n > config.height) then
          continue ()
        else
          let remainingValues = state.sumsOfOnes.(xy) in
          if remainingValues <= 1 then continue ()
          else
            let entropy =
              match config.use_entropy with
              | true -> state.entropies.(xy)
              | false ->
                  (* do not use entropy but number of the choices *)
                  float remainingValues
            in
            let noise = Random.State.float rng 1e-6 in
            let entropy = entropy +. noise in
            if remainingValues > 1 && entropy <= currentMin then
              loop (xy+1) entropy (Some xy)
            else continue ()
      in
      loop 0 Float.max_float None

(* Force [tile] impossible at [xy] *)
let ban ~xy ~tile config state =
  let _tiles, rest = List.partition (fun t -> t = tile) state.wave.(xy) in
  assert (state.sumsOfOnes.(xy) = List.length state.wave.(xy));
  (* if [rest = []], contradiction *)
  state.wave.(xy) <- rest;
  state.compatible.(xy).(tile) <- DirArray.zeros;
  state.stack <- (xy, tile) :: state.stack;
  state.sumsOfOnes.(xy) <- state.sumsOfOnes.(xy) - 1;

  (* Recalculate entropies, not from scratch but from the previous states *)
  let weight = config.weights.(tile) in
  let sum = state.sumsOfWeights.(xy) - weight in
  let sumLog = state.sumsOfWeightLogWeights.(xy) -. float weight *. log2 (float weight) in
  state.sumsOfWeights.(xy) <- sum;
  state.sumsOfWeightLogWeights.(xy) <- sumLog;
  let sum = float sum in
  state.entropies.(xy) <- log2 sum -. sumLog /. sum

(* Force [tiles] impossible at [xy] *)
let bans ~xy ~tiles config state =
  List.iter (fun tile -> ban ~xy ~tile config state) tiles

let observe ~xy rng config state =
  let w = state.wave.(xy) in
  (* choose a tile *)
  let r =
    let total_weights = list_sum_int @@ List.map (fun i -> config.weights.(i)) w in
    let f = Random.State.int rng total_weights in
    let rec loop f = function
      | [] -> assert false
      | [i] -> i
      | i::is ->
          let f = f - config.weights.(i) in
          if f > 0 then loop f is
          else i
    in
    loop f w
  in
  (* ban the tiles other than [r] *)
  bans ~xy ~tiles:(List.filter (fun tile -> tile <> r) w) config state

let propagate config state =
  let cntr = ref 0 in
  let rec loop () =
    incr cntr;
    match state.stack with
    | [] -> ()
    | (xy1, t1)::stack ->
        (* [t1] is impossible at [xy1] *)
        state.stack <- stack;
        let (x1, y1) = coords config ~xy:xy1 in
        Dir.iter (fun dir ->
            let dx, dy = dxy dir in
            let x2 = x1 + dx in
            let y2 = y1 + dy in
            if
              not config.periodic
              && (x2 < 0 || y2 < 0 || x2 + config.n > config.width || y2 + config.n > config.height)
            then
              () (* out of bounds *)
            else
              let x2 =
                if x2 < 0 then x2 + config.width
                else if x2 >= config.width then x2 - config.width
                else x2
              in
              let y2 =
                if y2 < 0 then y2 + config.height
                else if y2 >= config.height then y2 - config.height
                else y2
              in
              let xy2 = x2 + y2 * config.width in
              (* [p] : tiles possible at [dir] of [t1] *)
              let p = DirArray.get config.propagator.(t1) dir in
              let compat = state.compatible.(xy2) in
              let tzeros =
                List.filter (fun t2 ->
                    (* 1 less enabler of [t2] at [xy2] from [-dir] *)
                    let comp = compat.(t2) in
                    let n = DirArray.update comp dir (fun n -> n - 1) in
                    (* if no enabler of [t2] at [xy2] from [-dir], [t2] is impossible. *)
                    n = 0) p
              in
              bans ~xy:xy2 ~tiles:tzeros config state
          );
        loop ()
  in
  loop ();
  (* if the state falls into a contradiction, all points fall to 0 *)
  state.sumsOfOnes.(0) > 0, !cntr

let clear config =
  let wave = Array.init config.length (fun _ -> List.init config.ntiles Fun.id) in
  let compatible =
    Array.init config.length (fun _ ->
        Array.init config.ntiles (fun t ->
            DirArray.of_news
              ( (* opposite *)
                List.length (DirArray.get config.propagator.(t) S),
                List.length (DirArray.get config.propagator.(t) W),
                List.length (DirArray.get config.propagator.(t) E),
                List.length (DirArray.get config.propagator.(t) N) )))
  in
  let sumsOfOnes = Array.init config.length (fun _ -> config.ntiles) in
  let sumsOfWeights = Array.init config.length (fun _ -> config.sumOfWeights) in
  let sumsOfWeightLogWeights = Array.init config.length (fun _ -> config.sumOfWeightLogWeights) in
  let entropies = Array.init config.length (fun _ -> config.startingEntropy) in
  let state =
    { wave; compatible; sumsOfOnes; sumsOfWeights; sumsOfWeightLogWeights; entropies; observedSoFar = 0; stack = [] }
  in
  if config.ground then
    for x = 0 to config.width - 1 do
      (* The last tile is for the ground *)
      let gtile = config.ntiles - 1 in
      (* Ground: fix by gtile *)
      for tile = 0 to gtile - 1 do
        bans ~xy:(x + (config.height - 1) * config.width) ~tiles:[tile] config state;
      done;
      (* No ground: exclude the last tile *)
      for y = 0 to config.height - 2 do
        bans ~xy:(x + y * config.width) ~tiles:[gtile] config state;
      done
    done;
  state

let run rng ~limit config =
  let state = clear config in
  let rec loop l =
    if l = Some 0 then `Error
    else
      let l = Option.map (fun l -> l - 1) l in
      match nextUnobservedNode rng config state with
      | Some xy ->
          observe ~xy rng config state;
          let success, _cntr = propagate config state in
          if not success then `PropError
          else loop l
      | None ->
          `Finished (Array.map (function
              | [t] -> t
              | [] -> -1
              | _::_::_ -> assert false) state.wave)
  in
  loop limit
