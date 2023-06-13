type config

module Dir : sig
  type t = N | E | W | S

  val dxy : t -> int * int
  val iter : (t -> unit) -> unit
  val to_string : t -> string
end

module DirArray : sig
  type 'a t
  val of_array : 'a array -> 'a t
  val of_news : ('a * 'a * 'a * 'a) -> 'a t
  val create : (Dir.t -> 'a) -> 'a t
  val get : 'a t -> Dir.t -> 'a
  val set : 'a t -> Dir.t -> 'a -> unit
  val iter : (Dir.t -> 'a -> unit) -> 'a t -> unit
end

type tile_info = { weight : int ; propagator : int list DirArray.t }

val init :
  width:int ->
  height:int ->
  n:int ->
  periodic:bool ->
  use_entropy:bool ->
  scanline:bool ->
  ground:bool ->
  tiles: tile_info list ->
  config

val run :
  Random.State.t ->
  limit:int option ->
  config -> [> `Error | `PropError | `Finished of int array ]
