type 'a t

type 'a signal

type key

val return : 'a -> 'a t

val bind : 'a t -> ('a -> 'b t) -> 'b t

val map : 'a t -> ('a -> 'b) -> 'b t

val create_signal : unit -> 'a signal

val wait : 'a signal -> 'a t

val signal : 'a signal -> 'a -> unit

val broadcast : 'a signal -> 'a -> unit

val cancel : 'a t -> unit

val main : 'a t -> 'a option

val new_key : unit -> key

val watchers : (key, unit -> unit) Hashtbl.t

module Io : sig
  type in_channel

  val stdin : in_channel

  val open_in : string -> in_channel

  val read : in_channel -> int -> string t
end
