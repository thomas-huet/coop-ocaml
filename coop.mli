type 'a t

type 'a signal

val return : 'a -> 'a t

val bind : 'a t -> ('a -> 'b t) -> 'b t

val map : 'a t -> ('a -> 'b) -> 'b t

val create_signal : unit -> 'a signal

val wait : 'a signal -> 'a t

val signal : 'a signal -> 'a -> unit

val broadcast : 'a signal -> 'a -> unit

val cancel : 'a t -> unit
