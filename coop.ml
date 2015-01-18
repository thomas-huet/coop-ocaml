type 'a signal = ('a -> bool) Queue.t

type 'a state =
| Cancelled
| Done of 'a
| Waiting of 'a signal

type 'a t = 'a state ref

let return x = ref (Done x)

let signal s x = while not (Queue.is_empty s) && not ((Queue.pop s) x) do () done

let broadcast s x =
  while not (Queue.is_empty s) do
    (Queue.pop s) x;
  done

let rec bind coop f = match !coop with
| Cancelled -> ref Cancelled
| Done x -> f x
| Waiting q ->
  let ans = ref (Waiting (Queue.create ())) in
  let continue x = match !ans with
  | Cancelled -> false
  | Done _ -> assert false
  | Waiting q ->
    ans := !(f x);
    match !ans with
    | Cancelled -> false
    | Done y ->
      broadcast q y;
      true
    | Waiting p ->
      Queue.transfer q p;
      true
  in
  Queue.push continue q;
  ans

let map coop f = bind coop (fun x -> return (f x))

let create_signal = Queue.create

let wait s =
  let coop = ref (Waiting (Queue.create ())) in
  let continue x = match !coop with
  | Cancelled -> false
  | Done _ -> assert false
  | Waiting q ->
    coop := Done x;
    broadcast q x;
    true
  in
  Queue.push continue s;
  coop

let cancel coop = coop := Cancelled

type key = int

let watchers = Hashtbl.create 42

let rec main coop = match !coop with
| Cancelled -> None
| Done x -> Some x
| Waiting _ -> Hashtbl.iter (fun _ f -> f ()) watchers; main coop

let new_key =
  let n = ref 0 in
  fun () -> (incr n; !n)

module Io = struct
  type in_channel = Pervasives.in_channel

  let open_in = open_in_gen [Open_nonblock] 0

  let stdin = open_in "/dev/stdin"

  let read ic len =
    let buf = Bytes.create len in
    try
      let real = input ic buf 0 len in
      ref (Done (Bytes.sub_string buf 0 real))
    with Sys_blocked_io ->
      let coop = ref (Waiting (Queue.create ())) in
      let key = new_key () in
      let watcher () =
	match !coop with
	| Cancelled -> Hashtbl.remove watchers key
        | Done _ -> assert false
	| Waiting q -> try
          let real = input ic buf 0 len in
          let s = Bytes.sub_string buf 0 real in
          Hashtbl.remove watchers key;
          coop := Done s;
          broadcast q s
        with Sys_blocked_io -> ()
    in
    Hashtbl.add watchers key watcher;
    coop
end
