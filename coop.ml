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
