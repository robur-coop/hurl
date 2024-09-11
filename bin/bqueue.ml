type 'a t = {
    buffer: 'a array
  ; mutable rd: int
  ; mutable wr: int
  ; mutable closed: bool
  ; lock: Miou.Mutex.t
  ; non_empty: Miou.Condition.t
  ; non_full: Miou.Condition.t
}

let create size v =
  let lock = Miou.Mutex.create () in
  let non_empty = Miou.Condition.create () in
  let non_full = Miou.Condition.create () in
  {
    buffer= Array.make size v
  ; lock
  ; rd= 0
  ; wr= 0
  ; closed= false
  ; non_empty
  ; non_full
  }

let put t data =
  Miou.Mutex.protect t.lock @@ fun () ->
  if t.closed then invalid_arg "Bqueue.put";
  while (t.wr + 1) mod Array.length t.buffer = t.rd do
    Miou.Condition.wait t.non_full t.lock
  done;
  t.buffer.(t.wr) <- data;
  t.wr <- (t.wr + 1) mod Array.length t.buffer;
  Miou.Condition.signal t.non_empty

let close t =
  Miou.Mutex.protect t.lock @@ fun () ->
  t.closed <- true;
  Miou.Condition.signal t.non_empty

let get t =
  let ret =
    Miou.Mutex.protect t.lock @@ fun () ->
    while t.wr = t.rd && not t.closed do
      Miou.Condition.wait t.non_empty t.lock
    done;
    if t.wr = t.rd && t.closed then None
    else begin
      let data = t.buffer.(t.rd) in
      t.rd <- (t.rd + 1) mod Array.length t.buffer;
      Miou.Condition.signal t.non_full;
      Some data
    end
  in
  ret
