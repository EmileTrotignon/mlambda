module EphTable = Ephemeron.K1.Make (struct
  type t = Obj.t

  let hash = Hashtbl.hash

  let equal = ( == )
end)

module PhysTable = Hashtbl.Make (struct
  type t = Obj.t

  let hash = Hashtbl.hash

  let equal = ( == )
end)

module type ObjTable = sig
  type 'a t

  type key = Obj.t

  val create : int -> 'a t

  val find_opt : 'a t -> key -> 'a option

  val add : 'a t -> key -> 'a -> unit
end

type ('a, 'b) decorator = ('a -> 'b) -> 'a -> 'b

type ('a, 'b) rec_decorator = (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b

let index ?(start = 0) bound f =
  let cache = Array.make bound None in
  fun arg ->
    let index = arg - start in
    assert (index < bound) ;
    match cache.(index) with
    | Some r ->
        r
    | None ->
        let r = f arg in
        cache.(index) <- Some r ; r

let memo (module Table : ObjTable) f =
  let cache = Table.create 256 in
  fun arg ->
    let arg_obj = Obj.repr arg in
    match Table.find_opt cache arg_obj with
    | None ->
        let result = f arg in
        Table.add cache arg_obj result ;
        result
    | Some result ->
        result

(*
@mydec
def myfunc():
  ...


def myfunc():
  ...
myfunc=mydec(myfunc)

*)
let memo_rec (module Table : ObjTable) f =
  let cache = Table.create 256 in
  let rec self arg =
    let arg_obj = Obj.repr arg in
    match Table.find_opt cache arg_obj with
    | None ->
        let result = f self arg in
        Table.add cache arg_obj result ;
        result
    | Some result ->
        result
  in
  self

let module_of_policy policy =
  match policy with
  | `gc ->
      (module EphTable : ObjTable)
  | `perm ->
      (module PhysTable : ObjTable)

let memo_rec policy = memo_rec (module_of_policy policy)

let memo policy = (memo (module_of_policy policy) [@dec memo_rec `gc])
