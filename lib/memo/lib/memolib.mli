type ('a, 'b) decorator = ('a -> 'b) -> 'a -> 'b

type ('a, 'b) rec_decorator = (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b

val tabulate : ?start:int -> int -> (int, 'a) decorator

val memo : [< `gc | `perm] -> ('a, 'b) decorator

val memo_rec : [< `gc | `perm] -> ('a, 'b) rec_decorator
