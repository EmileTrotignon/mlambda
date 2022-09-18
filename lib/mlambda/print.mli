open Ast

module type Printer = sig
  type a

  val output : out_channel -> a -> unit

  val print : a -> unit

  val to_string : a -> string

  val pp : Format.formatter -> a -> unit
end

module Value : Printer with type a = value

module Env : Printer with type a = value Env.t

module Expr : Printer with type a = expr

module Pattern : Printer with type a = pattern

module Struct_item : Printer with type a = struct_item

module Program : Printer with type a = program
