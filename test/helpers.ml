open Mlambda
open Result_monad

let ( |.> ) v f = f v ; v

let dps_name name = name ^ "_dps"

let tailrecness env funcname =
  Alcotest.(check (result unit string))
    (sprintf "is %s tailrec" funcname)
    (Ok ())
    (let* is_rec, body =
       match funcname |> dps_name |> Program.find_def env with
       | None ->
           (* Program.print env ; *)
           Error
             (sprintf "Tailrecness : %S not found in env" (dps_name funcname))
       | Some (is_rec, body) ->
           Ok (is_rec, body)
     in
     if not is_rec then
       Error (sprintf "Tailrecness : %S is not recursive" (dps_name funcname))
     else if Expr.is_tailrec funcname body then Ok ()
     else
       Error
         (sprintf "Tailrecness : %S is not tail-recursive" (dps_name funcname))
    )

module type Test_params = sig
  val log : string

  val transform : Ast.program -> Ast.program

  val expects_tailrec : bool
end

module type Tests = sig
  val tests : unit Alcotest.test_case list
end

let value_test name v1 v2 =
  Alcotest.(check (Alcotest.testable Value.pp Value.( = )) name v1 v2)
