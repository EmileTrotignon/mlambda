module Set = struct
  include Set.Make (Stdlib.String)

  let env_domain map = map |> Env.to_seq |> Seq.map fst |> of_seq

  (* let to_env ~f set =
     fold (fun elt map -> Env.add elt (f elt) map) set Env.empty *)

  let ( + ) = union

  let ( - ) = diff

  let unions sets = List.fold_left union empty sets

  let remove_from_env env set = Env.filter (fun key _ -> not @@ mem key set) env
end

include Stdlib.String