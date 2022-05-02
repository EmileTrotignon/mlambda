include Map.Make (String)

let add key value env = env |> remove key |> add key value

let union env1 env2 = fold add env2 env1
