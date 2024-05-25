namespace Zjyslav.Fsharp.Financial

module Npv =
    let calculate cashFlows rate =
        cashFlows
        |> Array.indexed
        |> Array.map (fun (i, cf) -> Dcf.calculate cf rate i)
        |> Array.sum
