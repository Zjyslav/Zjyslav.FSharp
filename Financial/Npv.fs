namespace Zjyslav.Fsharp.Financial

/// Net Present Value
module Npv =
    /// <summary>Calculates Net Present Value of cash flows using discount rate.</summary>
    /// <param name="cashFlows">Array with values of cash flows. First element is T0.</param>
    /// <param name="rate">Discount rate
    /// 0.01m = 1%</param>
    /// <returns>Sum of discounted cash flows</returns>
    let calculate cashFlows rate =
        cashFlows
        |> Array.indexed
        |> Array.map (fun (i, cf) -> Dcf.calculate cf rate i)
        |> Array.sum
