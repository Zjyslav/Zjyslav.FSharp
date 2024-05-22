namespace Zjyslav.Fsharp.Financial

module IrrCalculator =
    let internal dcf cashFlow rate period =
        cashFlow / (pown (1.0 + rate) period)

    let internal npv cashFlows rate =
        cashFlows
        |> Array.indexed
        |> Array.map (fun (i, cf) -> dcf cf rate i)
        |> Array.sum

    let rec irr cashFlows initialValue increment precision =
        let presentValue = npv cashFlows initialValue

        if System.Math.Round(presentValue, 2) > 0 then
            irr cashFlows (initialValue + increment) increment precision
        else if precision = 0 then
            initialValue
        else
            irr cashFlows (initialValue - increment) (increment / 10.0) (precision - 1)