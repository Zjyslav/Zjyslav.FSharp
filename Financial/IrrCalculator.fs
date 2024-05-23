namespace Zjyslav.Fsharp.Financial

module IrrCalculator =
    let internal dcf cashFlow rate period =
        cashFlow / (pown (1m + rate) period)

    let internal npv cashFlows rate =
        cashFlows
        |> Array.indexed
        |> Array.map (fun (i, cf) -> dcf cf rate i)
        |> Array.sum

    let rec irr cashFlows initialValue increment precision =
        let presentValue = npv cashFlows initialValue

        if System.Decimal.Round(presentValue, 2) > 0m then
            irr cashFlows (initialValue + increment) increment precision
        else if precision = 0 then
            initialValue
        else
            irr cashFlows (initialValue - increment) (increment / 10m) (precision - 1)
