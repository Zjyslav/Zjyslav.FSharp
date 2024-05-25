namespace Zjyslav.Fsharp.Financial

module Irr =
    let rec calculate cashFlows initialValue increment precision =
        let presentValue = Npv.calculate cashFlows initialValue

        if System.Decimal.Round(presentValue, 2) > 0m then
            calculate cashFlows (initialValue + increment) increment precision
        else if precision = 0 then
            initialValue
        else
            calculate cashFlows (initialValue - increment) (increment / 10m) (precision - 1)
