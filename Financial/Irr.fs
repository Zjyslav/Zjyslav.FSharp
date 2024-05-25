namespace Zjyslav.Fsharp.Financial

/// Internal Rate of Return
module Irr =
    /// <summary>Caluclates Internal Rate of Return by finding a discount rate for which NPV of cash flows = 0.</summary>
    /// <param name="cashFlows">Array with values of cash flows.
    /// First element is T0.</param>
    /// <param name="initialValue">The starting point of calculation.
    /// If it's higher than actual IRR, return value will not be correct.
    /// If the approximate IRR is not known, initialValue of 0 is recommended.</param>
    /// <param name="increment">Value by which initial value will be incremented in each iteration.
    /// It determines the starting precision of calculation, but it's more efficient to use higher presision parameter than low increment.
    /// Recommended value is 0.1m</param>
    /// <param name="precision">Number of additional decimal places of precision for return value.
    /// For increment = 0.1m, precision = 2 means that the result will be calculated up to 0.001m</param>
    /// <returns>Internal Rate of Return.
    /// 0.01m = 1%</returns>
    let rec calculate cashFlows initialValue increment precision =
        let presentValue = Npv.calculate cashFlows initialValue

        if System.Decimal.Round(presentValue, 2) > 0m then
            calculate cashFlows (initialValue + increment) increment precision
        else if precision = 0 then
            initialValue
        else
            calculate cashFlows (initialValue - increment) (increment / 10m) (precision - 1)
