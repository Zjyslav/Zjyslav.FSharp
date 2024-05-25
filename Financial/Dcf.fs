namespace Zjyslav.Fsharp.Financial

/// Discounted Cash Flows
module Dcf =
    /// <summary>Calculates Discounted Cash Flow.</summary>
    /// <param name="cashFlow">Value of cash flow</param>
    /// <param name="rate">Discount rate
    /// 0.01m = 1%</param>
    /// <param name="period">Number of future period for which the DCF is calculated. For T0 (now) provide 0.</param>
    /// <returns>Present value of future cash flow.</returns>
    let calculate cashFlow rate period = cashFlow / (pown (1m + rate) period)
