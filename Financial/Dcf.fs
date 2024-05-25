namespace Zjyslav.Fsharp.Financial

module Dcf =
    let calculate cashFlow rate period = cashFlow / (pown (1m + rate) period)
