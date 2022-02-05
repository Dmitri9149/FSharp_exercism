module InterestIsInteresting
let interestRate (balance: decimal): single =
    if compare balance (decimal 0.0 ) < 0 then single 3.213 
    elif compare balance (decimal 1000) < 0 then single 0.5
    elif compare balance (decimal 5000) < 0 then single 1.621
    else single 2.475
let interest (balance: decimal): decimal =
    (balance |> interestRate |> decimal) * balance * (decimal 0.01)
let annualBalanceUpdate(balance: decimal): decimal =
    balance + interest balance 
let amountToDonate(balance: decimal) (taxFreePercentage: float): int = 
    if compare balance (decimal 0.0) < 0 then 0
    else balance * (decimal (taxFreePercentage * 0.01 *2.0)) |> int