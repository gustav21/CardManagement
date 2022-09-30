module CardManagement.Tests.CardDomain

open FsCheck.Xunit
open Common
open CardManagement.CardDomain

[<Property(Arbitrary = [| typeof<``16DigitsStringAndWithSpacesEvery4Digits``> |], Verbose = true)>]
let ``Card number with spaces every 4 digits`` (cardNumber,cardNumberWithSpaces) = 
    let createCardNumber = CardNumber.create "some name"
    createCardNumber cardNumber = createCardNumber cardNumberWithSpaces