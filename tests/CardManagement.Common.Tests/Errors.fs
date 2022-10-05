module CardManagement.Common.Tests.Errors

open FsCheck.Xunit
open CardManagement.Common
open CardManagement.Common.ValidationResult

// https://en.wikibooks.org/wiki/Haskell/Applicative_functors#Applicative_functor_laws
[<Property(Verbose = true)>]
let ``Validation result apply identity law`` (x:Errors.ValidationResult<int>) =
    Ok id <*> x = x

// https://en.wikibooks.org/wiki/Haskell/Applicative_functors#Applicative_functor_laws
[<Property(Verbose = true)>]
let ``Validation result apply homomorphism law`` (f:int->int,x) =
    Ok f <*> Ok x = Ok (f x)