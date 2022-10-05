namespace CardManagement

(*
This module contains command models, validated commands and validation functions.
In C# common pattern is to throw exception if input is invalid and pass it further if it's ok.
Problem with that approach is if we forget to validate, the code will compile and a program
either won't crash at all, or it will in some unexpected place. So we have to cover that with unit tests.
Here however we use different types for validated entities.
So even if we try to miss validation, the code won't even compile.
*)
module CardDomainCommandModels =
    open CardManagement.Common
    open CardDomain
    open CardManagement.Common.Errors
    open FsToolkit.ErrorHandling
    open ValidationResult

    type ActivateCommand = { CardNumber: CardNumber }

    type DeactivateCommand = { CardNumber: CardNumber }

    type SetDailyLimitCommand =
        { CardNumber: CardNumber
          DailyLimit: DailyLimit }

    type ProcessPaymentCommand =
        { CardNumber: CardNumber
          PaymentAmount: MoneyTransaction }

    type TopUpCommand =
        { CardNumber: CardNumber
          TopUpAmount: MoneyTransaction }

    [<CLIMutable>]
    type ActivateCardCommandModel =
        { CardNumber: string }

    [<CLIMutable>]
    type DeactivateCardCommandModel =
        { CardNumber: string }

    [<CLIMutable>]
    type SetDailyLimitCardCommandModel =
        { CardNumber: string
          Limit: decimal }

    [<CLIMutable>]
    type ProcessPaymentCommandModel =
        { CardNumber: string
          PaymentAmount: decimal }

    [<CLIMutable>]
    type TopUpCommandModel =
        { CardNumber: string
          TopUpAmount: decimal }

    [<CLIMutable>]
    type CreateAddressCommandModel =
        { Country: string
          City: string
          PostalCode: string
          AddressLine1: string
          AddressLine2: string }

    [<CLIMutable>]
    type CreateUserCommandModel =
        { Name: string
          Address: CreateAddressCommandModel option }

    [<CLIMutable>]
    type CreateCardCommandModel =
        { CardNumber : string
          Name: string
          ExpirationMonth: uint16
          ExpirationYear: uint16
          UserId: UserId option }

    (*
    This is a brief API description made with just type aliases.
    As you can see, every public function here returns a `Result` with possible `ValidationError`.
    No other error can occur in here.
    *)
    type ValidateActivateCardCommand   = ActivateCardCommandModel      -> ValidationResult<ActivateCommand>
    type ValidateDeactivateCardCommand = DeactivateCardCommandModel    -> ValidationResult<DeactivateCommand>
    type ValidateSetDailyLimitCommand  = SetDailyLimitCardCommandModel -> ValidationResult<SetDailyLimitCommand>
    type ValidateProcessPaymentCommand = ProcessPaymentCommandModel    -> ValidationResult<ProcessPaymentCommand>
    type ValidateTopUpCommand          = TopUpCommandModel             -> ValidationResult<TopUpCommand>
    type ValidateCreateAddressCommand  = CreateAddressCommandModel     -> ValidationResult<Address>
    type ValidateCreateUserCommand     = CreateUserCommandModel        -> ValidationResult<UserInfo>
    type ValidateCreateCardCommand     = CreateCardCommandModel        -> ValidationResult<Card>

    let private validateCardNumber = CardNumber.create "cardNumber"

    let validateActivateCardCommand : ValidateActivateCardCommand =
        fun cmd ->
            result {
                let! number = cmd.CardNumber |> validateCardNumber
                return { ActivateCommand.CardNumber = number }
            }

    let validateDeactivateCardCommand : ValidateDeactivateCardCommand =
        fun cmd ->
            result {
                let! number = cmd.CardNumber |> validateCardNumber
                return { DeactivateCommand.CardNumber = number }
            }

    let validateSetDailyLimitCommand : ValidateSetDailyLimitCommand =
        fun cmd ->
            result {
                let! number = cmd.CardNumber |> validateCardNumber
                let limit = DailyLimit.ofDecimal cmd.Limit
                return
                    { CardNumber = number
                      DailyLimit = limit }
            }

    let validateProcessPaymentCommand : ValidateProcessPaymentCommand =
        fun cmd ->
            result {
                let! number = cmd.CardNumber |> validateCardNumber
                let! amount = cmd.PaymentAmount |> MoneyTransaction.create
                return
                    { ProcessPaymentCommand.CardNumber = number
                      PaymentAmount = amount }
            }

    let validateTopUpCommand : ValidateTopUpCommand =
        fun cmd ->
        result {
            let! number = cmd.CardNumber |> validateCardNumber
            let! amount = cmd.TopUpAmount |> MoneyTransaction.create
            return
                { TopUpCommand.CardNumber = number
                  TopUpAmount = amount }
        }

    let validateCreateAddressCommand : ValidateCreateAddressCommand =
        fun cmd ->
        result {
            let! country = parseCountry cmd.Country
            let! city = LetterString.create "city" cmd.City
            let! postalCode = PostalCode.create "postalCode" cmd.PostalCode
            return
                { Address.Country = country
                  City = city
                  PostalCode = postalCode
                  AddressLine1 = cmd.AddressLine1
                  AddressLine2 = cmd.AddressLine2}
        }

    /// applicative version of address validation
    let validateCreateAddressCommandA : ValidateCreateAddressCommand =
        fun cmd ->
            let country = parseCountry cmd.Country
            let city = LetterString.create "city" cmd.City
            let postalCode = PostalCode.create "postalCode" cmd.PostalCode
            Address.create <!> country <*> city <*> postalCode <*> Ok cmd.AddressLine1 <*> Ok cmd.AddressLine2

    let validateCreateUserCommand userId : ValidateCreateUserCommand =
        fun cmd ->
        result {
            let! name = LetterString.create "name" cmd.Name
            let! address =
                match cmd.Address with
                | Some address -> Ok address
                | None -> validationError "Address" "Address must be specified"
            let! address = validateCreateAddressCommand address
            return
                { UserInfo.Id = userId
                  Name = name
                  Address = address }
        }

    /// applicative version of user validation
    let validateCreateUserCommandA userId : ValidateCreateUserCommand =
        fun cmd ->
            let name = LetterString.create "name" cmd.Name
            let address = result {
                let! address =
                    match cmd.Address with
                    | Some address -> Ok address
                    | None -> validationError "Address" "Address must be specified"
                return! validateCreateAddressCommandA address
            }
            UserInfo.create <!> Ok userId <*> name <*> address

    let validateCreateCardCommand : ValidateCreateCardCommand =
        fun cmd ->
        result {
            let! name = LetterString.create "name" cmd.Name
            let! number = CardNumber.create "cardNumber" cmd.CardNumber
            let! month = Month.create "expirationMonth" cmd.ExpirationMonth
            let! year = Year.create "expirationYear" cmd.ExpirationYear
            let! userId =
                match cmd.UserId with
                | Some userId -> Ok userId
                | None -> validationError "userId" "UserId must be specified"
            return
                { Card.CardNumber = number
                  Name = name
                  HolderId = userId
                  Expiration = month,year
                  AccountDetails =
                     AccountInfo.Default userId
                     |> Active }
        }

    /// applicative version of card validation
    let validateCreateCardCommandA : ValidateCreateCardCommand =
        let createTuple x y = x,y
        fun cmd ->
            let name = LetterString.create "name" cmd.Name
            let number = CardNumber.create "cardNumber" cmd.CardNumber
            let month = Month.create "expirationMonth" cmd.ExpirationMonth
            let year = Year.create "expirationYear" cmd.ExpirationYear
            let userId =
                match cmd.UserId with
                | Some userId -> Ok userId
                | None -> validationError "userId" "UserId must be specified"
            let accountDetails = userId |> Result.map (AccountInfo.Default >> Active)
            let expiration = createTuple <!> month <*> year
            Card.create <!> number <*> name <*> userId <*> expiration <*> accountDetails