﻿namespace CardManagement.Common

module ErrorMessages =

    open Errors

    let private entityDescription = sprintf "[%s] entity with id [%s]"

    let dataRelatedErrorMessage =
        function
        | EntityAlreadyExists (name, id) -> entityDescription name id |> sprintf "%s already exists."
        | EntityNotFound (name, id) -> entityDescription name id |> sprintf "%s was not found."
        | EntityIsInUse (name, id) -> entityDescription name id |> sprintf "%s is in use."
        | InsertError (name, id, message) ->
            message |> (entityDescription name id |> sprintf "%s failed to insert. Details:\n%s")
        | UpdateError (name, id, message) ->
            message |> (entityDescription name id |> sprintf "%s failed to update. Details:\n%s")

    let validationMessage (first,rest) =
        let createMsg { FieldPath = path; Message = message } =
            sprintf "Field [%s] is invalid. Message: %s" path message
        let firstMsg = createMsg first
        let restMsgs = rest |> List.map createMsg
        firstMsg::restMsgs |> String.concat "; "

    let operationNotAllowedMessage { Operation = op; Reason = reason } =
        sprintf "Operation [%s] is not allowed. Reason: %s" op reason

    let errorMessage error =
        match error with
        | ValidationError v -> validationMessage v
        | OperationNotAllowed o -> operationNotAllowedMessage o
        | DataError d -> dataRelatedErrorMessage d
        | Bug b -> sprintf "Oops, something went wrong.\n%A" b
