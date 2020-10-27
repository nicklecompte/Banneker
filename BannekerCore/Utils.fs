module Utils

type OKResult<'T> =
    | OK
    | Error of 'T

let notImpl() = failwith "not done"