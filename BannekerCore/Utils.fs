module Banneker.Utils

type OKResult<'T> =
    | OK
    | Error of 'T