module Banneker.Common

[<Struct>]
type FileLocation = {
    filename : string
    lineNumber : int
    column : int
}