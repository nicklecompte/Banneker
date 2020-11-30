module Common
//TODO We will need to patch these together in a .fsx somewhere.
(*
\subsection{Implementation of a combinatorial pure type system in \FSharp}

\subsubsection{Scaffolding for debugging and user-friendliness}

*)

[<Struct>]
type FileLocation = {
    filename : string
    lineNumber : int
    column : int
}