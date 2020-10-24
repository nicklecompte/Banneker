// http://home.sandiego.edu/~shulman/papers/injmodel-talk.pdf
// https://arxiv.org/pdf/1904.07004.pdf

(*
Standard categorical semantics of dependent type theory is:
• A category whose objects represent “types” or “contexts”.
• A class of “display maps” B  A representing dependent types
x : A ` B(x) type.
• Sections of a display map represent terms x : A ` b(x) : B(x).
• Further structure corresponding to all the rules of type theory.
*)
(*
Theorem (Awodey–Warren)
The elimination rule for identity types says exactly that the
reflexivity term A → IdA has the left lifting property with respect to
the display maps. Thus, if we regard display maps as fibrations,
then identity types are path objects.
*)