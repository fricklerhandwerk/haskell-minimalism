# An exercise in constructive minimalism with Haskell

Sketching ideas on how to teach thinking in terms of types and functions ("things and rules") using Haskell, but with a minimum amount of moving parts and pre-existing tools.

The code is a half-hearted attempt to implement natural numbers with addition and represent them as character strings in different bases, using only built-in language features.

Inded, the only non-core dependency strictly required for this is `GHC.Show` to display the results.
`GHC.Types.Char` is only needed to make all type annotations possible, without which the code would be much harder to read.
