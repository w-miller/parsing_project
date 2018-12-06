# Part 1

This part comprises modules that are used for part 1 A and part 1 B. See the
README in each of the directories `partA/` and `partB/` for details on how to
run the specific parts.

## Contents

* `Evaluate.hs`: Evaluates a Part 1 `Expr` to an integer.

* `IntTokeniser.hs`: Tokenises a string into lexemes, according to the valid
  tokens in the Part 1 grammar. Allows arbitrary integers, instead of just the
  character 'a'.

* `Part1Grammar.hs`: Defines the grammar used for Part 1.

* `PrettyPrint.hs`: Contains functions to allow pretty linear printing of a
  Part 1 Expr. N.B. this prints and canonically formats the expression from a
  parse tree; it does not pretty-print the parse tree itself, as that task was
  completed in Part 2.
