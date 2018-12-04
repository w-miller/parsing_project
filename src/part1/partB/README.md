# Part 1 B

To execute the parser, you must first ensure that the Glasgow Haskell Compiler
(GHC) is installed on your system. This code was tested with GHC 8.0.1.

## Parsing

You can test the Part 1 B predictive parser with a method similar to the
following:

```
[Will@Wills-MacBook-Air ~/parsing/src/part1/partB 16:31:36]
$ echo '1+2*3' | runhaskell -i.. -i../.. predictive.hs
Expr (Term (AtomInt 1) Term'Epsilon) (Expr'Plus (Term (AtomInt 2) (Term'Mul (AtomInt 3) Term'Epsilon)) Expr'Epsilon)
```

## Pretty Printing

You can test the Part 1 B linear pretty printing with a method similar to the
following:

```
[Will@Wills-MacBook-Air ~/parsing/src/part1/partB 16:34:57]
$ echo '1+2*3' | runhaskell -i.. -i../.. partBPrettyPrint.hs
1 + 2 * 3
```

## Evaluation

You can test the Part 1 B linear pretty printing with a method similar to the
following:

```
[Will@Wills-MacBook-Air ~/parsing/src/part1/partB 16:31:49]
$ echo '1+2*3' | runhaskell -i.. -i../.. partBEvaluate.hs
7
```
