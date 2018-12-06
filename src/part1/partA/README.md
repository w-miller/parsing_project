# Part 1 A

To execute the parser, you must first ensure that the Glasgow Haskell Compiler
(GHC) is installed on your system. This code was tested with GHC 8.0.1.

## Parsing

You can test the Part 1 A recursive descent parser with a method similar to the
following:

```
[Will@Wills-MacBook-Air ~/parsing/src/part1/partA 16:03:40]
$ echo '1+2*3' | runhaskell -i.. -i../.. RecDesc.hs
Just (Expr (Term (AtomInt 1) Term'Epsilon) (Expr'Plus (Term (AtomInt 2) (Term'Mul (AtomInt 3) Term'Epsilon)) Expr'Epsilon))
```

## Pretty Printing

You can test the Part 1 A linear pretty printing with a method similar to the
following:

```
[Will@Wills-MacBook-Air ~/parsing/src/part1/partA 15:55:19]
$ echo '1+1' | runhaskell -i../.. -i.. -Wall PartAPrettyPrint.hs
1 + 1
```

## Evaluation

You can test the Part 1 A linear pretty printing with a method similar to the
following:

```
[Will@Wills-MacBook-Air ~/parsing/src/part1/partA 16:17:38]
$ echo '1+2*3' | runhaskell -i.. -i../.. PartAEvaluate.hs
7
```
