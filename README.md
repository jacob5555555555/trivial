# trivial
A minimalistic programming language with pattern matching

This is a toy programming language interpereter that I implemented in haskell.

The goal of this project was to experiment with the (semantically) simplest possible programming language.
The language uses pattern matching as its sole method of doing computation.

Here is an example of a sequence of inputs to the interpereter that could calculate 3+5. See if you can figure out the syntax:
(Maybe later ill update this README with an actual full explanation)
```
> plus Z _a = _a
> plus (S _a) _b = S (plus _a _b)
> plus (S (S (S Z))) (S (S (S (S (S Z)))))
(S (S (S (S (S (S (S (S Z))))))))
```

The snippet of code will define addition, and then use the definition to add two numbers.
