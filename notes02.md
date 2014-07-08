# Chapter 2

## 2.2.2 Tree Traversal, Mapping over Trees

We can regard a tree as a sequence of sub-trees and use `map` to iterate over it:

```
(define (tree-map f tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map f sub-tree)
             (f sub-tree)))
       tree))
(define (square-tree3 tree)
  (tree-map sqr tree))
```

## 2.2.3 Sequences as Conventional Interfaces

The functions `map`, `filter`, `append`, etc. can all be implemented with the `accumulate` (aka `foldr`) function. See also ex 2.33.

In Racket the `foldr` function takes a proc as the first arguments. This proc takes two arguments `x` and `y`. In proc the `y` is constructed from the initial value. In lodash the order of the arguments `x` and `y` is reversed!
