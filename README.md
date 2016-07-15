### PathTree

This package contains two modules: [Data.LCRSTree][2] and [Data.PathTree][3].

A `PathTree` is a tree used to build unified paths from some node. This
means being able to merge multiple paths, that may overlap at the root, in
a sensible way. The module comes with a set of functions to add paths.

#### Usage

~~~haskell
>import Data.PathTree

>fromPaths [
    ['a', 'b', 'c', 'd']
  , ['a', 'b', 'k']
  , ['z', 'r']
  , ['a', 'b', 'c', 't', 'o']
  ]
Node 'a' (Node 'b' (Node 'c' (Node 't' (Leaf 'o' Empty) (Leaf 'd' Empty)) (Leaf 'k' Empty)) Empty) (Node 'z' (Leaf 'r' Empty) Empty)
~~~

Which would be turned into (in Rose tree view):

![Rose Tree view][5]

A [Left-Children-Right-Siblings tree][1] ([LCRSTree][2]) is a tree that
represents a multi-way tree (aka, a Rose Tree) in a binary-tree format. It is
the underlying implementation of [PathTree][3].

![RoseTree to LCRSTree][4]

[1]: https://en.wikipedia.org/wiki/Left-child_right-sibling_binary_tree
[2]: /src/Data/LCRSTree.hs
[3]: /src/Data/PathTree.hs
[4]: /images/RoseToLCRS.jpg
[5]: /images/PathTreeExample.jpg
