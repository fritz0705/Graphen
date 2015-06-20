Graphen
#######

Actually, this is just a simple graph library which takes another way than the
traditional Functional Graph Library fgl by using polymorphism and higher
abstraction on edge and vertex values. This allows one to use any type as edge
or vertex type. For example, you can create a graph consisting of points and
state changes, by using the vertex type P and the edge type (P -> P).

This graph library is just not designed for performance, but merely for
educational purposes. Therefore, just do not blame me for my inefficient
Dijkstra implementation; it is to show another way of solving the shortest path
problem, which is in fact the same as Dijkstra's algorithm but with a much
nicer implementation.

Install
=======

When you want to install Graphen to your local Haskell installation, then use
these steps:

::
  ~ $ git clone git://github.com/fritz0705/Graphen
  ~ $ cd Graphen
  Graphen $ cabal install

Alternatively, when you want to hack on the code, just replace the last step:

::
  Graphen $ cabal configure
  Graphen $ cabal repl

License
=======

MIT. See also ``Graphen.cabal`` and ``LICENSE``.
