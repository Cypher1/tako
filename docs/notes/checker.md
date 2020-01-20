# Semantic checking

The tako checker relies on a Hindley Milner inspired algorithm for its requirement checking.

The rough outline for this algorithm is that it takes the abstract syntax tree and from it, builds a set of graph of the calls between functions, a call graph.

The call graph is annotated with the pre and post conditions of each functions and is viewed as a set of equations.

Each call of a function must be in a context that conforms to the callee's requirements, while the callee must conform to it's caller's requirements.

This is what prevents functions called from a function without writing priv
  
  Nodes in the graph are functions
  Edges are calls

  Arguments?
   Each unique set of arguments is a new function
   We can make functions with parameters call those without, inlining provides an opportunity for argument aware optimisation and non-inlined functions cannot use them. e.g.
   f(a=3, b=2) calls f(a=3, b) or f(a, b=2) both of which call f(a, b).
   This allows us to move the requirements from f(a, b) out towards the call sites and satisfy requirements as we go, while also providing a framework for our optimisation work.

--

The call graph is made up of
- Nodes (functions)
- Edges (between caller functions and callee functions, annotated with the requirements of the callee's context)

As the graph is simplified, the requirements of a function are merged into the context requirements (requirements should be discarded if satisfied by that context) and then the function itself is replaced with its simplified expression.

All functions should be able to be simplified just once, 

```
a = b(c(), d())

b(x, y) = x*y
```

```
A -> C
A -> D
```
