```
X(foo: Int, baz: Y*) = Struct;
Y(beep: Int, bar: Int) = Struct;

W: Int = unimplemented;

quack(x: X) = unimplemented;
meow(y: Y*) = unimplemented;
```
There all need values unless we let unimplemented functions be constructors,
I guess I'll put in an unimplemented.
But the semantics should be that you can test code containing unimplemented and even compile it,
but there will be loads of warnings and it won't be considered safe
(i.e. needing some kind of 'I know this is unsafe' flag to be used by other code).

Now here's some hand waving code if ever there was some.
I'm assuming that there are some things we can do that I haven't worked out:
We're putting all the things we mutate into the type signature, which might be horrible for a user.
Maybe having a type alias for the different classes of mutation that we want to allow would make this better.
e.g.
```
MutThreadGlobalState(x) = Effects(quack(x)) & Mut(w);
```

I haven't really started with working out all the other rewrite rules for Tako types.
Still this is a great exercise, thanks for providing it. I think I should also write up a bunch of
the Rosetta code problems soon.

Aside: `(Eff1; Eff2) = Eff1 & Eff2 iff (Eff1; Eff2) == (Eff2; Eff1)`.
These type signatures are implicitly saying that x doesn't contain y or that the effects that we are applying to x and y
could happen in either order, this ruless out some operations.
e.g. `x.baz->beep+=1` and then `y.beep*=2` would either require a proof that either
1. `x.baz != y`
or changing the signature to
2. `(Mut(x.baz->beep); Mut(*y))`
I assume that most devs will not bother with either and so code will, by default enforce separation and reordering.

```
foo(x: X, y: Y*, z: Int): Int & Mut(*y) & Mut(x.baz->beep) & Effects(meow(y)) & MutGlobalState(x) = {
  // Internal effects (implementation details) don't reach the function type.
  x.foo = 12; // local copy changed
  z += 1; // local copy changed

  y->bar = 13; // mutation through pointer!
  x.baz->beep = 12; // mutation through pointer!
  quack(x); // quack could mutate through x if x contains any pointers
  // (but to do so it'll have to request permission via it's type and that type will be propagated)
  meow(y); // meow could mutate y
  w += 1; // we didn't get a `w` argument, global mutation!
}
```
