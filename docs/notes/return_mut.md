// Style-wise -> things that look like types: UpperCamel, other identifiers: lower_snake_case.

// Types are functions that 'return' a value of their own type.
X(foo: I32): X = Struct; // Some set of base types will be needed with constructors?
// Or possibly they have no definition, only declaration: A constructor could just be a function
// that can't be evaluated any more?
X(foo: I32): X; // This seems more 'formal' to me and might be a helpful prototyping tool
// You could leave a function uninstantiated and then pattern match on it to see if you even need to evaluate it.

// Alternatively, if we had two fields the following would be eq. to a newtype decl in Haskell.
X(foo: I32, bar: I32): X = product(foo, bar);

internal_mutation(x: X): X = { x.foo += 1; x}
// Hopefully non-controversial, same as {let x = X(x.foo+1) in x}
// We don't need mutation here, only allocation and even that is the return value so... all good.
// Without any effects, we know this function is pure (using Haskell's definition of purity).
// If we assume the existance of a function `pure(f: a, ...args): a = f(...args)`
// We could enforce purity with something like pure(f)(args).

external_mutation(x: X*): X* & Mut(x) = { x->foo += 1; x}
// Not sure if & is the best operator for this, but this says: 'I give back a reference to X with some Mutations on X.
// I think Mut should be the name or a type/trait for all writing to basic types.
// There'd be a subset for each operator, so the inferred result type for this function would actually just be
// &X & Incr(x.foo, 1) // Incr(thing, _) being a subtype of Mut(x.thing) itself being a subtype of Mut(x).

// Calculating the effect type is harder. I think the `;` operator would generally just union effects,
// so: x+=1 :: Incr(x,1); x+=2 :: Incr(x, 2)
// therefore: (x+=1;x+=2) :: Incr(x, {1, 2}).
// But it might also be nice to have a rewrite rule that says that
// forall x,u,v. p::Incr(x, u), q::Incr(x, v), ThreadLocal(x), ThreadLocal(u), ThreadLocal(v) |- p;q
//    -----------------------------------------------------------------------------------
//                      r::Incr(x, u+v), |- r
// So the above would become Incr(x, 3)
// I think I also should have read up on linearity, because it'd be handy to say: this adds one, once, and nothing else.
// For that I think you'd have to prove that if you had two things that you could mutate, they didn't overlap:
// i.e. incEachOnce(a: &I32, b: &I32, c: &I32): Once(Incr(a, 1))+Once(Incr(b, 1))+Once(Incr(c, 1)) = a+=1; b+=1; c+=1.

main(): I32 = {
  x1: X = X(0); // Type there should be optional, but for reference
  x2: X* = external_mutation(*x1); // I think I want to provide something like Rust's (reassignable) references but
  // I'll use pointer notation because we want to keep & for product and because more people know about pointers
  // that reassignable references imo.
  // This isn't a borrow though, you can still modify x1.
  // It's also not a pointer, you can't do arbitrary maths without a proof that the update you're doing will
  // point to the memory location of another value of the same type.
  // because x2 is never reassigned it should compile to just being x1.
  x3: X = internal_mutation(x1);

  // I'm moving to compiling print[ln] to use std::cout, for now anyway. Just seemed the simplest representation to deal with for now.
  println("x1.foo is ", x1.foo);
  // I think accessors should be available without dereference, but for now... I'll follow c.
  println("x2->foo is ", x2->foo);
  println("x3.foo is ", x3.foo);
}

/* output
  x1.foo is 1
  x2.foo is 1
  x3.foo is 2
*/