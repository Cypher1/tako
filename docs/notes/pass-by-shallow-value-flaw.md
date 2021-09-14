// Implicit is the following
{..} = import('stdlib');

// I'm assuming that Struct is able to read the last 'frame' and keep the values.
Inner(value: Int): Inner = Struct;
// I'm going to try to use a postfix reference notation so that T?*(pointer to a nullable)
// and T*? (nullable-pointer) aren't ambiguous.
// That is T? = T | null // That's typescript notation but I think I'll keep it. 
Outer(inner: Inner*, value: Int) = Struct;

// This could be just Mut(o) but that would imply that we could change o.inner, not just the value of *o.inner
// Note: I don't think we should copy o to pass it to quack, only to create a new version to modify it,
// but that might be an optimisation of COW semantics rather than the default semantics... not sure if it matters.
quack(o: Outer): Outer & Mut(o.value) & Mut(o.inner->value) = {
  o.value += 1;
  o.inner->value += 1;
  o
}

main(): Int {
  inner1 = Inner(0);
  // inner1 = {value: 0}
  outer1 = Outer(*inner1, 0);
  // outer1 = {inner: *inner1, value: 0}
  outer2 = quack(outer1);
  // outer2 = {inner: *inner1, value: 1}, inner = {value: 1}
  outer3 = quack(outer2);
  // outer3 = {inner: *inner1, value: 2}, inner = {value: 2}

  println("outer1.value is", outer1.value);
  println("outer2.value is", outer2.value);
  println("outer3.value is", outer3.value);
  println("inner1.value is", inner1.value);
  0
}

/* output:
  outer1.value is 0
  outer2.value is 1
  outer3.value is 2
  inner1.value is 2
*/
