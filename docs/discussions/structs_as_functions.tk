// One recurring problem is that structs / classes / objects seem like a complex addition to the Tako type system.

// To attempt to resolve this, I'm interested in modelling structs as a sort of semi-static map.

x = struct(
  a = 3,
  b = 4
)

// This is equivalent to:

x(field) = field == 'a' -| 3 ? field == 'b' -| 4

x: (field: 'a'|'b') => field == 'a' -| 3 ? field == 'b' -| 4

// This is a subtype of
y: (field: 'a'|'b') => 3|4
// and
y: (field: 'a'|'b') => field == 'a' -| 3 ? field == 'b' -| Int
// and
y: (field: 'a'|'b') => field == 'a' -| Int ? field == 'b' -| 4
// and
y: (field: 'a'|'b') => field == 'a' -| Int ? field == 'b' -| Int

// could be written with sugar as
y: map { 'a' => Int, 'b' => Int } = map { 'a' => 3, 'b' => 4 }

// indexed types could similarly be
fib_table = map { 0 => 1, 1 => 1, 2 => 2, 3 => 3, 4 => 5, 5 => 8 ... }
// this unifies them with function signatures for the equivalent non-tabluar mapping i.e.
fib = map { 0 => 1, 1 => 1, 2 => 2, 3 => 3, 4 => 5, 5 => 8 ... }
fib: (field: Nat) => field == 0 -| 1 ? field == 1 -| 1 ? field == 2 -| 2 ? field == 3 -| 3 ? field == 4 -| 4 ? field == 5 -| 8 ? ....
fib: (field: Nat) => field <= 1 -| 1 ? fib(field-1) + field(field-2)
fib(field: Nat) = field < 2 -| 1 ? fib(field-1) + field(field-2)

fib(field: Nat) = {
  doFib(n: Nat, curr: Nat, prev: Nat) = n == 0 -| curr ? doFib (n=n-1, curr=prev+curr, prev=curr);
  doFib(n=field, curr=1, prev=0)
}
