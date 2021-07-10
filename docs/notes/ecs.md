# Performance in compilers

Achieving high performance in compilers seems to fall into two main categories:
1. not redoing work that has already been done
2. not doing work that will not be used

Things that fall into category 1.
- incremental [re-]compilation / memoization
- caching
- normalization (to ensure that equivalent values are able to be reduced / matched)
- compact data representations (to reduce filling memory caches and spending time re-loading things into memory)
- cloning data that could just be referenced, or indexed

Things that fall into category 2.
- optimisation passes to reduce code sent to code generator
- ignoring unused code, provably unobservable side effects
- navigating pointers all across memory

We conjecture that graph based structures are very helpful for maximising both of these goals.
In particular, storing compiler information as graphs using indexes into arenas allows the compiler to quickly iterate,
not via a tree traversal, but via scan over memory. Further, items can be stored with improved locality, in groups of similar items.

This pattern is very similar to the Entity Component System pattern pioneered in the game industry. As such we will attempt to make use of
an off the self ECS (likely to be specs) or similar graph based data storage and update system (another alternative would be Froggy).
