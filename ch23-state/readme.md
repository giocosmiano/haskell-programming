### Definition
 - The State type in Haskell is a means of expressing state that may change in the course of evaluating code
   without resort to mutation. The monadic interface for State is more of a convenience than a strict
   necessity for working with State.

 - We have the option to capture the idea and convenience of a value which potentially changes with each computation
   without resorting to mutability. State captures this idea and cleans up the bookkeeping required. If you need
   in-place mutation, then the `ST` type is what you want.

```haskell
newtype State s a = State { runState :: s -> (a, s) }
```

 - State is a function that takes `input state` and returns an output value, `a`, tupled with the
  `new state value`. The key is that the previous state value from each application is chained to the next one,
   and this is not an uncommon pattern. `State` is often used for things like random number generators, solvers,
   games, and carrying working memory while traversing a data structure. The polymorphism means you don’t have
   to make a new state for each possible instantiation of `s` and `a`.

***Quick note on newtype***
 - Newtypes are Isomorphic. They have the same underlying representation as the type they wrap. This is
   because the newtype wrapper disappears at compile time. The function contained in the newtype must be
   isomorphic to the type it wraps. That is, there must be a way to go from the newtype to the thing it
   wraps and back again without losing information.

```haskell
State :: (s -> (a, s)) -> State s a

runState :: State s a -> s -> (a, s)
```

