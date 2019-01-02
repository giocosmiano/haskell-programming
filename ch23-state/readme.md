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

### `state` function
 - State has one data constructor also called `State`. It takes a function as an argument. The interesting thing
   is that this constructor is **not** exported from the library so you can't pattern match on it. If you want to
   create a new monadic `State`, use the function `state`.
 ```haskell
state :: (s -> (a, s)) -> State s a
```

### `runState` function
 - Instead of extracting an action from `State`, which you can't do, and acting with it on some state, you call
   the function `runState` which does it for you.
 ```haskell
runState :: State s a -> s -> (a, s)
```

***State vs random***
```haskell
State { runState ::                    s -> (a, s) }
        random   :: (Random a) => StdGen -> (a, StdGen)
```

***State vs randomR***
```haskell
State { runState ::                                      s -> (a, s) }
        randomR  :: (RandomGen g, Random a) => (a, a) -> g -> (a, g)
```

***State Monadic Structure***
 - The `(State s)` is a monadic structure on `State s a` thus the function will only be applied to `a`  
 - e.g. `(runState $ get >> put 5 >> return 9 >> modify (+3) >> return 12 >> modify (*5) >> return 9001) 3` evaluates to `(9001,40)`
    - `get` evaluates to `(3,3)`
    - `put 5` evaluates to `((),5)`
    - `return 9` evaluates to `(9,5)`
    - `modify (+3)` evaluates to `(9,8)`
    - `return 12` evaluates to `(12,8)`
    - `modify (*5)` evaluates to `(12,40)`
    - `return 9001` evaluates to `(9001,40)`

```haskell
Prelude> :t (>>)
(>>) :: Monad m => m a -> m b -> m b

Prelude> import Control.Monad.State

Prelude> :t get
get :: MonadState s m => m s

Prelude> :t put
put :: MonadState s m => s -> m ()

Prelude> :t runState
runState :: State s a -> s -> (a, s)

Prelude> (runState $ get >> put 5 >> return 9 >> modify (+3) >> return 12 >> modify (*5) >> return 9001) 3
(9001,40)
```

### State Monad Transformer
 - the `state` function is a constructor that takes a State-like function and embeds it in the State monad transformer.

```haskell
Prelude> import Control.Monad.Trans.State

Prelude> :t state
state :: Monad m => (s -> (a, s)) -> StateT s m a

Prelude> :t get
get :: Monad m => StateT s m s

Prelude> :t put
put :: Monad m => s -> StateT s m ()

Prelude> :t runStateT
runStateT :: StateT s m a -> s -> m (a, s)

Prelude> (runStateT $ get >> put 5 >> return 9 >> modify (+3) >> return 12 >> modify (*5) >> return 9001) 3
(9001,40)
```

### For further reading
 - [All About Monads](https://wiki.haskell.org/All_About_Monads)
 - [All About Monads - The State monad](https://wiki.haskell.org/All_About_Monads#The_State_monad)
 - [Basics of Haskell - by Bartosz Milewski](https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell)
 - [School of Haskell - State Monad - by Bartosz Milewski](https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell/12-State-Monad)
 - [The State Monad: A Tutorial for the Confused?](http://brandon.si/code/the-state-monad-a-tutorial-for-the-confused/)
 - [What I Wish I Knew When Learning Haskell - by Stephen Diehl](http://dev.stephendiehl.com/hask/#monads)
