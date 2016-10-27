---
author: 'Peter Hajdu'
title: 'Adventures in Monadia / Functor, Applicative, Monad'
...

# Why

# save the rebel base

``` haskell
attack :: WeakPoint -> Explosion

attack reactorCore
```

# save the rebel base

Express using _attack_ from the previous slide.

``` haskell
attack :: WeakPoint -> Explosion
realWorldAttack :: Maybe WeakPoint -> Maybe Explosion
realWorldAttack = ???
```

# save the rebel base

Express using _attack_ from the previous slide.

``` haskell
attack :: WeakPoint -> Explosion
realWorldAttack :: Maybe WeakPoint -> Maybe Explosion
realWorldAttack maybePoint =
  case maybePoint of
    Nothing     -> Nothing
    Just(point) -> Just (attack point)
```

There has to be a simpler way!

# Functor

A functor maps a function over some structure, without having any effect on the structure.

# Representation in haskell

``` haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b


f <$> fa == fmap f fa
```

# save the rebel base

Express using _attack_ from the previous slide.

``` haskell
attack :: WeakPoint -> Explosion
fmap   :: (a -> b) -> f a -> f b
realWorldAttack :: Maybe WeakPoint -> Maybe Explosion
realWorldAttack = fmap attack
```

# maybe example

``` haskell
show <$> Just 10
show <$> Nothing
```

# maybe functor instance

``` haskell
instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just a) = Just (f a)
```

# list example

``` haskell
show :: (Show a) => a -> String
show <$> [1..10]
```

# list functor instance

``` haskell
instance Functor [] where
  fmap = map
```

# function example

``` haskell
let f = show              <$> even
        Boolean -> String <$> Int -> Boolean
f 9
f 10
```

# function functor instance

``` haskell
instance Functor ((->) r) where
    fmap = (.)

<$> :: (a -> b) -> (r -> a) -> (r -> b)
```

# Functor laws

``` haskell
fmap (f . g) = fmap f . fmap g
fmap id = id
```

# Representation in scala

``` scala
trait Functor[F[_]] extends InvariantFunctor[F] {
  def fmap[A, B](r: F[A], f: A => B): F[B]
  ....
}
```

# ???

# save the rebel base

``` haskell
attack :: JediPower -> WeakPoint -> Explosion
realWorldAttack :: Maybe JediPower -> Maybe WeakPoint -> Maybe Explosion
realWorldAttack learnFrom(Yoda) throughParticleVent(reactorCore)
realWorldAttack = ???
```

# save the rebel base

``` haskell
attack :: JediPower -> WeakPoint -> Explosion
realWorldAttack :: Maybe JediPower -> Maybe WeakPoint -> Maybe Explosion
realWorldAttack maybePower maybePoint =
  case maybePower of
    Nothing -> Nothing
    Just power ->
      case maybePoint of
        Nothing -> Nothing
        Just point -> Just(attack power point)
```

# save the rebel base

``` haskell
attack :: JediPower -> WeakPoint -> Explosion
realWorldAttack :: Maybe JediPower -> Maybe WeakPoint -> Maybe Explosion
realWorldAttack maybePower maybePoint = realAttack
  where powerAttack = attack <$> maybePower :: ???
        realAttack = powerAttack <$> maybePoint :: Maybe Explosion
```

# save the rebel base

``` haskell
attack :: JediPower -> WeakPoint -> Explosion
realWorldAttack :: Maybe JediPower -> Maybe WeakPoint -> Maybe Explosion
realWorldAttack maybePower maybePoint = realAttack
  where powerAttack = attack <$> maybePower :: Maybe (WeakPoint -> Explosion)
        realAttack = powerAttack ??? maybePoint :: Maybe Explosion

<$> ::   (a -> b) -> f a -> f b
??? :: f (a -> b) -> f a -> f b
```

# applicative functors

An applicative maps a function that is contained in some structure over some other structure and then combines the two layers of structure.

``` haskell
<*> :: f (a -> b) -> f a -> f b
```

# save the rebel base

``` haskell
realWorldAttack :: Maybe JediPower -> Maybe WeakPoint -> Maybe Explosion
realWorldAttack maybePower maybePoint = realAttack
  where powerAttack = attack <$> maybePower :: Maybe (WeakPoint -> Explosion)
        realAttack = powerAttack <*> maybePoint :: Maybe Explosion
```

# save the rebel base

``` haskell
realWorldAttack :: Maybe JediPower -> Maybe WeakPoint -> Maybe Explosion
realWorldAttack maybePower maybePoint = attack <$> maybePower <*> maybePoint
```

# save the rebel base

``` haskell
realWorldAttack :: Maybe JediPower -> Maybe WeakPoint -> Maybe Explosion
realWorldAttack = liftA2 attack
```

# save the rebel base

``` haskell
realWorldAttack :: Maybe JediPower -> Maybe WeakPoint -> Maybe Explosion
realWorldAttack maybePower maybePoint =
  case maybePower of
    Nothing -> Nothing
    Just power -> case maybePoint of
                    Nothing -> Nothing
                    Just point -> Just(attack power point)
```

# applicative functors in haskell

``` haskell
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```

 * maps a function that is contained in some structure
 * _combines_ the two layers of structure
 * what did we apply?

# maybe applicative instance

``` haskell
instance Applicative Maybe where
  pure = Just
  Just f  <*> m = fmap f m
  Nothing <*> m = Nothing
```

 * how does it combine the layers?

# list applicative example

``` haskell
[(+2), (*3)]        <*> [10, 20]
[(++)"apple", tail] <*> ["tree", "pie"]


(*) <$> [1..3]      <*> [1, 2]
```

# list applicative instance

``` haskell
instance Applicative [] where
  pure x    = [x]
  fs <*> xs = [f x | f <- fs, x <- xs]
```

# function applicative instance

``` haskell
instance Applicative ((->) r) where
  pure = const
  f <*> g = \x -> f x (g x)

<*> :: (r -> a -> b) -> (r -> a) -> (r -> b)
```

# function example

``` haskell
listen :: Connection -> Request
reply :: Connection -> Request -> ErrorCode
service = reply <*> listen :: Connection -> ErrorCode
service = \conn -> reply conn (listen conn)
```

# Applicative laws

``` haskell
pure id <*> v = v (identity)
pure (.) <*> u <*> v <*> w = u <*> (v <*> w) (composition)
pure f <*> pure x = pure (f x) (homomorphism)
u <*> pure y = pure ($ y) <*> u (interchange)
```

# applicative functors in scala

``` scala
trait Applicative[F[_]] extends Apply[F] { self =>
  def point[A](a: => A): F[A]
  final def pure[A](a: => A): F[A] = point(a)
  ...
}

trait Apply[F[_]] extends Functor[F] { self =>
  def ap[A,B](fa: => F[A])(f: => F[A => B]): F[B]
  ...
}
```

# ???

# save the rebel base

``` haskell
attack <$> maybePower <*> maybePoint
```

 * What does the extra structure mean here?
 * Does _attack_ have any impact on the final structure?

# save the rebel base

``` haskell
learn    :: Boy  -> Maybe Jedi
getXWing :: Jedi -> Maybe SpaceShip
let jediLuke = learn Boy
let xwing =
  case jediLuke of
    Nothing      -> Nothing
    Just (ajedi) -> getXWing ajedi
```

# save the rebel base

``` haskell
getXWing               :: Jedi        -> Maybe SpaceShip
flyToDeathStar         :: SpaceShip   -> Maybe Destination
aimThroughParticleVent :: Destination -> Maybe Target
fireLasers             :: Target      -> Maybe Explosion

let xwing =
  case jediLuke of
    Nothing      -> Nothing
    Just (ajedi) ->
      case getXWing ajedi of
        Nothing    -> Nothing
        Just xwing ->
          case flyToDeathStar xwing of
            ...
```

# save the rebel base

``` haskell
jediluke :: Maybe Jedi              :: m a
getXWing :: Jedi -> Maybe SpaceShip :: a -> m b
xwing    :: Maybe SpaceShip         :: m b

<$>      ::   (a -> b) -> m a -> m b
<*>      :: m (a -> b) -> m a -> m b
???      :: (a -> m b) -> m a -> m b

(a -> m b) <$> m a :: m (m b)
```

# Monad

Maps a function over some structure and some more...

# Monad

Maps a function over some structure and some more...

``` haskell
<$>  :: (a -> b) -> m a        -> m b
>>=  :: m a      -> (a -> m b) -> m b
join :: m (m a)  -> m a
```

``` haskell
(a -> m b) <$> m a :: m (m b)
```

_flatmap_

# save the rebel base

``` haskell
getXWing               :: Jedi        -> Maybe SpaceShip
flyToDeathStar         :: SpaceShip   -> Maybe Destination
aimThroughParticleVent :: Destination -> Maybe Target
fireLasers             :: Target      -> Maybe Explosion

let xwing =
  case jediLuke of
    Nothing      -> Nothing
    Just (ajedi) ->
      case getXWing ajedi of
        Nothing    -> Nothing
        Just xwing ->
          case flyToDeathStar xwing of
            ...
```

# save the rebel base

``` haskell
learn                  :: Boy         -> Maybe Jedi
getXWing               :: Jedi        -> Maybe SpaceShip
flyToDeathStar         :: SpaceShip   -> Maybe Destination
aimThroughParticleVent :: Destination -> Maybe Target
fireLasers             :: Target      -> Maybe Explosion

>>= :: m a -> (a -> m b) -> m b

learn Boy >>= getXWing >>= flyToDeathStar >>=
  aimThroughParticleVent >>= fireLasers
```

# Representation in haskell

``` haskell
class Applicative m => Monad m where
  (>>=)  :: m a -> (a -> m b) -> m b
  return :: a -> m a
```

# maybe monad instance

``` haskell
instance Monad Maybe where
  (Just x) >>= f = f x
  Nothing  >>= _ = Nothing
  return         = pure
```

# list example

``` haskell
let xPlusDouble x = [x, 2*x]

[1..10] >>= xPlusDouble
concat(fmap xPlusDouble [1..10])
```

# list functor instance

``` haskell
instance Monad [] where
  xs >>= f = [y | x <- xs, y <- f x]
```

# function monad instance

``` haskell
instance Monad ((->) r) where
  f >>= k = \r -> k (f r) r

>>= :: (r -> a) -> (a -> (r -> b)) -> (r -> b)
```

# function example

openConn :: Conf -> Conn
sendReq  :: Conn -> Conf -> Resp
showResp :: Resp -> Conf -> Reaction
job = openConn >>= sendReq >>= showResp
job :: Conf -> Reaction

# monad laws

``` haskell
-- identity
m        >>= return = m
return x >>= f      = f x

-- associativity
(m >>= f) >>= g = m >>= (\x -> f x >>= g)
```

# monad representation in scala

``` scala
trait Monad[F[_]] extends Applicative[F] with Bind[F] { self =>
  ...
}

trait Bind[F[_]] extends Apply[F] { self =>
  def bind[A, B](fa: F[A])(f: A => F[B]): F[B]
  ...
}
```

# ???

