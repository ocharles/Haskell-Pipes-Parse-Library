-- | Parsing utilities for pipes

module Pipes.Parse (
    -- * Pushback and Leftovers
    -- $pushback
    draw,
    unDraw,

    -- * Utilities
    peek,
    isEndOfInput,
    drawAll,
    skipAll,
    passUpTo,
    passWhile,

    -- * Adapters
    -- $adapters
    wrap,
    unwrap,
    fmapPull,
    returnPull,
    bindPull,

    -- * Lenses
    -- $lenses
    zoom,
    _fst,
    _snd,

    -- * Re-exports
    -- $reexports
    module Data.Monoid
    ) where

import Control.Monad (forever)
import Control.Monad.Morph (hoist)
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State.Strict as S
import Pipes ((>->), (\>\), (//>), (>\\))
import qualified Pipes as P
import qualified Pipes.Prelude as P
import Data.Monoid (Monoid(mempty, mappend))

{- $pushback
    'unDraw' stores all leftovers in a 'StateP' buffer and 'draw' retrieves
    leftovers from this buffer before drawing new input from upstream.
-}

{-| Like @request ()@, except try to use the leftovers buffer first

    A 'Nothing' return value indicates end of input.
-}
draw :: Monad m => P.Consumer (Maybe a) (S.StateT [a] m) (Maybe a)
draw = do
    s <- lift S.get
    case s of
        []   -> P.request ()
        a:as -> do
            lift . S.put $ as
            return (Just a)
{-# INLINABLE draw #-}

-- | Push an element back onto the leftovers buffer
unDraw :: Monad m => a -> P.Effect (S.StateT [a] m) ()
unDraw a = lift . S.modify $ (a:)
{-# INLINABLE unDraw #-}

-- | Peek at the next element without consuming it
peek :: Monad m => P.Consumer (Maybe a) (S.StateT [a] m) (Maybe a)
peek = do
    ma <- draw
    case ma of
        Nothing -> return ()
        Just a  -> unDraw a
    return ma
{-# INLINABLE peek #-}

-- | Check if at end of input stream.
isEndOfInput :: Monad m => P.Consumer (Maybe a) (S.StateT [a] m) Bool
isEndOfInput = do
    ma <- peek
    case ma of
        Nothing -> return True
        Just _  -> return False
{-# INLINABLE isEndOfInput #-}

{-| Fold all input into a list

    Note: 'drawAll' is usually an anti-pattern.
-}
drawAll :: Monad m => () -> P.Consumer (Maybe a) (S.StateT [a] m) [a]
drawAll = \() -> go id
  where
    go diffAs = do
        ma <- draw
        case ma of
            Nothing -> return (diffAs [])
            Just a  -> go (diffAs . (a:))
{-# INLINABLE drawAll #-}

-- | Consume the input completely, discarding all values
skipAll :: Monad m => () -> P.Consumer (Maybe a) (S.StateT [a] m) ()
skipAll = \() -> go
  where
    go = do
        ma <- draw
        case ma of
            Nothing -> return ()
            Just _  -> go
{-# INLINABLE skipAll #-}

-- | Forward up to the specified number of elements downstream
passUpTo
    :: Monad m
    => Int -> () -> P.Pipe (Maybe a) (Maybe a) (S.StateT [a] m) r
passUpTo n0 = \() -> go n0
  where
    go n0 =
        if (n0 <= 0)
        then forever $ P.respond Nothing
        else do
            ma <- draw
            P.respond ma
            case ma of
                Nothing -> forever $ P.respond Nothing
                Just _  -> go (n0 - 1)
{-# INLINABLE passUpTo #-}

{-| Forward downstream as many consecutive elements satisfying a predicate as
    possible
-}
passWhile
    :: Monad m
    => (a -> Bool) -> () -> P.Pipe (Maybe a) (Maybe a) (S.StateT [a] m) r
passWhile pred = \() -> go
  where
    go = do
        ma <- draw
        case ma of
            Nothing -> forever $ P.respond Nothing
            Just a  ->
                if (pred a)
                then do
                    P.respond ma
                    go
                else do
                    unDraw a
                    forever $ P.respond Nothing
{-# INLINABLE passWhile #-}

{- $adapters
    Use 'wrap' and 'unwrap' to convert between guarded and unguarded pipes.

    'fmapPull', 'returnPull', and 'bindPull' promote compatibility with
    existing utilities that are not 'Maybe'-aware.
-}

{-| Guard a pipe from terminating by wrapping every output in 'Just' and ending
    with a never-ending stream of 'Nothing's.
-}
wrap :: Monad m => P.Proxy a' a b' b m r -> P.Proxy a' a b' (Maybe b) m s
wrap = \p -> do
    p //> \b -> P.respond (Just b)
    forever $ P.respond Nothing
{-# INLINABLE wrap #-}

{-| Compose 'unwrap' downstream of a guarded pipe to unwrap all 'Just's and
    terminate on the first 'Nothing'.
-}
unwrap :: Monad m => x -> P.Proxy x (Maybe a) x a m ()
unwrap = \x -> (go x)
  where
    go x = do
        ma <- P.request x
        case ma of
            Nothing -> return ()
            Just a  -> do
                x2 <- P.respond a
                go x2
{-# INLINABLE unwrap #-}

{-| Lift a 'Maybe'-oblivious pipe to a 'Maybe'-aware pipe by auto-forwarding
    all 'Nothing's.

> fmapPull f >-> fmapPull g = fmapPull (f >-> g)
>
> fmapPull pull = pull
-}
fmapPull
    :: Monad m
    => (() -> P.Pipe a b  m r)
    -> (() -> P.Pipe (Maybe a) (Maybe b) m r)
fmapPull f = bindPull (f >-> returnPull)
{-# INLINABLE fmapPull #-}

-- | Wrap all values flowing downstream in 'Just'.
returnPull :: Monad m => () -> P.Pipe a (Maybe a) m r
returnPull = P.map Just
{-# INLINABLE returnPull #-}

{-| Lift a 'Maybe'-generating pipe to a 'Maybe'-transforming pipe by
    auto-forwarding all 'Nothing's

> -- Using: f >>> g = f >-> bindPull g
>
> returnPull >>> f = f
>
> f >>> returnPull = f
>
> (f >>> g) >>> h = f >>> (g >>> h)

Or equivalently:

> returnPull >-> bindPull f = f
>
> bindPull returnPull = pull
>
> bindPull (f >-> bindPull g) = bindPull f >-> bindPull g
-}
bindPull
    :: Monad m
    => (x -> P.Proxy x        a  x (Maybe b) m r)
    -> (x -> P.Proxy x (Maybe a) x (Maybe b) m r)
bindPull f = up \>\ f
  where
    up a' = do
        ma <- P.request a'
        case ma of
            Nothing -> do
                a'2 <- P.respond Nothing
                up a'2
            Just a  -> return a
{-# INLINABLE bindPull #-}

{- $lenses
    Use 'zoom', '_fst', and '_snd' to mix pipes that have different leftover
    buffers or to isolate leftover buffers of different parsing stages.
-}

{-| 'zoom' in on a sub-state using a @Lens'@.

> zoom :: Lens' s1 s2 -> StateP s2 p a' a b' b m r -> StateP s1 p a' a b' b m r

> zoom (f . g) = zoom f . zoom g
>
> zoom id = id
-}
zoom
    :: Monad m
    => ((s2 -> (s2, s2)) -> (s1 -> (s2, s1)))
    -- ^ @Lens'@ s1 s2
    -> P.Proxy a' a b' b (S.StateT s2 m) r
    -- ^ Local state
    -> P.Proxy a' a b' b (S.StateT s1 m) r
    -- ^ Global state
zoom lens = hoist (morph lens)
  where
    morph l (S.StateT f) = S.StateT $ \s ->
      let (s2, _) = l (\x -> (x, x)) s
      in f s2 >>= \(r, s2') ->
          let (_, s1') = l (\x -> (x, s2')) s
          in return (r, s1')
{-# INLINABLE zoom #-}

{-| A @Lens'@ to the first element of a pair.

    Like @_1@, but more monomorphic

> _fst :: Lens' (a, b) a
-}
_fst :: (Functor f) => (a -> f b) -> (a, x) -> f (b, x)
_fst = \f (a, x) -> fmap (\b -> (b, x)) (f a)
{-# INLINABLE _fst #-}

{-| A @Lens'@ to the second element of a pair.

    Like @_2@, but more monomorphic

> _snd :: Lens' (a, b) b
-}
_snd :: (Functor f) => (a -> f b) -> ((x, a) -> f (x, b))
_snd = \f (x, a) -> fmap (\b -> (x, b)) (f a)
{-# INLINABLE _snd #-}

{- $reexports
    "Data.Monoid" re-exports the 'Monoid' class.
-}
