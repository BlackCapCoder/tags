-- This module has a tendency to follow me between projects..

module Coerce
  ( module Coerce
  , module Export
  )
  where

import Data.Kind
import Data.Coerce   as Export
import Unsafe.Coerce as Export
import Data.Type.Coercion

----

pattern Coerce a <- (coerce->a)
  where Coerce = coerce

pattern (:@) a b <- a@b
  where (:@) = asTypeOf

-- coerce a `asTypeOf` b
pattern (:#) a b =
  Coerce a :@ b

f .# g = f :# (f . g)
f #. g = g :# (f . g)
f #$ a = a :# f a
f $# a = f :# f a

a &# f = a :# f a
a #& f = f :# f a

( #.# ) ∷ c #~ a => (b -> c) -> (a -> b) -> (a -> c)
( #.# ) _ _ = coerce

{-# INLINE Coerce #-}
{-# INLINE (:@)   #-}
{-# INLINE (:#)   #-}
infixr 1 :#

{-# INLINE (.# ) #-}
{-# INLINE ( #.) #-}
{-# INLINE ( #.# ) #-}
{-# INLINE ( #$) #-}
{-# INLINE ($# ) #-}
{-# INLINE ( #&) #-}
{-# INLINE (&# ) #-}
infixr 0 #$, $#
infixl 1 #&, &#
infixr 1 .#, #., #.#

{-# INLINE (<#>) #-}
infixl 4 <#>

type ( #~ ) = Coercible
type (:#~ ) = Coercion

infix 4 #~, :#~

----

type Parametrically c f =
  ∀ a b. c a b => c (f a) (f b) ∷ Constraint

(<#>) _ = coerce
(<#>) ∷ Parametrically Coercible f => Coercible a b
      ⇒ (a -> b) -> f a -> f b

