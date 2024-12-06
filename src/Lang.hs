module Lang where

import Data.Map (Map)
import Data.Set (Set)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Functor
import Data.Function
import Data.List qualified as L
import Control.Monad

----

-- A binder
--
data B x
   = V x      -- Variable
   | T x Bool -- Tag
   deriving
     ( Show, Functor
     )


data Decl x a

   -- An arbitrary hole that we don't care about.
   -- It is left as-is in the syntax tree, so you can
   -- inject another language into this one.
   --
   = Hole a

   -- V binders are effectively module imports.
   -- T binders set and/or clear tags
   | Var (B x)
   --
   -- Tags look like this:
   --   [foo] [bar] [baz] { code code code }
   --
   -- They are alluxary extra binders you can attach to code- any code!
   -- The names have no inherent meaning. Tags are local to the scope,
   -- and each tag is attached to a specific piece of code.
   -- 
   -- At the moment tags can only be used to enable/disable code
   -- siminar to #define, but I want to find more ways to use tags.
   --
   -- Like using them to select what to import from a module!
   -- I've had that and a few other ideas in the back of my head since the
   -- start, but I am not sure if they CANT do that already!


   -- V binders bind the scope of the RHS to a name in the current scope
   -- T binders will skip the RHS if the tag does not match
   | Abs (B x) (Decl x a)

   -- Block does not introduce a new scope, it only
   -- groups declarations together
   | Block [Decl x a]

   -- Changes to the scope made on the LHS are cleared after the RHS.
   -- That is, the RHS recieves the scope from of the the LHS, makes
   -- its own changes, then, every change from the LHS that has not
   -- not been overwritten by the RHS are discarded.
   --
   -- If we only ever use the LHS, scope behaves like a traditional
   -- block scope. If we only ever use the RHS, it behaves as-if we only
   -- have a single global scope.
   --
   | Scope (Decl x a) (Decl x a)
   deriving
     ( Show, Functor
     )

----

-- It turns out that you can have recursive imports:
--
--   -- newline
--   NL: |
--   |
--
--   loop:
--   {
--     | foo | NL 1
--   }
--   1
--
-- It works: foo will we written repeadely to the output,
-- but it was not a deliberately designed.
--
-- I am not sure what to do with it yet, so I just detect
-- the recursion and stop it for now.
--
data Meta = Meta
  {
    occs ∷ Int
  }
  deriving (Show)

newMeta = Meta
  { occs = 0
  }


----

data Γ x a = Γ
  { tags ∷ Map x Bool
  , lets ∷ Map x (Decl x a, Meta)
  }
  deriving
    ( Show, Functor
    )

instance Ord x => Semigroup (Γ x a) where
  Γ tl ll <> Γ tr lr = Γ (tr <> tl) (lr <> ll)
instance Ord x => Monoid (Γ x a) where
  mempty = Γ mempty mempty


runWithOutput = go mempty where
  go γ@Γ {..} = \case

    Hole h ->
      mempty <$ putStr h

    Var (T t tv) -> pure $
      Γ (M.singleton t tv ) mempty

    Abs (V x) e  -> pure $
      Γ mempty (M.singleton x (e, newMeta))

    Abs (T t tv) d
      | M.findWithDefault False t tags == tv -> go γ d
      | let -> pure mempty

    Scope l r -> do
      s1 <- go γ l
      go (γ <> s1) r

    Block ds0 -> start ds0 where
      start =
        loop γ mempty 
      loop s r (d:ds) = do
        r2 <- go s d
        loop
          (s <> r2)
          (r <> r2)
          ds
      loop _ r _ =
        pure r

    Var (V x) ->
      case M.lookup x lets of
        Just (e, Meta {..})
          | occs >= 10 -> pure γ
          | occs <- occs + 1
          , lets <- M.insert x (e, Meta {occs, ..}) lets
          , γ <- γ {lets} -> go γ e
        Nothing ->
          pure γ

----

traceΓ Γ {..} = go where
  go = do

    -- print tags in scope
    ts <- case M.toList tags of
      (t, v):ts -> do
        putStr "tags: "
        unless v $ putChar '!'
        ts <$ putStr t
      _ -> do
        putStr "There are no tags in scope"
        pure []
    forM_ ts \(t, v) -> do
      putStr ", "
      unless v $ putChar '!'
      putStr t
    putChar '\n'

    if M.null lets then 
      putStr "There are no vars in scope"
    else do
      -- putStrLn "vars: "
      let q = M.toList lets
      forM_ q \(x, (v,m)) -> do
        putStr x
        putStr ": "
        outputDecl 0 v
        putStrLn ""
    putChar '\n'

outputDecl indent = \case
  Hole h -> do
    -- putChar '|'
    putStr (show h)
    -- putChar '|'
  Var v -> case v of
    V v -> putStr (v)
    T t tv -> do
      putStr if tv then "set" else "unset"
      putChar ' '
      putChar '['
      putStr t
      putChar ']'
  Abs v e -> case v of
    T t tv -> do
      putChar '['
      unless tv $ putChar '!'
      putStr t
      putChar ']'
      outputDecl indent e
    V v -> do
      putStr v
      putStr ": "
      outputDecl indent e
  Block [] -> do
    putStr "{} "
  Block ds -> do
    indent <- pure (indent + 2)
    -- putStr "block"
    newline
    putStr "{"
    forM_ ds \d -> do
      putChar '\n'
      do_indent indent
      outputDecl indent d
    newline
    putStrLn "}"
  Scope l r -> do
    putStr "scope "
    outputDecl indent l
    outputDecl indent r

  where
    newline = do
      putChar '\n'
      do_indent indent
    do_indent n = do
      putStr (replicate n ' ')


----

ppΓ Γ {..} = pretty
  where
    ts = M.toList tags <&> \case
      (t, v) | v -> t
             | let -> '!' : t
    pts = "[" <> L.intercalate ", " ts <> "]"
    ls = M.toList lets <&> \(x, v) ->
      x <> " = " <> show v
    pls = unlines ls

    pretty = "tags = " <> pts <> "\n" <> pls


ppDecl = \case
  Hole a -> "_" -- show a
  Scope (ppDecl->l) (ppDecl->r) -> "scope\n" <> (l <> r)
  Var (V v) -> v
  Var (T t v) -> set v <> " [" <> t <> "]"
  Abs (V v) (ppDecl->e) -> v <> ": " <> e
  Abs (T t v) (ppDecl->e) -> ("[" <> neg v <> t <> "] ") <> e
  Block [] -> "{}"
  Block ds -> ("{ " <> unwords (map ppDecl ds)) <> " }"

  where
    set x | x   = "set"
          | let = "unset"
    neg x | x   = ""
          | let = "!"

