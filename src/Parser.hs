module Parser where

import Lang
import P

import Data.Foldable
import Control.Monad
import Control.Applicative
import Data.Functor
import Control.Monad.IO.Class (MonadIO(liftIO))

----

declP = asum
  [ varT
  , blockP
  , scopeP
  , holeP
  , sugarP
  , absT
  , varV_or_absV
  ]


holeChar = '|'
holeP = do
  chrP holeChar
  a <- takeWhileP (/= holeChar)
  chrP holeChar
  spc
  pure (Hole a)

varT = do
  x <- asum
    [ True  <$ keyword "set"
    , False <$ keyword "unset"
    ]
  ts <- tagsP
  case ts <&> \ ~(T a b) -> Var $ T a (x && b)
    of [ ] -> empty
       [a] -> pure a
       as  -> pure $ Block as

absT = do
  ts <- tagsP
  guard (not (null ts))
  v <- declP
  pure $ foldr Abs v ts

varV_or_absV = do
  a <- ident
  x <- option False $ True <$ symbol ':'
  if x then do
    b <- declP
    pure $ Abs (V a) b
  else do
    pure $ Var (V a)


blockP = do
  ds <- brackets decls
  -- consP ';' -- semicolons are optional after a block
  pure $ Block ds

decls =
  many declP
  -- sepEndBy declP semi

-- single tag without braces
tagP = do
  b <- option True $ False <$ chrP '!'
  a <- ident
  pure (T a b)

-- zero or more [Foo, !Bar, Baz]
tagsP = do
  braces $ sepEndBy tagP $ symbol ','

scopeP = do
  -- symbol '~'

  -- getInput >>= \s ->
  --   liftIO $ print $ take 10 s
  lexeme do
    ( P \ok z -> \case
        '-':'-':_ -> z
        '-':s -> ok s ()
        _ -> z ) <|> void (strP "scope")

  l <- declP
  r <- declP
  pure $ Scope l r

----

fileP = do
  spc
  Block <$> decls

-- merge useless blocks?
cleanBlock ds = Block $ yo [] ds
  where
    yo z ds = foldr go z ds

    go (Block ds) z = yo z ds
    go d z = d : z

----

sugarP = asum
  [ localScopeP
  ]

localScopeP = do
  keyword "local"
  a <- declP
  pure $ Scope a NOP

pattern
  NOP = Block []

