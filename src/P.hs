-- There is nothing special about this parser, and I don't have a
-- good reason to use it over a library like megaparsec


module P where

import Control.Applicative
import Control.Monad
import GHC.Exts (State#, RealWorld, realWorld#)
import GHC.ST hiding (liftST)
import GHC.IO
import Data.STRef
import Data.Function
import Control.Monad.IO.Class
import Data.Char qualified as Char
import Data.Set  qualified as S
import Coerce

----

{-# INLINE _list #-}
_list z f = \case
  a:as -> f a as
  _    -> z

----

type P_Ty r s a =
        (String -> a -> State# s -> (# State# s, r #))
     -> (State# s -> (# State# s, r #))
     -> String
     -> State# s
     -> (# State# s, r #)

newtype P s a = P
  { unP ∷ ∀ r. P_Ty r s a }

deriving instance Functor (P s)

instance Applicative (P s) where
  pure a = P \ok z s -> ok s a
  P mf <*> P ma = P \ok z ->
    mf (\s f -> ma (\s a -> ok s (f a)) z s) z

instance Monad (P s) where
  P ma >>= k = P \ok z ->
    ma (\s (k->P mb) -> mb ok z s) z

instance Alternative (P s) where
  empty = P \_ z _ -> z
  P ml <|> P mr = P \ok z s ->
    ml ok (mr ok z s) s

instance MonadPlus (P s)
instance MonadFail (P s) where
  fail _ = empty

instance Semigroup a => Semigroup (P s a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (P s a) where
  mempty = pure mempty

instance MonadIO (P RealWorld) where
  liftIO = fromIO

----

runP ∷ (∀ s. P s a) -> String -> Maybe (String, a)
runP p str =
  runST (runP_ST p str)

runP_ST ∷ P s a -> String -> ST s (Maybe (String, a))
runP_ST (P f) str = ST do
  f do \s a st -> (# st, Just (s, a) #)
    do \st -> (# st, Nothing #)
    str

runP_IO ∷ P RealWorld a -> String -> IO (Maybe (String, a))
runP_IO m =
  stToIO #. runP_ST m

fromST ∷ ST s a -> P s a
fromST (ST f) = P \ok z str (f->(# s, a #)) ->
  ok str a s

fromIO (IO io) = P \ok z s (io->(# st, a #)) ->
  ok s a st

----

consP c = P \ok z s ->
  ok (c:s) ()

getInput = P \ok z s ->
  ok s s

chrP c = P \ok z ->
  _list z \a as ->
    if c == a then ok as a else z

strP s0 = P \ok z ->
  s0 & fix \go -> \cases
    (p:ps) (a:as) ->
      if p == a then go ps as else z
    [] as -> ok as s0
    _ _ -> z

splitAtP n0 = P \ok z ->
    go ok z n0
  where
    go ok z = go2 id where
      go2 x 0 s = ok s (x [])
      go2 x n (a:as) =
        go2 (\z -> x (a:z)) (n-1) as
      go2 _ _ _ = z


-- Alright, fine, I lied!
--
-- This function is why I wrote the parser originally,
-- but it's not actually used in this project.
-- 
-- It is an arbitrary lambda binding for parsers
--        λx.   ...
--   lamP \x -> ...
--
-- It allows us to parse recursive languages, which is a
-- lot of power on paper, but I don't find it very useful
-- in practice.
--
-- it backtracks aggressively and recursively, and it can
-- and must divierge, those are the rules unfortunately.
--
lamP ∷ (P s String -> P s a) -> P s a
lamP k = do
  ref <- fromST $ newSTRef Nothing
  0 & fix \go n -> k
    ( fromST (readSTRef ref) >>= \case
      Just r -> r
      _ -> do
        a <- splitAtP n
        fromST $ a <$ writeSTRef ref (Just (strP a))
    ) <|> do
  Just _ <- fromST (readSTRef ref)
  fromST (writeSTRef ref Nothing)
  go (n+1)

----

dropWhileP f = P \ok z s ->
  ok (dropWhile f s) ()

dropWhile1P f = P \ok z -> \case
  a:as | f a -> ok (dropWhile f as) ()
  _ -> z

takeWhileP f = P \ok z
  (span f->(l,r)) -> ok r l

takeWhile1P f = P \ok z -> \case
  a:as | f a, (l,r) <- span f as -> ok r (a:l)
  _ -> z

satisfy f = P \ok z -> \case
  a:as | f a -> ok as a
  _ -> z

between l r m =
  l *> m <* r

anySingle = P \ok z -> \case
  a:as -> ok as a
  _ -> z

eof = P \ok z -> \case
  [] -> ok [] ()
  _  -> z

someTill m end =
  liftA2 (:) m $
    manyTill m end

manyTill ∷ P s a -> P s end -> P s [a]
manyTill (P m) (P end) = P \ok z -> id & fix \go x s ->
  end (\s _ -> ok s (x []))
      (m (\s a -> go (\z -> x (a:z)) s) z s)
      s

sepEndBy (P m) (P sep) = P \ok z -> id & fix \go x s ->
  m (\s a -> sep
      (\s _ -> go (\z -> x (a:z)) s)
      (ok s (x [a])) s
    ) (ok s (x [ ])) s

sepEndWith (P m) mk_sep = P \ok z -> id & fix \go x s ->
  m (\s a@(mk_sep->P sep) -> sep
      (\s _ -> go (\z -> x (a:z)) s)
      (ok s (x [a])) s
    ) (ok s (x [ ])) s

-- sepEndBy ∷ ∀ a s x. P s a -> P s x -> P s [a]
-- sepEndBy (P m) (P sep) = P mk
--   where
--   mk ∷ ∀ r. P_Ty r s [a]
--   mk ok z = go id where
--     go x =
--       use m   x \(snocF x->x) ->
--       use sep x \_ -> go x
-- 
--     use ∷ P_Ty r s c
--        -> ([a] -> [a])
--        -> (c -> String -> State# s -> (# State# s, r #))
--        -> String
--        -> State# s
--        -> (# State# s, r #)
--     use m x f s =
--       m (flip f) (ok s (x [])) s
-- 
-- snocF as a z = as (a:z)
-- consF a as z = a : as z

-- type P_Ty r s a =
--         (String -> a -> State# s -> (# State# s, r #))
--      -> (State# s -> (# State# s, r #))
--      -> String
--      -> State# s
--      -> (# State# s, r #)

option def (P m) = P \ok z s ->
  m ok (ok s def) s



----

spc0 = dropWhileP  Char.isSpace
spc1 = dropWhile1P Char.isSpace

lineComment = do
  strP "--"
  dropWhileP (/= '\n')

spc = do
  spc0
  x <- option False (True <$ lineComment)
  when x spc

lexeme m =
  m <* spc

symbol c =
  lexeme (chrP c)

keyword kw =
  lexeme (strP kw)


semispace = dropWhileP \c ->
  Char.isSpace c || c == ';'

semi =
  chrP ';' *> semispace


parens = between
  (symbol '(') (symbol ')')

angles = between
  (symbol '<') (symbol '>')

braces = between
  (symbol '[') (symbol ']')

brackets = between
  (symbol '{') (symbol '}')


reserved_idents =
  [ "set", "unset", "scope", "local"
  ]

first_ident_char c =
  Char.isAlphaNum c || c == '_'
ident_char c =
  Char.isAlphaNum c || c == '_'

ident' = liftA2 (:)
  do satisfy first_ident_char
  do takeWhileP ident_char

ident = lexeme do
  a <- ident'
  a <$ guard (a `notElem` reserved_idents)


squote = chrP '\''
dquote = chrP '"'

