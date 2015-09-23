{-# LANGUAGE GADTs #-}
module Bourne
  ( Script
  , ScriptM
  , showScript
  -- * Tests
  , Test
  , test
  , fileNotExists
  , dirNotExists
  -- * Command
  , cmd
  , mkdirp
  , cd
  , touch
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Writer.Strict
import Data.List
import Data.Monoid
import Text.PrettyPrint hiding ((<>))

data Command = RawCommand [String]
  deriving (Eq, Ord, Show)

data Test = FileExists Bool FilePath
          | DirExists Bool FilePath
  deriving (Eq, Ord, Show)

data ScriptMon
  = Cmd Command
  | If Test ScriptMon
  | Block [ScriptMon]

instance Monoid ScriptMon where
  mempty = Block []

  Block [] `mappend` y         = y
  x        `mappend` Block []  = x
  Block xs `mappend` Block ys  = Block (xs <> ys)
  Block xs `mappend` y         = Block (xs <> [y])
  x        `mappend` Block ys  = Block ([x] <> ys)
  x        `mappend` y         = Block [x, y]

type Script = ScriptM ()
newtype ScriptM a = ScriptM { runScriptM ::  Writer ScriptMon a }

instance Functor ScriptM where
  fmap = liftM

instance Applicative ScriptM where
  pure = return
  (<*>) = ap
  
instance Monad ScriptM where
  return = ScriptM . return
  ScriptM x >>= f = ScriptM (x >>= runScriptM . f)

toScriptM :: ScriptMon -> Script
toScriptM = ScriptM . tell

execScriptM :: ScriptM a -> ScriptMon
execScriptM = execWriter . runScriptM

-- | @! -f path@
fileNotExists :: FilePath -> Test
fileNotExists = FileExists False

-- | @! -d path@
dirNotExists :: FilePath -> Test
dirNotExists = DirExists False

cmd :: String -> [String] -> Script
cmd c cs = toScriptM . Cmd . RawCommand $ c : cs

cd :: FilePath -> Script
cd d = cmd "cd" [d]

mkdirp :: FilePath -> Script
mkdirp d = cmd "mkdir" ["-p", d]

touch :: FilePath -> Script
touch f = cmd "touch" [f]

test :: Test -> Script -> Script
test t c = toScriptM (If t (execScriptM c))

showScript :: ScriptM a -> String
showScript = showScriptMon . execScriptM

showScriptMon :: ScriptMon -> String
showScriptMon = renderStyle style { lineLength = 140 } . go
  where go :: ScriptMon -> Doc 
        go (Cmd c)     = text (showCommand c ++ ";")
        go (Block xs)  = vcat (go <$> xs)
        go (If t c)    = sep [ text ("if [ " <> showTest t <> " ]; then")
                             , nest 2 (go c)
                             , text "fi;"
                             ]

showTest :: Test -> String
showTest (FileExists n p) = mark <> "-f " <> p
  where mark = if n then "" else "! "
showTest (DirExists n p) = mark <> "-d " <> p
  where mark = if n then "" else "! "

showCommand :: Command -> String
showCommand (RawCommand parts) = intercalate " " parts
