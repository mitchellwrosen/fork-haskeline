module System.Console.Haskeline.Backend.ANSI
  ( runANSIDraw,
  )
where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import qualified Control.Monad.Trans.Writer as Writer
import Data.String (IsString (..))
import System.Console.Haskeline.Backend.ANSILike hiding (Draw)
import qualified System.Console.Haskeline.Backend.ANSILike as ANSILike
import System.Console.Haskeline.Backend.Posix (Handles, PosixT, ehOut, posixLayouts, posixRunTerm)
import System.Console.Haskeline.Monads
import System.Console.Haskeline.Term (CommandMonad, EvalTerm (..), Layout, RunTerm, Term (..))
import System.IO (hFlush, hPutStr)

-- Mini string builder monoid

newtype StringBuilder
  = StringBuilder (String -> String)

instance IsString StringBuilder where
  fromString =
    build

instance Monoid StringBuilder where
  mempty =
    StringBuilder id

instance Semigroup StringBuilder where
  StringBuilder x <> StringBuilder y =
    StringBuilder (x . y)

build :: String -> StringBuilder
build s =
  StringBuilder (s ++)

runBuilder :: StringBuilder -> String
runBuilder (StringBuilder s) =
  s ""

-- The backend

actions :: Actions StringBuilder
actions =
  Actions
    { bellAudible = build "\a",
      bellVisual = mempty,
      clearAllA = \_ -> build "\ESC[2J\ESC[H",
      clearToLineEnd = build "\ESC[K",
      cr = build "\r",
      leftA = \n -> if n <= 0 then mempty else build "\ESC[" <> build (show n) <> build "D",
      nl = build "\r\n",
      rightA = \n -> if n <= 0 then mempty else build "\ESC[" <> build (show n) <> build "C",
      upA = \n -> if n <= 0 then mempty else build "\ESC[" <> build (show n) <> build "A",
      wrapLine = mempty,
      textA = build
    }

newtype Draw m a
  = Draw {unDraw :: ANSILike.Draw StringBuilder m a}
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadCatch,
      MonadIO,
      MonadReader (Actions StringBuilder),
      MonadReader Handles,
      MonadMask,
      MonadThrow,
      MonadTrans
    )

evalDraw :: forall m. (MonadReader Layout m, CommandMonad m) => EvalTerm (PosixT m)
evalDraw = EvalTerm eval liftE
  where
    liftE = Draw . liftPosixT
    eval = runDraw actions . unDraw

runANSIDraw :: Handles -> MaybeT IO RunTerm
runANSIDraw handles =
  liftIO $
    posixRunTerm
      handles
      (posixLayouts handles)
      []
      id
      evalDraw

runActionT :: (MonadIO m) => Writer.WriterT (TermAction StringBuilder) (ANSILike.Draw StringBuilder m) a -> Draw m a
runActionT m = do
  (x, action) <- Draw (Writer.runWriterT m)
  toutput <- asks action
  ttyh <- asks ehOut
  liftIO $ do
    hPutStr ttyh (runBuilder toutput)
    hFlush ttyh
  return x

instance (MonadIO m, MonadMask m, MonadReader Layout m) => Term (Draw m) where
  drawLineDiff xs ys = runActionT $ drawLineDiffT xs ys
  reposition layout lc = runActionT $ repositionT layout lc
  printLines xs = runActionT $ printLinesT xs
  clearLayout = runActionT clearLayoutT
  moveToNextLine _ = runActionT moveToNextLineT
  ringBell x = runActionT $ ringBellT x
