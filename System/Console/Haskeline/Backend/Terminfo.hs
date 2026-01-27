#if __GLASGOW_HASKELL__ < 802
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
#endif
module System.Console.Haskeline.Backend.Terminfo(
                            Draw(),
                            runTerminfoDraw
                            )
                             where

import System.Console.Terminfo
import Control.Monad
import Control.Monad.Catch
import System.IO
import qualified Control.Exception as Exception
import Data.Maybe (fromMaybe, mapMaybe)

import System.Console.Haskeline.Backend.ANSILike hiding (Draw)
import qualified System.Console.Haskeline.Backend.ANSILike as ANSILike
import System.Console.Haskeline.Monads as Monads
import System.Console.Haskeline.Term
import System.Console.Haskeline.Backend.Posix
import System.Console.Haskeline.Key

import qualified Control.Monad.Trans.Writer as Writer

----------------------------------------------------------------
-- Low-level terminal output

getActions :: Capability (Actions TermOutput)
getActions = do
    -- This capability is not strictly necessary, but is very widely supported
    -- and assuming it makes for a much simpler implementation of printText.
    autoRightMargin >>= guard

    leftA' <- moveLeft
    rightA' <- moveRight
    upA' <- moveUp
    clearToLineEnd' <- clearEOL
    clearAll' <- clearScreen
    nl' <- newline
    cr' <- carriageReturn
    -- Don't require the bell capabilities
    bellAudible' <- bell `mplus` return mempty
    bellVisual' <- visualBell `mplus` return mempty
    wrapLine' <- getWrapLine (leftA' 1)
    return Actions{leftA = leftA', rightA = rightA',upA = upA',
                clearToLineEnd = clearToLineEnd', nl = nl',cr = cr',
                bellAudible = bellAudible', bellVisual = bellVisual',
                clearAllA = clearAll',
                 wrapLine = wrapLine',
                 textA = termText}

-- If the wraparound glitch is in effect, force a wrap by printing a space.
-- Otherwise, it'll wrap automatically.
getWrapLine :: TermOutput -> Capability TermOutput
getWrapLine left1 = (do
    wraparoundGlitch >>= guard
    return (termText " " <#> left1)
    ) `mplus` return mempty

----------------------------------------------------------------
-- The Draw monad

newtype Draw m a = Draw {unDraw :: ReaderT Terminal (ANSILike.Draw TermOutput m) a}
    deriving (Functor, Applicative, Monad, MonadIO,
              MonadMask, MonadThrow, MonadCatch,
              MonadReader (Actions TermOutput), MonadReader Terminal, MonadState TermPos,
              MonadState TermRows, MonadReader Handles)

instance MonadTrans Draw where
    lift = liftANSILike . lift

liftANSILike :: Monad m => ANSILike.Draw TermOutput m a -> Draw m a
liftANSILike =
  Draw . lift

evalDraw :: forall m . (MonadReader Layout m, CommandMonad m) => Terminal -> Actions TermOutput -> EvalTerm (PosixT m)
evalDraw term actions = EvalTerm eval liftE
  where
    liftE = liftANSILike . liftPosixT
    eval = runDraw actions . runReaderT' term . unDraw


runTerminfoDraw :: Handles -> MaybeT IO RunTerm
runTerminfoDraw h = do
    mterm <- liftIO $ Exception.try setupTermFromEnv
    case mterm of
        Left (_::SetupTermError) -> mzero
        Right term -> do
            actions <- MaybeT $ return $ getCapability term getActions
            liftIO $ posixRunTerm h (posixLayouts h ++ [tinfoLayout term])
                (terminfoKeys term)
                (wrapKeypad (ehOut h) term)
                (evalDraw term actions)

-- If the keypad on/off capabilities are defined, wrap the computation with them.
wrapKeypad :: (MonadIO m, MonadMask m) => Handle -> Terminal -> m a -> m a
wrapKeypad h term f = (maybeOutput keypadOn >> f)
                            `finally` maybeOutput keypadOff
  where
    maybeOutput = liftIO . hRunTermOutput h term .
                            fromMaybe mempty . getCapability term

tinfoLayout :: Terminal -> IO (Maybe Layout)
tinfoLayout term = return $ getCapability term $ do
                        c <- termColumns
                        r <- termLines
                        return Layout {height=r,width=c}

terminfoKeys :: Terminal -> [(String,Key)]
terminfoKeys term = mapMaybe getSequence keyCapabilities
    where
        getSequence (cap,x) = do
                            keys <- getCapability term cap
                            return (keys,x)
        keyCapabilities =
                [(keyLeft,      simpleKey LeftKey)
                ,(keyRight,      simpleKey RightKey)
                ,(keyUp,         simpleKey UpKey)
                ,(keyDown,       simpleKey DownKey)
                ,(keyBackspace,  simpleKey Backspace)
                ,(keyDeleteChar, simpleKey Delete)
                ,(keyHome,       simpleKey Home)
                ,(keyEnd,        simpleKey End)
                ,(keyPageDown,   simpleKey PageDown)
                ,(keyPageUp,     simpleKey PageUp)
                ,(keyEnter,      simpleKey $ KeyChar '\n')
                ]



runActionT :: MonadIO m => Writer.WriterT (TermAction TermOutput) (ANSILike.Draw TermOutput m) a -> Draw m a
runActionT m = do
    (x,action) <- liftANSILike (Writer.runWriterT m)
    toutput <- asks action
    term <- ask
    ttyh <- liftM ehOut ask
    liftIO $ hRunTermOutput ttyh term toutput
    return x

----------------------------------------------------------------
-- High-level Term implementation

instance (MonadIO m, MonadMask m, MonadReader Layout m) => Term (Draw m) where
    drawLineDiff xs ys = runActionT $ drawLineDiffT xs ys
    reposition layout lc = runActionT $ repositionT layout lc

    printLines = mapM_ $ \line -> runActionT $ do
                                    outputText line
                                    output nl
    clearLayout = runActionT clearLayoutT
    moveToNextLine _ = runActionT moveToNextLineT
    ringBell True = runActionT $ output bellAudible
    ringBell False = runActionT $ output bellVisual
