#if __GLASGOW_HASKELL__ < 802
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
#endif
module System.Console.Haskeline.Backend.ANSILike
  ( Actions (..),
    Draw,
    runDraw,
    liftPosixT,
    TermPos (..),
    TermRows (..),
    TermAction,
    output,
    outputText,
    drawLineDiffT,
    clearLayoutT,
    moveToNextLineT,
    repositionT,
    printLinesT,
    ringBellT,
  )
where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans.Writer (WriterT)
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.IntMap as Map
import Data.List (foldl')
import System.Console.Haskeline.Backend.Posix
import System.Console.Haskeline.Backend.WCWidth
import System.Console.Haskeline.LineState
import System.Console.Haskeline.Monads as Monads
import System.Console.Haskeline.Term
import System.Console.Terminfo

----------------------------------------------------------------
-- Low-level terminal output

data Actions a = Actions
  { leftA, rightA, upA :: Int -> a,
    clearToLineEnd :: a,
    nl, cr :: a,
    bellAudible, bellVisual :: a,
    clearAllA :: LinesAffected -> a,
    wrapLine :: a,
    textA :: String -> a
  }

----------------------------------------------------------------
-- The Draw monad

-- denote in modular arithmetic;
-- in particular, 0 <= termCol < width
data TermPos = TermPos {termRow, termCol :: !Int}
  deriving (Show)

initTermPos :: TermPos
initTermPos = TermPos {termRow = 0, termCol = 0}

data TermRows = TermRows
  { -- | The length of each nonempty row
    rowLengths :: !(Map.IntMap Int),
    -- | The last nonempty row, or zero if the entire line
    -- is empty.  Note that when the cursor wraps to the first
    -- column of the next line, termRow > lastRow.
    lastRow :: !Int
  }
  deriving (Show)

initTermRows :: TermRows
initTermRows = TermRows {rowLengths = Map.empty, lastRow = 0}

setRow :: Int -> Int -> TermRows -> TermRows
setRow r len rs =
  TermRows
    { rowLengths = Map.insert r len (rowLengths rs),
      lastRow = r
    }

lookupCells :: TermRows -> Int -> Int
lookupCells (TermRows rc _) r = Map.findWithDefault 0 r rc

newtype Draw c m a = Draw
  {unDraw :: ReaderT (Actions c) (StateT TermRows (StateT TermPos (PosixT m))) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadMask,
      MonadThrow,
      MonadCatch,
      MonadReader (Actions c),
      MonadState TermPos,
      MonadState TermRows,
      MonadReader Handles
    )

instance MonadTrans (Draw c) where
  lift = liftPosixT . lift

runDraw :: (Monad m) => Actions c -> Draw c m a -> PosixT m a
runDraw actions =
  evalStateT' initTermPos
    . evalStateT' initTermRows
    . runReaderT' actions
    . unDraw

liftPosixT :: (Monad m) => PosixT m a -> Draw c m a
liftPosixT =
  Draw . lift . lift . lift

----------------------------------------------------------------
-- Terminal output actions
--
-- We combine all of the drawing commands into one big TermAction,
-- via a writer monad, and then output them all at once.
-- This prevents flicker, i.e., the cursor appearing briefly
-- in an intermediate position.

type TermAction a = Actions a -> a

output :: (Monad m) => TermAction c -> WriterT (TermAction c) (Draw c m) ()
output t = Writer.tell t

-- NB: explicit argument enables build with ghc-6.12.3
-- (Probably related to the monomorphism restriction;
-- see GHC ticket #1749).

outputText :: (Monad m) => String -> WriterT (TermAction c) (Draw c m) ()
outputText s = output (text s)

left, right, up :: Int -> TermAction a
left = flip leftA
right = flip rightA
up = flip upA

text :: String -> TermAction a
text = flip textA

clearAll :: LinesAffected -> TermAction a
clearAll = flip clearAllA

mreplicate :: (Monoid m) => Int -> m -> m
mreplicate n m
  | n <= 0 = mempty
  | otherwise = m `mappend` mreplicate (n - 1) m

-- We don't need to bother encoding the spaces.
spaces :: (Monoid c) => Int -> TermAction c
spaces 0 = mempty
spaces 1 = text " " -- share when possible
spaces n = text $ replicate n ' '

changePos :: (Monoid a) => TermPos -> TermPos -> TermAction a
changePos TermPos {termRow = r1, termCol = c1} TermPos {termRow = r2, termCol = c2}
  | r1 == r2 = if c1 < c2 then right (c2 - c1) else left (c1 - c2)
  | r1 > r2 = cr <#> up (r1 - r2) <#> right c2
  | otherwise = cr <#> mreplicate (r2 - r1) nl <#> right c2

moveToPos :: (Monoid c, Monad m) => TermPos -> WriterT (TermAction c) (Draw c m) ()
moveToPos p = do
  oldP <- get
  put p
  output $ changePos oldP p

moveRelative :: (Monoid c, MonadReader Layout m) => Int -> WriterT (TermAction c) (Draw c m) ()
moveRelative n =
  liftM3 (advancePos n) ask get get
    >>= \p -> moveToPos p

-- Note that these move by a certain number of cells, not graphemes.
changeRight, changeLeft :: (Monoid c, MonadReader Layout m) => Int -> WriterT (TermAction c) (Draw c m) ()
changeRight n
  | n <= 0 = return ()
  | otherwise = moveRelative n
changeLeft n
  | n <= 0 = return ()
  | otherwise = moveRelative (negate n)

-- TODO: this could be more efficient by only checking intermediate rows.
-- TODO: this is worth handling with QuickCheck.
advancePos :: Int -> Layout -> TermRows -> TermPos -> TermPos
advancePos k Layout {width = w} rs p = indexToPos $ k + posIndex
  where
    posIndex =
      termCol p
        + sum'
          ( map
              (lookupCells rs)
              [0 .. termRow p - 1]
          )
    indexToPos n = loopFindRow 0 n
    loopFindRow r m =
      r `seq`
        m `seq`
          let thisRowSize = lookupCells rs r
           in if m < thisRowSize
                || (m == thisRowSize && m < w)
                || thisRowSize <= 0 -- This shouldn't happen in practice,
                -- but double-check to prevent an infinite loop
                then TermPos {termRow = r, termCol = m}
                else loopFindRow (r + 1) (m - thisRowSize)

sum' :: [Int] -> Int
sum' = foldl' (+) 0

----------------------------------------------------------------
-- Text printing actions

printText :: (Monoid c, MonadReader Layout m) => [Grapheme] -> WriterT (TermAction c) (Draw c m) ()
printText [] = return ()
printText gs = do
  -- First, get the monadic parameters:
  w <- asks width
  TermPos {termRow = r, termCol = c} <- get
  -- Now, split off as much as will fit on the rest of this row:
  let (thisLine, rest, thisWidth) = splitAtWidth (w - c) gs
  let lineWidth = c + thisWidth
  -- Finally, actually print out the relevant text.
  outputText (graphemesToString thisLine)
  modify $ setRow r lineWidth
  if null rest && lineWidth < w
    then -- everything fits on one line without wrapping
      put TermPos {termRow = r, termCol = lineWidth}
    else do
      -- Must wrap to the next line
      put TermPos {termRow = r + 1, termCol = 0}
      output $ if lineWidth == w then wrapLine else spaces (w - lineWidth)
      printText rest

----------------------------------------------------------------
-- High-level Term implementation

drawLineDiffT :: (Monoid c, MonadReader Layout m) => LineChars -> LineChars -> WriterT (TermAction c) (Draw c m) ()
drawLineDiffT (xs1, ys1) (xs2, ys2) = case matchInit xs1 xs2 of
  ([], []) | ys1 == ys2 -> return ()
  (xs1', []) | xs1' ++ ys1 == ys2 -> changeLeft (gsWidth xs1')
  ([], xs2') | ys1 == xs2' ++ ys2 -> changeRight (gsWidth xs2')
  (xs1', xs2') -> do
    oldRS <- get
    changeLeft (gsWidth xs1')
    printText xs2'
    p <- get
    printText ys2
    clearDeadText oldRS
    moveToPos p

-- The number of nonempty lines after the current row position.
getLinesLeft :: (Monoid c, Monad m) => WriterT (TermAction c) (Draw c m) Int
getLinesLeft = do
  p <- get
  rc <- get
  return $ max 0 (lastRow rc - termRow p)

clearDeadText :: (Monoid c, Monad m) => TermRows -> WriterT (TermAction c) (Draw c m) ()
clearDeadText oldRS = do
  TermPos {termRow = r, termCol = c} <- get
  let extraRows = lastRow oldRS - r
  if extraRows < 0
    || (extraRows == 0 && lookupCells oldRS r <= c)
    then return ()
    else do
      modify $ setRow r c
      when (extraRows /= 0) $
        put TermPos {termRow = r + extraRows, termCol = 0}
      output $ clearToLineEnd <#> mreplicate extraRows (nl <#> clearToLineEnd)

clearLayoutT :: (Monoid c, MonadReader Layout m) => WriterT (TermAction c) (Draw c m) ()
clearLayoutT = do
  h <- asks height
  output (clearAll h)
  put initTermPos

moveToNextLineT :: (Monoid c, Monad m) => WriterT (TermAction c) (Draw c m) ()
moveToNextLineT = do
  lleft <- getLinesLeft
  output $ mreplicate (lleft + 1) nl
  put initTermPos
  put initTermRows

repositionT :: (Monoid c, MonadReader Layout m) => Layout -> LineChars -> WriterT (TermAction c) (Draw c m) ()
repositionT _ s = do
  oldPos <- get
  l <- getLinesLeft
  output $
    cr
      <#> mreplicate l nl
      <#> mreplicate (l + termRow oldPos) (clearToLineEnd <#> up 1)
  put initTermPos
  put initTermRows
  drawLineDiffT ([], []) s

printLinesT :: (Monoid c, Monad m) => [String] -> WriterT (TermAction c) (Draw c m) ()
printLinesT =
  mapM_ $ \line -> do
    outputText line
    output nl

ringBellT :: (Monad m) => Bool -> WriterT (TermAction c) (Draw c m) ()
ringBellT True = output bellAudible
ringBellT False = output bellVisual
