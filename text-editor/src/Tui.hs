{-# LANGUAGE OverloadedStrings #-}

module Tui where

import System.Directory

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import Data.Maybe
import qualified Data.Text.IO as T
import Graphics.Vty.Input.Events
import Path
import Path.IO
import Text.Show.Pretty

import Cursor.TextField

tui :: IO ()
tui = do
  initialState <- buildInitialState
  endState <- defaultMain tuiApp initialState
  print endState

data TuiState =
  TuiState
    { stateCursor :: TextFieldCursor
    }
  deriving (Show, Eq)

data ResourceName =
  ResourceName
  deriving (Show, Eq, Ord)

tuiApp :: App TuiState e ResourceName
tuiApp =
  App
    { appDraw = drawTui
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleTuiEvent
    , appStartEvent = pure
    , appAttrMap = const $ attrMap mempty []
    }

buildInitialState :: IO TuiState
buildInitialState = do
  path <- resolveFile' "example.txt"
  maybeContents <- forgivingAbsence $ T.readFile (fromAbsFile path)
  let contents = fromMaybe "" maybeContents
  let tfc = makeTextFieldCursor contents
  pure TuiState {stateCursor = tfc}

drawTui :: TuiState -> [Widget ResourceName]
drawTui _ts = [strWrap (ppShow _ts)]

handleTuiEvent :: TuiState -> BrickEvent n e -> EventM n (Next TuiState)
handleTuiEvent s e =
  case e of
    VtyEvent vtye ->
      case vtye of
        EvKey (KChar 'q') [] -> halt s
        _ -> continue s
    _ -> continue s
