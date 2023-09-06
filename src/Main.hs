{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Data.Text (Text, empty)
import Monomer
import Monomer.Lens ()
import TextShow

newtype AppModel = AppModel
  { _nameInput :: Text
  }
  deriving (Eq, Show)

data AppEvent
  = AppInit
  | AppInput Text
  deriving (Eq, Show)

makeLenses 'AppModel

buildUI ::
  WidgetEnv AppModel AppEvent ->
  AppModel ->
  WidgetNode AppModel AppEvent
buildUI _widgetEnv model = widgetTree
  where
    widgetTree =
      vstack
        [ label "Please input your name:",
          spacer,
          spacer,
          textField nameInput,
          spacer,
          spacer,
          label $ "Greetings " <> showt (model ^. nameInput)
        ]
        `styleBasic` [padding 25]

handleEvent ::
  WidgetEnv AppModel AppEvent ->
  WidgetNode AppModel AppEvent ->
  AppModel ->
  AppEvent ->
  [AppEventResponse AppModel AppEvent]
handleEvent _widgetEnv _node model event = case event of
  AppInit -> []
  AppInput input -> [Model $ model & nameInput .~ input]

main :: IO ()
main = do
  startApp model handleEvent buildUI config
  where
    config =
      [ appWindowTitle "Hello world",
        appWindowIcon "./assets/images/icon.png",
        appTheme lightTheme,
        appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
        appInitEvent AppInit
      ]
    model = AppModel empty
