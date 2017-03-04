module Main.Android where

import Prelude
import Control.Monad.Eff (Eff)
import React (ReactClass, createClass, spec)
import ReactNative.API (REGISTER, registerComponent)
import ReactNative.Components.Text (text)
import ReactNative.Components.View (view)
import ReactNative.PropTypes (center)
import ReactNative.PropTypes.Color (rgbi)
import ReactNative.Styles (Styles, backgroundColor, flex, margin, marginBottom, staticStyles)
import ReactNative.Styles.Flex (alignItems, justifyContent)
import ReactNative.Styles.Text (color, fontSize, textAlign)


app :: ReactClass Unit
app = createClass $ spec unit render
  where
    render ctx =
      pure $ view styles.container [
          text styles.welcome "Welcome to HelloMobileNavigation!"
          , text styles.instructions "To get started, edit src/Main.Android.purs"
          , text styles.instructions "Double tap R on your keyboard to reload, \n Shake or press menu button for dev menu"
        ]

styles :: {
  container :: Styles
  , welcome :: Styles
  , instructions :: Styles
}
styles = {
    container: staticStyles [
        flex 1
      , justifyContent center
      , alignItems center
      , backgroundColor $ rgbi 0xF5FCFF
    ]
    , welcome: staticStyles [
        fontSize 20
        , textAlign center
        , margin 10
    ]
    , instructions: staticStyles [
        textAlign center
        , color $ rgbi 0x333333
        , marginBottom 5
    ]
}

main :: forall eff. Eff ( register :: REGISTER | eff) Unit
main = registerComponent "HelloMobileNavigation" app
