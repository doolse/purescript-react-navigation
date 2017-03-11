module Main.Ios where

import Prelude
import Control.Monad.Eff (Eff)
import React (ReactClass, createClass, spec)
import ReactNative.API (REGISTER, registerComponent)
import ReactNative.Components.Text (text_)
import ReactNative.Components.View (view_)
import ReactNavigation (stackNavigator', applyNavigationOptions)

homeView :: ReactClass Unit
homeView = createClass $ spec unit \_ ->
  pure $ text_ "Hello, Navigation!"

app :: ReactClass Unit
app = stackNavigator' { home: { screen: applyNavigationOptions homeView {title: "Welcome"} } }

main :: forall eff. Eff ( register :: REGISTER | eff) Unit
main = registerComponent "HelloMobileNavigation" app
