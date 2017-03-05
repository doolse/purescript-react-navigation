module Main.Ios where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Function.Uncurried (Fn0, mkFn0)
import React (ReactClass, ReactElement, createClass, spec)
import ReactNative.API (REGISTER, registerComponent)
import ReactNative.Components.Text (text_)
import ReactNative.Components.View (view_)
import ReactNavigation (stackNavigator)

homeView :: ReactElement
homeView = view_ [ text_ "Hello, Navigation!" ]

-- Compiled PS code provides an object representing the module.
-- But any view used as screen has to be wrapped within a function to pass `validateRouteConfigMap`
-- See https://github.com/react-community/react-navigation/blob/master/src/routers/validateRouteConfigMap.js#L37
-- TODO: If there is no other way move it into ReactNavigation.purs
mkScreen :: ReactElement -> Fn0 ReactElement
mkScreen view = mkFn0 \_ -> view

app :: ReactClass Unit
app = createClass $ spec unit render
  where
    render ctx =
      pure $ stackNavigator { home: { screen: mkScreen homeView } }

main :: forall eff. Eff ( register :: REGISTER | eff) Unit
main = registerComponent "HelloMobileNavigation" app
