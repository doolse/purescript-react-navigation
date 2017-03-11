module Main.Ios where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Function.Eff (mkEffFn1)
import React (ReactClass, createClass, spec)
import ReactNative.API (REGISTER, registerComponent)
import ReactNative.Components.Button (button)
import ReactNative.Components.Text (text_)
import ReactNative.Components.View (view_)
import ReactNavigation (Navigation, RouteConfig, applyNavigationOptions, mkRouteConfig, stackNavigator')

homeScreen :: forall p. ReactClass { navigation :: Navigation | p }
homeScreen = createClass $ spec unit \_ ->
  pure $ view_
          [ text_ "Hello, Chat App!"
          , button "Chat with Lucy" clickHandler ]
  where
    -- TODO: Add clickHandler
    clickHandler = mkEffFn1 \_ -> pure unit

chatScreen :: forall p. ReactClass { navigation :: Navigation | p }
chatScreen = createClass $ spec unit \_ ->
  pure $ view_ [ text_ "Chat with Lucy" ]

type Routes =
    { home :: RouteConfig
    , chat :: RouteConfig
    }

routes :: Routes
routes =
  { home: mkRouteConfig _{ screen = applyNavigationOptions homeScreen _{ title = "Welcome" } }
  , chat: mkRouteConfig _{ screen = applyNavigationOptions chatScreen _{ title = "Chat with Lucy" } }
  }

app :: ReactClass Routes
app = stackNavigator' routes

main :: forall eff. Eff ( register :: REGISTER | eff) Unit
main = registerComponent "HelloMobileNavigation" app
