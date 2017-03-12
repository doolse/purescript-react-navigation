module Main.Ios where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Dispatcher (DispatchEffFn(..), effEval)
import Dispatcher.React (createComponent, getProps)
import React (ReactClass, createClass, spec)
import ReactNative.API (REGISTER, registerComponent)
import ReactNative.Components.Button (button)
import ReactNative.Components.Text (text_)
import ReactNative.Components.View (view_)
import ReactNavigation (Navigation, RouteConfig, applyNavigationOptions, mkRouteConfig, navigate, stackNavigator')

data Action = Navigate String

homeScreen :: forall p. ReactClass { navigation :: Navigation | p }
homeScreen = createComponent unit render (effEval eval)
  where
    render _ (DispatchEffFn d) =
      view_
        [ text_ "Hello, Chat App!"
        , button "Chat with Lucy" $ d \_ -> Navigate "chat"
        ]
    eval (Navigate target) = do
      {navigation} <- getProps
      liftEff $ navigate navigation target

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
