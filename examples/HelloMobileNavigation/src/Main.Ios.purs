module Main.Ios where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Maybe (Maybe(..))
import Dispatcher (DispatchEffFn(..), effEval)
import Dispatcher.React (ReactProps(..), createComponent, getProps)
import React (ReactClass)
import ReactNative.API (REGISTER, registerComponent)
import ReactNative.Components.Button (button)
import ReactNative.Components.Text (text_)
import ReactNative.Components.View (view_)
import ReactNavigation (NavigationProp, RouteConfig, applyNavigationOptions, mkRouteConfig, navigate, stackNavigator')

data Action = Navigate String

homeScreen :: forall p. ReactClass { navigation :: NavigationProp | p }
homeScreen = createComponent unit render (effEval eval)
  where
    render _ (DispatchEffFn d) =
      view_
        [ text_ "Hello, Chat App!"
        , button "Chat with Lucy" $ d \_ -> Navigate "chat"
        ]
    eval (Navigate target) = do
      {navigation} <- getProps
      liftEff $ navigate navigation target $ Just { user: "Lucy" }

chatScreen :: forall p. ReactClass { navigation :: NavigationProp | p }
chatScreen = createComponent unit render unit
  where
    render _ (ReactProps p) = view_ [ text_ $ "Chat with Lucy" ]

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
