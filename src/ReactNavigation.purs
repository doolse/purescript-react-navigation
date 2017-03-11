module ReactNavigation where

import Prelude
import Data.Function.Uncurried (Fn0)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toNullable)
import React (ReactClass, ReactElement, ReactThis)
import ReactNative.PropTypes (class NoneEnum, Prop)
import ReactNative.PropTypes.Color (Color)
import ReactNative.Styles (Styles)
import ReactNative.Unsafe.ApplyProps (unsafeApplyProps)

-- | StackNavigator https://reactnavigation.org/docs/navigators/stack#API-Definition

newtype Navigation = Navigation (forall props state. ReactThis props state)

foreign import stackNavigatorImpl :: forall r c props. { | r } -> { | c } -> ReactClass props

-- | Create a StackNavigator with a given map of RouteConfigs and StackNavigatorConfigs
stackNavigator :: forall r c props. { | r } -> { | c } -> ReactClass props
stackNavigator = stackNavigatorImpl

-- | Create a StackNavigator with a given map of RouteConfigs
stackNavigator' :: forall r props. { | r } -> ReactClass props
stackNavigator' routeConfig = stackNavigatorImpl routeConfig {}

foreign import applyNavigationOptionsImpl :: forall o p. ReactClass {navigation :: Navigation | p}
  -> o -> ReactClass {navigation :: Navigation | p}

-- | Applies `ScreenNavigationOptions` to a screen
applyNavigationOptions :: forall p. ReactClass { navigation :: Navigation | p }
  -> Prop ScreenNavigationOptions -> ReactClass { navigation :: Navigation | p }
applyNavigationOptions screen options = applyNavigationOptionsImpl screen $ unsafeApplyProps {} options

-- | RouteConfig https://reactnavigation.org/docs/navigators/stack#RouteConfigs
type RouteConfig =
  { screen :: forall p. ReactClass { navigation :: Navigation | p }
  , getScreen :: forall p. Fn0 (ReactClass { navigation :: Navigation | p })
  , path :: String
  , navigationOptions :: ScreenNavigationOptions
  }

mkRouteConfig :: Prop (RouteConfig) -> RouteConfig
mkRouteConfig config = unsafeApplyProps {} config

-- | ScreenNavigationOptions https://reactnavigation.org/docs/navigators/stack#Screen-Navigation-Options
type ScreenNavigationOptions =
  { title :: String
  , header :: HeaderBarConfig
  }

type HeaderBarConfig =
  { visible :: Boolean
  , title :: String -- TODO String or ReactElement
  , backTitle :: Nullable String
  , right :: ReactElement
  , left :: ReactElement
  , style :: Styles
  , titleStyle :: Styles
  , tintColor :: Color
  , cardStack :: CardStackConfig
  }

mkBackTitle :: Maybe String -> Nullable String
mkBackTitle = toNullable

type CardStackConfig =
  { gesturesEnabled :: Boolean
  }


-- | StackNavigatorConfig https://reactnavigation.org/docs/navigators/stack#StackNavigatorConfig

type StackNavigatorConfig =
  { initialRouteName :: String
  , initialRouteParams :: {}
  }

newtype StackNavigatorConfigMode = StackNavigatorConfigMode String

stackNavigatorConfigMode ::
  { modal :: StackNavigatorConfigMode
  , card :: StackNavigatorConfigMode
  }
stackNavigatorConfigMode =
  { modal: StackNavigatorConfigMode "modal"
  , card: StackNavigatorConfigMode "card"
  }

newtype StackNavigatorConfigHeaderMode = StackNavigatorConfigHeaderMode String

instance stackNavigatorConfigHeaderModeNone :: NoneEnum StackNavigatorConfigHeaderMode where
  none = StackNavigatorConfigHeaderMode "none"

stackNavigatorConfigHeaderMode ::
  { float :: StackNavigatorConfigHeaderMode
  , screen :: StackNavigatorConfigHeaderMode
  , none :: StackNavigatorConfigHeaderMode
  }
stackNavigatorConfigHeaderMode =
  { float: StackNavigatorConfigHeaderMode "float"
  , screen: StackNavigatorConfigHeaderMode "screen"
  , none: StackNavigatorConfigHeaderMode "none"
  }
