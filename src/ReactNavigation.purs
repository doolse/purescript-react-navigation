module ReactNavigation where

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toNullable)
import Data.Function.Uncurried (Fn2, runFn2)
import Unsafe.Coerce (unsafeCoerce)
import React (ReactElement)
import ReactNative.Styles (Styles)
import ReactNative.PropTypes.Color (Color)
import ReactNative.PropTypes (class NoneEnum)

-- | StackNavigator https://reactnavigation.org/docs/navigators/stack#API-Definition

foreign import stackNavigatorImpl :: forall r c. { | r } -> { | c } -> ReactElement

-- | Create a StackNavigator with a given RouteConfig
stackNavigator :: forall r. { | r } -> ReactElement
stackNavigator routeConfig = stackNavigatorImpl routeConfig {}

  -- | Create a StackNavigator with a given RouteConfig and StackNavigatorConfig
stackNavigator' :: forall r c. { | r } -> { | c } -> ReactElement
stackNavigator' routeConfigs stackNavConfig = stackNavigatorImpl routeConfigs stackNavConfig


-- | RouteConfig https://reactnavigation.org/docs/navigators/stack#RouteConfigs
type RouteConfig =
    { screen :: ReactElement
    , path :: String
    , navigationOptions :: ScreenNavigationOptions
    }

mkRouteConfig :: forall c. { screen :: ReactElement | c } -> RouteConfig
mkRouteConfig = unsafeCoerce

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
