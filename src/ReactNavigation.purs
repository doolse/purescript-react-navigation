module ReactNavigation where

import Data.Function.Uncurried (Fn0)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toNullable)
import React (ReactElement, ReactClass)
import ReactNative.PropTypes (class NoneEnum)
import ReactNative.PropTypes.Color (Color)
import ReactNative.Styles (Styles)
import Unsafe.Coerce (unsafeCoerce)

-- | StackNavigator https://reactnavigation.org/docs/navigators/stack#API-Definition

foreign import stackNavigatorImpl :: forall r c props. { | r } -> { | c } -> ReactClass props

-- | Create a StackNavigator with a given RouteConfig and StackNavigatorConfig
stackNavigator :: forall r c props. { | r } -> { | c } -> ReactClass props
stackNavigator = stackNavigatorImpl

-- | Create a StackNavigator with a given RouteConfig
stackNavigator' :: forall r props. { | r } -> ReactClass props
stackNavigator' routeConfig = stackNavigatorImpl routeConfig {}

foreign import applyNavigationOptionsImpl :: forall props o. ReactClass props -> o -> ReactClass props

-- | Applies `ScreenNavigationOptions` to a screen
applyNavigationOptions :: forall props o. ReactClass props -> o -> ReactClass props
applyNavigationOptions = applyNavigationOptionsImpl

-- | RouteConfig https://reactnavigation.org/docs/navigators/stack#RouteConfigs
type RouteConfig =
    { screen :: forall props. ReactClass props
    , getScreen :: forall props. Fn0 (ReactClass props)
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
