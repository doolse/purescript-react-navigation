const RN = require('react-navigation');

exports.stackNavigatorImpl = function (rConfig) {
  return function (snConfig) {
    return RN.StackNavigator(rConfig, snConfig);
  }
}

exports.applyNavigationOptionsImpl = function (screen) {
  return function (options) {
    screen.navigationOptions = options;
    return screen;
  }
}
