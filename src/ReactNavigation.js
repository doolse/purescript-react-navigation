const RN = require('react-navigation');

exports.stackNavigatorImpl = function (rConfig, snConfig) {
  return RN.StackNavigator(rConfig, snConfig);
}

exports.applyNavigationOptionsImpl = function (screen, options) {
  screen.navigationOptions = options;
  return screen;
}

exports.navigateImpl = function(_this, path) {
  return function() {
    _this.navigate(path);
  }
}
