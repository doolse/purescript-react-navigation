const RN = require('react-navigation');

exports.stackNavigatorImpl = function (rConfig, snConfig) {
  return RN.StackNavigator(rConfig, snConfig);
}

exports.applyNavigationOptionsImpl = function (screen, options) {
  screen.navigationOptions = options;
  return screen;
}

exports.navigateImpl = function(_this, path, params) {
  return function() {
    const p = params || {};
    _this.navigate(path, params);
  }
}

exports.getParamsImpl = function(_this) {
  return function() {
    return _this.state.params;
  };
}
