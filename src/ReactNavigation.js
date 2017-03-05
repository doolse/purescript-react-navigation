const RN = require('react-navigation');

exports.stackNavigatorImpl = function (rConfig, snConfig) {
  return function () {
    RN.StackNavigator(rConfig, snConfig);
  }
}
