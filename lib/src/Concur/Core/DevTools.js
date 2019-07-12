"use strict";

function hasDevTools() {
  return (process.env.NODE_ENV === 'development' && window.__REDUX_DEVTOOLS_EXTENSION__);
}

exports.connectDevTools = function() {
  if(hasDevTools()) {
    return window.__REDUX_DEVTOOLS_EXTENSION__.connect();
  } else {
    // ??
    return null;
  }
};

exports.disconnectDevTools = function() {
  if(hasDevTools()) {
    return window.__REDUX_DEVTOOLS_EXTENSION__.disconnect();
  } else {
    // ??
    return null;
  }
};

exports.sendToDevTools = function(connection) {
  return function(action) {
    return function(state) {
      return function() {
        if(hasDevTools()) {
          return connection.send(action, state);
        } else {
          // ??
          return null;
        }
      };
    };
  };
};

exports.subscribeDevTools = function(connection) {
  return function(handler) {
    return function() {
      if(hasDevTools()) {
        return connection.subscribe(function(message) {
          if (message.type === 'DISPATCH' && message.state) {
            // Extra () due to handler being a State -> Effect
            handler(message.state)();
          }
        });
      } else {
        // ??
        return null;
      }
    };
  };
};

exports.unsubscribeDevTools = function(connection) {
  return function() {
    connection.unsubscribe();
  };
};
