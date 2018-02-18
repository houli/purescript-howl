'use strict';
var Howl = require('howler').Howl;

exports.newImpl = function(src) {
  return function(options) {
    return function() {
      options.src = src;
      return new Howl(options);
    };
  };
};

exports.newSpriteImpl = function(src) {
  return function(sprite) {
    return function(options) {
      return function() {
        options.sprite = sprite;
        options.src = src;
        return new Howl(options);
      };
    };
  };
};

exports.play = function(howl) {
  return function() {
    howl.play();
  };
};

exports.playSprite = function(howl) {
  return function(sprite) {
    return function() {
      howl.play(sprite);
    };
  };
};

exports.pauseImpl = function(howl) {
  return function() {
    howl.pause();
  };
};

exports.stopImpl = function(howl) {
  return function() {
    howl.stop();
  };
};

exports.muteImpl = function(howl) {
  return function() {
    howl.mute(true);
  };
};

exports.unmuteImpl = function(howl) {
  return function() {
    howl.mute(false);
  };
};

exports.volumeImpl = function(howl) {
  return function(volume) {
    return function() {
      howl.volume(volume);
    };
  };
};

exports.rateImpl = function(howl) {
  return function(rate) {
    return function() {
      howl.rate(rate);
    };
  };
};

exports.seekImpl = function(howl) {
  return function(seek) {
    return function() {
      howl.seek(seek);
    };
  };
};

exports.loopImpl = function(howl) {
  return function(loop) {
    return function() {
      howl.loop(loop);
    };
  };
};

exports.playingImpl = function(howl) {
  return function() {
    return howl.playing();
  };
};
