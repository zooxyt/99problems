'use strict';

module.exports.compress = function (arr) {
  var r = [];
  if (arr.length == 0) return r;
  for (var i = 0; i < arr.length - 1; i++) {
    var ch = arr[i], ch_next = arr[i + 1];
    if (ch == ch_next) continue;
    else {
      r.push(ch);
    }
  }
  r.push(arr[arr.length - 1]);
  return r;
}

