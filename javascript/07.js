'use strict';

function is_array(x) {
  return Object.prototype.toString.call(x) === '[object Array]';
}

function flatten(arr) {
  let r = [];
  for (const item of arr) {
    if (is_array(item)) {
      let v = flatten(item);
      for (const item2 of v) {
        r.push(item2);
      }
    }
    else { r.push(item); }
  }
  return r;
}

module.exports.flatten = flatten;

