"use strict"

exports.replaceR_ = function(reg, mods, replacement, str){
  return str.replace(new RegExp(reg, mods), replacement);
}

