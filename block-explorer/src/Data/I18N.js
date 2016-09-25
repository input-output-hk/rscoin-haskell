"use strict"

exports.detectLanguage_ = function() {
    return window.navigator.languages ? window.navigator.languages[0] : (window.navigator.language || window.navigator.userLanguage)
}

