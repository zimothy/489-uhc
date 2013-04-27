var Signals = function () {
    "use strict";

    var Signal = function () {
        this.registeredListeners = []
        this.__isSignal = true
    }

    Signal.prototype.registerListener = function(callback) {
        this.registeredListeners.push(function (value) {
            if(!callback || !callback.args) return
            UHCFunction.apply(callback, value)
        })
    }

    Signal.prototype.push = function(value) {
        for(var i = 0, len = this.registeredListeners.length; i < len; i++) {
            this.registeredListeners[i](value);
        }
    }

    Signal.prototype.pipe = function(transform) {
        console.log("hello, world: ", transform);
        var newSignal = new Signal()
        this.registeredListeners.push(function (value) {
            newSignal.push(UHCFunction.apply(transform, value))
        });
        return newSignal
    }

    function createEventedSignal (elem, event, key) {
        if(elem && elem[0] instanceof NodeList) elem = elem[0][0]
        if(elem && elem.length) elem = elem[0]
        if(!elem || !elem.addEventListener || !event || typeof event !== "string")
            return undefined
        var s = new Signal()
        elem.addEventListener(event, function (e) {
            s.push(elem[key || 'value'], e)
        })
        return s
    }

    function bindToSignal (signal, callback) {
        if(!signal || !callback || !callback.args) return undefined
        
        signal.registerListener(callback)
        return {_1:{__aN__ : function() { return signal }}}
    }

    function ampersand(pair) {
        console.log(pair);
    }

    return {
        createEventedSignal: createEventedSignal,
        bindToSignal: bindToSignal,
        ampersand: ampersand
    }
}()