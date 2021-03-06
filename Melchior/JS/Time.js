var Time = function () {
    "use strict";
    
    var Time = function () {}

    Time.prototype.every = function(period) {
        var initial = 0, signal = new Signals.Signal(this)
        
        function push(t) {
            signal.push(t)
            setTimeout(push, period, t+period)
        }
        setTimeout(push, period, initial)
        return signal
    }

    Time.prototype.after = function(timeout) {
        var signal = new Signals.Signal(this)
        function push(t) {
            signal.push(t)
        }
        setTimeout(push, timeout)
        return signal
    }

    return new Time()

}()