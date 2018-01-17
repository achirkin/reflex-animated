/**
 * Created by Artem Chirkin
 */

// polyfill for performance.now
// @license http://opensource.org/licenses/MIT
// copyright Paul Irish 2015
(function() {
    'use strict';
    if( typeof window === typeof undefined) {return;}

    window.performance = (window.performance || {
      offset: Date.now(),
      now: function now(){
          return Date.now() - this.offset;
      }
    });

}());

// make sure that RequestAnimationFrame and CancelAnimationFrame work on all browsers
(function() {
    'use strict';
    if( typeof window === typeof undefined) {return;}

    var lastTime = 0;
    var vendors = ['ms', 'moz', 'webkit', 'o'];
    for(var x = 0; x < vendors.length && !window['requestAnimationFrame']; ++x) {
        window['requestAnimationFrame'] = window[vendors[x]+'RequestAnimationFrame'];
        window['cancelAnimationFrame'] = window[vendors[x]+'CancelAnimationFrame']
            || window[vendors[x]+'CancelRequestAnimationFrame'];
    }

    if (!window['requestAnimationFrame'])
        window['requestAnimationFrame'] = function(callback, element) {
            var currTime = new Date().getTime();
            var timeToCall = Math.max(0, 16 - (currTime - lastTime));
            var id = window.setTimeout(function() { callback(currTime + timeToCall); },
                timeToCall);
            lastTime = currTime + timeToCall;
            return id;
        };

    if (!window['cancelAnimationFrame'])
        window['cancelAnimationFrame'] = function(id) {
            clearTimeout(id);
        };
}());

// the main thing
var Animation = (function () {
    'use strict';
    if( typeof window === typeof undefined) {return undefined;}

    // helper function to enable MouseEvent.buttons
    var toButtons = function(btn){
        switch(btn) {
            case	0: return 1;
            case	1: return 4;
            case	2: return 2;
            case	3: return 8;
            case	4: return 16;
            case	5: return 32;
            default: return 0;
        }
    };
    var convertEvent = function(f, pType, g) {
        return function (ev) {
            var e = window.event || ev;
            e.preventDefault();
            e.stopPropagation();
            var pk = e.target.pointerKeeper;
            switch (pType) {
                case 0:
                    if (( (performance.now() - pk.downTime)
                        + (  Math.abs(pk.curPointers[0][0]-pk.downPointers[0][0])
                           + Math.abs(pk.curPointers[0][1]-pk.downPointers[0][1])
                          ) * (ev['touches'] ? 3 : 10)
                        ) < 200) {
                        g(new PointerEvent(e, pType));
                    } else {
                        f(new PointerEvent(e, pType));
                    }
                    break;
                case 2:
                    if ( pk.movedInThisFrame) {
                        return false;
                    } else {
                        f(new PointerEvent(e, pType));
                        pk.movedInThisFrame = this.running;
                        return false;
                    }
                    break;
                default:
                    f(new PointerEvent(e, pType));
                    break;
            }
            return false;
        };
    };

    // A pointer represents various kinds of pointer events.
    // eventType:
    //   0 - pointerUp;
    //   1 - pointerDown;
    //   2 - pointerMove;
    //   3 - pointerCancel;
    var PointerEvent = function(ev, t) {
        this.target = ev.target;
        var pk = this.target.pointerKeeper,
            isTouchEv = !!ev['touches'],
            touching  = isTouchEv && ev['touches'].length > 0;
        this.pointerEventType = t;
        this.type = ev.type;
        this['button'] = ev['button'] != undefined ? ev['button'] : (touching ? 0 : -1);

        if (isTouchEv) {
            pk['buttons'] = touching ? 1 : 0;
        } else if (ev['buttons'] != undefined) {
            pk['buttons'] = ev['buttons'];
        } else {
            var b = ev['button'] != undefined ? toButtons(ev['button']) : 0;
            switch(t) {
                case	0: // pointerUp
                    pk['buttons'] = pk['buttons'] & (~ b);
                    break;
                case	1: // pointerDown
                    pk['buttons'] = pk['buttons'] | b;
                    break;
                case	3: // pointerCancel
                    pk['buttons'] = pk['buttons'] & (~ b);
                    break;
                }
        }

        pk.altKey = ev.altKey ? ev.altKey : false;
        pk.ctrlKey = ev.ctrlKey ? ev.ctrlKey : false;
        pk.metaKey = ev.metaKey ? ev.metaKey : false;
        pk.shiftKey = ev.shiftKey ? ev.shiftKey : false;
        if (isTouchEv) {
            this.pointers = ev['touches'].length == 0 ? pk.curPointers :
                 Array.prototype.slice.call(ev['touches']).map(function(t) {return [t.clientX - pk.clientX, t.clientY - pk.clientY];});
        } else {
            this.pointers = ev.clientX ? [[ev.clientX - pk.clientX, ev.clientY - pk.clientY]] : pk.curPointers;
        }

        // update keeper's pointer position tracking
        pk.curPointers = this.pointers;
        if (t != 2) {
            pk.downPointers = this.pointers;
        }
        if (t == 1) {
            pk.downTime = performance.now();
        }
    };

    var PointerKeeper = function PointerKeeper(el, update, pointerUp, pointerClick, pointerDown, pointerMove, pointerCancel, resize) {
        var pk = this;
        this.target = el;
        this.busy = false;
        this.running = false;
        this.update = update;
        el.pointerKeeper = this;
        this.downPointers = [];
        this.curPointers = [];
        this.downTime = 0;
        this.sizeUpdated = false;
        this.scrolledInThisFrame = false;
        this.movedInThisFrame = false;
        this.updateLocationCallback = function() { pk.updateLocation.apply(pk, []); resize([pk.width, pk.height]); };
        window.addEventListener('scroll',function() { pk.updateLocation.apply(pk, []); });
        //window.addEventListener('resize', function() { pk.f.apply(pk, [false]); }  );

        el.addEventListener('contextmenu',function(ev){var e = window.event||ev; e.preventDefault();e.stopPropagation();return false;});
        el['style']['touch-action'] = "none";

        var pUp = convertEvent(pointerUp, 0, pointerClick);
        el.addEventListener('mouseup', pUp);
        el.addEventListener('touchend', pUp);
        var pDown = convertEvent(pointerDown, 1, null);
        el.addEventListener('mousedown', pDown);
        el.addEventListener('touchstart', pDown);
        var pMove = convertEvent(pointerMove, 2, null);
        el.addEventListener('mousemove', pMove);
        el.addEventListener('touchmove', pMove);
        var pCancel = convertEvent(pointerCancel, 3, null);
        el.addEventListener('mouseleave', pCancel);
        el.addEventListener('touchcancel', pCancel);

        document.addEventListener('webkitfullscreenchange', pk.updateLocationCallback, false);
        document.addEventListener('mozfullscreenchange', pk.updateLocationCallback, false);
        document.addEventListener('msfullscreenchange', pk.updateLocationCallback, false);
        document.addEventListener('fullscreenchange', pk.updateLocationCallback, false);
        // make sure we prepared scaling
        this.updateLocationCallback();
        window.addEventListener('load',pk.updateLocationCallback ,false);
    };

    // Update position of an element so that it serves as a veiwport for pointer position;
    // computes available space within margin, border, and padding (so that it works with an html canvas size).
    PointerKeeper.prototype.updateLocation = function () {
        var elstyle = window.getComputedStyle(this.target, null),
            pleft = parseFloat(elstyle.getPropertyValue('padding-left')) + parseFloat(elstyle.getPropertyValue('border-left-width')),
            ptop = parseFloat(elstyle.getPropertyValue('padding-top')) + parseFloat(elstyle.getPropertyValue('border-top-width')),
            pbottom = parseFloat(elstyle.getPropertyValue('padding-bottom')) + parseFloat(elstyle.getPropertyValue('border-bottom-width')),
            pright = parseFloat(elstyle.getPropertyValue('padding-right')) + parseFloat(elstyle.getPropertyValue('border-right-width')),
            bbox = this.target.getBoundingClientRect();
        this.clientX = bbox.left + pleft;
        this.clientY = bbox.top + ptop;
        this.width = bbox.width - pleft - pright;
        this.height = bbox.height - ptop - pbottom;
        this.bboxw = bbox.width;
        this.bboxh = bbox.height;
        this.target.width = this.width;
        this.target.height = this.height;
    };

    // run animation loop
    PointerKeeper.prototype.play = function () {
        this.running = true;
        this.busy = true;
        var pk = this;
        var bbox = pk.target.getBoundingClientRect();
        function step(timestamp) {
            if (pk.running) {
                pk.busy = true;
                pk.update(timestamp);
                pk.movedInThisFrame = false;
                pk.scrolledInThisFrame = false;

                bbox = pk.target.getBoundingClientRect();
                if (pk.bboxw != bbox.width || pk.bboxh != bbox.height) {
                    pk.updateLocationCallback();
                    // console.log('updating!', pk.bboxw, bbox.width, pk.bboxh, bbox.height);
                }
                window.requestAnimationFrame(step);
            } else {
                pk.busy = false;
            }
        }
        window.requestAnimationFrame(step);
    };

    PointerKeeper.prototype.playStep = function () {
        var pk = this;
        if(!pk.busy) {
            pk.busy = true;
            var bbox = pk.target.getBoundingClientRect();
            function step(timestamp) {
                pk.update(timestamp);
                pk.movedInThisFrame = false;
                pk.scrolledInThisFrame = false;
                bbox = pk.target.getBoundingClientRect();
                if (pk.bboxw != bbox.width || pk.bboxh != bbox.height) {
                    pk.updateLocationCallback();
                    // console.log('updating!', pk.bboxw, bbox.width, pk.bboxh, bbox.height);
                }
                pk.busy = false;
            }
            window.requestAnimationFrame(step);
        }
    };

    // stop animation loop
    PointerKeeper.prototype.stop = function () {
        this.running = false;
    };


    PointerKeeper.prototype.listenToWheel = function(f) {
        var pk = this;
        this.target.addEventListener('wheel', function(ev){
            var e = window.event || ev;
            e.preventDefault();
            e.stopPropagation();
            if(!pk.scrolledInThisFrame || !pk.running){
                f(e['wheelDelta'] > 0 || e['detail'] < 0 || e['deltaY'] < 0 ? (1.0) : (-1.0));
                pk.scrolledInThisFrame = true;
            }
            return false;
        });
    };

    return {
        PointerEvent: PointerEvent,
        PointerKeeper: PointerKeeper
    };
}());
