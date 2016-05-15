define("bundles/phoenix/components/AudioJS",["require","exports","module","react-with-addons","react-dom","videojs","bundles/videojs/layout/controls.html","jquery","underscore","css!bundles/phoenix/styl/audioPlayer","bundles/videojs/plugins/countdownDisplay","bundles/videojs/plugins/circleSliderHandle"],function(require,exports,module){"use strict";function _defaults(e,r){for(var i=Object.getOwnPropertyNames(r),n=0;n<i.length;n++){var o=i[n],t=Object.getOwnPropertyDescriptor(r,o);t&&t.configurable&&void 0===e[o]&&Object.defineProperty(e,o,t)}return e}function _classCallCheck(e,n){if(!(e instanceof n))throw new TypeError("Cannot call a class as a function")}function _inherits(n,e){if("function"!=typeof e&&null!==e)throw new TypeError("Super expression must either be null or a function, not "+typeof e);n.prototype=Object.create(e&&e.prototype,{constructor:{value:n,enumerable:!1,writable:!0,configurable:!0}}),e&&(Object.setPrototypeOf?Object.setPrototypeOf(n,e):_defaults(n,e))}var n=function(){function defineProperties(t,o){for(var n=0;n<o.length;n++){var e=o[n];e.enumerable=e.enumerable||!1,e.configurable=!0,"value"in e&&(e.writable=!0),Object.defineProperty(t,e.key,e)}}return function(e,n,o){return n&&defineProperties(e.prototype,n),o&&defineProperties(e,o),e}}(),o=function get(i,c,s){var o=!0;e:for(;o;){var n=i,l=c,a=s;o=!1,null===n&&(n=Function.prototype);var e=Object.getOwnPropertyDescriptor(n,l);if(void 0===e){var t=Object.getPrototypeOf(n);if(null===t)return void 0;i=t,c=l,s=a,o=!0,e=t=void 0;continue e}if("value"in e)return e.value;var r=e.get;if(void 0===r)return void 0;return r.call(a)}},t=require("react-with-addons"),r=require("react-dom"),i=require("videojs"),e=require("bundles/videojs/layout/controls.html"),$=require("jquery"),_=require("underscore");require("css!bundles/phoenix/styl/audioPlayer"),require("bundles/videojs/plugins/countdownDisplay"),require("bundles/videojs/plugins/circleSliderHandle"),module.exports=function(t){function AudioJS(){_classCallCheck(this,AudioJS),o(Object.getPrototypeOf(AudioJS.prototype),"constructor",this).apply(this,arguments)}return _inherits(AudioJS,t),n(AudioJS,[{key:"componentDidMount",value:function componentDidMount(){var o=this,t=r.findDOMNode(this),n=$(t).find("audio");n.addClass("vjs-coursera-phoenix-audio-skin").attr({preload:"metadata"}),_(n).map(function(n){var t={width:"100%",height:"28px",textTrackDisplay:{},bigPlayButton:!1,circlePlayButton:{},controlBar:{children:_.union([{name:"playToggle",el:$(e({controlName:"playToggle"}))[0]},{name:"progressControl",children:{seekBar:{handleName:"circleSliderHandle",children:{seekHandle:!1,circleSliderHandle:{el:$(e({controlName:"circleSliderHandle",classes:"vjs-seek-handle"}))[0]}}}}},"CountdownDisplay"])}},r=i(n,t);o.attachAudioListeners(r)})}},{key:"attachAudioListeners",value:function attachAudioListeners(e){e.on("play",function(){$(e.controlBar.childNameIndex_.playToggle.el_).find(".cif-play").removeClass("cif-play").addClass("cif-pause"),$(e.el_).focus()}),e.on("pause",function(){$(e.controlBar.childNameIndex_.playToggle.el_).find(".cif-pause").removeClass("cif-pause").addClass("cif-play")})}},{key:"render",value:function render(){return this.props.children}}]),AudioJS}(t.Component)});