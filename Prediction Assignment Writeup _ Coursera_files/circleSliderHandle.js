define("bundles/videojs/plugins/circleSliderHandle",["require","exports","module","bundles/videojs/layout/controls.html","videojs","jquery"],function(require,exports,module){"use strict";var r=require("bundles/videojs/layout/controls.html"),e=require("videojs"),$=require("jquery");e.CircleSliderHandle=e.SeekHandle.extend(),e.CircleSliderHandle.prototype.updateContent=function(){var i=this.player_.scrubbing?this.player_.getCache().currentTime:this.player_.currentTime();this.el_.innerHTML=r({controlName:"circleSeekHandleText",time:e.formatTime(i,this.player_.duration())})}});