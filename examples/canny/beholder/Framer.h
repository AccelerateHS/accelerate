
#import <Cocoa/Cocoa.h>
#import "Camera.h"

@interface Framer : NSObject {
        Camera*         mCamera;
}

- (id)  initWithCamera:(Camera*)camera;

// Get an image from the camera.
- (NSImage*) getImage:(NSInteger)mode
           enableBlur:(NSInteger)iBlurEnable
         enableInvert:(NSInteger)iInvertOutput
         thresholdLow:(float)thresholdLow
        thresholdHigh:(float)thresholdHigh;

- (CGColorSpaceRef)     getScreenColorSpace;

@end
