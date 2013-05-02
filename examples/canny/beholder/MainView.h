
#import <Cocoa/Cocoa.h>
#import <QTKit/QTKit.h>

#import "Camera.h"
#import "Framer.h"

@interface MainView : NSObject {
        IBOutlet QTCaptureView* oCaptureView;
        IBOutlet NSImageView*   oImageView;
        IBOutlet NSButton*      oBlurEnable;
        IBOutlet NSButton*      oInvertOutput;
        IBOutlet NSMatrix*      oOutputMode;
        IBOutlet NSSlider*      oThresholdHigh;
        IBOutlet NSSlider*      oThresholdLow;

        Framer*                 mFramer;
}

- (IBAction)    capture:(id)sender;

- (void)        initCaptureView:(Camera*)camera;
- (void)        setFramer:(Framer*)framer;
- (void)        pulse;

extern MainView* globalMainView;

@end
