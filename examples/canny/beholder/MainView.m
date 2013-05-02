
#import "MainView.h"

MainView* globalMainView;

@implementation MainView

// Called at startup.
- (void)awakeFromNib
{
        globalMainView = self;
}


// We get given the camera by the start up function,
// and pass it off to our previewer widget.
- (void)initCaptureView:(Camera*)camera
{
        [oCaptureView setCaptureSession: [camera session]];
}


// We get given the Framer by the startup function.
- (void)setFramer:(Framer*)framer
{
        mFramer = framer;
}


// This gets called when the used clicks the capture button.
- (IBAction)capture:(id)sender
{
        NSLog(@"Click");
        [self pulse];
}

// Call this to force a capture
- (void)pulse
{
        // Read the control parameters from the interface.
        int modeGrey    = [[oOutputMode cellWithTag:0] intValue];
        int modeBlur    = [[oOutputMode cellWithTag:1] intValue];
        int modeDX      = [[oOutputMode cellWithTag:2] intValue];
        int modeDY      = [[oOutputMode cellWithTag:3] intValue];
        int modeMag     = [[oOutputMode cellWithTag:4] intValue];
        int modeOrient  = [[oOutputMode cellWithTag:5] intValue];
        int modeMax     = [[oOutputMode cellWithTag:6] intValue];
        int modeLink    = [[oOutputMode cellWithTag:7] intValue];
        int mode        = modeGrey   ? 0 : modeBlur   ? 1 : modeDX   ? 2 : modeDY   ? 3
                        : modeMag    ? 4 : modeOrient ? 5 : modeMax  ? 6 : modeLink ? 7 : 0;

        NSInteger iBlurEnable   = [oBlurEnable    state];
        NSInteger iInvertOutput = [oInvertOutput  state];

        float thresholdLow      = [oThresholdLow  floatValue];
        float thresholdHigh     = [oThresholdHigh floatValue];

        if (thresholdLow > thresholdHigh) {
                thresholdLow = thresholdHigh;
        }

        NSImage* image
                = [mFramer getImage:mode
                         enableBlur:iBlurEnable
                       enableInvert:iInvertOutput
                       thresholdLow:thresholdLow
                      thresholdHigh:thresholdHigh];

        if ([oImageView image] != nil) {
            [[oImageView image] release];
        }
        [oImageView setImage: image];
}

@end
