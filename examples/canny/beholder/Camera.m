// Camera grabs frames from the camera and stores the current one.

#import "Camera.h"
#import "MainView.h"

@implementation Camera

// Initialise and open the first available camera.
- (id) init
{
        BOOL            success = NO;
        NSError*        error;
        mPulseMe                = nil;
        self    = [super init];

        NSLog(@"Initialsing capture system");

        // List available video devices ----------------------------------
        NSArray *devices        = [QTCaptureDevice inputDevices];
        for (QTCaptureDevice* device in devices) {
                NSLog(@"available device: %@", [device localizedDisplayName]);
        }

        // Choose a video device and open it -----------------------------
        QTCaptureDevice *videoDevice    = nil;

        // Try to use the first avilable video device.
        videoDevice     = [QTCaptureDevice defaultInputDeviceWithMediaType:QTMediaTypeVideo];
        success         = [videoDevice open:&error];

        // If a plain video input device can't be found or opened,
        //       try to find and open a muxed input device
        if (!success) {
                videoDevice = [QTCaptureDevice defaultInputDeviceWithMediaType:QTMediaTypeMuxed];
                success         = [videoDevice open:&error];
        }

        if (!success) {
                videoDevice = nil;
                NSLog(@"Can't open video device.");
                [[NSAlert alertWithError: error] runModal];
                abort();
        }

        NSLog(@"opened device: %@", [videoDevice localizedDisplayName]);


        // Setup capture session ------------------------------------------
        mSession         = [[QTCaptureSession alloc] init];

        // Add device input to the capture session.
        mDeviceInput = [[QTCaptureDeviceInput alloc] initWithDevice:videoDevice];
        success = [mSession addInput:mDeviceInput error:&error];

        if (!success) {
                [[NSAlert alertWithError: error] runModal];
                abort();
        }

        // The PreviewOutput grabs frames from the capture session --------
        mPreviewOutput  = [[QTCaptureVideoPreviewOutput alloc] init];

        // Set the delagate to ourselves, so the PreviewOutput calls our own captureOutput
        // method when it's got a new frame.
        [mPreviewOutput setDelegate:self];

        // Tell the Session to send frames to our frame grabber.
        success = [mSession addOutput:mPreviewOutput error:&error];
        if (!success) {
                [[NSAlert alertWithError:error] runModal];
                abort();
        }

        return self;
}


// Start the capturing session.
// We don't do this automatically at startup so that MainView has time to attach it's CaptureView UI widget.
- (void)start
{
        [mSession startRunning];
}


// Get the CaptureSession.
// Used by MainView.
- (QTCaptureSession*)session
{
        return mSession;
}


- (void)setNotify: (id)notify
{
        mPulseMe = notify;
}


// Get the current video frame.
// The caller is responsible for calling CVBufferRelease on it when it's done with it.
- (CVImageBufferRef)    currentBuffer
{
        CVImageBufferRef buffer;
        @synchronized (self) {
                buffer  = mCurrentBuffer;
                CVBufferRetain(buffer);
        }

        return buffer;
}


// This gets called whenever the PreviewOutput receieves a new frame.
// We store the latest frame in mCurrentImage, and release the old one.
- (void)captureOutput:  (QTCaptureOutput*)    captureOutput
  didOutputVideoFrame:  (CVImageBufferRef)    videoFrame
     withSampleBuffer:  (QTSampleBuffer*)     sampleBuffer
       fromConnection:  (QTCaptureConnection*)connection
{
        CVImageBufferRef releaseMe = nil;
        CVBufferRetain(videoFrame);

        // This must be done in a @synchronized block because the delegate
        // method is not called in the same thread.
        @synchronized (self) {
                releaseMe       = mCurrentBuffer;
                mCurrentBuffer  = videoFrame;
        }

        CVBufferRelease(releaseMe);

        if (mPulseMe != nil)
                [mPulseMe pulse];
}

@end
