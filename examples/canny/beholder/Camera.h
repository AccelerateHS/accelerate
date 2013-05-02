
#import <Cocoa/Cocoa.h>
#import <QTKit/QTKit.h>

@interface Camera : NSObject {
        QTCaptureDeviceInput*           mDeviceInput;
        QTCaptureSession*               mSession;
        QTCaptureVideoPreviewOutput*    mPreviewOutput;
        CVImageBufferRef                mCurrentBuffer;

        id                              mPulseMe;
}

- (id)  init;

- (void)start;

- (QTCaptureSession*)session;

- (CVImageBufferRef) currentBuffer;

- (void)setNotify: (id)notify;

- (void)captureOutput:  (QTCaptureOutput*)    captureOutput
  didOutputVideoFrame:  (CVImageBufferRef)    videoFrame
     withSampleBuffer:  (QTSampleBuffer*)     sampleBuffer
       fromConnection:  (QTCaptureConnection*)connection;

@end

