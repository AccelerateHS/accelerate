
#import "Framer.h"
#include "Process_stub.h"

// The framer takes the current frames from the camera
// and processes them into NSImages
@implementation Framer

- (id)  initWithCamera:(Camera*)camera
{
        self    = [super init];

        // Stash the camera for later.
        mCamera = camera;

        return self;
}


// Get an image from the camera.
- (NSImage*) getImage:(NSInteger)mode
           enableBlur:(NSInteger)iBlurEnable
         enableInvert:(NSInteger)iInvertOutput
         thresholdLow:(float)thresholdLow
        thresholdHigh:(float)thresholdHigh
{
//      NSImage* image = [[NSImage alloc] initWithContentsOfFile: @"/Users/benl/tmp/lena.bmp"];

        CVImageBufferRef currentBuffer  = [mCamera currentBuffer];

        // Make a CIImage, which wraps the raw, compressed data in the CVImageBuffer.
        CIImage* ciImage                = [CIImage imageWithCVImageBuffer:currentBuffer];

        // The image from my iSight comes as 1280x1024.
        // Scale this down to a more sensible size.
        // TODO: choose this based on the size instead of using a static scaling factor.
        if ([ciImage extent].size.width > 800) {

                CIFilter* filter                = [CIFilter filterWithName:@"CIAffineTransform"];
                NSAffineTransform* transform    = [NSAffineTransform transform];
                [transform scaleBy: 0.5];
                [filter setDefaults];
                [filter setValue: ciImage   forKey: @"inputImage"];
                [filter setValue: transform forKey: @"inputTransform"];
                ciImage                         = [filter valueForKey: @"outputImage"];
        }

        CGRect   extent                 = [ciImage extent];
//      NSLog(@"image size %d %d",      (int)[ciImage extent].size.width,
//                                        (int)[ciImage extent].size.height);


        // Create the CGContext which we render the final image into.
        // This holds the actual pixel buffer.
        size_t  width                   = (size_t)extent.size.width;
        size_t  height                  = (size_t)extent.size.height;
        size_t  bitsPerComponent        = 8;
        size_t  bytesPerPixel           = 4;
        size_t  bytesPerRow             = bytesPerPixel * width;
        size_t  dataSize                = height * bytesPerRow;
        CGColorSpaceRef colorSpace      = [self getScreenColorSpace];
        CGBitmapInfo    bitmapInfo      = kCGImageAlphaNoneSkipLast | kCGBitmapByteOrder32Host;
        uint8_t* data                   = malloc(dataSize);

        CGContextRef cgContext
        = CGBitmapContextCreate
                (  data, width, height
                 , bitsPerComponent
                 , bytesPerRow
                 , colorSpace, bitmapInfo);


        // Create the CIContext which will render the compressed image data into the bitmap.
        NSDictionary* ciContextOptions
         = [[NSDictionary alloc] initWithObjectsAndKeys:
                (id)colorSpace, kCIContextWorkingColorSpace,
                (id)colorSpace, kCIContextOutputColorSpace,
                nil];

        CIContext* ciContext
         = [CIContext
                contextWithCGContext: cgContext
                options: ciContextOptions];

        // Tell the context to draw its image.
        CGPoint point   = {0, 0};
        CGSize  size    = {width, height};
        CGRect  rect    = {point, size};
        [ciContext drawImage: ciImage atPoint: point fromRect: rect];


        // At this point we've got the pixel data sitting in the buffer.
        // Now we can fool around with it directly.
        processImage
                ((HsInt)mode, (HsBool)iBlurEnable, (HsBool)iInvertOutput
                 , thresholdLow, thresholdHigh, width, height, data);

        // Make an image from the pixel data in the context.
        CGImageRef cgImage      = CGBitmapContextCreateImage(cgContext);

        // Make the NSImage out of the CGImage
        // We need a NSImage to display in the window.
        NSSize  nsSize          = { extent.size.width, extent.size.height };
//      NSSize  nsSize          = { 512, 512 };
        NSImage* image          = [[NSImage alloc] initWithCGImage: cgImage size: nsSize];

        // TODO: release the cgImage when we're done with it.

        // Cleanup
        CGImageRelease      (cgImage);
        CGContextRelease    (cgContext);
        CGColorSpaceRelease (colorSpace);
        free(data);
        CVBufferRelease     (currentBuffer);

        return image;
}


- (CGColorSpaceRef)getScreenColorSpace
{
        CMProfileRef systemProfile = NULL;
        OSStatus status = CMGetSystemProfile(&systemProfile);
        NSParameterAssert( noErr == status);
        CGColorSpaceRef colorSpace = CGColorSpaceCreateWithPlatformColorSpace(systemProfile);
        CMCloseProfile(systemProfile);
        return colorSpace;
}


@end
