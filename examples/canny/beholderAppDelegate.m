
#import "beholderAppDelegate.h"

@implementation beholderAppDelegate

@synthesize window;

- (void)applicationDidFinishLaunching:(NSNotification *)aNotification 
{
	NSLog(@"Starting up");

	// Create the camera.
	Camera* camera	= [[Camera alloc] init];
	
	// Create the frame.
	Framer*	framer	= [[Framer alloc] initWithCamera: camera];
	
	[camera setNotify:globalMainView];
	
	// Initialise the capture viewer in the main window.
	[globalMainView	initCaptureView: camera];
	
	// Give the main window the framer so it can make new images.
	[globalMainView setFramer: framer];
	
	// Tell the camera to start capturing images.
	[camera start];
}

@end
