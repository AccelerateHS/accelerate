//
//  beholderAppDelegate.h
//  beholder
//
//  Created by Ben Lippmeier on 28/01/11.
//  Copyright 2011 Department of Computer Science, UNSW. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import "Camera.h"
#import "Framer.h"
#import "MainView.h"

@interface beholderAppDelegate : NSObject <NSApplicationDelegate> 
{
    NSWindow*	window;
}

@property (assign) IBOutlet NSWindow *window;

@end
