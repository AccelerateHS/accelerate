
#import <Cocoa/Cocoa.h>

#include <stdio.h>
#include "Process_stub.h"
#include "Rts.h"

//const RtsOptsEnabledEnum rtsOptsEnabled = RtsOptsAll;
//char* ghc_rts_opts                      = "-N2 ";

int main(int argc, char *argv[])
{
        // Start up the Haskell runtime system.
        RtsConfig conf          = defaultRtsConfig;
        conf.rts_opts_enabled   = RtsOptsAll;
        hs_init_ghc(&argc, &argv, conf);

        // Run the Cocoa application.
        int code        = NSApplicationMain(argc,  (const char **) argv);

        hs_exit();
        return code;
}
