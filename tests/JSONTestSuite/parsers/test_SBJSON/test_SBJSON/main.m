//
//  main.m
//  test_SBJSON
//
//  Created by nst on 04/09/16.
//  Copyright © 2016 Nicolas Seriot. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "SBJson4.h"

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        
        SBJson4Parser *parser = [SBJson4Parser parserWithBlock:^(id item, BOOL *stop) {
            //NSLog(@"-- %@", item);
            exit(0);
        }
                                                allowMultiRoot:NO
                                               unwrapRootArray:NO
                                                  errorHandler:^(NSError *error) {
                                                      //NSLog(@"%@", error);
                                                      exit(1);
                                                  }];
        
        NSString *path = [NSString stringWithCString:argv[1] encoding:NSUTF8StringEncoding];
        NSData *data = [NSData dataWithContentsOfFile:path];
        SBJson4ParserStatus status = [parser parse:data];
        
        if (status == SBJson4ParserComplete) exit(0);
        if (status == SBJson4ParserStopped) exit(1);
        if (status == SBJson4ParserWaitingForData) exit(1);
        if (status == SBJson4ParserError) exit(1);

        [[NSRunLoop currentRunLoop] run];
    }
    return 0;
}
