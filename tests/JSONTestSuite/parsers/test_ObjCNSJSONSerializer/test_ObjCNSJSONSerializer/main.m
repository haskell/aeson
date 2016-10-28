//
//  main.m
//  test_ObjCNSJSONSerializer
//
//  Created by Nicolas Seriot on 02/09/16.
//  Copyright © 2016 Swissquote. All rights reserved.
//

#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {

        if ([[NSProcessInfo processInfo].arguments count] != 2) {
            NSLog(@"Usage: ./%@ file.json", [NSProcessInfo processInfo].arguments[0]);
            return 1;
        }

        NSString *path = [NSProcessInfo processInfo].arguments[1];
        NSURL *url = [NSURL fileURLWithPath:path];
        NSData *data = [NSData dataWithContentsOfURL:url];
        
        id o = [NSJSONSerialization JSONObjectWithData:data options:NSJSONReadingAllowFragments error:nil];
        
        if (o == nil) {
            return 1;
        } else {
            NSLog(@"-- %@", o);
        }
    }
    return 0;
}
