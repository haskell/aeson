//
//  main.m
//  test_TouchJSON
//
//  Created by nst on 12.10.16.
//  Copyright © 2016 Nicolas Seriot. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "CJSONDeserializer.h"

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *path = [NSString stringWithCString:argv[1] encoding:NSUTF8StringEncoding];
        NSData *data = [NSData dataWithContentsOfFile:path];
        CJSONDeserializer *theDeserializer = [CJSONDeserializer deserializer];
        theDeserializer.options |= kJSONDeserializationOptions_AllowFragments;
        NSError *theError = NULL;
        id theDeseralizedValue = [theDeserializer deserialize:data error:&theError];
        //NSLog(@"%@ %@", theDeseralizedValue, theError);
        if(theDeseralizedValue != nil) exit(0);
        exit(1);
    }
    return 0;
}
