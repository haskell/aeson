//
//  main.m
//  test-JSONKit
//
//  Created by nst on 08/08/16.
//  Copyright © 2016 Nicolas Seriot. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "JSONKit.h" // https://github.com/johnezang/JSONKit

#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <dirent.h>
#include <stdio.h>
#include <string.h>

typedef enum testStatus {ERROR, PASS, FAIL} TestStatus;

TestStatus test_file(char* filename, int printParsingResults) {

    NSString *path = [NSString stringWithCString:filename encoding:NSUTF8StringEncoding];
    
    //NSLog(@"--- %@", path);
    
//    if([path hasSuffix:@"n_structure_100000_opening_arrays.json"]) return ERROR;
//    if([path hasSuffix:@"n_structure_empty.json"]) return ERROR;
    
    NSData *data = [NSData dataWithContentsOfFile:path];
    
    JSONDecoder *decoder = [JSONDecoder decoderWithParseOptions:JKParseOptionNone];

    NSError *error = nil;
    id o = [decoder objectWithData:data error:&error];
    
    if (printParsingResults) {
        NSLog(@"--------------------------------\n");
        NSLog(@"-- file: %@\n", path);
        NSLog(@"-- contents: %@\n", data);
        NSLog(@"-- parsed: %d\n", o != NULL);
        if(o != NULL) {
            NSLog(@"-- o: %@", o);
        }
    }
    
    //    if (value == NULL) {
    //        fprintf(stderr, "Unable to parse data\n");
    //        free(file_contents);
    //        return 1;
    //    }
    
    return o != nil ? PASS : FAIL;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
     
        const char* path = argv[1];
        
        int printParsingResults = 0;
        
        int result = test_file(path, printParsingResults);
        
        if (result == PASS) {
            return 0;
        } else {
            return 1;
        }

    }
    return 0;
}
