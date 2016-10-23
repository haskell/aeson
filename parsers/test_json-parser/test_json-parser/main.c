//
//  main.c
//  test_json_parser
//
//  Created by nst on 13/09/16.
//  Copyright © 2016 Nicolas Seriot. All rights reserved.
//

#include <stdlib.h>
#include <stdio.h>
#include "json.h"

#include <sys/stat.h>
#include <dirent.h>
#include <string.h>

typedef enum testStatus {ERROR, PASS, FAIL} TestStatus;

TestStatus testFile(const char *filename) {
    FILE *f=fopen(filename,"rb");
    if(f == NULL) { return ERROR; };
    fseek(f,0,SEEK_END);
    long len=ftell(f);
    fseek(f,0,SEEK_SET);
    char *data=(char*)malloc(len+1);
    fread(data,1,len,f);
    data[len]='\0';
    fclose(f);
    
    json_char* json = (json_char*)data;
    
    json_value* value = json_parse(json,len);
    
    if (value == NULL) {
        fprintf(stderr, "Unable to parse data\n");
        free(data);
        return FAIL;
    }
    
    json_value_free(value);
    
    return PASS;
}

int main(int argc, char* argv[]) {
    
    if (argc != 2) {
        printf("Usage: %s test.json\n", argv[0]);
        return 1;
    }
    
    const char* path = argv[1];
    
    int result = testFile(path);
    
    if (result == PASS) {
        //        printf("-- PASS\n");
        return 0;
    } else if (result == FAIL) {
        //        printf("-- FAIL\n");
        return 1;
    } else if (result == ERROR) {
        //        printf("-- ERROR\n");
        return 1;
    }
}
