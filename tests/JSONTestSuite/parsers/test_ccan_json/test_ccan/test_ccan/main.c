//
//  main.c
//  test_ccan
//
//  Created by nst on 27/08/16.
//  Copyright © 2016 Nicolas Seriot. All rights reserved.
//

#include <stdio.h>
#include <stdlib.h>
#include "json.h"

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
    
    bool isValid = json_validate(data);

    free(data);
    
    return isValid ? PASS : FAIL;
}

int main(int argc, const char * argv[]) {

    const char* path = argv[1];
    
    int result = testFile(path);
    
    if (result == PASS) {
        return 0;
    } else {
        return 1;
    }
}
