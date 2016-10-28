//
//  main.c
//  test_jsmn
//
//  Created by nst on 30/08/16.
//  Copyright © 2016 Nicolas Seriot. All rights reserved.
//

#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <dirent.h>
#include <stdio.h>
#include <string.h>
#include "jsmn.h"

#define JSMN_STRICT 1

int testFile(const char *filename) {
    
    FILE *f=fopen(filename,"rb");
    if(f == NULL) { return -1; };
    fseek(f,0,SEEK_END);
    long len=ftell(f);
    fseek(f,0,SEEK_SET);
    char *data=(char*)malloc(len+1);
    fread(data,1,len,f);
    data[len]='\0';
    fclose(f);

    jsmn_parser p;
    jsmntok_t tokens[128]; // a number >= total number of tokens
    
    jsmn_init(&p);
    int resultCode = jsmn_parse(&p, data, 100, tokens, 50);
    //printf("-- %d\n", resultCode);

    free(data);
    
    return resultCode;
}

int main(int argc, const char * argv[]) {

    int resultCode = testFile(argv[1]);
    
    if (resultCode > 0) return 0;
    if (resultCode <= 0) return 1; // error
    return resultCode;
}
