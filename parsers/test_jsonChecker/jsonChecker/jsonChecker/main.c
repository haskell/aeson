#include <stdlib.h>
#include <stdio.h>
#include "JSON_checker.h"

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
    
    JSON_checker jc = new_JSON_checker(20); // max depth
    for (int i = 0; i < len; i++) {
        char c = data[i];
        if (JSON_checker_char(jc, c) == 0) {
            return FAIL;
        }
    }
    return JSON_checker_done(jc) ? PASS : FAIL;
}

int main(int argc, char* argv[]) {
    
    if (argc != 2) {
        printf("Usage: %s test.json\n", argv[0]);
        return 1;
    }
    
    const char* path = argv[1];
    
    int result = testFile(path);
    
    if (result == PASS) {
        printf("-- PASS\n");
        return 0;
    } else if (result == FAIL) {
        printf("-- FAIL\n");
        return 1;
    } else if (result == ERROR) {
        printf("-- ERROR\n");
        return 1;
    }
}
