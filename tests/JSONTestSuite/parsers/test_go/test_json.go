// Go offers built-in support for JSON encoding and
// decoding, including to and from built-in and custom
// data types.

package main

import "encoding/json"
import "io/ioutil"
import "os"

//import "fmt"

func main() {
    
    b, file_err := ioutil.ReadFile(os.Args[1])
    if file_err != nil {
        os.Exit(-1)
    }
    
    var f interface{}
    err := json.Unmarshal(b, &f)
    
    //fmt.Println(f)
    
    if err != nil {
        os.Exit(1)
    }

    os.Exit(0)
}
