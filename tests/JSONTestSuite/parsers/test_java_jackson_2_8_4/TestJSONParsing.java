import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

import java.io.File;
import java.util.Map;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.annotation.JsonAutoDetect;

public class TestJSONParsing {
    
    public static boolean isValidJSON(String s) {
        try {        
            JsonFactory factory = new JsonFactory();
            ObjectMapper mapper = new ObjectMapper(factory);
            JsonNode rootNode = mapper.readTree(s);  
            return rootNode != null;
        } catch (Exception e) {
            System.out.println(e);
            return false;
        }
    }

    public static void main(String[] args) {
        
        if(args.length == 0) {
            System.out.println("Usage: java TestJSONParsing file.json");            
            System.exit(2);
        }
        
        try {
            String s = new String(Files.readAllBytes(Paths.get(args[0])));
            if(isValidJSON(s)) {
                System.out.println("valid");
                System.exit(0);            
            }
            System.out.println("invalid");
            System.exit(1);
        } catch (IOException e) {
            System.out.println("not found");
            System.exit(2);
        }   
    }
}
