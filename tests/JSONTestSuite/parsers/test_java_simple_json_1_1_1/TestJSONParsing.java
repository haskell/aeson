// javac -cp ".:json-simple-1.1.1.jar" TestJSONParsing.java && java -classpath ".:json-simple-1.1.1.jar" TestJSONParsing x.json

import org.json.simple.parser.ParseException;
import org.json.simple.parser.JSONParser;
import java.nio.charset.StandardCharsets;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

public class TestJSONParsing {
    
    public static boolean isValidJSON(String s) {
        try {
            JSONParser parser = new JSONParser();
            Object obj = parser.parse(s);
        } catch (ParseException pe) {
            System.out.println(pe);
            return false;
        }
        return true;
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
