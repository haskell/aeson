// javac -cp ".:gson-2.7.jar" TestJSONParsing.java && java -classpath ".:gson-2.7.jar" TestJSONParsing x.json

import java.nio.charset.StandardCharsets;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonParseException;

public class TestJSONParsing {
    
    public static boolean isValidJSON(String s) {
        try {
       
            //GsonBuilder builder = new GsonBuilder();
            //Object obj = builder.create().fromJson(s, Object.class);
        
            Gson gson = new Gson();
            Object obj = gson.fromJson(s, Object.class);
            System.out.println(obj);
            System.out.println(obj.getClass().getSimpleName());
            return true;
        } catch (JsonParseException e) {
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
