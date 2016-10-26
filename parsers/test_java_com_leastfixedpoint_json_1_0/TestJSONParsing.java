import java.nio.charset.StandardCharsets;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

import com.leastfixedpoint.json.*;

public class TestJSONParsing {
    public static boolean isValidJSON(String s) {
        try {
            Object obj = JSONReader.readFrom(s);
            System.out.println(obj);
            System.out.println(obj.getClass().getSimpleName());
            return true;
        } catch (IOException e) {
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
