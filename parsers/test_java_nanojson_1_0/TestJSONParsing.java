import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

import java.io.File;
import java.util.Map;

import com.grack.nanojson.JsonParser;

public class TestJSONParsing {

    public static boolean isValidJSON(byte[] b) {
        try {
            Object o = JsonParser.any().from(new ByteArrayInputStream(b));
            if (new String(b).trim().equals("null"))
                return o == null;
            return o != null;
        } catch (Exception e) {
            System.out.println(e);
            return false;
        }
    }

    public static void main(String[] args) {

        if (args.length == 0) {
            System.out.println("Usage: java TestJSONParsing file.json");
            System.exit(2);
        }

        try {
            byte[] b = Files.readAllBytes(Paths.get(args[0]));
            if(isValidJSON(b)) {
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
