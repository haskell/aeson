javac -cp ".:com.leastfixedpoint.json-1.0.jar" TestJSONParsing.java

jar cvfm TestJSONParsing.jar META-INF/MANIFEST.MF com.leastfixedpoint.json-1.0.jar TestJSONParsing.class

java -jar TestJSONParsing.jar
