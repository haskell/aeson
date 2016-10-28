javac -cp ".:gson-2.7.jar" TestJSONParsing.java

jar cvfm TestJSONParsing.jar META-INF/MANIFEST.MF gson-2.7.jar TestJSONParsing.class

java -jar TestJSONParsing.jar
