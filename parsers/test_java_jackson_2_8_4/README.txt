javac -cp ".:jackson-core-2.8.4.jar:jackson-databind-2.3.1.jar:jackson-annotations-2.2.3.jar" TestJSONParsing.java

jar cvfm TestJSONParsing.jar META-INF/MANIFEST.MF jackson-core-2.8.4.jar jackson-databind-2.3.1.jar jackson-annotations-2.2.3.jar TestJSONParsing.class

java -jar TestJSONParsing.jar
