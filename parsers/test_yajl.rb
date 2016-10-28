#!/usr/bin/env ruby

require 'yajl'

f = ARGV[0]

o = nil

begin
    puts(f)
    json = File.read(f)
    parser = Yajl::Parser.new
    o = parser.parse( json )
    p o
    
    if o == nil
        exit 1
    else
        exit 0
    end
rescue JSON::ParserError => e
    puts(e)
    exit 1
end
