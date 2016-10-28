#!/usr/bin/env ruby

require 'oj'

f = ARGV[0]

o = nil

Oj.default_options = { :mode => :compat }

begin
    puts(f)
    json = File.read(f)
    o = Oj.load( json )
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
