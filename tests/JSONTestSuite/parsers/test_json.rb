#!/usr/bin/ruby

require 'json'

f = ARGV[0]

o = nil

begin
    puts(f)
    json = File.read(f)
    o = JSON.parse(json, {:quirks_mode => true})
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
