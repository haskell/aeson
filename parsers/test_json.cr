require "json"

file = ARGV[0]

begin
    puts(file)
    json = File.read(file)
    result = JSON.parse_raw(json)
    p result
    
    if result.nil?
        exit 1
    else
        exit 0
    end
rescue ex : JSON::Error
    puts(ex)
    exit 1
end
