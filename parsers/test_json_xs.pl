#!/usr/bin/perl

# cpan install Test::JSON

use JSON::XS;
use warnings;

my $path = $ARGV[0];

open( my $fh, '<', $path ) or die "Can't open $path: $!";

my $output = 0;

my $json = JSON::XS->new->utf8->allow_nonref; # RFC 7159 // TODO: try with utf8 enabled, means you get an UTF-8 encoded octet/binary

while ( my $data = <$fh> ) {
    eval {
        $output = $json->decode ($data);
    };
}

close $fh;

if ($output != 0) {
    exit 0;
}

exit 1;
