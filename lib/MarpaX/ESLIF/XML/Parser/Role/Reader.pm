use strict;
use warnings FATAL => 'all';

package MarpaX::ESLIF::XML::Parser::Role::Reader;
use Role::Tiny;

requires qw/read eof data/;

1;
