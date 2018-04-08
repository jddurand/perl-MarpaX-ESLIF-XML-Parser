use strict;
use warnings FATAL => 'all';

package MarpaX::ESLIF::XML::Parser::Role::Grammar;
use Role::Tiny;

requires qw/document_bnf element_bnf extParsedEnt_bnf/;

1;
