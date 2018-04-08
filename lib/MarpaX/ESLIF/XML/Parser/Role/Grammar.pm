use strict;
use warnings FATAL => 'all';

package MarpaX::ESLIF::XML::Parser::Role::Grammar;
use Role::Tiny;

requires qw/document_bnf element_bnf extParsedEnt_bnf
            element_start_event element_end_event
            element_value_symbol
            element_start_symbols element_end_symbols/;

1;
