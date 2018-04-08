use strict;
use warnings FATAL => 'all';

package MarpaX::ESLIF::XML::Parser;
use Carp qw/croak/;
use Data::Section -setup;
use I18N::Charset qw/iana_charset_name/;
use Log::Any qw/$log/, filter => \&_log_filter;
use MarpaX::ESLIF;
use MarpaX::ESLIF::XML::Parser::Recognizer::Interface;
use MarpaX::ESLIF::XML::Parser::Value::Interface::BOM;
use MarpaX::ESLIF::XML::Parser::Value::Interface::Guess;
use MarpaX::ESLIF::XML::Parser::Value::Interface::XmlDecl;
use MarpaX::ESLIF::XML::Parser::Grammar::XML10;

my $ESLIF = MarpaX::ESLIF->new($log);
my $xml10_document_grammar;
my $xml10_element_grammar;
my $xml10_extParsedEnt_grammar;
my $xml10_element_start_event;
my $xml10_element_end_event;
my $xml10_element_value_symbol;
my $xml10_element_start_symbols;
my $xml10_element_end_symbols;

use Class::Tiny qw/reader/,
{
    #
    # Lazy class attributes
    #
    xml10_document_grammar      => sub { return $xml10_document_grammar      //= MarpaX::ESLIF::Grammar->new($ESLIF, MarpaX::ESLIF::XML::Parser::Grammar::XML10->document_bnf) },
    xml10_element_grammar       => sub { return $xml10_element_grammar       //= MarpaX::ESLIF::Grammar->new($ESLIF, MarpaX::ESLIF::XML::Parser::Grammar::XML10->element_bnf) },
    xml10_extParsedEnt_grammar  => sub { return $xml10_extParsedEnt_grammar  //= MarpaX::ESLIF::Grammar->new($ESLIF, MarpaX::ESLIF::XML::Parser::Grammar::XML10->extParsedEnt_bnf) },
    xml10_element_start_event   => sub { return $xml10_element_start_event   //= MarpaX::ESLIF::XML::Parser::Grammar::XML10->element_start_event },
    xml10_element_end_event     => sub { return $xml10_element_end_event     //= MarpaX::ESLIF::XML::Parser::Grammar::XML10->element_end_event },
    xml10_element_value_symbol  => sub { return $xml10_element_value_symbol  //= MarpaX::ESLIF::XML::Parser::Grammar::XML10->element_value_symbol },
    xml10_element_start_symbols => sub { return $xml10_element_start_symbols //= MarpaX::ESLIF::XML::Parser::Grammar::XML10->element_start_symbols },
    xml10_element_end_symbols   => sub { return $xml10_element_end_symbols   //= MarpaX::ESLIF::XML::Parser::Grammar::XML10->element_end_symbols }
};

my $BOM_GRAMMAR     = MarpaX::ESLIF::Grammar->new($ESLIF, ${__PACKAGE__->section_data('BOM')});
my $GUESS_GRAMMAR   = MarpaX::ESLIF::Grammar->new($ESLIF, ${__PACKAGE__->section_data('Guess')});
my $XMLDECL_GRAMMAR = MarpaX::ESLIF::Grammar->new($ESLIF, ${__PACKAGE__->section_data('XmlDecl')});

# =============================================================================
# _log_filter
#
# Log filtering
# =============================================================================
sub _log_filter {
    my ($category, $level, $msg) = @_;

    return if $MarpaX::ESLIF::XML::Silent;
    return $msg
}

# =============================================================================
# _iana_charset
#
# Returns a hopefully IANA compatible charset name
# =============================================================================
sub _iana_charset {
    my ($self, $origcharset) = @_;

    my $charset;
    if (defined($origcharset)) {
        if (lc($origcharset) eq 'unicode') {
            #
            # Common pitfall
            #
            $charset = 'UTF-16'
        } else {
            #
            # This should never fail.
            #
            $charset = uc(iana_charset_name($origcharset)) || croak "Failed to get charset name from $origcharset";
            #
            # We always use the uppercased version so that _merge_charsets()
            # does not have to take care of case sensitivity
            #
            $charset = uc($charset)
        }
    }

    return $charset
}

# =============================================================================
# _analyse_bom
#
# Get charset from BOM, eventually
# =============================================================================
sub _analyse_bom {
    my ($self, $recognizerInterface) = @_;

    local $MarpaX::ESLIF::XML::Silent = 1;
    my $valueInterface = MarpaX::ESLIF::XML::Parser::Value::Interface::BOM->new();

    my ($charset, $bytes);
    if ($BOM_GRAMMAR->parse($recognizerInterface, $valueInterface)) {
        #
        # Values are already IANA compatible and uppercased
        #
        ($charset, $bytes) = @{$valueInterface->getResult};
        #
        # Get data that has to be reinjected
        #
        my $bookkeeping = $recognizerInterface->bookkeeping();
        #
        # ... Minus the number of bytes used by the BOM: they are formally NOT
        # part of the XML grammar
        #
        substr($bookkeeping, 0, $bytes, '');
        $recognizerInterface->bookkeeping($bookkeeping)
    }

    return $charset
}

# =============================================================================
# _analyse_guess
#
# Get charset from first bytes, eventually
# =============================================================================
sub _analyse_guess {
    my ($self, $recognizerInterface) = @_;

    local $MarpaX::ESLIF::XML::Silent = 1;
    my $valueInterface = MarpaX::ESLIF::XML::Parser::Value::Interface::Guess->new();

    my $charset;
    if ($GUESS_GRAMMAR->parse($recognizerInterface, $valueInterface)) {
        #
        # Values are already IANA compatible and uppercased
        #
        $charset = $valueInterface->getResult
    }

    return $charset
}

# =============================================================================
# _analyse_xmldecl
#
# Get charset from XMLDecl, eventually
# =============================================================================
sub _analyse_xmldecl {
    my ($self, $recognizerInterface) = @_;

    local $MarpaX::ESLIF::XML::Silent = 1;
    my $valueInterface = MarpaX::ESLIF::XML::Parser::Value::Interface::XmlDecl->new();

    my ($charset, $version, $standalone);
    if ($XMLDECL_GRAMMAR->parse($recognizerInterface, $valueInterface)) {
        #
        # In case this is an alias, uppercased IANA version is preferred
        #
        ($charset, $version, $standalone) = @{$valueInterface->getResult}
    }

    return ($self->_iana_charset($charset), $version, $standalone)
}

# =============================================================================
# This is the "Raw XML charset encoding detection" as per rometools,
# extended to UTF-32.
# =============================================================================
#
# C.f. https://rometools.github.io/rome/RssAndAtOMUtilitiEsROMEV0.5AndAboveTutorialsAndArticles/XMLCharsetEncodingDetectionHowRssAndAtOMUtilitiEsROMEHelpsGettingTheRightCharsetEncoding.html
#
# I disagree with them nevertheless in the following case:
#
# if BOMEnc is NULL
#   if XMLGuessEnc is NULL or XMLEnc is NULL
#     encoding is 'UTF-8'                                                 [1.0]
#   endif
# endif
#
#
# Because if XMLEnc is set, it should be used, then XMLGuessEnc, then 'UTF-8.
# ======================================================================================
sub _merge_charsets {
    my ($self, $charset_from_bom, $charset_from_guess, $charset_from_xmldecl) = @_;

    my $charset;
    if (! defined($charset_from_bom)) {
        if (! defined($charset_from_guess) || ! defined($charset_from_xmldecl)) {
            $charset = $charset_from_xmldecl // $charset_from_guess // 'UTF-8'
        } else {
            if (($charset_from_xmldecl eq 'UTF-16') && ($charset_from_guess eq 'UTF-16BE' || $charset_from_guess eq 'UTF-16LE')) {
                $charset =  $charset_from_guess
            } elsif (($charset_from_xmldecl eq 'UTF-32') && ($charset_from_guess eq 'UTF-32BE' || $charset_from_guess eq 'UTF-32LE')) {
                $charset =  $charset_from_guess
            } else {
                $charset = $charset_from_xmldecl
            }
        }
    } else {
        if ($charset_from_bom eq 'UTF-8') {
            if (defined($charset_from_guess) && $charset_from_guess ne 'UTF-8') {
                croak "Encoding mismatch between BOM $charset_from_bom and guess $charset_from_guess"
            }
            if (defined($charset_from_xmldecl) && $charset_from_xmldecl ne 'UTF-8') {
                croak "Encoding mismatch between BOM $charset_from_bom and declaration $charset_from_xmldecl"
            }
            $charset = 'UTF-8'
        } else {
            if ($charset_from_bom eq 'UTF-16BE' or $charset_from_bom eq 'UTF-16LE') {
                if (defined($charset_from_guess) && $charset_from_guess ne $charset_from_bom) {
                    croak "Encoding mismatch between BOM $charset_from_bom and guess $charset_from_guess"
                }
                if (defined($charset_from_xmldecl) && ($charset_from_xmldecl ne 'UTF-16' and $charset_from_xmldecl ne $charset_from_bom)) {
                    croak "Encoding mismatch between BOM $charset_from_bom and declaration $charset_from_xmldecl"
                }
                $charset = $charset_from_bom
            } elsif ($charset_from_bom eq 'UTF-32BE' or $charset_from_bom eq 'UTF-32LE') {
                if (defined($charset_from_guess) && $charset_from_guess ne $charset_from_bom) {
                    croak "Encoding mismatch between BOM $charset_from_bom and guess $charset_from_guess"
                }
                if (defined($charset_from_xmldecl) && ($charset_from_xmldecl ne 'UTF-32' and $charset_from_xmldecl ne $charset_from_bom)) {
                    croak "Encoding mismatch between BOM $charset_from_bom and declaration $charset_from_xmldecl"
                }
                $charset = $charset_from_bom
            } else {
                croak 'Encoding setup failed'
            }
        }
    }

    return $charset
}

sub parse {
    my ($self) = @_;

    # ------------------
    # Charset from BOM ?
    # ------------------
    my $recognizerInterface = MarpaX::ESLIF::XML::Parser::Recognizer::Interface->new(
        isCharacterStream => 0,
        isWithExhaustion  => 1,
        reader            => $self->reader,
        remember          => 1
        );
    my $charset_from_bom = $self->_analyse_bom($recognizerInterface);
    $log->tracef("Charset from BOM: %s, bookkeeping: %d bytes", $charset_from_bom, bytes::length($recognizerInterface->bookkeeping));

    # --------------------
    # Charset from guess ?
    # --------------------
    $recognizerInterface = MarpaX::ESLIF::XML::Parser::Recognizer::Interface->new(
        isWithExhaustion => 1,
        reader           => $self->reader,
        remember         => 1,
        initial_data     => $recognizerInterface->bookkeeping,
        initial_eof      => $recognizerInterface->isEof
        );
    my $charset_from_guess = $self->_analyse_guess($recognizerInterface);
    $log->tracef("Charset from guess: %s, bookkeeping: %d bytes", $charset_from_guess, bytes::length($recognizerInterface->bookkeeping));

    # --------------------------
    # Charset from declaration ?
    # --------------------------
    $recognizerInterface = MarpaX::ESLIF::XML::Parser::Recognizer::Interface->new(
        isWithExhaustion => 1,
        reader           => $self->reader,
        remember         => 1,
        initial_data     => $recognizerInterface->bookkeeping,
        initial_eof      => $recognizerInterface->isEof);
    my ($charset_from_xmldecl, $version_from_xmldecl, $standalone_from_xmldecl) = $self->_analyse_xmldecl($recognizerInterface);
    $log->tracef("Charset/version/standalone from declaration: %s/%s/%s, bookkeeping: %d bytes", $charset_from_xmldecl, $version_from_xmldecl, $standalone_from_xmldecl, bytes::length($recognizerInterface->bookkeeping));

    # --------------
    # Merge charsets
    # --------------
    my $charset = $self->_merge_charsets(
        $charset_from_bom,
        $charset_from_guess,
        $charset_from_xmldecl);   
    $log->tracef("Charset: %s", $charset);

    # -----------------------------
    # Decide the XML version to use
    # -----------------------------
    my $version = $version_from_xmldecl // '1.0';

    # --------------------------------------------
    # Prepare all we need from this implementation
    # --------------------------------------------
    my %impl;
    if ($version eq '1.0') {
        $impl{document_grammar}      = $self->xml10_document_grammar;
        $impl{element_grammar}       = $self->xml10_element_grammar;
        $impl{extParsedEnt_grammar}  = $self->xml10_extParsedEnt_grammar;
        $impl{element_start_event}   = $self->xml10_element_start_event;
        $impl{element_end_event}     = $self->xml10_element_end_event;
        $impl{element_value_symbol}  = $self->xml10_element_value_symbol;
        $impl{element_start_symbols} = $self->xml10_element_start_symbols;
        $impl{element_end_symbols}   = $self->xml10_element_end_symbols;
    } else {
        croak 'MarpaX::ESLIF::XML::Parser::Grammar::XML11 not yet implemented'
    }

    # --------
    # Scan XML
    # --------
    $recognizerInterface = MarpaX::ESLIF::XML::Parser::Recognizer::Interface->new(
        reader       => $self->reader,
        initial_data => $recognizerInterface->bookkeeping,
        initial_eof  => $recognizerInterface->isEof,
        encoding     => $charset
        );

    my $eslifRecognizer = MarpaX::ESLIF::Recognizer->new(
        $impl{document_grammar},
        $recognizerInterface
        );
    #
    # At the very beginning ELEMENT_END events must never occur
    #
    map { $eslifRecognizer->eventOnOff($_, [ MarpaX::ESLIF::Event::Type->MARPAESLIF_EVENTTYPE_BEFORE ], 0) } @{$impl{element_end_symbols}};

    return $self->_parse($eslifRecognizer, \%impl, 0) # level 0
}

# ======================================================================================
# _parse
#
# XML parse implementation that recurses on element
# ======================================================================================
sub _parse {
    no warnings 'recursion';
    my ($self, $currentRecognizer, $impl, $level) = @_;

    return 0 unless $currentRecognizer->scan();
    return 0 unless $self->_manage_events($currentRecognizer, $impl, $level);
    if ($currentRecognizer->isCanContinue) {
        do {
            return 0 unless $currentRecognizer->resume;
            my $rcb = $self->_manage_events($currentRecognizer, $impl, $level);
            return 0 unless $rcb;
            return 1 if ($rcb < 0)
        } while ($currentRecognizer->isCanContinue)
    }

    return 1
}

# ======================================================================================
# _manage_events
#
# XML events manager. Will pile up as many recognizers as there are composite elements
# to get correct parsing without exhausting memory. This is also where the support of
# SAX events is happening.
# ======================================================================================
sub _manage_events {
    my ($self, $currentRecognizer, $impl, $level) = @_;

    foreach (@{$currentRecognizer->events()}) {
        
        $log->tracef('Event %s', $_);

        my $event = $_->{event};
        if ($event eq $impl->{element_start_event}) {
            #
            # Get symbol and data
            #
            my $symbol = $_->{symbol};
            my $data = $currentRecognizer->lexemeLastPause($_->{symbol});
            #
            # Create an element recognizer
            #
            my $elementRecognizer = $currentRecognizer->newFrom($impl->{element_grammar});
            #
            # Enable element end "before" events
            #
            map { $elementRecognizer->eventOnOff($_, [ MarpaX::ESLIF::Event::Type->MARPAESLIF_EVENTTYPE_BEFORE ], 1) } @{$impl->{element_end_symbols}};
            #
            # Push the lexeme
            #
            return 0 unless $elementRecognizer->lexemeRead($symbol, $data, bytes::length($data));
            #
            # Call for the element parsing
            #
            return 0 unless $self->_parse($elementRecognizer, $impl, $level + 1);
            #
            # Push the ELEMENT_VALUE
            #
            return $currentRecognizer->lexemeRead($impl->{element_value_symbol}, undef, 0)
        } elsif ($event eq $impl->{element_end_event}) {
            #
            # Get symbol and data
            #
            my $symbol = $_->{symbol};
            my $data = $currentRecognizer->lexemeLastPause($_->{symbol});
            #
            # Allow exhaustion
            #
            $currentRecognizer->set_exhausted_flag(1);
            #
            # Push the lexeme
            #
            return $currentRecognizer->lexemeRead($symbol, undef, bytes::length($data))
        } else {
            #
            # Should never happen
            #
            return 0
        }
    }

    return 1
}

1;

__DATA__
__[ BOM ]__
#################################################################
# Grammar for BOM detection
#################################################################
#
# Unusual ordering is not considered
#
BOM ::= [\x{00}] [\x{00}] [\x{FE}] [\x{FF}] action => UTF_32BE
      | [\x{FF}] [\x{FE}] [\x{00}] [\x{00}] action => UTF_32LE
      | [\x{FE}] [\x{FF}]                   action => UTF_16BE
      | [\x{FF}] [\x{FE}]                   action => UTF_16LE
      | [\x{EF}] [\x{BB}] [\x{BF}]          action => UTF_8

__[ Guess ]__
#################################################################
# Grammar for encoding guess
#################################################################
#
# Unusual ordering is not considered nor EBCDIC with code page
# Guess encoding should not return UTF-8, there are two many
# encodings that looks like UTF-8 and will fail later
#
Guess ::= [\x{00}] [\x{00}] [\x{00}] [\x{3C}] action => UTF_32BE # '<'
        | [\x{3C}] [\x{00}] [\x{00}] [\x{00}] action => UTF_32LE # '<'
        | [\x{00}] [\x{3C}] [\x{00}] [\x{3F}] action => UTF_16BE # '<?'
        | [\x{3C}] [\x{00}] [\x{3F}] [\x{00}] action => UTF_16LE # '<?'

__[ XmlDecl ]__
#################################################################
# Grammar for XML declaration
#################################################################
#
# XmlDecl grammar is the same between XML 1.0 and XML 1.1
#
XMLDecl               ::= '<?' 'xml' VersionInfo <EncodingDecl maybe> <SDDecl maybe> <S maybe> '?>' action => XmlDecl
VersionInfo           ::= S 'version' Eq "'" VersionNum "'"                                         action => ::copy[4]
                        | S 'version' Eq '"' VersionNum '"'                                         action => ::copy[4]
VersionNum            ::= '1.' <digit many>
<digit many>          ::= <digit>+
<digit>               ::= [0-9]
EncodingDecl          ::= S 'encoding' Eq '"' EncName '"'                                           action => ::copy[4]
                        | S 'encoding' Eq "'" EncName "'"                                           action => ::copy[4]
EncName               ::= <EncName header> <EncName trailer any>
<EncName header>      ::= [A-Za-z]
<EncName trailer>     ::= [A-Za-z0-9._-]
<EncName trailer any> ::= <EncName trailer>*
SDDecl                ::= S 'standalone' Eq "'" <yes or no> "'"                                     action => ::copy[4]
                        | S 'standalone' Eq '"' <yes or no> '"'                                     action => ::copy[4]
<yes or no>           ::= 'yes'
                        | 'no'
S1                    ::= [\x{20}\x{9}\x{D}\x{A}]
S                     ::= S1+
Eq                    ::= <S maybe> '=' <S maybe>

<S maybe>             ::= S
<S maybe>             ::=
<EncodingDecl maybe>  ::= EncodingDecl
<EncodingDecl maybe>  ::=
<SDDecl maybe>        ::= SDDecl
<SDDecl maybe>        ::=
