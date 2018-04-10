use strict;
use warnings FATAL => 'all';

package MarpaX::ESLIF::XML::Parser::Grammar::XML10;
use Class::Tiny qw/valid wellformed/;
use Log::Any qw/$log/;
use Role::Tiny::With;

# ABSTRACT: XML 1.0 grammar role implementation

# VERSION

# AUTHORITY

=head1 DESCRIPTION

XML 1.0 grammar role implementation

  my $document_bnf    = MarpaX::ESLIF::XML::Parser::Grammar::XML10->document_bnf;
  my $element_bnf     = MarpaX::ESLIF::XML::Parser::Grammar::XML10->element_bnf;
  my $extParseEnt_bnf = MarpaX::ESLIF::XML::Parser::Grammar::XML10->extParseEnt_bnf;

=cut

# ----------------------------------------------------------------------------
# Main XML 1.0 grammar
# ----------------------------------------------------------------------------
my $DATA  = do { local $/; <DATA> };

=head1 METHODS

=head2 $class->document_bnf

Returns I<document> grammar. This is a class method.

=cut

my $DOCUMENT_BNF = $DATA . "\n:start ::= document\n";
sub document_bnf {
    return $DOCUMENT_BNF
}

=head2 $class->element_bnf

Returns I<element> bnf. This is a class method.

=cut

my $ELEMENT_BNF = $DATA . "\n:start ::= element\n";
sub element_bnf {
    return $ELEMENT_BNF
}

=head2 $class->extParsedEnt_bnf

Returns I<extParsedEnt> bnf. This is a class method.

=cut

my $EXTPARSEDENT_BNF = $DATA . "\n:start ::= extParsedEnt\n";
sub extParsedEnt_bnf {
    return $EXTPARSEDENT_BNF
}

=head2 $class->element_start_event

Returns element start event name. This is a class method.

=cut

sub element_start_event {
    return '^ELEMENT_START'
}

=head2 $class->element_end_event

Returns element end event name. This is a class method.

=cut

sub element_end_event {
    return '^ELEMENT_END'
}

=head2 $class->element_value_symbol

Returns element value symbol name. This is a class method.

=cut

sub element_value_symbol {
    return 'ELEMENT_VALUE'
}

=head2 $class->element_start_symbols

Returns element start symbol names. This is a class method.

=cut

sub element_start_symbols {
    return [ qw/ELEMENT_START/ ]
}

=head2 $class->element_end_symbols

Returns element start symbol names. This is a class method.

=cut

sub element_end_symbols {
    return [ qw/ELEMENT_END1 ELEMENT_END2/ ]
}

with 'MarpaX::ESLIF::XML::Parser::Role::Grammar';

1;

__DATA__
#
# From https://www.w3.org/TR/REC-xml (5th edition)
#
# Take care, "official" grammar has several ambiguities:
# - https://lists.w3.org/Archives/Public/xml-editor/2011OctDec/0000
# - https://lists.w3.org/Archives/Public/xml-editor/2011OctDec/0001
# - https://lists.w3.org/Archives/Public/xml-editor/2011OctDec/0002
#
# Note:
# Concerning XML Exceptions there are three categories:
# - Comment interior: We create a character class without for Char minus the '-' character
# - PITarget        : Native BNF exception is doing it
# - Others          : They are ALL in the form: <character> - ( <character>* <exception longer than one character> <character>* )
#                     where <exception longer than one character> is always an expected terminal preceeding and/or succeeding <character>* !
#                     So this mean there is NO needed to write exception...: the grammar will natively stop <character>* parsing as soon
#                     as it sees <exception longer than one character> in stream, because it is always working in the LATM (Longest Acceptable
#                     Token Match) mode


document           ::= prolog element <Misc any>

Char               ::= [\x{9}\x{A}\x{D}\x{20}-\x{D7FF}\x{E000}-\x{FFFD}\x{10000}-\x{10FFFF}]:u name => Char

S                  ::= S1+
S1                 ::= [\x{20}\x{9}\x{D}\x{A}]

NameStartChar      ::= [:A-Z_a-z\x{C0}-\x{D6}\x{D8}-\x{F6}\x{F8}-\x{2FF}\x{370}-\x{37D}\x{37F}-\x{1FFF}\x{200C}-\x{200D}\x{2070}-\x{218F}\x{2C00}-\x{2FEF}\x{3001}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFFD}\x{10000}-\x{EFFFF}]:u

NameChar           ::= [:A-Z_a-z\x{C0}-\x{D6}\x{D8}-\x{F6}\x{F8}-\x{2FF}\x{370}-\x{37D}\x{37F}-\x{1FFF}\x{200C}-\x{200D}\x{2070}-\x{218F}\x{2C00}-\x{2FEF}\x{3001}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFFD}\x{10000}-\x{EFFFF}\-.0-9\x{B7}\x{0300}-\x{036F}\x{203F}-\x{2040}]:u

# Name               ::= NameStartChar <NameChar any>
Name               ::= <NAME>

Names              ::= Name+ separator => [\x{20}]

Nmtoken            ::= NameChar+

Nmtokens           ::= Nmtoken+ separator => [\x{20}]

EntityValue        ::= '"' <EntityValue1 any>   '"'
                     | "'" <EntityValue2 any>   "'"

AttValue           ::= '"' <AttValue1 any>      '"'
                     | "'" <AttValue2 any>      "'"

SystemLiteral      ::= '"' <SystemLiteral1 any> '"'
                     | "'" <SystemLiteral2 any> "'"

PubidLiteral       ::= '"' <PubidChar1 any>     '"'
                     | "'" <PubidChar2 any>     "'"

PubidChar          ::= [\x{20}\x{D}\x{A}a-zA-Z0-9\-'()+,./:=?;!*#@$_%]

CharData           ::= <CharData Exceptioned>

Comment            ::= '<!--' <Comment Interior> '-->'

PI                 ::= '<?' PITarget                    '?>'
                     | '<?' PITarget S <PI Exceptioned> '?>'

CDSect             ::= CDStart CData CDEnd

CDStart            ::= '<![CDATA['

CData              ::= <CData Exceptioned>

CDEnd              ::= ']]>'

prolog             ::= <XMLDecl maybe> <Misc any>
                     | <XMLDecl maybe> <Misc any> doctypedecl <Misc any>

#
# Note: it is important to split '<?xml' into '<?' 'xml' because of PI whose defintion is: '<?' PITarget
#
XMLDecl            ::= '<?' 'xml' VersionInfo <EncodingDecl maybe> <SDDecl maybe> <S maybe> '?>' ## Decl_action => XmlDecl

VersionInfo        ::= S 'version' Eq "'" VersionNum "'" ## Decl_action => ::copy[4]
                     | S 'version' Eq '"' VersionNum '"' ## Decl_action => ::copy[4]

Eq                 ::= <S maybe> '=' <S maybe>

VersionNum         ::= '1.' <digit many>
#
# https://lists.w3.org/Archives/Public/xml-editor/2011OctDec/0002
#
# Use S1 instead of S in Misc
#

Misc               ::= Comment
                     | PI
                     | S1

doctypedecl        ::= '<!DOCTYPE' S Name              <S maybe>                             '>'
                     | '<!DOCTYPE' S Name              <S maybe> '[' intSubset ']' <S maybe> '>'
                     | '<!DOCTYPE' S Name S ExternalID <S maybe>                             '>'
                     | '<!DOCTYPE' S Name S ExternalID <S maybe> '[' intSubset ']' <S maybe> '>'
#
# https://lists.w3.org/Archives/Public/xml-editor/2011OctDec/0001
#
# Change S in DeclSep to S1
#

DeclSep            ::= PEReference
                     | S1

intSubset          ::= <intSubset1 any>

markupdecl         ::= elementdecl
                     | AttlistDecl
                     | EntityDecl
                     | NotationDecl
                     | PI
                     | Comment

extSubset          ::=          extSubsetDecl
                     | TextDecl extSubsetDecl

extSubsetDecl      ::= <extSubsetDecl1 any>

SDDecl             ::= S 'standalone' Eq "'" <yes or no> "'"
                     | S 'standalone' Eq '"' <yes or no> '"'
element            ::= EmptyElemTag
                     | STag content ETag
                     | ELEMENT_VALUE

STag               ::= ELEMENT_START Name <STag1 any> <S maybe> '>'

Attribute          ::= Name Eq AttValue

ETag               ::= '</' Name <S maybe> ELEMENT_END1

content            ::= <CharData maybe> <content1 any>

EmptyElemTag       ::= ELEMENT_START Name <EmptyElemTag1 any> <S maybe> ELEMENT_END2

elementdecl        ::= '<!ELEMENT' S Name S contentspec <S maybe> '>'

contentspec        ::= 'EMPTY' | 'ANY' | Mixed | children

children           ::= <choice or seq> <sequence maybe>

cp                 ::= <Name or choice or seq> <sequence maybe>

choice             ::= '(' <S maybe> cp <choice1 many> <S maybe> ')'

seq                ::= '(' <S maybe> cp <seq1 any>     <S maybe> ')'

Mixed              ::= '(' <S maybe> '#PCDATA' <Mixed1 any> <S maybe> ')*'
                     | '(' <S maybe> '#PCDATA'              <S maybe> ')'

AttlistDecl        ::= '<!ATTLIST' S Name <AttDef any> <S maybe> '>'

AttDef             ::= S Name S AttType S DefaultDecl

AttType            ::= StringType | TokenizedType | EnumeratedType

StringType         ::= 'CDATA'

TokenizedType      ::= 'ID'
                     | 'IDREF'
                     | 'IDREFS'
                     | 'ENTITY'
                     | 'ENTITIES'
                     | 'NMTOKEN'
                     | 'NMTOKENS'

EnumeratedType     ::= NotationType
                     | Enumeration

NotationType       ::= 'NOTATION' S '(' <S maybe> Name    <NotationType1 any> <S maybe> ')'

Enumeration        ::=              '(' <S maybe> Nmtoken <Enumeration1 any>  <S maybe> ')'

DefaultDecl        ::= '#REQUIRED'
                     | '#IMPLIED'
                     |            AttValue
                     | '#FIXED' S AttValue

conditionalSect    ::= includeSect | ignoreSect

includeSect        ::= '<![' <S maybe> 'INCLUDE' <S maybe> '[' extSubsetDecl ']]>'
#
# The rule <ignoreSectContents any>  ::= ignoreSectContents* will trigger MARPA_ERR_COUNTED_NULLABLE: Nullable symbol on RHS of a sequence rule
# because ignoreSectContents is a nullable, so we revisit the whole ignore sections by making
# Ignore not nullable.
#
# ORIGINAL:
# ignoreSect         ::= '<![' <S maybe> 'IGNORE' <S maybe> '[' <ignoreSectContents any> ']]>'
# ignoreSectContents ::= Ignore <ignoreSectContents1 any>
# Ignore             ::= <CHARDATA any> - <IGNORE EXCEPTION>
# Ignore             ::= # Because a lexeme cannot be a nullable


ignoreSect         ::= '<![' <S maybe> 'IGNORE' <S maybe> '[' <ignoreSectContents any> ']]>'
                     | '<![' <S maybe> 'IGNORE' <S maybe> '['                          ']]>'

ignoreSectContents ::= Ignore <ignoreSectContents1 any>

Ignore             ::= <Ignore Exceptioned>

CharRef            ::= '&#' <digit many> ';'
                     | '&#x' <hexdigit many> ';'

Reference          ::= EntityRef
                     | CharRef

EntityRef          ::= '&' Name ';'

PEReference        ::= '%' Name ';'

EntityDecl         ::= GEDecl | PEDecl

GEDecl             ::= '<!ENTITY' S Name S EntityDef <S maybe> '>'

PEDecl             ::= '<!ENTITY' S '%' S Name S PEDef <S maybe> '>'

EntityDef          ::= EntityValue
                     | ExternalID
                     | ExternalID NDataDecl

PEDef              ::= EntityValue | ExternalID

ExternalID         ::= 'SYSTEM' S SystemLiteral
                     | 'PUBLIC' S PubidLiteral S SystemLiteral

NDataDecl          ::= S 'NDATA' S Name

#
# Note: it is important to split '<?xml' into '<?' 'xml' because of PI whose defintion is: '<?' PITarget
#
TextDecl           ::= '<?' 'xml' <VersionInfo maybe> EncodingDecl <S maybe> '?>'

extParsedEnt       ::= <TextDecl maybe> content

EncodingDecl       ::= S 'encoding' Eq '"' EncName '"'                               ## Decl_action => ::copy[4]
                     | S 'encoding' Eq "'" EncName "'"                               ## Decl_action => ::copy[4]

EncName            ::= <EncName header> <EncName trailer any>

NotationDecl       ::= '<!NOTATION' S Name S ExternalID <S maybe> '>'
                     | '<!NOTATION' S Name S PublicID   <S maybe> '>'

PublicID           ::= 'PUBLIC' S PubidLiteral


<Misc any>                ::= Misc*

<NameChar any>            ::= NameChar*

<EntityValue1>            ::= EntityValueDQInner
                            | PEReference
                            | Reference

<EntityValue2>            ::= EntityValueSQInner
                            | PEReference
                            | Reference

<EntityValue1 any>        ::= <EntityValue1>*

<EntityValue2 any>        ::= <EntityValue2>*

<AttValue1>               ::= AttValueDQInner
                            | Reference

<AttValue2>               ::= AttValueSQInner
                            | Reference

<AttValue1 any>           ::= <AttValue1>*

<AttValue2 any>           ::= <AttValue2>*

<SystemLiteral1>          ::= SystemLiteralDQInner

<SystemLiteral2>          ::= SystemLiteralSQInner

<SystemLiteral1 any>      ::= <SystemLiteral1>*

<SystemLiteral2 any>      ::= <SystemLiteral2>*

<PubidChar1 any>          ::= PubidChar*

<PubidChar2>              ::= [\x{20}\x{D}\x{A}a-zA-Z0-9\-()+,./:=?;!*#@$_%]  # Same as PUBIDCHAR but without '

<PubidChar2 any>          ::= <PubidChar2>*

<XMLDecl maybe>           ::= XMLDecl
<XMLDecl maybe>           ::=

<EncodingDecl maybe>      ::= EncodingDecl
<EncodingDecl maybe>      ::=

<SDDecl maybe>            ::= SDDecl
<SDDecl maybe>            ::=

<S maybe>                 ::= S
<S maybe>                 ::=

<digit>                   ::= [0-9]

<digit many>              ::= <digit>+

<hexdigit>                ::= [0-9a-fA-F]

<hexdigit many>           ::= <hexdigit>+

<intSubset1>              ::= markupdecl
                            | DeclSep

<intSubset1 any>          ::= <intSubset1>*

<extSubsetDecl1>          ::= markupdecl
                            | conditionalSect
                            | DeclSep

<extSubsetDecl1 any>      ::= <extSubsetDecl1>*

<yes or no>               ::= 'yes' | 'no'

<STag1>                   ::= S Attribute

<STag1 any>               ::= <STag1>*

<CharData maybe>          ::= CharData
<CharData maybe>          ::=

<content1>                ::= element   <CharData maybe>
                            | Reference <CharData maybe>
                            | CDSect    <CharData maybe>
                            | PI        <CharData maybe>
                            | Comment   <CharData maybe>

<content1 any>            ::= <content1>*

<EmptyElemTag1>           ::= S Attribute

<EmptyElemTag1 any>       ::= <EmptyElemTag1>*

<choice or seq>           ::= choice | seq

<sequence>                ::= '?' | '*' | '+'

<sequence maybe>          ::= <sequence>
<sequence maybe>          ::=

<Name or choice or seq>   ::= Name | choice | seq

<choice1>                 ::= <S maybe> '|' <S maybe> cp

<choice1 many>            ::= <choice1>+

<seq1>                    ::= <S maybe> ',' <S maybe> cp

<seq1 any>                ::= <seq1>*

<Mixed1>                  ::= <S maybe> '|' <S maybe> Name

<Mixed1 any>              ::= <Mixed1>*

<AttDef any>              ::= AttDef*

<NotationType1>           ::= <S maybe> '|' <S maybe> Name

<NotationType1 any>       ::= <NotationType1>*

<Enumeration1>            ::= <S maybe> '|' <S maybe> Nmtoken

<Enumeration1 any>        ::= <Enumeration1>*

<ignoreSectContents any>  ::= ignoreSectContents*

<ignoreSectContents1>     ::= '<![' ignoreSectContents ']]>' Ignore

<ignoreSectContents1 any> ::= <ignoreSectContents1>*

<VersionInfo maybe>       ::= VersionInfo
<VersionInfo maybe>       ::=

<TextDecl maybe>          ::= TextDecl
<TextDecl maybe>          ::=

<EncName header>          ::= [A-Za-z]

<EncName trailer>         ::= [A-Za-z0-9._-]

<EncName trailer any>     ::= <EncName trailer>*

#############################
# Grammar subtilities
#############################
<EntityValueDQInner>      ::= [\x{9}\x{A}\x{D}\x{20}-\x{21}\x{23}-\x{24}\x{27}-\x{D7FF}\x{E000}-\x{FFFD}\x{10000}-\x{10FFFF}]:u
<EntityValueSQInner>      ::= [\x{9}\x{A}\x{D}\x{20}-\x{24}\x{28}-\x{D7FF}\x{E000}-\x{FFFD}\x{10000}-\x{10FFFF}]:u
<AttValueDQInner>         ::= /[\x{9}\x{A}\x{D}\x{20}-\x{21}\x{23}-\x{25}\x{27}-\x{3b}\x{3d}-\x{D7FF}\x{E000}-\x{FFFD}\x{10000}-\x{10FFFF}]+/u
<AttValueSQInner>         ::= /[\x{9}\x{A}\x{D}\x{20}-\x{25}\x{28}-\x{3b}\x{3d}-\x{D7FF}\x{E000}-\x{FFFD}\x{10000}-\x{10FFFF}]+/u
<SystemLiteralDQInner>    ::= /[\x{9}\x{A}\x{D}\x{20}-\x{21}\x{23}-\x{D7FF}\x{E000}-\x{FFFD}\x{10000}-\x{10FFFF}]+/u
<SystemLiteralSQInner>    ::= /[\x{9}\x{A}\x{D}\x{20}-\x{26}\x{28}-\x{D7FF}\x{E000}-\x{FFFD}\x{10000}-\x{10FFFF}]+/u

#############################
# For element start detection
#############################
:lexeme ::= ELEMENT_START pause => before event => ^ELEMENT_START
ELEMENT_START               ~ '<'

#############################
# For element end detection
#############################
:lexeme ::= ELEMENT_END1 pause => before event => ^ELEMENT_END
ELEMENT_END1                ~ '>'

:lexeme ::= ELEMENT_END2 pause => before event => ^ELEMENT_END
ELEMENT_END2                ~ '/>'

#############################
# For element valuation injected in parent recognizer
#############################
ELEMENT_VALUE               ~ [^\s\S]

#########
# Lexemes
#########
# <_NAMESTARTCHAR>           ~ [:A-Z_a-z\x{C0}-\x{D6}\x{D8}-\x{F6}\x{F8}-\x{2FF}\x{370}-\x{37D}\x{37F}-\x{1FFF}\x{200C}-\x{200D}\x{2070}-\x{218F}\x{2C00}-\x{2FEF}\x{3001}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFFD}\x{10000}-\x{EFFFF}]:u
# <_NAMECHAR>                ~ [:A-Z_a-z\x{C0}-\x{D6}\x{D8}-\x{F6}\x{F8}-\x{2FF}\x{370}-\x{37D}\x{37F}-\x{1FFF}\x{200C}-\x{200D}\x{2070}-\x{218F}\x{2C00}-\x{2FEF}\x{3001}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFFD}\x{10000}-\x{EFFFF}\-.0-9\x{B7}\x{0300}-\x{036F}\x{203F}-\x{2040}]:u
# <_NAMECHAR any>            ~ <_NAMECHAR>*
# <_NAME>                    ~ <_NAMESTARTCHAR> <_NAMECHAR any>

<_NAME>                    ~ /[:A-Z_a-z\x{C0}-\x{D6}\x{D8}-\x{F6}\x{F8}-\x{2FF}\x{370}-\x{37D}\x{37F}-\x{1FFF}\x{200C}-\x{200D}\x{2070}-\x{218F}\x{2C00}-\x{2FEF}\x{3001}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFFD}\x{10000}-\x{EFFFF}][:A-Z_a-z\x{C0}-\x{D6}\x{D8}-\x{F6}\x{F8}-\x{2FF}\x{370}-\x{37D}\x{37F}-\x{1FFF}\x{200C}-\x{200D}\x{2070}-\x{218F}\x{2C00}-\x{2FEF}\x{3001}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFFD}\x{10000}-\x{EFFFF}\-.0-9\x{B7}\x{0300}-\x{036F}\x{203F}-\x{2040}]*/u
<NAME>                     ~ <_NAME>

################
# XML Exceptions
################
#
# -------------------------------------------------------------
# Comment ::= '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
# -------------------------------------------------------------
#

<Char minus sign>       ::= [\x{9}\x{A}\x{D}\x{20}-\x{2C}\x{2E}-\x{D7FF}\x{E000}-\x{FFFD}\x{10000}-\x{10FFFF}]:u  # '-' is \x{2D}
<Comment Interior Unit> ::=     <Char minus sign>
                          | '-' <Char minus sign>
<Comment Interior>      ::= <Comment Interior Unit>*

#
# -----------------------------------------------------------
# PI ::= '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'
# -----------------------------------------------------------
#
# No need for exception, because '?>' is longer than Char
#
<PI Exceptioned>        ::= Char*

#
# ---------------------------------------------------------
# PITarget ::= Name - (('X' | 'x') ('M' | 'm') ('L' | 'l'))
# ---------------------------------------------------------
#
# The following is working, but we want this module to be
# more user-friendly, saying that a PITarget cannot be 'xml':i more explicitly.
# Since we will use events anyway because of SAX support, we add an explicit

<_XML>                    ~ [Xx] [Mm] [Ll]
<PITarget>              ::= <_NAME> - <_XML>

#
# If you like to handle this in user-space, this could be... with an event on PITarget$, then getting lastLexemePause('PITarget'):
#
# <NAMESTARTCHAR>           ~ [:A-Z_a-z\x{C0}-\x{D6}\x{D8}-\x{F6}\x{F8}-\x{2FF}\x{370}-\x{37D}\x{37F}-\x{1FFF}\x{200C}-\x{200D}\x{2070}-\x{218F}\x{2C00}-\x{2FEF}\x{3001}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFFD}\x{10000}-\x{EFFFF}]:u
# <NAMECHAR>                ~ [:A-Z_a-z\x{C0}-\x{D6}\x{D8}-\x{F6}\x{F8}-\x{2FF}\x{370}-\x{37D}\x{37F}-\x{1FFF}\x{200C}-\x{200D}\x{2070}-\x{218F}\x{2C00}-\x{2FEF}\x{3001}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFFD}\x{10000}-\x{EFFFF}\-.0-9\x{B7}\x{0300}-\x{036F}\x{203F}-\x{2040}]:u
# <NAMECHAR any>            ~ <NAMECHAR>*
# <NAME>                    ~ <NAMESTARTCHAR> <NAMECHAR any>
# <PITarget>                ~ <NAME>

#
# ---------------------------------------
# CData ::= (Char* - (Char* ']]>' Char*))
# ---------------------------------------
#
# No need for exception, because ']]>' is longer than Char
#
<CData Exceptioned>     ::= Char*

#
# ------------------------------------------------
# Ignore ::= Char+ - (Char+ ('<![' | ']]>') Char+)
# ------------------------------------------------
#
# Note that we made Ignore not nullable.
# No need for exception, because '<![' and ']]>' are longer than Char
#
<Ignore Exceptioned>    ::= Char+

#
# -------------------------------------------
# CharData ::= [^<&]* - ([^<&]* ']]>' [^<&]*)
# -------------------------------------------
#
# Note that we made CharData not nullable.
# No need for exception, because ']]>' is longer than <CharData Unit>
#
# All text that is not markup constitutes the character data of the document, and since
# a character data cannot contain markup characters (nor CDATA section-close delimiter)
# we raise its priority.
#
<_CHARDATA UNIT>          ~ [\x{9}\x{A}\x{D}\x{20}-\x{25}\x{26}-\x{3b}\x{3d}-\x{D7FF}\x{E000}-\x{FFFD}\x{10000}-\x{10FFFF}]:u
<_CHARDATA UNIT ANY>      ~ <_CHARDATA UNIT>*
<CHARDATA>                ~ <_CHARDATA UNIT ANY>
#<CHARDATA>               ~ /[\x{9}\x{A}\x{D}\x{20}-\x{25}\x{26}-\x{3b}\x{3d}-\x{D7FF}\x{E000}-\x{FFFD}\x{10000}-\x{10FFFF}]+/u
<CHARDATA EXCEPTION>      ~ /.*\]\]>/u  # Faster with a regexp, because it works on an already matched area: <CHARDATA>, so no need to rematch <_CHARDATA UNIT ANY>
<CharData Exceptioned>  ::= <CHARDATA> - <CHARDATA EXCEPTION>
