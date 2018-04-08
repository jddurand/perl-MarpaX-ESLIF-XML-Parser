use strict;
use warnings FATAL => 'all';
use Log::Log4perl qw/:easy/;
use Log::Any::Adapter;
use Log::Any qw/$log/;
use MarpaX::ESLIF::XML::Parser;
use MarpaX::ESLIF::XML::Parser::Reader::File;
use POSIX qw/EXIT_SUCCESS/;

our $defaultLog4perlConf = '
log4perl.rootLogger                               = TRACE, Screen
log4perl.appender.Screen                          = Log::Log4perl::Appender::Screen
log4perl.appender.Screen.stderr                   = 1
log4perl.appender.Screen.layout                   = PatternLayout
log4perl.appender.Screen.Threshold                = TRACE
log4perl.appender.Screen.layout.ConversionPattern = %d %-5p %6P %m{chomp}%n
';
Log::Log4perl::init(\$defaultLog4perlConf);
Log::Any::Adapter->set('Log4perl');

while (@ARGV) {
    my $filename = shift;
    $log->infof('Parsing %s', $filename);
    my $reader = MarpaX::ESLIF::XML::Parser::Reader::File->new(filename => $filename);
    my $parser = MarpaX::ESLIF::XML::Parser->new(reader => $reader);

    $parser->parse();
}

exit(EXIT_SUCCESS);
