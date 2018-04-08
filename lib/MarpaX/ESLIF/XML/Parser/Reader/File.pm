use strict;
use warnings FATAL => 'all';

package MarpaX::ESLIF::XML::Parser::Reader::File;
use constant BUFSIZ => 1048576;
use Carp qw/croak/;
use Class::Tiny qw/filename _fh _data/, { bufsiz => sub { BUFSIZ } };
use IO::File;
use Role::Tiny::With;

sub BUILD {
    my ($self, $args) = @_;

    my $fh = IO::File->new($self->filename, 'r') || croak sprintf("Failed to open %s, %s", $self->filename // '', $!);
    $fh->binmode;
    $self->_fh($fh)
}

# ABSTRACT: XML reader role implementation a file

# VERSION

# AUTHORITY

=head1 DESCRIPTION

XML reader role implementation a file

  my $reader = MarpaX::ESLIF::XML::Parser::Reader::File->new(
                              filename => shift,
                              bufsiz => 1024 # Default is 1024*1024
                            );
=cut

=head1 METHODS

=head2 read

Reads data.

=cut

{
    no warnings 'redefine';
    sub read {
        my ($self) = @_;

        my $data;
        return 0 unless $self->_fh->read($data, $self->bufsiz);
        $self->_data($data);
        return 1
    }
}

=head2 eof

Returns EOF state.

=cut

sub eof {
    my ($self) = @_;

    return $self->_fh->eof
}

=head2 data

Returns data.

=cut

sub data {
    my ($self) = @_;

    return $self->_data
}

with 'MarpaX::ESLIF::XML::Parser::Role::Reader';

1;
