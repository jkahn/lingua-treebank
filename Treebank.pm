package Lingua::Treebank;

use 5.008;
use strict;
use warnings;

##################################################################
use Carp;

require Exporter;

our @ISA = qw ( Exporter ) ;
our @EXPORT_OK = qw();
our @EXPORT = qw();
our $VERSION = '0.03';

our $MAX_WARN_TEXT = 100;
our $VERBOSE = 1;
our $CONST_PACKAGE = 'Lingua::Treebank::Const';
##################################################################
use Text::Balanced 'extract_bracketed';
use Lingua::Treebank::Const;
##################################################################
sub from_penn_file {
    my ($class, $file) = @_;

    open (my $fh, "<", $file) or die "couldn't open $file: $!\n";
    my @results = $class->from_penn_fh($fh);
    close $fh or die "couldn't close $file: $!\n";

    return @results;
}
##################################################################
sub from_penn_fh {
    my ($class, $fh) = @_;

    my $rawTrees;
  LINE:

    while (<$fh>) {

        chomp;              # remove newlines

        if (   substr( $_, 0, 3 ) eq '*x*'
            or substr( $_, 0, 10 ) eq '=' x 10 )
        {
            # skip header copyright comments, bar of ====
            next LINE;
        }

	next if /^\s*$/; # skip entirely blank lines

	# slurp in the rest of the merge file all at once
	local $/;
	undef $/;
	$rawTrees = $_ . (<$fh>);
    }


    my (@utterances);
    while ($rawTrees) {
	my $token;

	($token, $rawTrees) = extract_bracketed($rawTrees, '()');

	if (defined $@) {
	    croak "Text::Balanced said: $@->{error} at $@->{pos} in string ",
	      cite_warning ($rawTrees);
	}

	if (length $token) {
	    my $utt = $CONST_PACKAGE->new();
	    $utt = $utt->from_penn_string($token);
	    if (defined $utt) {
		push @utterances, $utt;
	    }
	    else {
		carp "couldn't parse '", cite_warning($token),
		  "' remaining data '", cite_warning($rawTrees),
		    "' in filehandle ignored";
		last;
	    }
	}
	else {
	    # no token extractable
	    carp "unrecognized data '", cite_warning($rawTrees),
	      "' remaining in filehandle ignored";
	    last;
	}
	$rawTrees =~ s/^\s*//;
    }

    return @utterances;
}
##################################################################
sub cite_warning {
    my $text = shift;
    my $warning;
    if (length $text > $MAX_WARN_TEXT) {
	$warning =
	  substr($text, 0, $MAX_WARN_TEXT / 2);
	$warning .= ' [ ... OMITTED ... ] ';
	$warning .=
	  substr($text, -($MAX_WARN_TEXT / 2) );
    }
    else {
	$warning = $text;
    }
    return $warning;
}
##################################################################
1;

__END__

=head1 NAME

Lingua::Treebank - Perl extension for manipulating the Penn Treebank format

=head1 SYNOPSIS

  use Lingua::Treebank;

  my @utterances = Lingua::Treebank->from_penn_file($filename);

  foreach (@utterances) {
    # $_ is a Lingua::Treebank::Const now

    foreach ($_->get_all_terminals) {
      # $_ is a Lingua::Treebank::Const that is a terminal (word)

      print $_->word(), ' ' $_->tag(), "\n";
    }

    print "\n\n";

  }

=head1 ABSTRACT

  Modules for abstracting out the "natural" objects in the Penn
  Treebank format.

=head1 DESCRIPTION

Almost all the interesting tree-functionality is in the
constituent-forming package (included in this distribution, see
L<Lingua::Treebank::Const>).

=head1 Methods

=head2 Class methods

=over

=item from_penn_file

given a file, open it, extract the constituents, and return the roots.

=item from_penn_fh

given a filehandle, extract the constituents and return the roots.

=back

=head2 EXPORT

None by default.

=head1 HISTORY

=over 8

=item 0.01

Original version; created by h2xs 1.22 with options

  -CAX
	Lingua::Treebank

=item 0.02

Improved documentation.

=item 0.03

added a VERBOSE variable that can be set.

=back

=head1 SEE ALSO

TO DO: mention documentation of Penn Treebank

=head1 AUTHOR

Jeremy Gillmor Kahn, E<lt>kahn@cpan.orgE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright 2003 by Jeremy Gillmor Kahn

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
