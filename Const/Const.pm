package Lingua::Treebank::Const;

use 5.008;
use strict;
use warnings;
use Carp;

our $VERSION = '0.01';
##################################################################
use Class::Struct;
struct ( tag      => '$', # e.g. "NP"
	 annot    => '$', # e.g. "SUBJ"
	 word     => '$', # e.g. "John" -- terminal only
	 parent   => __PACKAGE__,  # back-pointer up tree
	 children => '@' );
use Text::Balanced 'extract_bracketed';
##################################################################
our $INDENT_CHAR = ' ' x 4;
our $CHILD_PROLOG = "\n";
##################################################################
sub find_common_ancestor {

    # returns lowest common ancestor, or undef if there is none.

    my $self = shift;
    my $cousin = shift;

    # error checking
    croak "cousin arg not defined" if not defined $cousin;
    croak "cousin not a " . __PACKAGE__
      unless UNIVERSAL::isa($cousin, __PACKAGE__);

    my $matriarch = $self->root();
    if ( $cousin->root() != $matriarch ) {
	return; # no common ancestor
    }

    my @self_lineage   = $self->path_up_to( $matriarch );
    my @cousin_lineage = $cousin->path_up_to( $matriarch );

    while (@self_lineage and @cousin_lineage) {
	my $self_gramma   = pop @self_lineage;
	my $cousin_gramma = pop @cousin_lineage;
	if ($self_gramma == $cousin_gramma) {
	    $matriarch = $self_gramma;
	}
	else {
	    # stop looking -- once unshared, thereafter its a waste to
	    # keep looking. No incestuous trees here, one would hope.
	    last;
	}
    }

    return $matriarch;
}
##################################################################
# height/depth functions
##################################################################
sub depth_from {
    # return depth from given target. returns undef if $target is not
    # the ancestor of $self

    my $self   = shift;
    my __PACKAGE__ $target = shift;

    if ($self == $target) {
	return 0;
    }
    elsif ($self->is_root()) {
	carp "depth_from argument not an ancestor of instance";
	# we could check this explicitly, but users may already know
	# this isn't going to happen, so let's not waste cycles
	return (); # not defined
    }
    else {
	return $self->parent->depth_from($target) + 1;
    }
}
##################################################################
sub depth {

    # returns how many steps from self up to root

    my $self = shift;

    # implemented using more general function -- but it does require
    # two traversals of the tree... other implementations may be easier
    return $self->depth_from( $self->root() );

# simple recursive implementation
#      if ( $self->is_root() ) {
#  	return 0;
#      }
#      else {
#  	return $self->{parent}->depth() + 1;
#      }

# non-recursive implementation
#      my $d = 0;
#      my $p = $self->{parent};
#      while (defined $p) {
#  	$h++;
#  	$p = $p->{parent};
#      }
#      return $d;

}
##################################################################
sub height {
    # returns longest distance from self down to any leaf

    # could be re-implemented with get_all_terminals, path_up_to and
    # array lengths, but that seems unnecessary
    my $self = shift;

    if ($self->is_terminal()) {
	return 0;
    }
    else {
	my ($max) = 0;

	# choose the largest height among the children, return that
	# (+1)
	foreach (@{$self->children()}) {
	    my $this_height = $_->height();
	    if ($max < $this_height) {
		$max = $this_height;
	    }
	}
	return $max + 1;
    }
}
##################################################################
sub get_index {
    my $self = shift;
    my __PACKAGE__ $daughter = shift;

    if ($self->is_terminal) {
	carp "get_index called on terminal node, can't get_index";
	return;
    }

    if ( $daughter->parent != $self ) {
	carp "argument not daughter of instance, can't get index";
	return ;
    }

    for ( 0 .. $#{$self->children} ) {
	if ( $self->children($_) == $daughter ) {
	    return $_;
	}
    }

    carp "malformed tree:",
      " daughter identifies instance as parent, but parent does ",
	"not claim daughter";
    return ;
}
##################################################################
# node retrieval functions
##################################################################
sub path_up_to {
    my $self = shift;
    my $terminus = shift;

    # could be done non-recursively, but this is grammatical structure
    # -- very small heights.  Besides, recursivity is cooler, and
    # easier to think about

    if ($self == $terminus) {
	return ($self);
    }
    elsif ( $self->is_root() ) {
	carp "terminus argument not an ancestor of instance!";
	return ;
    }
    else {
	my @path = $self->parent->path_up_to( $terminus );
	if (not @path) {
	    return; # not found
	}
	else {
	    return ( $self, @path );
	}
    }
}
##################################################################
sub root {
    # returns the root of a given node
    my $self = shift;
    if ($self->is_root()) {
	return $self;
    }
    else {
	return $self->parent->root();
    }
}
##################################################################
sub get_all_terminals {
    # returns all leaves in a left-right traversal

    my $self = shift;

    my @terminals;

    if ( $self->is_terminal() ) {
	@terminals = ( $self ); # parens force list return
    }
    else {
	foreach ( @{$self->children} ) {
	    push @terminals, $_->get_all_terminals;
	}
    }
    return @terminals;
}
##################################################################
sub next_sib {
    my $self = shift;

    return if $self->is_root; # no sib, return undef

    my $parent = $self->parent;

    my $index = $parent->get_index($self);

    if ($index == $#{$parent->children}) {
	# this is the rightmost of the group of siblings
	return; # no right sib
    }
    return $parent->children($index + 1);
}
##################################################################
sub prev_sib {
    my $self = shift;

    return if $self->is_root; # no sib, return undef

    my $parent = $self->parent;

    my $index = $parent->get_index($self);

    if ($index == 0) {
	# this is the leftmost of the group of siblings
	return; # no left sib
    }
    return $parent->children($index - 1);
}
##################################################################
sub right_leaf {
    my $self = shift;
    # returns rightmost leaf of current node

    if ($self->is_terminal) {
	return $self;
    }
    else {
	my $right_daughter = $self->children(-1);
	return $right_daughter->right_leaf();
    }
}
##################################################################
sub left_leaf {
    my $self = shift;
    # returns leftmost leaf of current node

    if ($self->is_terminal) {
	return $self;
    }
    else {
	my $left_daughter = $self->children(0);
	return $left_daughter->left_leaf();
    }
}
##################################################################
sub prev_leaf {
    # return the next leaf to the left (back in time), not dominated
    # by the current node

    # should behave correctly even when called on a non-terminal --
    # returns the first leaf to the left not-dominated by the current
    my $self = shift;

    my $left_sib = $self->prev_sib;

    if (defined $left_sib) {
	return $left_sib->right_leaf();
    }
    else {
	# no immediate left sib, go up the tree

	if ( $self->is_root() ) {
	    return; # no previous leaves
	}
	else {
	    return $self->parent->prev_leaf();
	}
    }
}
##################################################################
sub next_leaf {
    # return the next leaf to the right (forward in time)

    # should behave correctly even when called on a non-terminal --
    # returns the first leaf to the right not-dominated by the current
    my $self = shift;

    my $right_sib = $self->next_sib;

    if (defined $right_sib) {
	return $right_sib->left_leaf();
    }
    else {
	# no immediate right sib, go up the tree

	if ( $self->is_root() ) {
	    return; # no previous leaves
	}
	else {
	    return $self->parent->next_leaf();
	}
    }
}
##################################################################
# boolean requests (one additional argument)
##################################################################
sub is_descendant_of {
    my $self = shift;
    my $grandma = shift;

    if ($self->is_root) {
	return 0; # root is descendant of nobody, grandma or otherwise
    }
    if ($self == $grandma) {
	return 1; # yes, you are your own descendant. :p
    }
    else {
	return $self->parent->is_descendant_of($grandma);
    }
}
##################################################################
sub is_ancestor_of {
    my $self = shift;
    my $candidate = shift;
    return $candidate->is_descendant_of($self);
}
##################################################################
# I/O methods (to/from text)
##################################################################
sub as_penn_text {
    my $self = shift;
    my $step = shift;
    my $indentChar = shift;
    my $child_prolog = shift;

    # set defaults (in case called without full specification)
    $step = 0 if not defined $step;
    $indentChar = $INDENT_CHAR if not defined $indentChar;
    $child_prolog = $CHILD_PROLOG if not defined $child_prolog;

    # begin composition of text
    my $text = '(' . $self->tag() . ' ';

    if ($self->is_terminal) {
	$text .= $self->word();
    }
    else {
	# non-terminal

	foreach (@{$self->children}) {
	    $text .= $child_prolog;
	    $text .= ($indentChar x $step);
	    $text .= $_->as_penn_text($step + 1, $indentChar, $child_prolog);
	}
    }

    $text .= ')';

    return $text;
}
##################################################################
sub from_penn_string {
    my $self = shift;
    my $text = shift;
    # pass it a complete constituent in text form.

    # records the tag plus a list of its subconstituents. If
    # subconstituents themselves have structure, then they will be
    # arrayrefs
    my (@tags) = shift;
    my ($tag, $childrentext) =
      ($text =~ /^ \( ( [\S]* ) \s (.*\S) \s* \) $/sx);

    if (not defined $tag or not defined $childrentext) {
	warn "couldn't match '$_' as a constituent!\n";
    }

    if ($tag =~ m/ ^ ( [^-]+? ) - ( .* ) $/x ) {
	$tag = $1;
	$self->annot( $2 );
    }

    $self->tag($tag);

    while ($childrentext) {
	my $childtext = extract_bracketed($childrentext, '()');
	if (defined $childtext) {
	    # child is itself a constituent
	    my $child =
	      __PACKAGE__->new->from_penn_string($childtext);

	    $self->append($child);

#  	    $child->parent($self);
#  	    push @{$self->children}, $child;

	    warn "trouble -- child constituent found " .
	      "in token that already had word\n"
		if defined $self->word;
	}
	else {
	    # this is a word; we're done
	    $self->word($childrentext);
	    warn "trouble --  word found in token that "
	      . "already had child constituents\n"
		if @{$self->children};
	}
    }
}
##################################################################
# Tree modification methods
##################################################################
sub flatten {
    # pull up all terminals to be children of the instance here,
    # regardless of how deep they are

    #  A->flatten()
    #
    #      /         /
    #     A   ==>   A__
    #    / \	   /|\ \
    #   X   B	  C F D G
    #  /|\   \
    # C F D   E
    #          \
    #           G

    my $self = shift;

    if ($self->is_terminal) {
	carp "flatten called on terminal node";
	return;
    }

    foreach my __PACKAGE__ $daughter (@$self->children) {

	next if $daughter->is_terminal; # this child's done

	# pull up all descendants of non-terminal daughter to depend
	# directly on the daughter
	$daughter->flatten();

	# now reparent all the grandchildren to self, by retracting
	# the daughter
	$self->retract($daughter);
    }

    return $self;

    # could probably be reimplemented by "get_all_terminals" and
    # judicious use of insert, but this recursive strategy is more
    # elegant and takes advantage of brains of retract() method
}
##################################################################
sub retract {
    # pulls in and removes one layer of non-terminal nodes, attaching
    # their children directly to the current node, retaining what
    # surface order they originally had:

    #  A->retract(X)
    #
    #      /         /
    #     A   ==>   A
    #    / \	   /|\
    #   X   B	  C D B
    #  / \   \     / \ \
    # C   D   E   F   G E
    #    / \
    #   F   G

    my $self = shift;
    my __PACKAGE__ $daughter = shift;

    if ($daughter->parent != $self) {
	carp "argument daughter does not claim instance as mother,",
	  " can't retract!";
	return;
    }

    if ($daughter->is_terminal) {
	carp "daughter is a terminal node, can't retract!";
	return;
    }

    $self->replace($daughter, @{$daughter->children});

    return $self;

}
##################################################################
sub replace {
    # replace target arg with replacement list
    my $self         = shift;
    my $target       = shift;
    my @replacements = @_;

    carp "argument not a child of instance, can't replace!"
      unless ($_->parent == $self);

    my $index = $self->get_index($target);

    $self->detach_at($index);

    $self->insert_at($index, @replacements);
}
##################################################################
sub detach {
    # removes an entire subtree.
    my $self = shift;

    my $daughter = shift;

    # actually do the detachment
    my $index = $self->get_index($daughter);

    $self->detach_at($index);

}
##################################################################
sub detach_at {
    # remove one daughter node at index
    my $self = shift;
    my $index = shift;

    if (not defined $index) {
	croak "no index provided to detach_at method";
    }

    my $d = $self->children($index);

    if (not defined $d) {
	carp "no daughter at index $index";
	return;
    }

    # remove links
    $d->parent(undef);
    splice @{$self->children}, $index, 1, (); # replace with empty list
}
##################################################################
sub prepend {
    my $self = shift;
    my @daughters = @_;
    $self->insert_at(0, @daughters);
}
##################################################################
sub append {
    my $self = shift;
    my @daughters = @_;
    $self->insert_at(@{$self->children}, @daughters);
}
##################################################################
sub insert_at {
    my $self     = shift;
    my $position = shift;
    my @daughters = @_;

    foreach my __PACKAGE__ $d (@daughters) {
	$d->parent($self);
    }

    splice @{$self->children}, $position, 0, @daughters;
    return $self;
}
##################################################################
# FEATURES OF THE CURRENT NODE
##################################################################
sub is_root {
    my $self = shift;
    return ( not defined $self->parent );
}
##################################################################
sub is_terminal {
    my $self = shift;
    if (defined $self->word) {
	if ( @{$self->{children}} ) {
	    carp "how did I get children AND a word?";
	}
	return 1;
    }
    else {
	if ( not @{$self->{children}} ) {
	    carp "how did I get neither a word NOR children?";
	}
	return 0;
    }
}
##################################################################

1;

__END__

=head1 NAME

Lingua::Treebank::Const - Object modeling constituent from a treebank

=head1 SYNOPSIS

  use Lingua::Treebank::Const;

  my $text = <<EOTREE
  (S
    (NP-SBJ (DT this) )
    (VP (VBZ is)
      (NP-PRD (NNP Lisa) ))
    (. .) )
  TREE

  my $utt = Lingua::Treebank::Const->new->from_penn_text($text)

  print $utt->as_penn_text(), "\n";;

Results:

  (S
      (NP-SBJ
          (DT this))
      (VP
          (VBZ is)
          (NP-PRD
              (NNP Lisa) ))
      (. .))

This is configurable (TO DO: document how so).


=head1 ABSTRACT

  module defines methods for accessing syntactic constituents; it
  identifies its parents and its children, and can write itself out in
  a variety of formats (currently Penn treebank style).

=head1 DESCRIPTION

Module for describing simple constituents of the Penn
Treebank. Recursive behaviors are implied.

Note assumption that terminal nodes (those with defined C<word>
values) will not have C<children>, and vice versa. This assumption is
currently unchecked by the code.

=head2 Class methods

=over

=item new

Constructs a new (uninitialized) token.  If starting from text, can be
used together with the C<from_penn_text> initialization method, as
below:

  my $text = <<EOTREE
  (S
    (NP-SBJ (DT this) )
    (VP (VBZ is)
      (NP-PRD (NNP Lisa) ))
    (. .) )
  TREE

  my $utt = Lingua::Treebank::Const->new->from_penn_text($text)

Otherwise, resulting new unit will have no values (C<parent>,
C<children>, C<tag> or C<word> set by default.

=back

=head2 Instance methods

Each constituent has the following methods:

=over

=item tag

Records the tag of this constituent (0-domination).

=item word

If this constituent is terminal, then C<word> should contain the
lexical item that is represented.

=item children

returns a reference to an array of C<Lingua::Treebank::Const> objects
that are the children of the current node.

Currently does not check whether C<word> is populated.

=item is_terminal

Return whether self is a leaf.  Does not check whether C<children> are
populated; if automatically generated from the C<from_penn_text>
method then this will always be correct.

=item is_root

Boolean. Is the instance a root node (no parents).

=item root

returns the root node for the instance in question (might be itself)

=item path_up_to

given another node (assumed to be an ancestor of the instance),
returns a list of all the nodes (local first) between them. Returns
I<undefined> and C<carp>s when the given node is not an ancestor of
the instance.

=item height

What's the I<farthest> from the current node to a terminal node?

=item depth

what's the distance from the current node to the root?

=item depth_from

what's the distance from the current node up to the node given as
argument? (return I<undefined> if the node given is not the ancestor
of the instance node)

=item find_common_ancestor

given a second node, return the lowest ancestor they share (or
undefined if they do not share an ancestor)

=item as_penn_text

Returns a text string representing this constituent.

B<To do: document additional parameters to this, and the possible
effects of changing them>

=back

=head1 To Do

check that destroy doesn't leak (undo parent links?)

dump as latex tree

read in other treebank formats (latex trees?)

other methods:

self->insert($child, $position)
self->append($node)
self->prepend($node)

self->insert_before($node)  # error if node not child of self

self->next_sib
self->prev_sib


=head2 EXPORT

None by default.


=head1 HISTORY

=over 8

=item 0.01

Original version; created by h2xs 1.22 with options

  -CAX
	Lingua::Treebank::Const

=back



=head1 SEE ALSO

Documentation for Penn treebank L<http://www.cis.upenn.edu/~treebank/>.

=head1 AUTHOR

Jeremy Gillmor Kahn, E<lt>kahn@cpan.orgE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright 2003 by Jeremy Gillmor Kahn

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
