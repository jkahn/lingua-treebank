# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl 1.t'

#########################

# change 'tests => 1' to 'tests => last_test_to_print';

use Test::More tests => 22;

BEGIN {
    #01
    use_ok('Lingua::Treebank::Const')
};

#########################

# Insert your test code below, the Test::More module is use()ed here so read
# its man page ( perldoc Test::More ) for help writing this test script.
use constant PACK => 'Lingua::Treebank::Const';

#02
can_ok(PACK,
       qw( is_root is_terminal ),
       qw( insert_at append prepend ),
       qw( left_leaf right_leaf ),
       qw( prev_leaf next_leaf ),
      );

my $d = PACK->new();

#03
ok( defined $d, "new() returned something" );

#04
isa_ok($d, PACK, 'root node');

my $ex1 = <<EOEX1;
(S
  (NP
    (NNP Joe)
  )
  (VP
    (VB likes)
    (NP
      (NNP Bach)
    )
  )
  (. .)
)
EOEX1

my $d = PACK->new();


$d->from_penn_string($ex1);

#05
ok(1, "passed from_penn_string");

#06
is( $d->tag(), 'S', 'top tag is "S"' );

#07
ok( $d->is_root(), 'top is root' );

#08
ok( not $d->is_terminal() );

my PACK $joe = $d->left_leaf();
#09
isa_ok($joe, PACK, '"Joe" node');

#10
ok( $joe->is_terminal() , ' left leaf is terminal');

#11
is( $joe->word(), 'Joe', ' leftmost word is "Joe"');

#12
ok( (not $joe->is_root()) , ' leftmost not a root');

#13
ok( (not defined $joe->prev_leaf()), " leftmost's prev_leaf not defined");

#14
is ($joe->right_leaf(), $joe, ' right leaf of terminal is self');

#15
is($joe->left_leaf(), $joe, ' left leaf of terminal is self');

my $likes = $joe->next_leaf();

#16
isa_ok($likes, PACK, '"likes" node');

#17
is($likes->prev_leaf, $joe, ' next-previous is original');

#18
is($likes->word(), 'likes', ' "likes" node has right word');

#19
is($likes->tag(), 'VB', '"likes" node has right tag');

#20
isnt($likes->parent(), $joe->parent(),
     '"Joe" and "likes" do not share a parent');

#21
is ($likes->parent()->parent(), $d, ' root is grandparent of "likes"');

#22
is ($joe->parent()->parent(), $d, ' root is grandparent of "joe"');

my $rl = $d->right_leaf();


#isa_ok


