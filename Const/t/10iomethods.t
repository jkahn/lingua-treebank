# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl 1.t'

#########################

# change 'tests => 1' to 'tests => last_test_to_print';

use Test::More tests => 9;
BEGIN {
    #01
    use_ok('Lingua::Treebank::Const');
};

#########################

# Insert your test code below, the Test::More module is use()ed here so read
# its man page ( perldoc Test::More ) for help writing this test script.

use constant PACK => 'Lingua::Treebank::Const';

#2
can_ok(PACK, qw( from_penn_string as_penn_text ) );

my $d = PACK->new();
#3
ok( defined $d, "new() returned something" );
#4
isa_ok($d, PACK, "and it's a " . PACK);

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

$d->from_penn_string($ex1);

#5
ok(1, "passed from_penn_string");

my $ex2 = <<EOEX2;
(S (NP  (NNP Joe) ) (VP  (VB likes) (NP (NNP Bach) ) )  (. .))
EOEX2

my $d2 = PACK->new();

#6
isa_ok($d2, PACK);

$d2->from_penn_string($ex2);

#7
ok(1, "passed second from_penn_string");

#8
ok( ( $d->equiv_to($d2) ) , 'trees are equivalent');

# Put in TODO tests for malformed data here


# put in handling cases for writing data out as well
#  my $ex1prime = $d->as_text();
#9
is ( $d->as_penn_text(), $d2->as_penn_text(), 'tree texts match');

# diag ( $d->as_penn_text() );
