# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl 1.t'

#########################

# change 'tests => 1' to 'tests => last_test_to_print';

use Test::More tests => 5;
BEGIN { use_ok('Lingua::Treebank::Const') };

#########################

# Insert your test code below, the Test::More module is use()ed here so read
# its man page ( perldoc Test::More ) for help writing this test script.

use constant PACK => 'Lingua::Treebank::Const';

can_ok(PACK, qw( from_penn_string as_penn_text ) );

my $d = PACK->new();
ok( defined $d, "new() returned something" );
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

ok(1, "passed from_penn_string");

my $ex2 = <<EOEX2;
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
EOEX2

# Put in TODO tests for malformed data here


# put in handling cases for writing data out as well
#  my $ex1prime = $d->as_text();
