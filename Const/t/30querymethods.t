# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl 1.t'

#########################

# change 'tests => 1' to 'tests => last_test_to_print';

use Test::More tests => 24;
BEGIN { use_ok('Lingua::Treebank::Const') };

#########################

# Insert your test code below, the Test::More module is use()ed here so read
# its man page ( perldoc Test::More ) for help writing this test script.

use constant PACK => 'Lingua::Treebank::Const';

can_ok(PACK,
       qw{ root },
       qw{ path_up_to },
       qw( get_all_terminals ),
#         qw{ find_common_ancestor  },
#         qw{ equiv_to  },
#         qw{ depth_from depth },
#         qw{ height },
#         qw{ get_index },
      );

my $d = PACK->new();

ok( defined $d, "new() returned something" );

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

$d->from_penn_string($ex1);

#05
ok(1, "passed from_penn_string");

my $ll = $d->left_leaf();

isa_ok($ll, PACK, 'leftleaf');

is( $ll->root(), $d, 'root of leftleaf is orig root');

my @lineage = $ll->path_up_to($d);

cmp_ok(scalar @lineage, '==', 3, '3 elements in lineage of "Joe"');

is( (join '-', map { $_->tag() } @lineage), 'NNP-NP-S', 'NNP-NP-S');


my @lterms = $ll->get_all_terminals();

#10
cmp_ok(scalar @lterms, '==', 1);

is($lterms[0], $ll, 'll is own terminal');

my @words = $d->get_all_terminals();

cmp_ok(scalar @words, '==', 4, '4 terminal words under root');

my $string = '';
#13->20 (4x2 tests)
foreach (@words) {
    isa_ok($_, PACK);
    ok( $_->is_terminal() );
    $string .= ' ';
    $string .= $_->word();
}

is($string, ' Joe likes Bach .', "'Joe likes Bach .'");

my $ex30 = <<EOEX30;
(SQ
    (S
      (INTJ (UH Uh) )
      (, ,)
      (NP-SBJ (EX there) )
      (VP (BES 's)
        (ADVP (RB really) )
        (NP-PRD (DT a) (NN lot) )))
    (, ,)
    (SQ (VBZ is) (RB n't)
      (NP-SBJ (RB there) )
      (NP-PRD (-NONE- *?*) ))
    (. .) (-DFL- E_S) ))
EOEX30
my $funky = PACK->new();

isa_ok($funky, PACK, 'funky');

$funky->from_penn_string($ex30);
ok(1, 'able to read in "funky" string');

my @terminals = $funky->get_all_terminals();
cmp_ok(scalar @terminals, '==', 14, "14 terminal nodes in funky");
