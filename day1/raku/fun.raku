use v6.d;

# is this a comment
# how do i slurp file ?

my $str = slurp("../input");

my $len = chars($str);

say "length is $len";

# dont get it ? why not just str{i} say?
my $floor = 0;
my @comb = $str.comb;
loop (my $i = 0 ; $i < $len ; $i++ ) {
    #my $ch = $str.comb[$i];
    my $ch = @comb[$i];
    if ($ch eq '(') {
	$floor = $floor + 1 ;
    }
    if ($ch eq ')') {
	$floor = $floor - 1 ;
    }    
}

say "the floor is $floor";







