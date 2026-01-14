use v6.d;

# is this a comment
# how do i slurp file ?

my $str = slurp("../input");

#remove any spaces from input
$str = $str.trans('\n' => '');
$str = $str.trans(' '=> '');
$str = $str.trans('"'=> '');


my $len = chars($str);

say "length is $len";

# dont get it ? why not just str{i} say?
my $floor = 0;

# we need to remove any other chars except ( ) 
my @comb = $str.comb;

sub t_twice ($x) {
    return 2 * $x ;
}

say t_twice(1);
say t_twice(2);
say t_twice(3);
say t_twice(4);
say t_twice(5);

sub check () {
    loop (my $i = 0 ; $i < $len ; $i++ ) {
	my $ch = @comb[$i];
	if ($ch eq '(') {
	    # ok
	}
	elsif ($ch eq ')') {
	    # ok
	}
	else {
	    say "the character is $ch";
	    return -1;
	}
    }
    return 1;
}

# some random block
{
    my $foo2 = check;
    say "check yields $foo2";
}

#say "foo2 is bound to $foo2" ;



# # how do we break out of a loop ?
sub foo () {
# return i + 1 
    loop (my $i = 0 ; $i < $len ; $i++ ) {
	#my $ch = $str.comb[$i];
	my $ch = @comb[$i];
	if ($ch eq '(') {
	    $floor = $floor + 1 ;
	}
	if ($ch eq ')') {
	    $floor = $floor - 1 ;
	    if ($floor < 0) {
		say "santa has entered the basement at char index $i";
		return $i + 1 ;
	    }
	}
    }
}

say "the floor is {foo()}";







