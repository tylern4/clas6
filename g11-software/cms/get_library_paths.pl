@a=();
$dir_linker="";
if(-e$ARGV[0]){
    open(COMPILER,"more $ARGV[0] |");
    while(<COMPILER>){
	chop;
	if($_=~"ld"&&$_=~"-linker"){
	    @a=split(/\s+/,$_);
	    foreach$e(@a){
		if($e=~"-L"){
		    $dir_linker=$e." ".$dir_linker;
		}
	    }
	}
    }
    close(COMPILER);
    print STDOUT $dir_linker;
}
