#!/usr/bin/perl
#run_spp_eg1.pl SL ph Q1 1 W1 (first argument model=SL,DMT,MAID second=phi,th,Q third=Q1,Q2,Q3 fourth=1..17 fifth=W1,W2.. )
die "Not enough arguments\n" if @ARGV<5;
$input="input.txt";
open(ROOTOUT,"> $input");
%model=("AO",1,"latham",2,"SL",3,"DMT",4,"MAID98",5,"MAID2000",6,"MAID07",7,"MAID03",8);
$channel=1;
$res_in_models=0;

#%Q2binmin=("Q1",0.223,"Q2",0.379,"Q3",0.540);
#%Q2binmax=("Q1",0.379,"Q2",0.540,"Q3",0.770);

%Q2binmin=("Q1",0.187,"Q2",0.317,"Q3",0.452);
%Q2binmax=("Q1",0.317,"Q2",0.452,"Q3",0.770);
#%Q2binmax=("Q1",0.317,"Q2",0.452,"Q3",0.645);
%Wbinmin=("W1",1.1,"W2",1.15,"W3",1.2,"W4",1.25,"W5",1.3,"W6",1.4,"W7",1.5,"W8",1.6);
%Wbinmax=("W1",1.1499,"W2",1.199,"W3",1.2499,"W4",1.299,"W5",1.399,"W6",1.499,"W7",1.599,"W8",1.699);

$nthetabins=10.;
$steptheta=2./$nthetabins;

if($ARGV[1] eq "ph"){
	for($index=0;$index<$nthetabins;$index++){
		$binmin[$index]=-1.+$index*$steptheta;
		$binmax[$index]=-1.+$steptheta+$index*$steptheta;

	}
#	@binmin=(-1.,-0.8,-0.6,-0.4,-0.2,0.0,0.2,0.4,0.6,0.8);
#	@binmax=(-0.8,-0.6,-0.4,-0.2,0.0,0.2,0.4,0.6,0.8,1);
		$binsize=15.;#this is actuall n bins
		$var=11;
}
elsif(($ARGV[1] eq "th") || ($ARGV[1] eq "Q")){
	@binmin=(-180,-156,-132,-108,-84,-60,-36,-12,12,36,60,84,108,132,156);
	@binmax=(-156,-132,-108,-84,-60,-36,-12,12,36,60,84,108,132,156,180);
	if($ARGV[1] eq "th"){
			$binsize=$steptheta;
			$var=12;
	}
	else {
		$binsize=0.2;
		$var=7;
	}
}
else{
print "unrecongnized option $ARGV[1]\n";
}
if($ARGV[4] eq "W1" || $ARGV[4] eq "W2" || $ARGV[4] eq "W3"  || $ARGV[4] eq "W4"){
$wbinsize=0.05;#binsize 0.05 Wmin-Wmax-0.0499 so only one loop..still does int over W in 10 steps.
}
else {
$wbinsize=0.1;#binsize 0.05 Wmin-Wmax-0.0499 so only one loop..still does int over W in 10 steps.
}
$beamen=1.607;
$angmon=5;
$spline=2;
if(($ARGV[0] eq "MAID98") || ($ARGV[0] eq "MAID03") || ($ARGV[0] eq "MAID2000")|| ($ARGV[0] eq "MAID07")){
if(($ARGV[1] eq "th") || ($ARGV[1] eq "ph")){
print ROOTOUT "$model{$ARGV[0]}\n$channel\n$res_in_models\n$var\n$beamen\n$Q2binmin{$ARGV[2]}\n$Q2binmax{$ARGV[2]}\n$wbinsize\n$Wbinmin{$ARGV[4]}\n$Wbinmax{$ARGV[4]}\n$binmin[$ARGV[3]-1]\n$binmax[$ARGV[3]-1]\n$binsize\n$angmon\n$spline\n";
}
else {
print ROOTOUT "$model{$ARGV[0]}\n$channel\n$res_in_models\n$var\n$beamen\n$binsize\n$Q2binmin{\"Q1\"}\n$Q2binmax{\"Q3\"}\n$wbinsize\n$Wbinmin{$ARGV[4]}\n$Wbinmax{$ARGV[4]}\n$binmin[$ARGV[3]-1]\n$binmax[$ARGV[3]-1]\n$angmon\n$spline\n";
}
}
elsif($ARGV[0] eq "SL"|| ($ARGV[0] eq "DMT")){
if(($ARGV[1] eq "th") || ($ARGV[1] eq "ph")){
print ROOTOUT "$model{$ARGV[0]}\n$channel\n$var\n$beamen\n$Q2binmin{$ARGV[2]}\n$Q2binmax{$ARGV[2]}\n$wbinsize\n$Wbinmin{$ARGV[4]}\n$Wbinmax{$ARGV[4]}\n$binmin[$ARGV[3]-1]\n$binmax[$ARGV[3]-1]\n$binsize\n$angmon\n$spline\n";
}
else {
print ROOTOUT "$model{$ARGV[0]}\n$channel\n$var\n$beamen\n$binsize\n$Q2binmin{\"Q1\"}\n$Q2binmax{\"Q3\"}\n$wbinsize\n$Wbinmin{$ARGV[4]}\n$Wbinmax{$ARGV[4]}\n$binmin[$ARGV[3]-1]\n$binmax[$ARGV[3]-1]\n$angmon\n$spline\n";
}


}
close(ROOTOUT);

if($ARGV[1] eq "th"){
$out="th$ARGV[4]$ARGV[2]ph$ARGV[3]_$ARGV[0]";
}
elsif($ARGV[1] eq "ph"){
$out="ph$ARGV[4]$ARGV[2]th$ARGV[3]_$ARGV[0]";
}
else {
$out="Qph$ARGV[3]_$ARGV[4]_$ARGV[0]";

}
$end=".out";
$out_int=$out."int".$end;
$out_exact=$out."exact".$end;
$out_disp=$out."disp".$end;
unlink $out if -e $out;
system("spp_int_eg4 < $input");
system("mv spp_int_pol.out $out_int");
system("mv spp_int_pol_exactdisplay.out $out_disp");
system("mv spp_int_pol_exact.out $out_exact");




