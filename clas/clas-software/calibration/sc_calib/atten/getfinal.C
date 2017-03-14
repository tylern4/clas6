#ifndef __CINT__
#include "ROOT.h"
#endif

void getfinal(int runno, char* RunIndex){
//gROOT->Reset();
//gStyle->SetPalette(1);

cout<<runno<<endl;


char s[120];
int locNumChannels;

if(runno < 55357){
  sprintf(s,"SC_CALIBRATIONS");
  locNumChannels = 288;
}else{
  sprintf(s,"SC_CALIBRATIONS_V2");
  locNumChannels = 342;
}

char * ss="delta_T";
char * i="left_right";
char * r= new char[10];
sprintf(r,"%d",runno);

cout << "created" << endl;

char fname[120];
sprintf(fname,"left_right_fromCALDB.dat");
char command_rm[120];
sprintf(command_rm,"rm -f %s",fname);
gSystem->Exec(command_rm);

cout << "removed" << endl;

char command_get[250];
sprintf(command_get,"/group/clas/tools/caldb/caldb_show_constants_run.pl s=%s ss=%s i=%s r=%s it=%s | cat > %s", s, ss, i, r, RunIndex,fname);
cout << "command is:" << endl << command_get << endl << endl;
gSystem->Exec(command_get);

cout << "consts gotten" << endl;

ifstream datainput_old(fname);
if (!datainput_old.is_open()) return;
//get rid off two text lines
char textline[120];
datainput_old.getline(textline,79); 
datainput_old.getline(textline,79);

cout << "datalized" << endl;

char filename[80];
sprintf(filename,"delta_t_runnumber.dat");
ifstream datainput_new(filename);
if (!datainput_new.is_open()) {cout << "can't open delta_t.dat"<< endl; return; }

cout << "g32wf" << endl;

char foname[120];
sprintf(foname,"left_right_final.dat");
ofstream dataoutput(foname);

cout << "pooploop" << endl;

for (int l=0;l < locNumChannels;l++){
	double oldvalue,delta_t,newvalue;
	datainput_old>>oldvalue;
	datainput_new>>delta_t;
	if (delta_t==-37.12500) newvalue=0; // for dead chanels
	else newvalue=oldvalue+delta_t;
	cout <<  "chanel "<< l+1 << ": "<<oldvalue << " + "<<delta_t << " -> " <<newvalue <<endl;
	dataoutput<<newvalue<<endl;
	
}

cout << "close" << endl;

dataoutput.close();
datainput_old.close();
datainput_new.close();
}
