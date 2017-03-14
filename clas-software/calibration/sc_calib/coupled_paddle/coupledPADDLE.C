//Script to do coupledPADDLE calibrations for TOF for photon runs
void coupledPADDLE()
{
gROOT->Reset();
gStyle->SetPalette(1);
gStyle->SetFrameFillColor(19);
gStyle->SetNumberContours(30);
gStyle->SetOptStat(1111);
gStyle->SetOptFit(1);

ofstream TOdb1,TOdb2;
ifstream dbvals1,dbvals2,offsets1,offsets2;

dbvals1.open("coupledpaddle1_INDB.dat");
if(!dbvals1){
cout<<"there is no input file "<<endl;
exit(1);
}

dbvals2.open("coupledpaddle2_INDB.dat");
if(!dbvals2){
cout<<"there is no input file "<<endl;
exit(1);
}

offsets1.open("coupled1.dat");
if(!offsets1){
cout<<"there is no input file "<<endl;
exit(1);
}

offsets2.open("coupled2.dat");
if(!offsets2){
cout<<"there is no input file "<<endl;
exit(1);
}

TOdb1.open("COUPLEDPADDLE1TO_THE_DB.dat");
TOdb2.open("COUPLEDPADDLE2TO_THE_DB.dat");

float from_db1[54], from_db2[54];
float off1[54], off2[54];
float to_db1[54], to_db2[54];

 //READ IN CALDB VALUES FROM FILE
 for(int i=0;i<54;i++){
 dbvals1>>from_db1[i];
 dbvals2>>from_db2[i];
 offsets1>>off1[i];
 offsets2>>off2[i];
 }

 //CALCULATE NEW COUPLED PADDLE CONSTANTS
 cout<<"DB:\t"<<"OFFSET:\t"<<"NEW:"<<endl;
 for(int i=0;i<54;i++){
 to_db1[i]=from_db1[i]-off1[i];
 to_db2[i]=from_db2[i]-off2[i];
 TOdb1<<to_db1[i]<<endl;
 TOdb2<<to_db2[i]<<endl;
 cout<<"COUPLED1:"<<from_db1[i]<<"\t"<<off1[i]<<"\t"<<to_db1[i]<<endl;
 cout<<"COUPLED2:"<<from_db2[i]<<"\t"<<off2[i]<<"\t"<<to_db2[i]<<"\n"<<endl;

 }

}



