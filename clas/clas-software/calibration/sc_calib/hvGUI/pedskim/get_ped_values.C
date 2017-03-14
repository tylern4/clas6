//filename should be from clasrun@clon01, ${CLON_PARMS}/pedman/Tfiles/sc.trans_clasprod_0${RUNNO}.txt
//blank lines and ALL header lines should be removed from the copied file such that it's just the numbers

void get_PED_VALUES(char *filename)
{
gSystem->Load("libPhysics");

int k,l,Left,Right;
double m,n;

ifstream f;
f.open(filename);
if(!f){
cout<< "There is no input file"<<filename<<endl;
exit(1);
}

ofstream f2;
f2.open("PED_LEFT2DB.dat");
ofstream f3;
f3.open("PED_RIGHT2DB.dat");


 cout<<"LEFT"<<"\tRIGHT"<<endl;
 for(int i=0;i<342;i++){
 f>>k>>l>>Left>>m>>Right>>n;
 f2<<Left<<endl;
 f3<<Right<<endl;
 cout<<Left<<"\t"<<Right<<endl;
 }


}
