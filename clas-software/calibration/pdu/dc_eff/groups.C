#include <iostream>
#include <fstream>
#include <iomanip>
using namespace std;

extern "C"
{
 #include <map_manager.h>
}

const int LAYERS       = 36;
const int SECTORS      = 6;
const int MAX_WIRES    = 192;
char   *ASCIIoutfile   = NULL;
char   *GROUPSinfile   = NULL;
char   *inputFile      = NULL;
ofstream OUT;                    // ascii file to put in map
int WRITING            = 0;

int main(int argc,char **argv)
{
 double efficiency[SECTORS][LAYERS][MAX_WIRES]; // wire efficiency
 int NUMPAR = 6912;
 float status[NUMPAR];

 int WRITING = 0;

 void PrintUsage(char *processName);
 char *argptr = NULL;
 cout << endl;
 for (int i = 1; i < argc; i++)
 {
  argptr = argv[i];
  if(*argptr == '-')
  {
   argptr++;
   switch (*argptr)
   {
    case 'h':
     PrintUsage(argv[0]);
     exit(0);
    break;

    case 'o':
     ASCIIoutfile = (++argptr);
     cout << " Writing ASCII output: " << ASCIIoutfile << endl;
     OUT.open(ASCIIoutfile, ios::out);
    break;

    case 'g':
     GROUPSinfile = (++argptr);
     cout << " Opening GROUPS file " <<  GROUPSinfile << endl;
    break;

    case 'W':
     WRITING = atoi(++argptr);
     cout << " Attention: values will be written in database (map)! for run " << WRITING << endl;
    break;
   }
  }
  else
  {
   inputFile = argptr;
   cout << " Analyzing input file: " << inputFile << endl;
  }
 }
 cout << endl;

 // Reading input file
 ifstream IN(inputFile);
 while(IN)
  for(int layer = 0; layer < LAYERS; layer++)
   for(int wire = 0; wire < MAX_WIRES; wire++)
    for(int sec = 0; sec < SECTORS; sec++)
     IN >> efficiency[sec][layer][wire];
 IN.close();

 int w,W,l,L,S,G;    // wire min/max   layer min/max   Sector  Group
 double mean;
 // Reading group file
 ifstream ING(GROUPSinfile);
 ING >> S >> w >> W >> l >> L >> G ;
 while(ING)
 {
  mean = 0;

  for(int ll=l; ll<L+1; ll++)
   for(int ww=w; ww<W+1; ww++)
    mean = mean + efficiency[S-1][ll-1][ww-1];

  mean = mean/((L-l+1)*(W-w+1));

  if (G!=0) cout << " Mean for group " << G << ": " << mean << "   value in map: "  << mean + G*1000 << endl;
  if (G==0) cout << " Mean for group " << G << ": " << mean << "   value in map: 0" << endl;
  for(int ll=l; ll<L+1; ll++)
   for(int ww=w; ww<W+1; ww++)
    {
     if (G!=0) efficiency[S-1][ll-1][ww-1] = mean + G*1000 ;
     if (G==0) efficiency[S-1][ll-1][ww-1] = 0 ;
    }


  ING >> S >> w >> W >> l >> L >> G ;
 }



 //Writing map file
 if(ASCIIoutfile)  cout << endl << " Saving gpp efficiency to file " << ASCIIoutfile << " ..." << endl;

 for(int layer = 0; layer < LAYERS; layer++)
  for(int wire = 0; wire < MAX_WIRES; wire++)
  {
   for(int sec = 0; sec < SECTORS; sec++) OUT << efficiency[sec][layer][wire] << "\t";
   OUT << endl;
  }

 if(ASCIIoutfile) OUT.close();
 cout << " Done" << endl << endl;

 // Writing in database. There are 18 entries, 3 regions for each sector
 char Item[100];
 char *MapDir;
 char System[300];
 int dummy;

 if(WRITING)
 {
  MapDir = (char *) getenv("CLAS_PARMS");
  sprintf(System,"%s/Maps/GPP.map", MapDir);
  cout << " Now writing constants to database (map) SYSTEM GPP, SUBSYSTEM DC_WIRE " << endl;


  for(int sec = 0; sec < SECTORS; sec++)
  {
   sprintf(Item, "sector%1d", sec+1);
   cout << " writing item " << Item << endl;

   for(int layer = 0; layer < LAYERS; layer++)
    for(int wire = 0; wire < MAX_WIRES; wire++)
     status[wire + layer*192] = efficiency[sec][layer][wire];

     map_put_float(System, "DC_WIRE", Item, NUMPAR, status, WRITING);
  }
 }

 cout << " Done" << endl << endl;
 return (0);
}


void PrintUsage(char *processName)
{
 cout << endl << processName << " will read efficiency table from inputfile and group of layer and wires from GROUP file" << endl;
 cout << " then average the entries and write 1000*GROUP + efficiency in table." << endl;
 cout << " Group file format: " << endl;
 cout << "\t SECTOR   wire min   wire max    min layer    max layer   GROUP ID" << endl;
 cout << " For example,in order to group wires 60 to 80, layers 20 to 30 of sector 5 in group number 2" << endl;
 cout << " GROUP file should contain the line:" << endl;
 cout << "\t 5 60 80 20 30 2" << endl;
 cout << " If the average efficiency for those wires is 72%, the number 2000.72 will be written" << endl;
 cout << " in output file for all those wires." << endl;
 cout << endl << " Usage:" << endl;
 cout << processName << " <options> <inputfile>" << endl;
 cout << "\toptions are:" << endl;
 cout << "\t-h\t\tPrint this message.\n";
 cout << "\t-o<filename> \tOutput ASCII file name." << endl;
 cout << "\t-g<filename> \tGROUPS input file name." << endl;
 cout << "\t-W<runnumber>\tWrite constants in database (map), GPP system, " << endl;
 cout << endl;
}






























