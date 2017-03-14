#include <iostream>
#include <string>
#include <vector>
#include <stdio.h>
#include <stdlib.h>
extern "C" {
#include <map_manager.h>
}

void main(int argc,char **argv)
{

  string PARMS(getenv("CLAS_PARMS"));
  string SC;
  string SUB("delta_Tu");
  string ITEM("paddle2paddle");
  string MAP;
  int length = 288;
  int run = atoi(argv[1]);
  int firsttime;
  float floatarray[288];

  vector<double> p2p;

  // get the existing constants

  cerr << "CLAS_PARMS: " << PARMS.c_str() << endl;
  MAP = PARMS + "/Maps/SC_CALIBRATIONS.map";
  
  cerr << "MAP: " << MAP.c_str() << endl;

  map_get_float(MAP.c_str(), SUB.c_str(), ITEM.c_str(), length, floatarray,run, &firsttime); 
  

  cerr << "Existing p2p constants in the map: from run " << firsttime << "\n" << endl;


  for(int i=0; i < length ; i++){
    int tube = i % 48 + 1;
    int sec = i/48 + 1;
    cerr << i + 1 << "\t" << sec << "\t" << tube << "\t" << floatarray[i] << endl;
  }


  

  // now read in new constants

  {
    double a;
    int sec,tube;

    cerr << "New values:\n" << endl;

    while ( !(cin >> sec >> tube >> a).eof()) {
      p2p.push_back(a);
    }

    for (int indx = 0; indx < p2p.size(); indx++) {
      tube = indx % 48 + 1;
      sec = indx/48 + 1;
      cerr << indx + 1 << "\t" << sec << "\t" << tube << "\t" << p2p[indx] << endl;
    }
  }



  // now write out the new constants


 for (int indx = 0; indx < p2p.size(); indx++) {
      cout << indx + 1 << "\t" << p2p[indx] - floatarray[indx] << endl;
    }
     


}

  
