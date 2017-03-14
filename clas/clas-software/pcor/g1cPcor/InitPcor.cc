#include <fstream.h>
#include <stdio.h>
#include <iostream>
#include <cstdlib>
#include <cstring>

int Nx,Ny,Nz,Cmax,dtime,Ngr;
double Q[(int)1e6],Xmin,Ymin,Zmin,Dx,Dy,Dz;

extern "C"{

  void InitPcor();

  void initpcor_();

}

void initpcor_(){
  // fortran wrapper
  InitPcor();
}

void InitPcor(){
  // Inititalizes the g1cPcor package

  int i;

  // if CLAS_PACK is not set to this packages directory, then change the path 
  // on the next line to the correct location of your copy of the 
  // Bfield_ascii.dat file.
  // Then uncomment the next line and comment out the other ifstream line
  // ifstream inFile("/home/????/packages/utilities/g1cPcor/Bfield_ascii.dat");  

  char fstr[200];
  sprintf(fstr,"%s/utilities/g1cPcor/Bfield_ascii.dat",getenv("CLAS_PACK"));
  ifstream inFile(fstr);  

  if(inFile.peek() == EOF){
    printf("File read error\n");
    //    cout << "File read error." << endl;
    return ;
  }
  inFile >> Nx;
  inFile >> Ny;
  inFile >> Nz;
  inFile >> Xmin;
  inFile >> Ymin;
  inFile >> Zmin;
  inFile >> Dx;
  inFile >> Dy;
  inFile >> Dz;
  inFile >> Cmax;
  inFile >> dtime;
  inFile >> Ngr;

  for(i=0; i < Ngr; i++){
    if(inFile.peek() == EOF){
      printf("File read error\n");
      //      cout << "File read error." << endl;
      return ;
    }
    inFile >> Q[i];
  }
  inFile.close();

}

