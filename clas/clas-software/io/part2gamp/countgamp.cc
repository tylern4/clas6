#include <iostream>

using namespace std;

main()
{
  int npart;
  int Nevent = 0;
  int pT,q;
  double px,py,pz,t;

  while(!(cin>>npart).eof()) {
    for (int i = 0; i < npart; ++i) {
      cin >> pT >> q >> px >> py >> pz >> t;
    }
    if (!(Nevent++ % 100))
      cerr << Nevent << "\r" << flush;

  }
  cerr << Nevent << endl;
}

