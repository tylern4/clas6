{
// gSystem->Load("/home/anciant/CLAS/slib/Linux/libROOTmapmanager.so")

TMapMan m("TAG_CALIB.map");
m.Print();
m.SetMap("tag_t");
m.Print();
m.SetMap("ci");
m.Print(12456);                                                          ;
float ci[121];
m.GetFloatArray(121,ci,12456);
{for (int k=0;k<20;k++) cout <<k<<" : "<<ci[k]<<endl;}
{for (int k=0;k<80;k++) ci[k]+=1;}
m.PutFloatArray(121,ci,12456);
m.Print(12456);
}
