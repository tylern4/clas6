// macro to test libClasRuns.so
void test_ClasRuns(){

  gSystem->Load("../lib/libClasRuns.so"); // load the library
  ClasRuns *clasRuns = ClasRuns::Instance(); // get instance

  cout << "________________________________________________________________"
       << "________________" << endl;

  cout << "This macro calls ClasRuns::Print(). This function prints ALL run "
       << "period info " << endl;
  cout << "known to COBRA. This is read in from the file ClasRunInfo.lst "
       << "located " << endl;
  cout << "in COBRASYS/clasruns. If no run info is printed, then the file "
       << "either can't be " << endl;
  cout << "located or can't be opened...if this is the case, make sure your "
       << "COBRASYS " << endl;
  cout << "enviornment variable is properly set. If a run you need is not on "
       << "the list, you " << endl;
  cout << "can add it by editing the ClasRunInfo.lst file. Simply add the "
       << "info, then quit" << endl;
  cout << "ROOT and restart it, then run this macro again...the new run info "
       << "should appear." << endl;
  cout << "________________________________________________________________"
       << "________________" << endl;
  
  clasRuns->Print();

  cout << endl;    
}
