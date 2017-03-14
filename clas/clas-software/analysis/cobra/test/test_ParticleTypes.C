// macro to test libPTypes.so
void test_ParticleTypes(){

  gSystem->Load("../lib/libPTypes.so"); // load the library
  ParticleTypes *ptypes = ParticleTypes::Instance(); // get instance

  cout << "________________________________________________________________"
       << "________________" << endl;

  cout << "This macro calls ParticleTypes::Print(). This function prints ALL"
       << " particle " << endl;
  cout << "info known to COBRA. This is read in from the file "
       << " ListOfParticles.lst located" << endl;
  cout << "in COBRASYS/ptypes. If no particle info (other than the default" 
       << " +,-,0 paritlces)" << endl;
  cout << "is output, then the file is not being found. If this happens, "
       << "check the COBRASYS" << endl;
  cout << "enviornment variable. If you need a particle which is not listed, "
       << "you can add it" << endl;
  cout << "by simply editing ListOfParticles.lst. Just add the info, quit "
       << "ROOT and restart" << endl;
  cout << "it, then run this macro again...the new info should appear." 
       << endl;
  
  cout << "________________________________________________________________"
       << "________________" << endl;
    
  ptypes->Print();
  cout << endl;
}
