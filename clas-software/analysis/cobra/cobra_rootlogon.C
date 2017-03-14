// ROOT logon script to load COBRA when ROOT starts
void cobra_rootlogon(){

  cout << "Loading ROOT Physics lib...";
  // To use classes such as TLorentzVector
  string lib_list = gSystem->GetLibraries();
  if(lib_list.find("Physics") == string::npos) // this means it wasn't in list
    gSystem->Load("libPhysics");
  cout << "done." << endl;

  
  string cobrasys = getenv("COBRASYS");

  cout << "Loading COBRA libs...";
  
  // Load the ROOT compatible libs
  string cobra_lib = cobrasys + "/lib/";
  gSystem->Load((cobra_lib + "libPTypes.so").c_str()); // for ParticleTypes
  gSystem->Load((cobra_lib + "libPArray.so").c_str()); // for ParticleArray
  gSystem->Load((cobra_lib + "libEvent.so").c_str()); // for Event
  gSystem->Load((cobra_lib + "libClasRuns.so").c_str()); // for ClasRuns

  cout << "done." << endl;

  string cur_dir = gSystem->pwd();
  // Load cobra utility macros
  cout << "Loading COBRA utils...";
  int err;
  TSystemDirectory dir("/",(cobrasys + "/utils/").c_str());
  TList *files = dir.GetListOfFiles();

  string fileName;
  string::size_type size;
  if(files) {
    TIter next(files);
    TSystemFile *file;
    while ((file=(TSystemFile*)next())) {
      fileName = file->GetName();
      size = fileName.size();
      if(fileName.find(".C") != string::npos){
	// it ends with .C, load it
	gROOT->LoadMacro((cobrasys + "/utils/" + fileName).c_str(),&err);
      }
    }
  }
  gSystem->cd(cur_dir.c_str());
  cout << "done." << endl;
}
