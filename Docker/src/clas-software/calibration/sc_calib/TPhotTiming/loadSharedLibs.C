Int_t loadSharedLibs() {
  Int_t status = 0;
  cout << "Loading Libraries" << endl;

  TString LibList( gSystem->GetLibraries() );

  if ( ! LibList.Contains("libTree.so") )
    status |= gSystem->Load("libTree.so") ;
  if ( ! LibList.Contains("libmysqlclient.so") )
    status |= gSystem->Load("libmysqlclient.so") ;
  if ( ! LibList.Contains("Physics.so") )
    status |= gSystem->Load("libPhysics.so") ;
  if ( ! LibList.Contains("EG.so") )
    status |= gSystem->Load("libEG.so") ;  
  if ( ! LibList.Contains("ClasTool") )
    status |= gSystem->Load("libClasTool.so");
  if ( ! LibList.Contains("libMapUtils.so") )
    status |= gSystem->Load("libMapUtils.so");
  if ( ! LibList.Contains("libPhotTiming.so" ) )
       status |= gSystem->Load("libPhotTiming.so");

  return status;
}
