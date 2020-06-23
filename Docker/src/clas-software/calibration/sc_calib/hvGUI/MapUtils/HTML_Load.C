{
SourceDirs.Append(":../MapUtils"); // Tell root where the sources are
mHtml->SetSourceDir(SourceDirs);  
// Load the lib.
if(Load_A_Lib("libMapUtils.so")){cerr << "ERROR loading.\n";gSystem->Exit(1);}

TMapUtils   *mmaputil  = new TMapUtils();    mHtml->MakeClass("TMapUtils"); 
TDBItem     *mdbitem   = new TDBItem();            mHtml->MakeClass("TDBItem"); 
TQuery    *mquery2  = new TQuery();           mHtml->MakeClass("TQuery"); 
TRunIndex *mtrind = new TRunIndex();          mHtml->MakeClass("TRunIndex"); 
}
