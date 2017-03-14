// declare CLASdata and ClasEvent objects at begginning of program
CLASdata *data = new CLASdata();
ClasEvent *clasevent = new ClasEvent("p:pi+:pi-");

// open a CLAS compressed ROOT file and grab the CLASdata object
TFile *inFile = new TFile("somefile.root");
TTree *tree = (TTree*)inFile->Get("Data");
data->Init(tree);

// loop over the events in the file
int num_entries = (int)tree->GetEntries();
int entry = 0;
while(entry < num_entries){
  data->GetEntry(entry); // load the current entry into memory
  
  if(data->IsFluxEvent()){ // is this event in a good trip interval?
    clasevent->SetClasEvent(data); // initialize clasevent for this event

    // loop over particle/photon combos
    while(clasevent->GetEvent(data)){

	// analyze the event...

    }
  }
  event++;
}

delete inFile;
inFile = 0;