/** @file PlotVertex.C
 *  @brief Utility macro to plot the event vertex position for events
 *         in an Event file.
 *
 *  @param file  Input Event file name
 *  @param maxEvents  Maximum number of events to process (defaults to all)
 *
 *  This macro is loaded automatically by the cobra_rootlogon.C script. The
 *  input Event file must have a TTree named @a T with a branch called 
 *  @a event that stores Event class objects.
 *
 *  <b>Example Usage</b>: <br>
 *
 *  Produce the file @a 43582_p_pi+_pi-.root using 
 *  <a class="el" href="Skim__p__pi+__pi-_8cc-source.html">Skim_p_pi+_pi-.cc
 *  </a>, a compressed 
 *  g11 ROOT file for run 43582. Then start ROOT and type,
 *
 *  root [0] PlotVertex("43582_p_pi+_pi-.root");
 *
 *  Which produces the following output,
 *
 *  <img src="vertex.gif">
 *
 */
//_____________________________________________________________________________

void PlotEventVertex(const string &__file,int __maxEvents = -1){

  gROOT->Reset();  
  gStyle->SetTitleH(0.075);
  gStyle->SetTitleW(0.2);

  TFile inFile(__file.c_str()); // open the input file
  TTree *tree = (TTree*)inFile.Get("T"); // grab the tree

  Event *event = new Event();
  tree->SetBranchAddress("event",&event); // read tree info into event

  int num_entries = (int)tree->GetEntries();
  if(__maxEvents > 0){
    if(__maxEvents < num_entries) num_entries = __maxEvents;
  }

  // Declare the histos  
  TH3F *hVertex = new TH3F("hVertex","Vertex",50,-10,10,50,-10,10,100,-75,25);
  TH1F *hVx = new TH1F("hVx","X",100,-10,10);
  TH1F *hVy = new TH1F("hVy","Y",100,-10,10);
  TH1F *hVz = new TH1F("hVz","Z",100,-75,25);
  TVector3 vertex;

  for(int entry = 0; entry < num_entries; entry++){

    tree->GetEntry(entry); // read next event into memory

    vertex = event->Vertex();
    hVertex->Fill(vertex.X(),vertex.Y(),vertex.Z());
    hVx->Fill(vertex.X());
    hVy->Fill(vertex.Y());
    hVz->Fill(vertex.Z());
  }

  // Set up canvas
  TCanvas *can = new TCanvas("can","Event Info",200,200,1000,600);
  can->Divide(3,1);
    
  can->cd(1);
  hVx->SetFillColor(11);
  hVx->DrawCopy();

  can->cd(2);
  hVy->SetFillColor(11);
  hVy->DrawCopy();

  can->cd(3);
  hVz->SetFillColor(11);
  hVz->DrawCopy();

  can->cd(0);
  can->Update();
}
