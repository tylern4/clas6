/** @file PlotPhotonInfo.C
 *  @brief Utility macro to plot some tagged photon information for events
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
 *  root [0] PlotPhotonInfo("43582_p_pi+_pi-.root");
 *
 *  Which produces the following output,
 *
 *  <img src="photon_info.gif">
 *
 */
//_____________________________________________________________________________

void PlotPhotonInfo(const string &__file,int __maxEvents = -1){

  gROOT->Reset();  
  gStyle->SetTitleH(0.075);

  TFile inFile(__file.c_str()); // open the input file
  TTree *tree = (TTree*)inFile.Get("T"); // grab the tree

  Event *event = new Event();
  tree->SetBranchAddress("event",&event); // read tree info into event

  int num_entries = (int)tree->GetEntries();
  if(__maxEvents > 0){
    if(__maxEvents < num_entries) num_entries = __maxEvents;
  }
  Photon photon;

  // Declare the histos  
  TH1F *hE = new TH1F("hE","Photon Energy",100,0.4,5.7);
  TH1F *hE_Id = new TH1F("hE_Id","E-counter Id",767,0.5,767.5);
  TH1F *hT_Id = new TH1F("hT_Id","T-counter Id",121,0.5,121.5);
  TH1F *hVertexTime = new TH1F("hVertexTime","Vertex Time",100,-10.,40.);

  for(int entry = 0; entry < num_entries; entry++){

    tree->GetEntry(entry); // read next event into memory
    photon = event->GetPhoton(); // get the photon 

    hE->Fill(photon.E()); // energy
    hE_Id->Fill(photon.E_Id()); // E-paddle id
    hT_Id->Fill(photon.T_Id()); // T-paddle id
    hVertexTime->Fill(photon.VertexTime()); // vertex time
  }

  // Set up canvas
  TCanvas *can = new TCanvas("can","Photon Info",200,200,600,600);
  can->Divide(2,2);

  can->cd(1);
  hE->SetFillColor(11);
  hE->DrawCopy();
  can->cd(2);
  hVertexTime->SetFillColor(11);
  hVertexTime->DrawCopy();
  can->cd(3);
  hE_Id->SetFillColor(11);
  hE_Id->DrawCopy();
  can->cd(4);
  hT_Id->SetFillColor(11);
  hT_Id->DrawCopy();

  can->cd(0);
  can->Update();
}
//_____________________________________________________________________________
