/** @file PlotEventInfo.C
 *  @brief Utility macro to plot some global event information for events
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
 *  root [0] PlotEventInfo("43582_p_pi+_pi-.root");
 *
 *  Which produces the following output,
 *
 *  <img src="event_info.gif">
 *
 */
//_____________________________________________________________________________

void PlotEventInfo(const string &__file,int __maxEvents = -1){

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

  // Declare the histos  
  TH3F *hVertex = new TH3F("hVertex","Vertex",50,-10,10,50,-10,10,100,-75,25);
  TH1F *hW = new TH1F("hW","W",100,0.93827,3.5);
  TH1F *hCL = new TH1F("hCL","Confidence Level",100,0.,1.);
  TH1F *hVertexTime = new TH1F("hVertexTime","Vertex Time",100,-10,40);

  TVector3 vertex;

  for(int entry = 0; entry < num_entries; entry++){

    tree->GetEntry(entry); // read next event into memory

    vertex = event->Vertex();
    hVertex->Fill(vertex.X(),vertex.Y(),vertex.Z());
    hW->Fill(event->W());
    hCL->Fill(event->Prob());
    hVertexTime->Fill(event->VertexTime());

  }

  // Set up canvas
  TCanvas *can = new TCanvas("can","Event Info",200,200,600,600);
  can->Divide(2,2);
    
  can->cd(1);
  hVertex->SetFillColor(11);
  hVertex->DrawCopy();

  can->cd(2);
  hW->SetFillColor(11);
  hW->DrawCopy();

  can->cd(3);
  hCL->SetFillColor(11);
  hCL->DrawCopy();

  can->cd(4);
  hVertexTime->SetFillColor(11);
  hVertexTime->DrawCopy();

  can->cd(0);
  can->Update();
}
