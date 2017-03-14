/** @file PlotCosThetaCM.C
 *  @brief Utility macro to plot \f$cos(\theta_{CM})\f$ for a particle stored
 *         in an Event file.
 *
 *  @param file  Input Event file name
 *  @param part  Particle name (ex. @a pi-)
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
 *  root [0] PlotCosThetaCM("43582_p_pi+_pi-.root","pi+");
 *
 *  Which produces the following output,
 *
 *  <img src="cos_theta_cm_pi+.gif">
 *
 */
//_____________________________________________________________________________

void PlotCosThetaCM(const string &__file,const string &__part,
		    int __maxEvents = -1){

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

  // Declare the histo
  char h_title[100];
  string latex_name = ParticleTypes::Instance()->GetParticle(__part).LaTexName(); 
  if(latex_name[0] == '\\') latex_name[0] = '#'; // TLatex uses # instead  
  sprintf(h_title,"cos(#theta_{CM}^{%s})",latex_name.c_str());
  TH1F *hCosThetaCM = new TH1F("hCosThetaCM",h_title,50,-1,1);

  TLorentzVector p4tot,p4part;
  TVector3 boost;

  for(int entry = 0; entry < num_entries; entry++){

    tree->GetEntry(entry); // read next event into memory

    p4tot = event->TotalP4(); // total 4-momentum
    p4part = event->GetParticle(__part).P4(); // particle's 4-momentum

    boost = -p4tot.BoostVector(); // boost vector

    p4part.Boost(boost); // boost to CM frame

    hCosThetaCM->Fill(p4part.CosTheta());
  }

  TCanvas *can = new TCanvas("can","Event Info",200,200,600,600);
  can->cd(0);

  hCosThetaCM->SetFillColor(11);
  hCosThetaCM->DrawCopy();

  can->Update();
}
//_____________________________________________________________________________
