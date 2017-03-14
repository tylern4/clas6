/** @file PlotPulls.C
 *  @brief Utility macro to plot pull distributions for events
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
 *  root [0] PlotPulls("43582_p_pi+_pi-.root");
 *
 *  Which produces the following output,
 *
 *  <img src="pulls.gif">
 *
 */
//_____________________________________________________________________________

void PlotPulls(const string &__file,int __maxEvents = -1){

  gROOT->Reset();  
  gStyle->SetTitleH(0.2);

  TFile inFile(__file.c_str()); // open the input file
  TTree *tree = (TTree*)inFile.Get("T"); // grab the tree

  Event *event = new Event();
  tree->SetBranchAddress("event",&event); // read tree info into event

  Particle particle;

  int num_entries = (int)tree->GetEntries();
  if(__maxEvents > 0){
    if(__maxEvents < num_entries) num_entries = __maxEvents;
  }

  // grab 1st entry to see how many particles there are
  tree->GetEntry(0);
  const int num_particles = event->NumParticles();

  string p_names[num_particles];
  for(int p = 0; p < num_particles; p++) 
    p_names[p] = event->GetElement(p).Name(); // get the particle names

  // Set up the histos
  char h_name[100],h_title[100];
  string latex_name;

  TH1F *hPull_Eg = new TH1F("hPull_Eg","E_{#gamma}",50,-5.,5.);
  TH1F *hPull[10][3]; // even with const, root wouldn't allow num_particles
  for(int p = 0; p < num_particles; p++){
    latex_name = event->GetElement(p).LaTexName(); 
    if(latex_name[0] == '\\') latex_name[0] = '#'; // TLatex uses # instead

    sprintf(h_name,"hPull_%d_%d",p,1);  
    sprintf(h_title,"p_{%s}",latex_name.c_str());
    hPull[0][p] = new TH1F(h_name,h_title,50,-5.,5.);

    sprintf(h_name,"hPull_%d_%d",p,2);  
    sprintf(h_title,"#lambda_{%s}",latex_name.c_str());
    hPull[1][p] = new TH1F(h_name,h_title,50,-5.,5.);

    sprintf(h_name,"hPull_%d_%d",p,3);  
    sprintf(h_title,"#phi_{%s}",latex_name.c_str());
    hPull[2][p] = new TH1F(h_name,h_title,50,-5.,5.);
  }
   
  // loop over events
  for(int entry = 0; entry < num_entries; entry++){
    tree->GetEntry(entry);

    if(event->Prob() > 0.02){

      hPull_Eg->Fill(event->GetPhoton().Pull());

      for(int p = 0; p < num_particles; p++){

	particle = event->GetParticle(p);
	for(int pull = 0; pull < 3; pull++){
	  hPull[pull][p]->Fill(particle.Pull(pull));
	}
      }
    }
  }

  // define the fitting function
  TF1 *fgaus = new TF1("fgaus","gaus",-5.,5.);
  fgaus->SetLineColor(2);

  // Set up the canvas
  TCanvas *can = new TCanvas("can","Pull Distributions",200,200,600,600);
  can->Divide(3,num_particles+1);

  for(int p = 0; p < num_particles; p++){
    cout << "________________________________________________________________"
	 << "________________" << endl;
    cout << "Fitting " << p_names[p] << " pulls..." << endl;
    for(int pull = 0; pull < 3; pull++){
      can->cd(3*p + pull + 1);
      hPull[pull][p]->SetFillColor(11);
      hPull[pull][p]->DrawCopy();
      hPull[pull][p]->Fit("fgaus","nr");
      fgaus->DrawCopy("same");
    }
  }
  can->cd(3*num_particles + 2);
  cout << "________________________________________________________________"
       << "________________" << endl;
  cout << "Fitting photon pull..." << endl;
  hPull_Eg->SetFillColor(11);
  hPull_Eg->DrawCopy();
  hPull_Eg->Fit("fgaus","nr");  
  fgaus->DrawCopy("same");

  can->cd(0);
  can->Update();
}
//_____________________________________________________________________________
