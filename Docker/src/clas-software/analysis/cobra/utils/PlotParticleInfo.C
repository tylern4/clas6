/** @file PlotParticleInfo.C
 *  @brief Utility macro to plot some detected particle  information for events
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
 *  root [0] PlotParticleInfo("43582_p_pi+_pi-.root");
 *
 *  Which produces the following output,
 *
 *  <img src="particle_info.gif">
 *
 */
//_____________________________________________________________________________

void PlotParticleInfo(const string &__file,int __maxEvents = -1){

  gROOT->Reset();  
  gStyle->SetTitleH(0.15);
  gStyle->SetTitleW(.95);

  TFile inFile(__file.c_str()); // open the input file
  TTree *tree = (TTree*)inFile.Get("T"); // grab the tree

  Event *event = new Event();
  tree->SetBranchAddress("event",&event); // read tree info into event

  int num_entries = (int)tree->GetEntries();
  if(__maxEvents > 0){
    if(__maxEvents < num_entries) num_entries = __maxEvents;
  }
  Particle particle;

  // grab 1st entry to see how many particles there are
  tree->GetEntry(0);
  const int num_particles = event->NumParticles();

  string p_names[num_particles];
  for(int p = 0; p < num_particles; p++) 
    p_names[p] = event->GetElement(p).Name(); // get the particle names

  // Set up some histo stuff
  string histo_names[8];
  histo_names[0] = "p";
  histo_names[1] = "#theta";
  histo_names[2] = "#phi";
  histo_names[3] = "Sector";
  histo_names[4] = "SC Paddle";
  histo_names[5] = "Vertex Time";
  histo_names[6] = "Time of Flight";
  histo_names[7] = "Path Length";

  int num_bins[8];
  for(int h = 0; h < 8; h++) num_bins[h] = 50;
  num_bins[3] = 6;
  num_bins[4] = 48;

  double hmin[8],hmax[8];
  hmin[0] = 0.; hmax[0] = 3.5;
  hmin[1] = 0.; hmax[1] = 2.5;
  hmin[2] = -3.14159; hmax[2] = 3.14159;
  hmin[3] = 0.5; hmax[3] = 6.5;
  hmin[4] = 0.5; hmax[4] = 48.5;
  hmin[5] = -10.; hmax[5] = 40.;
  hmin[6] = 0.; hmax[6] = 100.;
  hmin[7] = 300.; hmax[7] = 700.;

  // Set up the histos
  char h_name[100],h_title[100];
  string latex_name;
  TH1F *histo[10][8];

  for(int p = 0; p < num_particles; p++){
    latex_name = event->GetElement(p).LaTexName(); 
    if(latex_name[0] == '\\') latex_name[0] = '#'; // TLatex uses # instead
    
    for(int h = 0; h < 8; h++){
      sprintf(h_name,"h_%d_%d",p,h);
      sprintf(h_title,"%s : %s",latex_name.c_str(),histo_names[h].c_str());
      histo[p][h] = new TH1F(h_name,h_title,num_bins[h],hmin[h],hmax[h]);
    }
  }

  // loop over events
  for(int entry = 0; entry < num_entries; entry++){
    tree->GetEntry(entry);

    for(int p = 0; p < num_particles; p++){

      particle = event->GetParticle(p);
      histo[p][0]->Fill(particle.P4().P());
      histo[p][1]->Fill(particle.P4().Theta());
      histo[p][2]->Fill(particle.P4().Phi());
      histo[p][3]->Fill(particle.Sector());
      histo[p][4]->Fill(particle.SC_Id());
      histo[p][5]->Fill(particle.VertexTime());
      histo[p][6]->Fill(particle.TOF());
      histo[p][7]->Fill(particle.PathLength());
    }
  }


  // Set up the canvas
  TCanvas *can = new TCanvas("can","Particle Info",200,200,1000,600);
  can->Divide(8,num_particles);

  double max;
  for(int p = 0; p < num_particles; p++){
    for(int h = 0; h < 8; h++){
      can->cd(8*p + h + 1);
      histo[p][h]->SetFillColor(11);
      histo[p][h]->SetMinimum(0);
      max = histo[p][h]->GetMaximum();
      histo[p][h]->SetMaximum(1.25*max);
      histo[p][h]->DrawCopy();
    }
  }

  can->cd(0);
  can->Update();
}
//_____________________________________________________________________________
