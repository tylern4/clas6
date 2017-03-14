/** @file PlotDalitz.C
 *  @brief Utility macro to make Dalitz plot for 3 particles stored in an 
 *         Event file.
 *
 *  @param file Input Event file
 *  @param part1 Particle name (ex. @a pi-)
 *  @param part2 Particle name 
 *  @param part3 Particle name 
 *  @param w_min Minimum @a W to plot (defaults to all)
 *  @param w_max Maximum @a W to plot (defaults to all)
 *  @param maxEvents  Maximum number of events to process (defaults to all)
 *
 *  This macro is loaded automatically by the cobra_logon.C script. The
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
 *  root [0] PlotDalitz("43582_p_pi+_pi-.root","p","pi+","pi-",1.7,1.9);
 *
 *  Which produces the following output,
 *
 *  <img src="dalitz.gif">
 *
 */
//_____________________________________________________________________________

void PlotDalitz(const string &__file,const string &__part1,
		const string &__part2,const string &__part3,
		double __w_min = 0.,double __w_max = 100.,
		int __maxEvents = -1){
  
  gROOT->Reset();  

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
  double max12,max13,min12,min13;
  double m1 = ParticleTypes::Instance()->GetParticle(__part1).Mass();
  double m2 = ParticleTypes::Instance()->GetParticle(__part2).Mass();
  double m3 = ParticleTypes::Instance()->GetParticle(__part3).Mass();
  min12 = 0.5*(m1+m2)*(m1+m2);
  min13 = 0.5*(m1+m3)*(m1+m3);
  if(__w_min != 0 && __w_max != 100){ 
    sprintf(h_title,"Dalitz Plot (%3.3f < W < %3.3f)",__w_min,__w_max);
    max12 = __w_max*__w_max - m3*m3 - 2*m3*(m1+m2);
    max13 = __w_max*__w_max - m2*m2 - 2*m2*(m1+m3);
  }
  else {
    sprintf(h_title,"Dalitz Plot");
    max12 = 5.;
    max13 = 5.;
  }
 
  
  TH2F *hDalitz = new TH2F("hDalitz",h_title,100,min12,max12,100,min13,max13);

  char title[100];
  string latex_name1,latex_name2,latex_name3;
  latex_name1 = ParticleTypes::Instance()->GetParticle(__part1).LaTexName();
  if(latex_name1[0] == '\\') latex_name1[0] = '#';
  latex_name2 = ParticleTypes::Instance()->GetParticle(__part2).LaTexName();
  if(latex_name2[0] == '\\') latex_name2[0] = '#';
  latex_name3 = ParticleTypes::Instance()->GetParticle(__part3).LaTexName();
  if(latex_name3[0] == '\\') latex_name3[0] = '#';

  sprintf(title,"#left(p_{%s} + p_{%s}#right)^{2}",latex_name1.c_str(),latex_name2.c_str());
  hDalitz->GetXaxis()->SetTitle(title);
  hDalitz->GetXaxis()->SetTitleOffset(1.25);
  sprintf(title,"#(){p_{%s} + p_{%s}}^{2}",latex_name1.c_str(),latex_name3.c_str());
  hDalitz->GetYaxis()->SetTitle(title);
  hDalitz->GetYaxis()->SetTitleOffset(1.5);

  TLorentzVector p4;
  double mass12,mass13,w;

  max13 = 0;
  max12 = 0;
  min13 = 100;
  min12 = 100;

  string part1 = __part1;
  string part2 = __part2;
  string part3 = __part3;
  // Get the 1st entry and see if each particle is in the file...if not use
  // the missing 4-momentum for that particle
  tree->GetEntry(0);
  if(!event->HasParticle(__part1)){
    cout << "No " << __part1 << " particle stored, will use total missing "
	 << "4-momentum." << endl;
    part1 = "missing";
  }
  if(!event->HasParticle(__part2)){
    cout << "No " << __part2 << " particle stored, will use total missing "
	 << "4-momentum." << endl;
    part2 = "missing";
  }
  if(!event->HasParticle(__part3)){
    cout << "No " << __part3 << " particle stored, will use total missing "
	 << "4-momentum." << endl;
    part3 = "missing";
  }
  

  for(int entry = 0; entry < num_entries; entry++){

    tree->GetEntry(entry); // read next event into memory

    w = event->W();
    if(w > __w_min && w < __w_max){

      // I'll get the mass^2 2 ways just to show how to do it
      p4 = event->GetP4(part1,part2);
      mass12 = p4.M2(); // mass squared of 1 and 2
      mass13 = event->InvariantMass2(part1,part3); 

      hDalitz->Fill(mass12,mass13);

      if(mass12 > max12) max12 = mass12;
      if(mass12 < min12) min12 = mass12;
      if(mass13 > max13) max13 = mass13;
      if(mass13 < min13) min13 = mass13;      
    }
  }

  TCanvas *can = new TCanvas("can","Dalitz Plot",200,200,600,600);
  can->cd(0);
  gPad->SetLeftMargin(0.15);
  gPad->SetRightMargin(0.15);
  gPad->SetBottomMargin(0.15);

  hDalitz->GetXaxis()->SetLimits(0.75*min12,1.25*max12);
  hDalitz->GetYaxis()->SetLimits(0.75*min13,1.25*max13);
  hDalitz->DrawCopy("colz");

  can->Update();
}
//_____________________________________________________________________________
