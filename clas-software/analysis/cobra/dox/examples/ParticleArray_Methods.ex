// create an array with a pi+ and pi- storing TLorentzVector's
ParticleArray<TLorentzVector> array("pi+:pi-");

// grab the mass' of the pions
double mpip = array("pi+").Mass(); // this returns 0.13957
double mpim = array("pi-").Mass(); // this returns 0.13957

// set the 4-vectors
array("pi+").SetPxPyPzE(1.,0.,0.,sqrt(1.+ mpip*mpip));
array("pi-").SetPxPyPzE(1.,0.,0.,sqrt(1.+ mpim*mpim));

// now get some info from the 4-vectors
double beta_pip = array("pi+").Beta();
double theta_pim = array("pi-").Theta();
// etc...

// be aware that this:
mpip = array("pi+").M(); 
// this calls ParticleInfo::M() NOT TLorentzVector::M()...but if the value 
// returned is the same.

