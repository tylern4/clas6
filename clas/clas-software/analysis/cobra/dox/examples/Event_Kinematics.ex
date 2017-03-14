// create an event object for p pi+ pi- (pi0) final state
Event event("p:pi+:pi-");

// assume that it's filled somehow...

// get some kinematic quantities:
double mass;
TLorentzVector p4;
mass = event.MissingMass(); // total missing mass
mass = event.MissingMassOff("p"); // missing mass off the proton
p4 = event.MissingP4(); // total missing p4
p4 = event.GetP4("p","pi+"); // proton,pi+ 4-momentum
p4 = event.GetP4("p","missing"); // proton,pi0 4-momentum
mass = event.W(); // center-of-mass energy
mass = event.InvariantMass("pi+","pi-"); // invariant mass of pi+,pi- system
mass = event.InvariantMass("pi+","missing",0.13498); // invariant mass of
	 					     // pi+,pi0 forcing the 
						     // pi0 mass = 0.13498 GeV

// etc...