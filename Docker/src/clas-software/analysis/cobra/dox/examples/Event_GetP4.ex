// create an event with a detected proton,pi+,pi-
Event event("p:pi+:pi-");

// assume that it's filled with info from a gamma p --> p pi+ pi- (pi0) event

// get 4-momentum of pi+,pi- system
TLorentzVector p4 = event.GetP4("pi+","pi-");

// get 4-momentum of pi+,pi0 system using total missing p4 for pi0
p4 = event.GetP4("pi+","missing"); 

// get 4-momentum of pi+,pi0 using total missing p3 but actual pi0 mass
p4 = event.GetP4("pi+","missing",0.13498);

