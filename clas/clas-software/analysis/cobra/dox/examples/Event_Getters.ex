// create a proton pi+ pi- event
Event event("p:pi+:pi-");

// assume it's set somehow

// get some basic quantities
event.RunNumber(); // CLAS run number
event.TargetP4(); // target 4-momentum
event.Chi2(); // chi^2 from kinematic fitting
event.Prob(); // confidence level from kinematic fitting

// etc...

// get access to charged particle info
event.GetParticle("pi+"); // get reference to pi+ Particle object
event.GetParticle("p").P4(); // get reference to proton 4-momentum

// etc...

// get access to tagged photon info
event.GetPhoton(); // gets reference to Photon object
event.GetPhoton().E_Id(); // get photon's e-counter id

// etc...