// create an event with 2 protons and a pi-
Event event("p:p:pi-");

// get access to the individual particles
event.GetParticle("p"); // reference to 1st proton
event.GetParticle("p",0); // also a reference to 1st proton
event.GetParticle("p",1); // reference to 2nd proton
event.GetParticle("pi-"); // reference to the pi-