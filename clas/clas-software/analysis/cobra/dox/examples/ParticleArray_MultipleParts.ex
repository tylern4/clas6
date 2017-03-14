// make an array with multiple identical particles
ParticleArray<float> array("p:p:pi+:p"); 

// get access to the protons
array("p"); // the 1st one
array("p",0); // also the 1st one
array("p",1); // the 2nd one
array("p",2); // the 3rd one