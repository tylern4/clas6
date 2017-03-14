// create an array of floats
ParticleArray<float> array("p:pi+:pi-");

// set the values
array("p") = 1.1;
array("pi+") = 2.2;
array("pi+") = 3.3;

// set some floats to these:
float p = array("p"); // now p = 1.1
float pip = array("pi+"); // now pip = 2.2
float pim = array("pi-"); // now pim = 3.3

// pass an element to a function that takes flaots
float root_p = sqrt(array("p")); // root_p = sqrt(1.1);