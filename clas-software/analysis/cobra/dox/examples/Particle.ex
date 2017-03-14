// declare a particle object
Particle part;

// Set the 4-momentum 
TLorentzVector p4(0.,0.,1.,sqrt(1.+mass*mass)); // for some mass
part.SetP4(p4);

// now get any kinematic quantity
double theta = part.P4().Theta(); // polar angle
double gamma = part.P4().Gamma(); // 1/sqrt(1-beta^2)

// etc...see ROOT TLorentzVector documentation for complete list of methods