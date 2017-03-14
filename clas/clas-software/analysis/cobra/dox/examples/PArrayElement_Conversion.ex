// primitive instantiation
PArrayElement<float> elem_float("pi+",3.14159);
float x = elem_float; // sets x = 3.14159
x = elem_float.Mass(); // sets x = 0.13957

// object instantiation
PArrayElement<TLorentzVector> elem_p4("p"); // stored 4-vector is (0,0,0,0)
TLorentzVector p4(1.,1.,1.,1.); 
p4 = elem_p4; // now p4 = (0,0,0,0)
double value = elem_p4.P(); // value = 0.

// something to be aware of
value = elem_p4.M(); // value = 0.93827 (it's using ParticleInfo::M() NOT
                     // TLorentzVector::M()