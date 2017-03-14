#ifdef __CINT__

#pragma link off all globals;
#pragma link off all classes;
#pragma link off all functions;

// link photon and particle classes
#pragma link C++ class Particle;
#pragma link C++ class Photon;

// link what we need for Event
#pragma link C++ class TypeInfo<Particle>;
#pragma link C++ class PArrayElement<Particle>;
#pragma link C++ class ParticleArray<Particle>;
#pragma link C++ class Event;

// link a ParticleArray of TLorentzVector's (in case we want to write one out)
#pragma link C++ class TypeInfo<TLorentzVector>;
#pragma link C++ class PArrayElement<TLorentzVector>;
#pragma link C++ class ParticleArray<TLorentzVector>;


#endif
