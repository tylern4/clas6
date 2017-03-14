// grab a pointer to the ParticleTypes instance
ParticleTypes *ptypes = ParticleTypes::Instance();

// now get the pi+ mass
double m_pip = ptypes->GetParticle("pi+").Mass();

// or, if you only want 1 piece of info, you can skip declaring the pointer:
double pdgId_pip = ParticleType::Instance()->GetParticle("pi+").PDG_Id();