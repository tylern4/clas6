#include "ParticleData.h"
#include "Particle.h"

Particle::Particle()
{
	id = 0;						// PDG particle id
	name = "not setup";				// particle name
	charge = 99;					// charge
	stable = true;					// stable or unstable 
	parent = -1;					// PDG id of mother particle
	mass = 0.0;					// mean PDG mass
	spin = "not setup";				// spin
	lifeTime = 0;					// life time
	fullWidth = 0;					// full width
	resonance = false;				// is reonance
	baryon = true;					// is baryon
	lvec.SetXYZT(0.0, 0.0, 0.0, 0.0);		// energy-momentum vector
	vec.SetXYZT(0.0, 0.0, 0.0, 0.0);
}

Particle::Particle(int i)
{
	ParticleData *paData = new ParticleData();
	Particle *pa = paData->getParticle(i);

	id = pa->getId();				// PDG particle id
	name = pa->getName();				// particle name
	charge = pa->getCharge();			// charge
	stable = pa->isStable();			// stable or unstable 
	parent = pa->getParent();			// PDG id of mother particle
	mass = pa->getMass();				// mean PDG mass
	spin = pa->getSpin();				// spin
	lifeTime = pa->getLifeTime();			// life time
	fullWidth = pa->getFullWidth();			// full width
	resonance = pa->isResonance();			// is reonance
	baryon = pa->isBaryon();			// is baryon
	meson = pa->isMeson();
	lepton = pa->isLepton();
	strangeness = pa->getStrangeness();
	lvec.SetXYZM(0.0, 0.0, 0.0, mass);		// energy-momentum vector
	vec.SetXYZM(0.0, 0.0, 0.0, mass);
	delete paData;
	delete pa;
}


Particle::Particle(const Particle &part)
{
	id = part.getId();
	name = part.getName();
	charge = part.getCharge();
	stable = part.isStable();
	parent = part.getParent();
	mass = part.getMass();
	spin = part.getSpin();
	lifeTime = part.getLifeTime();
	fullWidth = part.getFullWidth();
	resonance = part.isResonance();
	baryon = part.isBaryon();
	meson = part.isMeson();
	lepton = part.isLepton();
	strangeness = part.getStrangeness();
	lvec = part.getLorentzVector();
	vec = part.getVertexTime();
//	*this = part;
}
