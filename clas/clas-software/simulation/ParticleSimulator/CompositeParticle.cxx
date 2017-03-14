#include "CompositeParticle.h"
#include "ParticleData.h"

CompositeParticle::CompositeParticle(int i)
{
	polFun = new TF1("polFun", "0.5*(1.0 + [0]*[1]*x)", -1.0, 1.0);
	polFun->SetParameter(0, alpha);
	polFun->SetParameter(1, pol);

	ParticleData *paData = new ParticleData();
	Particle *pa = paData->getParticle(i);

	*this = pa;
	delete paData;
	delete pa;	
}

CompositeParticle::CompositeParticle(Particle *pa)
{
	polFun = new TF1("polFun", "0.5*(1.0 + [0]*[1]*x)", -1.0, 1.0);
	polFun->SetParameter(0, alpha);
	polFun->SetParameter(1, pol);

	Particle::setId(pa->getId());
	Particle::setCharge(pa->getCharge());
	Particle::setName(pa->getName());
	Particle::setStable(pa->isStable());
	Particle::setMass(pa->getMass());
	Particle::setSpin(pa->getSpin());
	Particle::setLifeTime(pa->getLifeTime());
	Particle::setFullWidth(pa->getFullWidth());
	Particle::setResonance(pa->isResonance());
	Particle::setBoson(pa->isBoson());
	Particle::setLepton(pa->isLepton());
	Particle::setMeson(pa->isMeson());
	Particle::setStrangeness(pa->getStrangeness());
	Particle::setParent(0);
}

CompositeParticle::CompositeParticle(const CompositeParticle& pa)
{
//	Particle::.setId(pa.getId());
}
