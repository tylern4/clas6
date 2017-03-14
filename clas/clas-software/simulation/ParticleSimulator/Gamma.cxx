#include "Gamma.h"
#include "ParticleData.h"

Gamma::Gamma()
{
	init();
	mode = -1;
	eMin = -1.0;
	eMax = -1.0;
	eGamma = -1.0;
}

Gamma::Gamma(double min, double max, int mo)
{
	eMin = min;
	eMax = max;
	mode = mo;

	if(eMin <= 0.0 || eMax <= 0.0 || eMin >= eMax) {
		cout << "Either photon energy range is either not set up or has wrong set up: aborting" << endl;
		exit(-1);
	}
	cout << endl;

	if(mode == 1) {
	//	cout << "Will generate flat energy distribution between " << eMin << " and " << eMax << " GeV" << endl;	
	}
	else if(mode == 2) {
	//	cout << "Will generate bremsstrulung photon weighted by 1/E between " << eMin << " and " << eMax << " GeV" << endl;
		fun = new TF1("fun", "1/x", eMin, eMax);
	}	
	else {
		cout << "Invalid photon beam distribtion mode, should be 1 for uniform and 2 for bremsstrulung" << endl;
		exit(-1);
	}
	
	rand = new TRandom3();
	rand->SetSeed(0);

	init();
}	

Gamma::Gamma(double beam)
{
	if(beam <=0.0) {
		cout << "energy for photon beam with fixed energy is set to <= 0.0: aborting" << endl;
		exit(-1);
	}
	eMin = beam;
	eMax = beam;

	mode = 0;
	eGamma = beam;
	init();
}

void Gamma::init()
{
	gRandom->SetSeed(0);
	ParticleData *paData = new ParticleData();
	Particle *ph = (Particle*) paData->getParticle(22);

	Particle::setId(ph->getId());
	Particle::setName(ph->getName());
	Particle::setSpin(ph->getSpin());
	Particle::setCharge(ph->getCharge());
	Particle::setMass(ph->getMass());
	Particle::setStable(ph->isStable());
	Particle::setLifeTime(ph->getLifeTime());
	Particle::setBoson(ph->isBoson());
	Particle::setMeson(ph->isMeson());
	Particle::setBaryon(ph->isBaryon());
	Particle::setLepton(ph->isLepton());
	Particle::setParent(ph->getParent());
}

void Gamma::generateGamma()
{
	if(mode < 0 || mode > 2) {
		cout << "photon energy distribution is not set up properly: aborting" << endl;
		exit(-1);
	}
	if(eGamma < 0.0 || eMin < 0.0 || eMax < 0.0) {
		cout << "photon energy range is not set up: aborting" << endl;	
		exit(-1);
	} 
	
	if(mode == 1 || mode == 2) {
		if(eMin >= eMax) {
			cout << "photon eMin >= eMax: aborting" << endl;
			exit(-1);
		}
	}

	double energy;
	if(mode == 0) energy = eGamma;				// fixed energy
	if(mode == 1) {
		energy = rand->Uniform(eMin, eMax);		// uniform distribution
		eGamma = energy;
	}
	if(mode == 2) {						// bremsstrahlung distribution, weighted by 1/E
		energy = fun->GetRandom(eMin, eMax);
		eGamma = energy;	
	}	
	
	TLorentzVector lv;
	lv.SetXYZM(0.0, 0.0, energy, 0.0);
	Particle::setLorentzVector(lv);
}
