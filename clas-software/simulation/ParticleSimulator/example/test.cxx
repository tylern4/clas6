#include<iostream>
#include<vector>
#include<ctime>
#include "TLorentzVector.h"
#include "Particle.h"
#include "CompositeParticle.h"
#include "Gamma.h"
#include "ParticleSimulator.h"
#include "WriteFile.h"
using namespace std;

int main(int argc, char** argv)
{
	if(argc < 2) {
		cout << "wrong number of argument: ./test.exe nevent" << endl << endl;
		exit(-1);
	}

	// Example for \gamma + p -->     K_s 	    +  \Sigma+ 
	//			  --> \pi+ + \pi-   +  p + \pi0				


	// ---- set-up photon beam ----
	//Gamma *beam = new Gamma(1.0, 4.0, 1);		// uniform photon beam in energy range 1.0-4.0 GeV
	Gamma *beam = new Gamma(1.0, 4.0, 2);		// bremsstrahlung photon beam in energy range 1.0-4.0 GeV
	//Gamma *beam = new Gamma(2.5);			// photon beam with fixed energy 2.5 GeV
	//-----------------------------


	// Particles are set by their PDF id
	// Particle properties are set from PDF, they can be over-written for a particular particle by using methonds in Particle class

	// ---- set-up target ----	
	Particle *target = new Particle(2212);

	// set-up decaying particle Ks and its decay products
	CompositeParticle *ks = new CompositeParticle(310);
	ks->setPol(0.0);
	Particle *pip = new Particle(211);
	pip->setParent(310);
	Particle *pim = new Particle(-211);
	pim->setParent(310);
	ks->setDecayProducts(pip);
	ks->setDecayProducts(pim);

	// set-up decaying particle \Sigma+ and its decay products
	CompositeParticle *sigmap = new CompositeParticle(3222);
	sigmap->setPol(0.0);
	Particle *p = new Particle(2212);
	p->setParent(2212);
	Particle *pi0 = new Particle(111);
	pi0->setParent(2212);
	sigmap->setDecayProducts(p);
	sigmap->setDecayProducts(pi0);

	// set-up main simulation object
	ParticleSimulator *sim = new ParticleSimulator();
	sim->setTSlope(4.0);
	sim->setBeam(beam);
	sim->setTarget(target);
	sim->setTargetPos(-30.0, 10.0);
	sim->setMidFinal(ks);
	sim->setMidFinal(sigmap);
	sim->init();	

	// check conservation laws are satisfied or not
	sim->checkConservation();

	// to write events in a file
	WriteFile *write = new WriteFile("check.txt");

	time_t seconds;
	seconds = time(NULL);
	
	int nEvent = atoi(argv[1]);				// number of events to generate
	for(int i=0; i<nEvent; i++) {
		if(!((i*100)%nEvent)) cout << i << "/" << nEvent << " ==> " << float(i)*100/nEvent << " %" << endl;

		sim->initEvent();				// initialize simulation	
	
		beam->generateGamma();				// generate one photon
		if(!sim->makeEvent()) continue;			// generate one event and check it is ok or not


		// ==== prepare to write information to the file ====

		// ParticleSimulator does generate parameters for HEAD bank, write your own here
		// int head[] = {version, nrun, nevent, type, roc, evtclass, trigbits};
		int headA[] = {1, 1, i+1, -2, 0, 15, 1};
		vector<int> head (headA, headA + sizeof(headA) / sizeof(headA[0]) );

		// write to the file
		write->clear();
		write->fill(head, seconds, sim);
	}
	
	write->finish();					// finish event

//	part->setId(211);
//	part->setCharge(-1);
//	part->setLorentzVector(1.0, 1.3, 2.5, 0.135);
//	cout << part->getMass() << "\t" << part->getLorentzVector().Py() << endl;
}
