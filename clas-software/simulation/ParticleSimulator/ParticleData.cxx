#include "ParticleData.h"


Particle* ParticleData::getParticle(int id)
{
	for(unsigned int i=0; i<part.size(); i++) {
		Particle *pa = part[i];
		int d = pa->getId();
		if(d == id) return pa;
	}
	cout << "particle is not found" << endl;
	return 0;
}


ParticleData::ParticleData()
{
	Particle *photon = new Particle();
	photon->setId(22);
	photon->setName("photon");
	photon->setCharge(0);
	photon->setMass(0.0E-21);
	photon->setStable(true);
	photon->setSpin("one");
	photon->setLifeTime(9.9E99);
	photon->setFullWidth(0.0E-33);
	photon->setBaryon(false);
	photon->setMeson(false);
	photon->setBoson(true);
	photon->setLepton(false);
	photon->setStrangeness(0);
	part.push_back(photon);
	//-----------------

	Particle *electron = new Particle();
	electron->setId(11);
	electron->setName("electron");
	electron->setCharge(-1);
	electron->setMass(0.510998910E-3);
	electron->setStable(true);
	electron->setSpin("one half");
	electron->setLifeTime(9.9E99);
	electron->setFullWidth(0.0E-33);
	electron->setBaryon(false);
	electron->setMeson(false);
	electron->setBoson(false);
	electron->setLepton(true);
	electron->setStrangeness(0);
	part.push_back(electron);
	//-----------------

	Particle *aelectron = new Particle();
	aelectron->setId(-11);
	aelectron->setName("positron");
	aelectron->setCharge(1);
	aelectron->setMass(0.510998910E-3);
	aelectron->setStable(true);
	aelectron->setSpin("one half");
	aelectron->setLifeTime(9.9E99);
	aelectron->setFullWidth(0.0E-33);
	aelectron->setBaryon(false);
	aelectron->setMeson(false);
	aelectron->setBoson(false);
	aelectron->setLepton(true);
	aelectron->setStrangeness(0);
	part.push_back(aelectron);
	//-----------------
	
	// Mesons
	Particle *pi0 = new Particle();
	pi0->setId(111);
	pi0->setName("pi0");
	pi0->setCharge(0);
	pi0->setMass(134.9766E-3);
	pi0->setStable(false);
	pi0->setSpin("one");
	pi0->setLifeTime(8.4E-17);
	pi0->setFullWidth(0.0E-33);
	pi0->setBaryon(false);
	pi0->setMeson(true);
	pi0->setBoson(false);
	pi0->setLepton(false);
	pi0->setStrangeness(0);
	part.push_back(pi0);

	//----------------
	Particle *pip = new Particle();
	pip->setId(211);
	pip->setName("pi+");
	pip->setCharge(1);
	pip->setMass(139.57018E-3);
	pip->setStable(false);
	pip->setSpin("one");
	pip->setLifeTime(2.6033E-8);
	pip->setFullWidth(0.0E-33);
	pip->setBaryon(false);
	pip->setMeson(true);
	pip->setBoson(false);
	pip->setLepton(false);
	pip->setStrangeness(0);
	part.push_back(pip);
	
	//---------------------
	Particle *pim = new Particle();
	pim->setId(-211);
	pim->setName("pi-");
	pim->setCharge(-1);
	pim->setMass(139.57018E-3);
	pim->setStable(false);
	pim->setSpin("one");
	pim->setLifeTime(2.6033E-8);
	pim->setFullWidth(0.0E-33);
	pim->setBaryon(false);
	pim->setMeson(true);
	pim->setBoson(false);
	pim->setLepton(false);
	pim->setStrangeness(0);
	part.push_back(pim);

	//---------------------
	Particle *rho770 = new Particle();
	rho770->setId(113);
	rho770->setName("rho(770)");
	rho770->setCharge(0);
	rho770->setMass(775.49E-3);
	rho770->setStable(false);
	rho770->setSpin("one");
	rho770->setLifeTime(2.6033E-8);
	rho770->setFullWidth(149.4);
	rho770->setBaryon(false);
	rho770->setMeson(true);
	rho770->setBoson(false);
	rho770->setLepton(false);
	rho770->setStrangeness(0);
	part.push_back(rho770);

	//----------------------
	Particle *rhop770 = new Particle();
	rhop770->setId(213);
	rhop770->setName("rho(770)+");
	rhop770->setCharge(1);
	rhop770->setMass(775.49E-3);
	rhop770->setStable(false);
	rhop770->setSpin("one");
	rhop770->setLifeTime(2.6033E-8);
	rhop770->setFullWidth(149.4);
	rhop770->setBaryon(false);
	rhop770->setMeson(true);
	rhop770->setBoson(false);
	rhop770->setLepton(false);
	rhop770->setStrangeness(0);	
	part.push_back(rhop770);

	//--------------------------
	Particle *eta = new Particle();
	eta->setId(221);
	eta->setName("eta");
	eta->setCharge(0);
	eta->setMass(547.853E-3);
	eta->setStable(false);
	eta->setSpin("zero");
	eta->setLifeTime(2.6033E-8);
	eta->setFullWidth(0.0013);
	eta->setBaryon(false);
	eta->setMeson(true);
	eta->setBoson(false);
	eta->setLepton(false);
	eta->setStrangeness(0);
	part.push_back(eta);

	//-----------------------
	Particle *etap = new Particle();
	etap->setId(321);
	etap->setName("eta'");
	etap->setCharge(0);
	etap->setMass(957.66E-3);
	etap->setStable(false);
	etap->setSpin("zero");
	etap->setLifeTime(9);
	etap->setFullWidth(0.205);
	etap->setBaryon(false);
	etap->setMeson(true);
	etap->setBoson(false);
	etap->setLepton(false);
	etap->setStrangeness(0);
	part.push_back(etap);
	
	//--------------------
	Particle *omega = new Particle();
	omega->setId(223);
	omega->setName("omega");
	omega->setCharge(0);
	omega->setMass(782.65E-3);
	omega->setStable(false);
	omega->setSpin("one");
	omega->setLifeTime(9);
	omega->setFullWidth(8.49);
	omega->setBaryon(false);
	omega->setMeson(true);
	omega->setBoson(false);
	omega->setLepton(false);
	omega->setStrangeness(0);
        part.push_back(omega);

	//---------------------
	Particle *phi = new Particle();
	phi->setId(333);
	phi->setName("phi");
	phi->setCharge(0);
	phi->setMass(1019.455E-3);
	phi->setStable(false);
	phi->setSpin("one");
	phi->setLifeTime(9);
	phi->setFullWidth(4.26);
	phi->setBaryon(false);
	phi->setMeson(true);
	phi->setBoson(false);
	phi->setLepton(false);
	phi->setStrangeness(0);
	part.push_back(phi);

	//------------------------
	Particle *kl = new Particle();
	kl->setId(130);
	kl->setName("Kl");
	kl->setCharge(0);
	kl->setMass(497.614E-3);
	kl->setStable(false);
	kl->setSpin("zero");
	kl->setLifeTime(5.116E-8);
	kl->setFullWidth(0.0);
	kl->setBaryon(false);
	kl->setMeson(true);
	kl->setBoson(false);
	kl->setLepton(false);
	kl->setStrangeness(0);
	part.push_back(kl);

	//------------------------
	Particle *ks = new Particle();
	ks->setId(310);
	ks->setName("Ks");
	ks->setCharge(0);
	ks->setMass(497.614E-3);
	ks->setStable(false);
	ks->setSpin("zero");
	ks->setLifeTime(0.8958E-10);
	ks->setFullWidth(0.0);
	ks->setBaryon(false);
	ks->setMeson(true);
	ks->setBoson(false);
	ks->setLepton(false);
	ks->setStrangeness(0);
	part.push_back(ks);
	
	//-------------------------
	Particle *k0 = new Particle();
	k0->setId(311);
	k0->setName("K0");
	k0->setCharge(0);
	k0->setMass(497.614E-3);
	k0->setStable(false);
	k0->setSpin("zero");
	k0->setLifeTime(0.8958E-10);
	k0->setFullWidth(0.0);
	k0->setBaryon(false);
	k0->setMeson(true);
	k0->setBoson(false);
	k0->setLepton(false);
	k0->setStrangeness(0);
	part.push_back(k0);

	//-------------------
	Particle *kp = new Particle();
	kp->setId(321);
	kp->setName("K+");
	kp->setCharge(1);
	kp->setMass(493.677E-3);
	kp->setStable(false);
	kp->setSpin("zero");
	kp->setLifeTime(1.2380E-8);
	kp->setFullWidth(0.0);
	kp->setBaryon(false);
	kp->setMeson(true);
	kp->setBoson(false);
	kp->setLepton(false);
	kp->setStrangeness(0);
	part.push_back(kp);
	
	//---------------------------
	Particle *km = new Particle();
	km->setId(-321);
	km->setName("Km");
	km->setCharge(-1);
	km->setMass(493.677E-3);
	km->setStable(false);
	km->setSpin("zero");
	km->setLifeTime(1.2380E-8);
	km->setFullWidth(0.0);
	km->setBaryon(false);
	km->setMeson(true);
	km->setBoson(false);
	km->setLepton(false);
	km->setStrangeness(0);
	part.push_back(km);

	//------------------------
	Particle *ks0 = new Particle();
	ks0->setId(313);
	ks0->setName("K*0");
	ks0->setCharge(0);
	ks0->setMass(891.66E-3);
	ks0->setStable(false);
	ks0->setSpin("one");
	ks0->setLifeTime(1.2380E-8);
	ks0->setFullWidth(50.3);
	ks0->setBaryon(false);
	ks0->setMeson(true);
	ks0->setBoson(false);
	ks0->setLepton(false);
	ks0->setStrangeness(0);
	part.push_back(ks0);

	//------------------------
	Particle *ksp = new Particle();
	ksp->setId(323);
	ksp->setName("K*+");
	ksp->setCharge(1);
	ksp->setMass(891.66E-3);
	ksp->setStable(false);
	ksp->setSpin("one");
	ksp->setLifeTime(1.2380E-8);
	ksp->setFullWidth(50.3);
	ksp->setBaryon(false);
	ksp->setMeson(true);
	ksp->setBoson(false);
	ksp->setLepton(false);
	ksp->setStrangeness(0);
	part.push_back(ksp);


// "delta++", "delta+", "delta0", "delta-", "lambda", "sigma+", "sigma0", "sigma-"};
// 2224, 2214, 2114, 1114, 3122, 3222, 3212, 3112};

	//----------------------
	//Baryons

	Particle *proton = new Particle();
        proton->setId(2212);
        proton->setName("proton");
        proton->setCharge(1);
        proton->setMass(938.27203E-3);
        proton->setStable(true);
        proton->setSpin("one half");
        proton->setLifeTime(2.1E29);
        proton->setFullWidth(0.0);
        proton->setBaryon(true);
        proton->setMeson(false);
        proton->setBoson(false);
        proton->setLepton(false);
        proton->setStrangeness(0);
        part.push_back(proton);

	//-------------------------
	Particle *neutron = new Particle();
	neutron->setId(2112);
	neutron->setName("neutron");
	neutron->setCharge(0);
	neutron->setMass(939.56536E-3);
	neutron->setStable(true);
	neutron->setSpin("one half");
	neutron->setLifeTime(2.1E29);
	neutron->setFullWidth(0.0);
	neutron->setBaryon(true);
	neutron->setMeson(false);
	neutron->setBoson(false);
	neutron->setLepton(false);
	neutron->setStrangeness(0);
	part.push_back(neutron);

	//-----------------------
	Particle *lambda = new Particle();
	lambda->setId(3122);
	lambda->setName("Lambda");
	lambda->setCharge(0);
	lambda->setMass(1115.683E-3);
	lambda->setStable(false);
	lambda->setSpin("one half");
	lambda->setLifeTime(2.631E-10);
	lambda->setFullWidth(0.0);
	lambda->setBaryon(true);
	lambda->setMeson(false);
	lambda->setBoson(false);
	lambda->setLepton(false);
	lambda->setStrangeness(-1);
	part.push_back(lambda);

	//-----------------------
//"delta++", "delta+", "delta0", "delta-"};
// 2224, 2214, 2114, 1114, };

	//----------------------
	Particle *sigmap = new Particle();
	sigmap->setId(3222);
	sigmap->setName("Sigma+");
	sigmap->setCharge(1);
	sigmap->setMass(1189.37E-3);
	sigmap->setStable(false);
	sigmap->setSpin("one half");
	sigmap->setLifeTime(0.8018E-10);
	sigmap->setFullWidth(0.0);
	sigmap->setBaryon(true);
	sigmap->setMeson(false);
	sigmap->setBoson(false);
	sigmap->setLepton(false);
	sigmap->setStrangeness(-1);
	part.push_back(sigmap);

	//-------------------------
	Particle *sigma0 = new Particle();
	sigma0->setId(3212);
	sigma0->setName("Sigma0");
	sigma0->setCharge(1);
	sigma0->setMass(1192.642E-3);
	sigma0->setStable(false);
	sigma0->setSpin("one half");
	sigma0->setLifeTime(7.4E-20);
	sigma0->setFullWidth(0.0);
	sigma0->setBaryon(true);
	sigma0->setMeson(false);
	sigma0->setBoson(false);
	sigma0->setLepton(false);
	sigma0->setStrangeness(-1);
	part.push_back(sigma0);

	//--------------------------
	Particle *sigmam = new Particle();
	sigmam->setId(3112);
	sigmam->setName("Sigma0");
	sigmam->setCharge(1);
	sigmam->setMass(1197.449E-3);
	sigmam->setStable(false);
	sigmam->setSpin("one half");
	sigmam->setLifeTime(1.479E-10);
	sigmam->setFullWidth(0.0);
	sigmam->setBaryon(true);
	sigmam->setMeson(false);
	sigmam->setBoson(false);
	sigmam->setLepton(false);
	sigmam->setStrangeness(-1);
	part.push_back(sigmam);

	//------------------------
	Particle *thetap = new Particle();
	thetap->setId(9221132);
	thetap->setName("Theta+");
	thetap->setCharge(1);
	thetap->setMass(1540E-3);
	thetap->setStable(false);
	thetap->setSpin("one half");
	thetap->setLifeTime(1.479E-10);
	thetap->setFullWidth(1E-10);
	thetap->setBaryon(true);
	thetap->setMeson(false);
	thetap->setBoson(false);
	thetap->setLepton(false);
	thetap->setStrangeness(-1);
	part.push_back(thetap);
}
