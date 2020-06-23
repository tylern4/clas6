#include "ParticleSimulator.h"
#include<iostream>
#include<cmath>
#include <iomanip>
#include "TTree.h"
using namespace std;

ParticleSimulator::ParticleSimulator()
{
	rand = new TRandom3();
	rand->SetSeed(0);

	midFinal.clear();
	finalParticles.clear();
	rand->SetSeed(seed);

	tSlope = 3.0;
	tFun = new TF1("tFun", "TMath::Exp(-TMath::Sqrt([0])*x)", 0.0, 20.0);
	tFun->FixParameter(0, tSlope);	

	targetPos.Set(-30.0, 10.0);
	cout << endl;
}


void ParticleSimulator::init() 
{
	tFun->FixParameter(0, tSlope);
	print();
}

bool ParticleSimulator::checkConservation()
{
	// check charge conservation at interaction vertax
	int chI = beam->getCharge() + target->getCharge();
	int chF = 0;
	for(unsigned int i=0; i<midFinal.size(); i++) chF += midFinal[i]->getCharge();
	if(chI != chF) {
		cout << "Charge in not conserve at interaction vertax, aborting" << endl;
		exit(-1);
	}

	// check charge and energy conservation for resonances decay
	for(unsigned int i=0; i<midFinal.size(); i++){
		int chII = midFinal[i]->getCharge();
		double massII = midFinal[i]->getMass();
		int chFF = 0;
		double massFF = 0.0;
		for(unsigned int j=0; j<midFinal[i]->getDecayProducts().size(); j++){
			chFF += midFinal[i]->getDecayProducts()[j]->getCharge();
			massFF += midFinal[i]->getDecayProducts()[j]->getMass();
		}
		if(chII != chFF && midFinal[i]->getDecayProducts().size() != 0) {
			cout << "charge not conserve for decay of unstable particle " << midFinal[i]->getId() << " aborting" << endl;
			exit(-1);
		}
		if(massFF > massII && midFinal[i]->getDecayProducts().size() != 0) {
			cout << "mass of unstable particle " << midFinal[i]->getId() << " is less than its decay products, aborting" << endl;
			exit(-1);
		}
	}
	cout << "==> conservation is ok, proceeding" << endl;
	cout << endl;
	return true;
}

bool ParticleSimulator::makeEvent()
{
	if(beam->getLorentzVector().E() < 0.5) {
		cout << "Beam is not setup, aborting" << endl;
		exit(-1);
	}
	if(target->getLorentzVector().E() < 0.1) {
		cout << "Target is not setup, aborting" << endl;
		exit(-1);
	}
	if(midFinal.size() == 0) {
		cout << "Mid-final particles are not setup, aborting" << endl;
		exit(-1);
	}

	double z = rand->Uniform(targetPos.X(), targetPos.Y());
	beam->setVertexTime(0.0, 0.0, z, 0.0);
	target->setVertexTime(0.0, 0.0, z, 0.0);

	if(setMomentum()) {
//	cout << "momentum setup done" << endl;
		decayResonance();
		return true;
	}
	else return false;
}


bool ParticleSimulator::setMomentum()
{	
	//=============================
	//    First do in CM frame
	//=============================
	
	// generate BW mass of the resonances
	double mass3 = midFinal[0]->getMass();	
	double width3 = midFinal[0]->getFullWidth();

	double mass4 = midFinal[1]->getMass();
	double width4 = midFinal[1]->getFullWidth();

	double massBW3 = rand->BreitWigner(mass3, width3);
	double massBW4 = rand->BreitWigner(mass4, width4);

	// generate momenta of the particles in CM frame
	double s = (beam->getLorentzVector() + target->getLorentzVector()).M2();
       	double e3 = (s + massBW3*massBW3 - massBW4*massBW4) / (2.0*sqrt(s));

	if( (e3*e3 - massBW3*massBW3) < 0.0 ) return false;
	double p3 = sqrt(e3*e3 - massBW3*massBW3);
	if( (e3*e3 - massBW3*massBW3) < 0.0) return false;
	double e4 = (s + massBW4*massBW4 - massBW3*massBW3) / (2.0*sqrt(s));
	if(e3 < 0.0 || e4 < 0.0) return false;
	double p4 = sqrt(e4*e4 - massBW4*massBW4);

	if(fabs(p3-p4) > 0.000001) {
		cout << "momentum of the two resonances in CM frame are not equal, something wrong" << endl;
		exit(-1);
	}
	double p = p3;

	// generate t according to t-slope
	TLorentzVector beamCM = beam->getLorentzVector();
	TLorentzVector targetCM = target->getLorentzVector();
	TVector3 boostVector = (beam->getLorentzVector() + target->getLorentzVector()).BoostVector();
	beamCM.Boost(-boostVector);
	targetCM.Boost(-boostVector);	

	double e1 = beamCM.E();
	double p1 = beamCM.P();
	double e2 = targetCM.E();
	double p2 = targetCM.P();

	if(fabs(e1+e2-e3-e4) > 0.000001) return false;
	if(fabs(p1-p2-p3+p4) > 0.000001) return false;

	double t0 = (e1 - e3)*(e1 - e3) - (p1 - p3)*(p1 - p3);
	double t1 = (e1 - e3)*(e1 - e3) - (p1 + p3)*(p1 + p3);

	double t = tFun->GetRandom(fabs(t0), fabs(t1));
	double costheta = -t - (e1 - e3)*(e1 - e3) + (p1*p1 + p3*p3);
	costheta = costheta / (2.0*p1*p3);
	if(abs(costheta) > 1.0) return false;

       	double sintheta = sqrt(1.0 - costheta*costheta);
	double phi = TMath::TwoPi()*rand->Uniform();

	double px = p * sintheta * cos(phi);
	double py = p * sintheta * sin(phi);
	double pz = p * costheta;

	TLorentzVector lv3, lv4;
	lv3.SetXYZM(px, py, pz, massBW3);
	lv4.SetXYZM(-px, -py, -pz, massBW4);

	//==================================
	// Boost to lab frame
	//==================================
	
	lv3.Boost(boostVector);
	lv4.Boost(boostVector);
	
	midFinal[0]->setLorentzVector(lv3);
	midFinal[1]->setLorentzVector(lv4);

	midFinal[0]->setVertexTime(beam->getVertexTime());
	midFinal[1]->setVertexTime(beam->getVertexTime());
	return true;
}

void ParticleSimulator::decayResonance()
{	
	finalParticles.clear();

	for(unsigned int i=0; i<midFinal.size(); i++){
		if(midFinal[i]->isStable()) {
			finalParticles.push_back(*midFinal[i]);
			continue;
		}

		CompositeParticle *res = midFinal[i];

		double massRes = res->getLorentzVector().M();
		vector<Particle*> decay = res->getDecayProducts();

		double mass1 = decay[0]->getMass();
		double mass2 = decay[1]->getMass();

		double p1 = massRes*massRes - (mass1 + mass2)*(mass1 + mass2);
		double p2 = massRes*massRes - (mass1 - mass2)*(mass1 - mass2);
		double p = TMath::Sqrt(p1 * p2) / (2.0 * massRes);		// momentum in center of mass frame of res1
	
		double costheta = res->getDecayFun()->GetRandom();
		double sintheta = sqrt(1.0 - costheta*costheta);
		double phi = rand->Uniform(TMath::TwoPi());

		double pxRest = p * sintheta * cos(phi);
		double pyRest = p * sintheta * sin(phi);
		double pzRest = p * costheta;

		TLorentzVector lv1, lv2;
		lv1.SetXYZM(pxRest, pyRest, pzRest, mass1);

		TLorentzVector lvRes = res->getLorentzVector();

		lv1.RotateX(-lvRes.Phi());
		lv1.RotateY(TMath::Pi()/2.0);
		lv1.RotateZ(TMath::Pi()/2.0);
		lv2.SetVectM(-lv1.Vect(), mass2);

		TVector3 normal = (beam->getLorentzVector().Vect()).Cross(lvRes.Vect());

		TVector3 boostToRes = lvRes.BoostVector();
		lv1.Boost(boostToRes);
		lv2.Boost(boostToRes);

		decay[0]->setLorentzVector(lv1);
		decay[1]->setLorentzVector(lv2);

		TLorentzVector decayVertex = getDecayPoint(midFinal[i]);
		decay[0]->setVertexTime(decayVertex);
		decay[1]->setVertexTime(decayVertex);

		finalParticles.push_back(*decay[0]);
		finalParticles.push_back(*decay[1]);
	}
}

TLorentzVector ParticleSimulator::getDecayPoint(Particle *part)
{
	double p = part->getLorentzVector().P();
	double e = part->getLorentzVector().E();
	double time = part->getLifeTime();
	double mass = part->getLorentzVector().M();

	double theta = part->getLorentzVector().Theta();
	double phi = part->getLorentzVector().Phi();

	/*
		************************************
		NOTE: do not delete	
		P(x) = exp(-mass * x * / (p * time))
 		P(x) is uniform in [0,1], say u
		ln u = - mass * x / (p * time)
		See, PDG page 321
		************************************
 	*/

	double l0 = (time * p) / mass;
	double rn = rand->Uniform(1.0/l0);
	double l = - l0 * TMath::Log(rn * l0);
	double x = l * TMath::Sin(theta) * TMath::Cos(phi);
	double y = l * TMath::Sin(theta) * TMath::Sin(phi);
	double z = l * TMath::Cos(theta);
	double t = l * e / p;
	TLorentzVector lv(x, y, z, t);

	TLorentzVector tempVect = part->getVertexTime() + lv;
	return tempVect;
}

	
vector<Particle> ParticleSimulator::getResult()
{
	Particle part1 = *midFinal[0];
	Particle part2 = *midFinal[1];
	finalParticles.push_back(part1);
	finalParticles.push_back(part2);
	return finalParticles; 
}

void ParticleSimulator::print()
{
	int mode = beam->getMode();
	double emin = beam->getEmin();
	double emax = beam->getEmax();

	int size = 90;
	for(int i=0; i<size; i++) cout << "*";
	cout << endl;

	string st = "Simulate 1 + 2 -> 3 + 4 reaction";
	cout << "*"; for(unsigned int i=0; i<(size-st.size())/2; i++) cout << " ";
	cout << st; for(unsigned int i=0; i<(size-st.size())/2-2; i++) cout << " "; cout << "*" << endl;
	
	st = beam->getName() + " + " + target->getName() + " --> " + midFinal[0]->getName() + " + " + midFinal[1]->getName()  + " --> "  + 
	(midFinal[0]->getDecayProducts())[0]->getName() + " " +
	(midFinal[0]->getDecayProducts())[1]->getName() + " + " + (midFinal[1]->getDecayProducts())[0]->getName() + " " + 
	(midFinal[1]->getDecayProducts())[1]->getName();		
	cout << "*"; for(unsigned int i=0; i<(size-st.size())/2; i++) cout << " ";
	cout << st;
	for(unsigned int i=0; i<(size-st.size())/2-2; i++) cout << " "; cout << "*" << endl;
	cout << "*"; for(int i=0; i<size-2; i++) cout << " "; cout << "*" << endl;
	for(int i=0; i<size; i++) cout << "*";
	cout << endl;

	if(mode == 0) {
		char ch[200];
		sprintf(ch, "%.1f", emin); 
		string st = "    Will generate photon of fixed energy " + string(ch) + " GeV";
		cout << "*" << st;
		for(unsigned int i=0; i<size-st.size()-2; i++) cout << " ";
		cout << "*" << endl;
	}
	else if(mode == 1) {
		char ch1[200], ch2[200];
		sprintf(ch1, "%.1f", emin);
		sprintf(ch2, "%.1f", emax);
		string st = "    Will generate flat photon energy distribution between " + string(ch1) + " and " + string(ch2) + " GeV";
		cout << "*" << st;
		for(unsigned int i=0; i<size-st.size()-2; i++) cout << " "; 
		cout << "*" << endl;
        }
        else if(mode == 2) {
		char ch1[200], ch2[200];
		sprintf(ch1, "%.1f", emin);
		sprintf(ch2, "%.1f", emax);
		string st = "    Will generate bremsstrulung photon weighted by 1/E between " + string(ch1) + " and " + string(ch2) + " GeV";
		cout << "*" << st; 
		for(unsigned int i=0; i<size-st.size()-2; i++) cout << " "; 
		cout << "*" << endl;
	}
	else {
		cout << "beam energy distribution is not set up: aborting" << endl;
		exit(-1);
	}

	char ch1[200], ch2[200];

	sprintf(ch1, "%.1f", tSlope);
	st = "    t-slope set to  " + string(ch1);
	cout << "*" + st;
	for(unsigned int i=0; i<size-st.size()-2; i++) cout << " ";
	cout << "*" << endl;

	sprintf(ch1, "%.1f", targetPos.X());
	sprintf(ch2, "%.1f", targetPos.Y());
	st = "    target position " + string(ch1) + " " + string(ch2) + " cm"; 
	cout << "*" << st;
	for(unsigned int i=0; i<size-st.size()-2; i++) cout << " ";
	cout << "*" << endl;

	sprintf(ch1, "%.1f", midFinal[0]->getPol());
	st = "    polarization of " + midFinal[0]->getName() + " set to " + string(ch1);
	cout << "*" << st;
	for(unsigned int i=0; i<size-st.size()-2; i++) cout << " ";
	cout << "*" << endl;

	sprintf(ch1, "%.1f", midFinal[1]->getPol());
	st = "    polarization of " + midFinal[1]->getName() + " set to " + string(ch1);
	cout << "*" << st;
	for(unsigned int i=0; i<size-st.size()-2; i++) cout << " ";
	cout << "*" << endl;

	sprintf(ch1, "%.1f", midFinal[0]->getAlpha());
	st = "    decay parameter alpha of " + midFinal[0]->getName() + " set to " + string(ch1);
	cout << "*" << st;
	for(unsigned int i=0; i<size-st.size()-2; i++) cout << " ";
	cout << "*" << endl;
	
	sprintf(ch1, "%.1f", midFinal[1]->getAlpha());
	st = "    decay parameter alpha of " + midFinal[1]->getName() + " set to " + string(ch1);
	cout << "*" << st;
	for(unsigned int i=0; i<size-st.size()-2; i++) cout << " ";
	cout << "*" << endl;

	st = "    ParticleSimulator initialization done";
	cout << "*" << st;
	for(unsigned int i=0; i<size-st.size()-2; i++) cout << " "; 
	cout << "*" << endl;

	cout << "*"; for(int i=0; i<size-2; i++) cout << " "; cout << "*" << endl;
	for(int i=0; i<size; i++) cout << "*";
	cout << endl << endl;
}	
