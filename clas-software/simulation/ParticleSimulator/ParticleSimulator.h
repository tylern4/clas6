//*************************************
//*    Author: Chandra Shekhar Sah    *
//*    ParticleSimulator version 1    *
//*          ODU, March 2012          *
//*************************************
//* Main class for particle simulator

#ifndef __ParticleSimulator__
#define __ParticleSimulator__

#include<vector>
#include "Particle.h"
#include "Gamma.h"
#include "CompositeParticle.h"
#include "TRandom3.h"
#include "TF1.h"
#include "TH1D.h"
#include<cmath>
#include "TFile.h"
using namespace std;

class ParticleSimulator {
	private:
		int seed;					// random seed

		Gamma *beam;					// beam particle object
		Particle *target;				// target particle object
		vector<CompositeParticle*> midFinal;		// vector of unstable particle objects
		vector<Particle> finalParticles;		// vector of final stable particles objects
		TVector2 targetPos;				// target position in lab frame
		double tSlope;					// t-slope

		TRandom3 *rand;
		TF1 *tFun;

	public:
		ParticleSimulator();
		virtual ~ParticleSimulator()	{ }

		void setSeed(int se)				{ seed = se; }
		void init();
		void initEvent()				{ finalParticles.clear(); }
		void print();

		void setBeam(Gamma *be)				{ beam = be; }		
		void setTarget(Particle *tr)			{ target = tr; }
		void setMidFinal(CompositeParticle *fn)		{ midFinal.push_back(fn); }
		void setFinalParticles(Particle pa)		{ finalParticles.push_back(pa); }
		void setTargetPos(double x1, double x2)		{ targetPos.Set(x1, x2); }
		void setTSlope(double t)			{ tSlope = t; }

		Gamma* getBeam()				const { return beam; } 		
		Particle* getTarget()				const { return target; }
		vector<CompositeParticle*> getMidFinal()	const { return midFinal; }
		vector<Particle> getFinalParticles()		const { return finalParticles; }	
		TLorentzVector getDecayPoint(Particle *part);
		double getTSlope()				{ return tSlope; }


		bool checkConservation();			// check conservation of charge and energy at the interaction vertex
		
		bool setMomentum();
		void decayResonance();
		bool makeEvent();
		vector<Particle> getResult();
};
#endif
