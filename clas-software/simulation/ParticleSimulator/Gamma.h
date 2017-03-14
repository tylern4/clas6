//*************************************
//*    Author: Chandra Shekhar Sah    *
//*    ParticleSimulator version 1    *
//*          ODU, March 2012	      *
//*************************************
//* Class to generate photon beam, derived from Particle Class.
/*! This class is for photon beam. It generates the photon beam of desired energy distribution
*/ 

#ifndef __Gamma__
#define __Gamma__

#include "Particle.h"
#include "TRandom3.h"
#include "TF1.h"

class Gamma : public Particle {
	private:
		int seed;				// random seed

		double eMin;				// gamma minimum energy
		double eMax;				// gamma maximum energy
		double eGamma;				// gamma energy
		int mode;				// mode of energy distribution: 0 = fixed, 1 = uniform, 2 = bremsstrahlung
		TRandom3 *rand;
		TF1 *fun;

	public:
		Gamma();
		Gamma(double);				// fixed gamma energy 
		Gamma(double, double, int);		// min energy, max energy, mode of energy distribution: 1 = uniform, 2 = bremsstrahlung

		virtual ~Gamma() { }

		void init();
		void setSeed(int se )           { seed = se; }
		void setEmin(double min)	{ eMin = min; }
		void setEmax(double max)	{ eMax = max; }
		void setE(double eg)		{ eGamma = eg; }	
		void setMode(int mo)		{ mode = mo; }

		double getEmin()		const { return eMin; }
		double getEmax()		const { return eMax; }
		double getE()			const { return eGamma; }
		int getMode()			const { return mode; }

		void generateGamma();			// Generate gamma energy and set 4-vector of the gamma. 	
};
#endif
