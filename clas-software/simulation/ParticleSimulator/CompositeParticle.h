//************************************
//*   Author: Chandra Shekhar Sah    *
//*   ParticleSimulator version 1    *
//*          ODU, March 2012         *
//************************************
//* Class to hold the resonanaces, inharited from Particle.
/*! This class holds a resonance which decay to stable particles.
 */

#ifndef __CompositeParticle__
#define __CompositeParticle__

#include "Particle.h"
#include<vector>
#include "TF1.h"
using namespace std;

class CompositeParticle : public Particle {
	private:
		TF1 *polFun;					// function for decay distribution
		double alpha;					// decay distribution asymmetry parameter
		double pol;					// polarization of the decaying particle
		vector<Particle*> decayProducts;

	public:
		CompositeParticle(int i);
		CompositeParticle(Particle *pa);
		CompositeParticle(const CompositeParticle& pa);
		virtual ~CompositeParticle() { }

		void setPol(double p = 0.00000000)		{ pol = p; }
		void setAlpha(double al = 0.00000000)		{ alpha = al; }
		void setDecayProducts(Particle* de)		{ decayProducts.push_back(de); }

		double getPol()					{ return pol; }
		double getAlpha()				{ return alpha; }
		TF1 *getDecayFun()				{ return polFun; }
		vector<Particle*> getDecayProducts()		const { return decayProducts; }
};
#endif
