//************************************
//*   Author: Chandra Shekhar Sah    *
//*                                  *
//************************************
//* Class to hold Particle properties, inharited from Particle. Properties are set from PDF
/*! This class holds particle peroperties.
*/

#ifndef __ParticleData__
#define __ParticleData__

#include<iostream>
#include<string>
#include<vector>
#include "Particle.h"
using namespace std;

class ParticleData : public Particle {
	private:
		vector<Particle*> part;	

	public:
		ParticleData();
		virtual ~ParticleData() { }

		Particle *getParticle(int );

};
#endif
