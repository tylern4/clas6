//***************************************
//*	Author: Chandra Shekhar Sah     *
//*	ParticleSimulator version 1     *
//*          ODU, March 2012	        *
//***************************************
//* base class to store single particle properties
//* This class hold one particle


#ifndef __Particle__
#define __Particle__

#include<string>
#include "TLorentzVector.h"
#include "TProcessID.h"
using namespace std;

class Particle {
	private:
		int id;					// PDG particle id
		string name;				// particle name
		int charge;				// charge
		bool stable;				// stable or unstable 
		int parent;
		int child;
		int type;
		double mass;				// PDG mass
		string spin;				// spin
		double lifeTime;			// life time
		double fullWidth;			// full width
		bool resonance;				// resonance or not
		bool boson;	
		bool lepton;
		bool baryon;				// baryon or not
		bool meson;				// mason or not
		int strangeness;
		TLorentzVector lvec;			// energy-momentum vector
		TLorentzVector vec;			// particle vertex and time
		
	public:
		Particle();
		Particle(int id);
		Particle(const Particle &part);
		virtual ~Particle() { }
	
		void setId(int i)			{ id = i; }
		void setName(string na)			{ name = na; }
		void setCharge(int i)			{ charge = i; }
		void setStable(bool st)			{ stable = st; }
		void setParent(int j)			{ parent = j; }
		void setChile(int j)			{ child = j; }
		void setType(int j)			{ type = j; }
		void setMass(double mas)		{ mass = mas; }
		void setSpin(string sp)			{ spin = sp; }
		void setLifeTime(double tm)		{ lifeTime = tm; }
		void setFullWidth(double width)		{ fullWidth = width; }
		void setResonance(bool st)		{ resonance = st; }
		void setBoson(bool b)			{ boson = b; }
		void setBaryon(bool b)			{ baryon = b; }
		void setMeson(bool m)			{ meson = m; }
		void setLepton(bool l)			{ lepton = l; }
		void setStrangeness(int s)		{ strangeness = s; }
		void setLorentzVector(TLorentzVector lv){ lvec = lv; }
		void setLorentzVector(double x, double y, double z, double m)	{ lvec.SetXYZM(x, y, z, m); }
		void setVertexTime(TLorentzVector v)	{ vec = v; }
		void setVertexTime(double x, double y, double z, double t)	{ vec.SetXYZT(x, y, z, t); }	

		int getId()			const { return id; }
		string getName()		const { return name; }
		int getCharge()			const { return charge; }
		bool isStable()			const { return stable; }
		int getParent()			const { return parent; }
		int getChild()			const { return child; }
		int getType()			const { return type; }
		double getMass()		const { return mass; }
		string getSpin()		const { return spin; }
		double getLifeTime()		const { return lifeTime; }
		double getFullWidth()		const { return fullWidth; }
		bool isResonance()		const { return resonance; }
		bool isBoson()			const { return boson; }
		bool isBaryon()			const { return baryon; }
		bool isMeson()			const { return meson; }
		bool isLepton()			const { return lepton; }
		int getStrangeness()		const { return strangeness; }
		TLorentzVector getLorentzVector() const	{ return lvec; }
		TLorentzVector getVertexTime()	  const { return vec; }		
};
#endif
