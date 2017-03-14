#ifndef __MCTK__
#define __MCTK__

#include "TObject.h"

class MCTK : public TObject {
	private:
		float cx, cy, cz, pmom, mass;
		int id, charge, flag, beg_vtx, end_vtx, parent;
		
	public:
		MCTK() { }
		MCTK(MCTK* );
		virtual ~MCTK() { }

		void setCx(float x)		{ cx = x; }
		void setCy(float y)		{ cy = y; }
		void setCz(float z)		{ cz = z; }
		void setPmom(float p)		{ pmom = p; }
		void setMass(float m)		{ mass = m; }
		void setId(int i)		{ id = i; }
		void setCharge(int c)		{ charge = c; }
		void setFlag(int f)		{ flag = f; }
		void setBeg_vtx(int b)		{ beg_vtx = b; }
		void setEnd_vtx(int e)		{ end_vtx = e; }
		void setParent(int p)		{ parent = p; }

		float getCx()			const { return cx; }
		float getCy()			const { return cy; }
		float getCz()			const { return cz; }
		float getPmom()			const { return pmom; }
		float getMass()			const { return mass; }
		int getId()			const { return id; }
		int getCharge()			const { return charge; }
		int getFlag()			const { return flag; }
		int getBeg_vtx()		const { return beg_vtx; }
		int getEnd_vtx()		const { return end_vtx; }
		int getParent()			const { return parent; }	

		ClassDef(MCTK, 1);
};
#endif
