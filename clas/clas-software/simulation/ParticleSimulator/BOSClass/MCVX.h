#ifndef __MCVX__
#define __MCVX__

#include "TObject.h"

class MCVX : public TObject {
	private:
		float x, y, z;
		int flag;
		float tof;
	
	public:
		MCVX() { }
		MCVX(MCVX* );
		virtual ~MCVX() { }

		void setX(float xx)		{ x = xx; }
		void setY(float yy)		{ y = yy; }
		void setZ(float zz)		{ z = zz; }
		void setFlag(int f)		{ flag = f; }
		void setTof(float t)		{ tof = t; }

		float getX()			const { return x; }
		float getY()			const { return y; }
		float getZ()			const { return z; }
		int getFlag()			const { return flag; }
		float getTof()			const { return tof; }
	
		ClassDef(MCVX, 1);
};
#endif
