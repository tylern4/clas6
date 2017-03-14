#ifndef __TAGR__
#define __TAGR__

#include "TObject.h"

class TAGR {
	private:
		float ERG, TTAG, TPHO;
		int STAT, T_id, E_id;
	
	public:
		TAGR() { }
		virtual ~TAGR() { }

		void setERG(float e)		{ ERG = e; }
		void setTTAG(float t)		{ TTAG = t; }
		void setTPHO(float t)		{ TPHO = t; }
		void setSTAT(int s)		{ STAT = s; }
		void setT_id(int i)		{ T_id = i; }
		void setE_id(int e)		{ E_id = e; }

		float getERG()			const { return ERG; }
		float getTTAG()			const { return TTAG; }
		float getTHO()			const { return TPHO; }
		int getSTAT()			const { return STAT; }
		int getT_id()			const { return T_id; }
		int getE_id()			const { return E_id; }

		ClassDef(TAGR, 1);
};
#endif
