#ifndef __HEAD__
#define __HEAD__

#include<time.h>
#include "TObject.h"
using namespace std;

class TDirectory;

class HEAD {
	private:
		int version;	
		int nrun;
		int nevent;
		time_t time;
		int type;
		int roc;
		int evtclass;
		int trigbits;

	public:
		HEAD() { }
		virtual ~HEAD() { }
		
		void setVersion(int v)		{ version = v; }
		void setNrun(int r)		{ nrun = r; }
		void setNevent(int e)		{ nevent = e; }
		void setTime(time_t t)		{ time = t; }
		void setType(int t)		{ type = t; }
		void setRoc(int r)		{ roc = r; }
		void setEvtclass(int e)		{ evtclass = e; }
		void setTrigbits(int t)		{ trigbits = t; }

		int getVersion()		const { return version; }
		int getNrun()			const { return nrun; }
		int getNevent()			const { return nevent; }
		time_t getTime()		const { return time; }
		int getType()			const { return type; }
		int getRoc()			const { return roc; }
		int getEvtclass()		const { return evtclass; }
		int getTrigbits()		const { return trigbits; }

		ClassDef(HEAD, 1);
};
#endif		
