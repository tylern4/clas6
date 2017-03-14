//***************************************
//*	Author: Chandra Shekhar Sah     *
//*	ParticleSimulator version 1     *
//*          ODU, March 2012	        *
//***************************************
//class for output to root or bos file. It recognizes the file format by file extension.


#ifndef __WriteFile__
#define __WriteFile__

#include<iostream>
#include<string>
#include<vector>
#include<fstream>
#include "Particle.h"
#include "TLorentzVector.h"
#include "TClonesArray.h"
#include "TTree.h"
#include "TFile.h"
#include "TBranch.h"
#include "ParticleSimulator.h"
#include "HEAD.h"
#include "TAGR.h"
#include "MCTK.h"
#include "MCVX.h"
#include "include.h"

using namespace std;

extern "C" {
	#define NBCS  700000
	#include "bosio.h"
	#include "bosfun.h"
	typedef struct boscommon {
		int junk[5];
		int iw[NBCS];
	} BOScommon;
	extern BOScommon bcs_;
}

class WriteFile {
	private:
		bool root;	
		bool bos;
		bool lund;
		int bosFileHandleOut;

		HEAD *fhead;
		TAGR *ftagr;		

		TFile *file;
		TTree *simEvent;

		ofstream out;

		int fNmctk, fNmcvx;

		TClonesArray *mctkArray;
		TClonesArray *mcvxArray;

		static TClonesArray *gmctkArray;
		static TClonesArray *gmcvxArray;

	public:
		WriteFile(string name);
		virtual ~WriteFile() { }

		void init();

		int fill(vector<int> hd, time_t time, ParticleSimulator* );
		int fillToRoot(vector<int> hd, time_t time, ParticleSimulator* );
		int fillToBos(vector<int> hd, time_t time, ParticleSimulator* );
		int fillToLund(vector<int> hd, time_t time, ParticleSimulator* );

		void clear();
		void finish();

		TClonesArray *getMCTKtracks()	const {return mctkArray; }
		TClonesArray *getMCVXtracks()	const {return mcvxArray; }
};
#endif
