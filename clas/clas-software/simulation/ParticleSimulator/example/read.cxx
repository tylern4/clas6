#include<iostream>
#include "TFile.h"
#include "TTree.h"
#include "TChain.h"
#include "TBranch.h"
#include "TLorentzVector.h"
#include "TClonesArray.h"
#include "HEAD.h"
#include "TAGR.h"
#include "MCTK.h"
#include "MCVX.h"
using namespace std;

int main(int argc, char** argv)
{
	TChain *chain = new TChain("simEvent");
	chain->Add("check.root");
	int nEvent = chain->GetEntries();
	cout << "Total entries = " << nEvent << endl;

	TBranch *headBr = chain->GetBranch("HEAD");
	TBranch *tagrBr = chain->GetBranch("TAGR");
	TBranch *mctkBr = chain->GetBranch("MCTK");
	TBranch *mcvxBr = chain->GetBranch("MCVX");

	HEAD *head = new HEAD();
	TAGR *tagr = new TAGR();
	
	headBr->SetAddress(&head);
	tagrBr->SetAddress(&tagr);
	TClonesArray *mctkA = new TClonesArray("MCTK", 10000);
	mctkBr->SetAddress(&mctkA);
	TClonesArray *mcvxA = new TClonesArray("MCVX", 10000);
	mcvxBr->SetAddress(&mcvxA);

	for(int i=0; i<nEvent; i++) {
		chain->GetEntry(i);
		if(!((i*100)%nEvent)) cout << i << "/" << nEvent << " ==> " << float(i)*100/nEvent << " %" << endl;

		double eg = tagr->getERG();
		if(eg <= 0) continue;

		int size1 = mctkA->GetEntries();
		int size2 = mcvxA->GetEntries();
		if(size1 != size2) {
			cout << "entries in MCTK and MCVX are not equal, something wrong: aborting" << endl;
			exit(-1);		
		}

		for(int j=0; j<size1; j++) {
			MCTK *mctk = (MCTK*) mctkA->At(j);
			double cx = mctk->getCx();
			double cy = mctk->getCy();
			double cz = mctk->getCz();
			double p = mctk->getPmom();
			double px = p * cx;
			double py = p * cy;
			double pz = p * cz;		

			MCVX *mcvx = (MCVX*) mcvxA->At(j);
			double x = mcvx->getX();
			double y = mcvx->getY();
			double z = mcvx->getZ();
			cout << p << endl;
		}
	}
}
