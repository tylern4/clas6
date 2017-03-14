#include "WriteFile.h"
#include "TLorentzVector.h"
#include<iomanip>

TClonesArray *WriteFile::gmctkArray = 0;
TClonesArray *WriteFile::gmcvxArray = 0;

WriteFile::WriteFile(string name)
{
	root = false;
	bos = false;
	lund = false;

	int len = name.size();
	string ext;
	int st = 0;
	for(int i=0; i<len; i++) {
		if(name[i] == '.') st = 1;
		if(st == 1) ext += name[i];
	}
	cout << "=========== " << name << "\t" << ext << endl;
	if(ext == string(".root")) root = true;
	else if(ext == string(".bos")) bos = true;
	else if(ext == string(".txt")) lund = true;
	else {
		cout << "wrong file extension for the output file: aborting  " << name << "\t" << ext << endl << endl;;
		exit(-1);
	}

	if(root) { 
		if(!gmctkArray) gmctkArray = new TClonesArray("MCTK", 10000);
		mctkArray = gmctkArray;
		fNmctk = 0;

		if(!gmcvxArray) gmcvxArray = new TClonesArray("MCVX", 10000);
		mcvxArray = gmcvxArray;
		fNmcvx = 0;

		file = new TFile(name.c_str(), "recreate");
		simEvent = new TTree("simEvent", "simEvent");

		fhead = new HEAD();
		ftagr = new TAGR();

		simEvent->Branch("HEAD", "HEAD", &fhead, 88000, 2);
		simEvent->Branch("TAGR", "TAGR", &ftagr, 88000, 2);
		simEvent->Branch("MCTK", &gmctkArray, 88000, 2);
		simEvent->Branch("MCVX", &gmcvxArray, 88000, 2);
	}
	
	else if(bos) {
		cout << "Output Bos File ==> " << name << endl;;

		bosInit(bcs_.iw, NBCS);
		
		char ch[200];
		sprintf(ch, "%s", name.c_str());

		int iostatus = bosOpen(ch, "w", &bosFileHandleOut);
		if(iostatus)
		{
			cout << "ERROR: File " << name << "  can not be opened" << endl;
			exit(-1);
		}

		int status = bosNformat(bcs_.iw, (void*) "HEAD", "8I");
		if(status != 0) {
			cout << "HEAD formate problem" << endl;
			exit(-1);
		}

		status = bosNformat(bcs_.iw, (void*) "MCTK", "6F,5I");
		if(status != 0) {
			cout << "MCTK formate problem" << endl;
			exit(-1);
		}

		status = bosNformat(bcs_.iw, (void*)"MCVX", "4F,I");
		if(status != 0) {
			cout << "MCVX formate problem" << endl;
			exit(-1);
		}

		status = bosNformat(bcs_.iw, (void*)"TAGR", "3F,3I");
		if(status != 0) {
			cout << "TAGR formate problem" << endl;
			exit(-1);
		}

		int ind =  bosNcreate(bcs_.iw, "HEAD", 0, 8, 1);
		if(ind < 1) {
			cout << "HEAD create problem" << endl;
			exit(-1);
		}

		ind =  bosNcreate(bcs_.iw, "MCTK", 0, 11, 4);
		if(ind < 1) {
			cout << "MCTK create problem" << endl;
			exit(-1);
		}

		ind =  bosNcreate(bcs_.iw, "MCVX", 0, 5, 4);
		if(ind < 1) {
			cout << "MCVX create problem" << endl;
			exit(-1);
		}

		ind =  bosNcreate(bcs_.iw, "TAGR", 0, 6, 1);
		if(ind < 1) {
			cout << "TAGR create problem" << endl;
			exit(-1);
		}
	}

	if(lund) {
		cout << "Output text File ==> " << name << endl;
		out.open(name.c_str());
	}	
}	


void WriteFile::clear()
{
	if(root) {
		fNmctk = 0;
		fNmcvx = 0;
	}
	if(bos) {
		bosLdrop(bcs_.iw, "E");
		bosNgarbage(bcs_.iw);
	}
}

int WriteFile::fill(vector<int> hd, time_t time, ParticleSimulator *sim)
{
	int n = 0;

	if(root) {
		n = fillToRoot(hd, time, sim);
		return n;
	}

	if(bos) {
		n = fillToBos(hd, time, sim);
		return n;
	}

	if(lund) {
		n = fillToLund(hd, time, sim);
		return n;
	}

	return n;
}

int WriteFile::fillToRoot(vector<int> hd, time_t time, ParticleSimulator *sim)
{
	fhead->setVersion(hd[0]);
	fhead->setNrun(hd[1]);
	fhead->setNevent(hd[2]);
	fhead->setTime(time);
	fhead->setType(hd[3]);
	fhead->setRoc(hd[4]);
	fhead->setEvtclass(hd[5]);
	fhead->setTrigbits(hd[6]);

	ftagr->setERG((float) sim->getBeam()->getLorentzVector().E());
	ftagr->setTTAG(-(float) sim->getBeam()->getVertexTime().Z() / 29.9792);
	ftagr->setTPHO(-(float) sim->getBeam()->getVertexTime().Z() / 29.9792);
	ftagr->setSTAT(15);
	ftagr->setT_id(0);
	ftagr->setE_id(0);

	vector<Particle> part = sim->getFinalParticles();

        int size = part.size();
        for(int i=0; i<size; i++){
                Particle pt = part[i];

                MCTK *fmctk = new MCTK();
                float p = pt.getLorentzVector().P();
                fmctk->setCx((float) (pt.getLorentzVector().Px()/p));
                fmctk->setCy(pt.getLorentzVector().Py()/p);
                fmctk->setCz(pt.getLorentzVector().Pz()/p);
                fmctk->setPmom(p);
                fmctk->setMass(pt.getLorentzVector().M());
                fmctk->setId(pt.getId());
                fmctk->setCharge(pt.getCharge());
                fmctk->setFlag(1);
                fmctk->setBeg_vtx(i+1);
                fmctk->setEnd_vtx(0);
                fmctk->setParent(pt.getParent());

                TClonesArray &mctkA = *mctkArray;
                new(mctkA[fNmctk++]) MCTK(fmctk );

                MCVX *fmcvx = new MCVX();
                fmcvx->setX(pt.getVertexTime().X());
                fmcvx->setY(pt.getVertexTime().Y());
                fmcvx->setZ(pt.getVertexTime().Z());
                fmcvx->setFlag(1);
                fmcvx->setTof(0.0);

		TClonesArray &mcvxA = *mcvxArray;
                new(mcvxA[fNmcvx++]) MCVX(fmcvx );
        }
        simEvent->Fill();
        return 1;
}

int WriteFile::fillToBos(vector<int> hd, time_t time, ParticleSimulator *sim)
{
	int ind = bosNlink(bcs_.iw, "HEAD", 0);

	clasHEAD_t *headPtr;
	headPtr = (clasHEAD_t*) &bcs_.iw[ind];
	headPtr->head[0].version = hd[0];
	headPtr->head[0].nrun = hd[1];
	headPtr->head[0].nevent = hd[2];
	headPtr->head[0].time = time;
	headPtr->head[0].type = hd[3];
	headPtr->head[0].roc = hd[4];
	headPtr->head[0].evtclass = hd[5];
	headPtr->head[0].trigbits = hd[6];

	clasTAGR_t *tagrPtr;
	ind = bosNlink(bcs_.iw, "TAGR", 0);
	tagrPtr = (clasTAGR_t*) &bcs_.iw[ind];
	tagrPtr->tagr[0].ERG = (float) sim->getBeam()->getLorentzVector().E();
	tagrPtr->tagr[0].TTAG = -(float) sim->getBeam()->getVertexTime().Z() / 29.9792;
	tagrPtr->tagr[0].TPHO = -(float) sim->getBeam()->getVertexTime().Z() / 29.9792;
	tagrPtr->tagr[0].STAT = 15;
	tagrPtr->tagr[0].T_id = 0;
	tagrPtr->tagr[0].E_id = 0;

	clasMCTK_t *mctkPtr;
	ind = bosNlink(bcs_.iw, "MCTK", 0);
	mctkPtr = (clasMCTK_t*) &bcs_.iw[ind];

	clasMCVX_t *mcvxPtr;
	ind = bosNlink(bcs_.iw, "MCVX", 0);
	mcvxPtr = (clasMCVX_t*) &bcs_.iw[ind];

	vector<Particle> part = sim->getFinalParticles();

	for(unsigned int j=0; j<part.size(); j++) {
		Particle pt = part[j];

		mctkPtr->mctk[j].cx = (float) (pt.getLorentzVector().Px()/pt.getLorentzVector().P());
		mctkPtr->mctk[j].cy = (float) (pt.getLorentzVector().Py()/pt.getLorentzVector().P());
		mctkPtr->mctk[j].cz = (float) (pt.getLorentzVector().Pz()/pt.getLorentzVector().P());
		mctkPtr->mctk[j].pmom = (float) (pt.getLorentzVector().P());
		mctkPtr->mctk[j].mass = (float) (pt.getLorentzVector().M());
		mctkPtr->mctk[j].id = pt.getId();
		mctkPtr->mctk[j].charge = pt.getCharge();
		mctkPtr->mctk[j].flag = 1;
		mctkPtr->mctk[j].beg_vtx = j+1;
		mctkPtr->mctk[j].end_vtx = 0;
		mctkPtr->mctk[j].parent = pt.getParent();

		mcvxPtr->mcvx[j].x = (float) pt.getVertexTime().X();
		mcvxPtr->mcvx[j].y = (float) pt.getVertexTime().Y();
		mcvxPtr->mcvx[j].z = (float) pt.getVertexTime().Z();
		mcvxPtr->mcvx[j].flag = 1;
		mcvxPtr->mcvx[j].tof = 0.0;
	}
	int status = bosWrite(bosFileHandleOut, bcs_.iw, "HEADTAGRMCTKMCVXPARTMCHD");
	if(status != 0){
		cout << "writing error in bos file: aborting" << endl;
		exit(-1);
	}
	bosLdrop(bcs_.iw, "E");
	bosNgarbage(bcs_.iw);
	return 1;
}


int WriteFile::fillToLund(vector<int> hd, time_t time, ParticleSimulator *sim)
{
	vector<Particle> part = sim->getFinalParticles();
	int nPart = part.size();
	int tNucleon = 1;
	int tProron = 1;
	float tPolarization = 0.0;
	float bPolarization = 0.0;
	double x = 0;
	double y = 0;
	double w = (sim->getBeam()->getLorentzVector() + sim->getTarget()->getLorentzVector()).M2();
	double Q2 = 0;
	double nu = 0;

	int n1 = 4;
	int n2 = 12;

	out << fixed << nPart << setw(n1) << tNucleon << setw(n1) << tProron << setw(n2) << tPolarization << setw(n2) << bPolarization << setw(n2) << x << setw(n2) << y << setw(n2) << w << setw(n2) << Q2 << setw(n2) << nu << endl;

	for(int i=0; i<nPart; i++) {
		Particle pt = part[i];
		int charge = pt.getCharge();
		int type = pt.getType();
		int id = pt.getId();
		int parent = pt.getParent();
		int child = pt.getChild();
		double px = pt.getLorentzVector().Px();
		double py = pt.getLorentzVector().Py();
		double pz = pt.getLorentzVector().Pz();
		double e = pt.getLorentzVector().E();
		double mass = pt.getLorentzVector().M();
		double x = pt.getVertexTime().X();
		double y = pt.getVertexTime().Y();
		double z = pt.getVertexTime().Z();

		out << fixed << i+1 << setw(n1) << charge << setw(n1) << type << setw(n2) << id << setw(n2) << parent << setw(n2) << child << setw(n2) << px << setw(n2) << py << setw(n2) << pz << setw(n2) << e << setw(n2) << mass << setw(n2) << x << setw(n2) << y << setw(n2) << z << endl;
	}

	return 1;
}

void WriteFile::finish()
{
	if(root) {
		file->Write();
		file->Close();
	}
	
	else if(bos) {
		bosLdrop(bcs_.iw, "E");
		bosNgarbage(bcs_.iw);
		bosWrite(bosFileHandleOut, NULL, "0");
		bosClose(bosFileHandleOut);
	}

	else if(lund) out.close();
}
