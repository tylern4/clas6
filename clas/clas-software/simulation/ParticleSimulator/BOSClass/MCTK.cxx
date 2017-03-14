#include "MCTK.h"
using namespace std;

MCTK::MCTK(MCTK *tk)
{
	cx = tk->getCx();
	cy = tk->getCy();
	cz = tk->getCz();
	pmom = tk->getPmom();
	mass = tk->getMass();
	id = tk->getId();
	charge = tk->getCharge();
	flag = tk->getFlag();
	beg_vtx = tk->getBeg_vtx();
	end_vtx = tk->getEnd_vtx();
	parent = tk->getParent();
}
