#include "MCVX.h"

MCVX::MCVX(MCVX *vx)
{
	x = vx->getX();
	y = vx->getY();
	z = vx->getZ();
	flag = vx->getFlag();
	tof = vx->getTof();
} 
