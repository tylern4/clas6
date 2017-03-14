#include <string.h>
#include <ntypes.h>
#include <bostypes.h>
#include <kinematics.h>
#include <utility.h>
#include <particleType.h>

int sortpart(clasPART_t *PART, sortpart_t *sortp){
  int i;

  sortp->nelec = sortp->nprot = sortp->nPip = sortp->nPim = sortp->nKp = 0;
  sortp->nKm = sortp->ngam = sortp->nneut = sortp->nUnk = sortp->nDeut = 0;
  sortp->npart = 0;

  if (PART){
    for(i=0; i < PART->bank.nrow; i++){
      switch (PART->part[i].pid) {
      case Electron:
	sortp->elec[sortp->nelec++] = &PART->part[i];
	break;
      case Proton:
	sortp->prot[sortp->nprot++] = &PART->part[i];
	break;
      case PiPlus:
	sortp->Pip[sortp->nPip++] = &PART->part[i];
	break;
      case PiMinus:
	sortp->Pim[sortp->nPim++] = &PART->part[i];
	break;
      case Gamma:
	sortp->gam[sortp->ngam++] = &PART->part[i];
	break;
      case KPlus:
	sortp->Kp[sortp->nKp++] = &PART->part[i];
	break;
      case KMinus:
	sortp->Km[sortp->nKm++] = &PART->part[i];
	break;
      case Neutron:
	sortp->neut[sortp->nneut++] = &PART->part[i];
	break;
      case Deuteron:
	sortp->Deut[sortp->nDeut++] = &PART->part[i];
	break;
      case Unknown:
	sortp->Unk[sortp->nUnk++] = &PART->part[i];
	break;
      default:
	break;
      }
    }
    sortp->npart = PART->bank.nrow;
    return(1);
  }
  return(0);
}
