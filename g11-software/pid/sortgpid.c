#include <string.h>
#include <ntypes.h>
#include <bostypes.h>
#include <kinematics.h>
#include <utility.h>
#include <particleType.h>

int sortgpid(clasGPID_t *GPID, sortgpid_t *sortg){
  int i;

  sortg->nelec = sortg->nprot = sortg->nPip = sortg->nPim = sortg->nKp = 0;
  sortg->nKm = sortg->ngam = sortg->nneut = sortg->nUnk = sortg->nDeut = 0;
  sortg->npart = sortg->npos = sortg->nTrit = sortg->nantip = 0; 

  if (GPID){
    for(i=0; i < GPID->bank.nrow; i++){
      switch (GPID->gpid[i].pid) {
      case Electron:
	sortg->elec[sortg->nelec++] = &GPID->gpid[i];
	break;
      case Positron:
	sortg->pos[sortg->npos++] = &GPID->gpid[i];
	break;
      case Proton:
	sortg->prot[sortg->nprot++] = &GPID->gpid[i];
	break;
      case PiPlus:
	sortg->Pip[sortg->nPip++] = &GPID->gpid[i];
	break;
      case PiMinus:
	sortg->Pim[sortg->nPim++] = &GPID->gpid[i];
	break;
      case Gamma:
	sortg->gam[sortg->ngam++] = &GPID->gpid[i];
	break;
      case KPlus:
	sortg->Kp[sortg->nKp++] = &GPID->gpid[i];
	break;
      case KMinus:
	sortg->Km[sortg->nKm++] = &GPID->gpid[i];
	break;
      case Neutron:
	sortg->neut[sortg->nneut++] = &GPID->gpid[i];
	break;
      case Deuteron:
	sortg->Deut[sortg->nDeut++] = &GPID->gpid[i];
	break;
      case Triton:
	sortg->Trit[sortg->nTrit++] = &GPID->gpid[i];
	break;
      case AntiProton:
	sortg->Antip[sortg->nantip++] = &GPID->gpid[i];
	break;
      case Unknown:
	sortg->Unk[sortg->nUnk++] = &GPID->gpid[i];
	break;
      default:
	break;
      }
    }
    sortg->npart = GPID->bank.nrow;
    return(1);
  }
  return(0);
}
