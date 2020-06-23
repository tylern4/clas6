
#ifndef PARTIND_INCLUDED_
#define PARTIND_INCLUDED_

#include "c_stds.h"
#include "phys_consts.h"
#include "bos_pod.h"
#include "RootF.h"
#include "BatchService.h"
#include "RunControl.h"
#include "TLorentzVector.h"

typedef struct 
{
 int part_id;
 int part_ind;
} partinfo_t;


class physEVNT_c
{

public:
    
        TLorentzVector   Beam ;
        TLorentzVector   Target ;
   	

		   physEVNT_c();
		   ~physEVNT_c();

	void       Insert(int Pid, int evnt_ind);

	int        GetNpartID(int Pid);
	int        GetNpart();
	int	   IsOK() ;
	
	TLorentzVector   GetPart( int Pid, int wich );
	TLorentzVector   GetPart( int Pid, int which , const char* what );
	
	float	   Get_Mass_Hadron2(int pnum);
	float	   Get_Mass_Hadron2(int Pid, int num);
	

	int        GetInv( float *iW, float *iQ2, float *iEps, float *iGamma );
	float	   GetECEnergy( int PID, int which );
	float 	   GetBField();

	int	   BuildEVNT( );
	
	void       Reset();
	void       Print();


	BatchService *World ;	
	
private:

   	clasEVNT_t *Event ;
	clasHEVT_t *Hevt  ;
	clasSCPB_t *SCPB  ;
	clasECPB_t *ECPB  ;
	clasDCPB_t *DCPB  ;
	
        partinfo_t parts[20];
 
    	int 	   StatusFlag;
	int        nparts;
	float      W;
	float      Q2;
	float      Eps;
	float      Gamma;
	float 	   BField;
			
	int	   Electron_ID();
	int	   Proton_ID();
	int	   Neutron_ID();
	int	   Pi_Plus_ID();
	int	   Pi_Minus_ID();
	int	   K_Plus_ID();
	int	   K_Minus_ID();
	int	   Deutron_ID();
	
        float Mass_Of_ID(int Pid)	;
};


#endif

