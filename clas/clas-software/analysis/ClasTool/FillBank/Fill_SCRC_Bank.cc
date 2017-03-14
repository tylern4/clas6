//////////////////////////////////////////////////////////////
//
//   File Fill_SCPB_Bank.cc
//
//   This procedure Fills TSCPBClass class from BOS Bank 
//
//  Author :  Gagik Gavalian   UNH  11/10/1999
//
//
//  This file was automaticaly Generated on : 
//	Tue Nov 23 18:21:50 EST 1999 
//////////////////////////////////////////////////////////////


#include "TROOT.h"
#include "TSCRCClass.h"

#include "clasbanks.h"


// Function Prototype 
void  Fill_SCRC_Bank(TSCRCClass *gcSCRC,SCRC *pSCRC , int nrow);


void  Fill_SCRC_Bank(TSCRCClass *gcSCRC, SCRC *pSCRC , int nrow){
gcSCRC->ID  = pSCRC->get_id(nrow);
gcSCRC->Energy  = pSCRC->get_energy(nrow);
gcSCRC->Time  = pSCRC->get_time(nrow);
gcSCRC->dEnergy  = pSCRC->get_denergy(nrow);
gcSCRC->dTime  = pSCRC->get_dtime(nrow);

gcSCRC->X  = pSCRC->get_x(nrow);
gcSCRC->Y  = pSCRC->get_y(nrow);
gcSCRC->Z  = pSCRC->get_z(nrow);
gcSCRC->dX  = pSCRC->get_dx(nrow);
gcSCRC->dY  = pSCRC->get_dy(nrow);
gcSCRC->dZ  = pSCRC->get_dz(nrow);

gcSCRC->Status  = pSCRC->get_status(nrow);
}



////////////////////////////////////////////////////////////////////////////////////////
//     End Of File  Fill_SCPB_Bank.cc
//////////////////////////////////////////////////////////////////////////////////////

