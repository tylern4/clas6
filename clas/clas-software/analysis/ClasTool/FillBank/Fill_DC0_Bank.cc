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
#include "TDC0Class.h"

#include "clasbanks.h"


// Function Prototype 
void  Fill_DC0_Bank(TDC0Class *gcDC0,DC0 *pDC0 , int nrow);


void  Fill_DC0_Bank(TDC0Class *gcDC0, DC0 *pDC0 , int nrow){
gcDC0->ID  = pDC0->get_ID(nrow);
gcDC0->TDC  = pDC0->get_TDC(nrow);
}



////////////////////////////////////////////////////////////////////////////////////////
//     End Of File  Fill_SCPB_Bank.cc
//////////////////////////////////////////////////////////////////////////////////////

