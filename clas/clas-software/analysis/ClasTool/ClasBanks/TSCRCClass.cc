// @(#)ClasTool/ClasBanks:$Name:  $:$Id: TSCPBClass.cc,v 1.4 2007/03/01 22:26:59 holtrop Exp $
// Author:  Maurik Holtrop <http://www.physics.unh.edu/~maurik>
//____________________ 
//Begin_Html <!--
/* -->
<h1>TSCPBClass</h1>
<pre>
 Modified:
           Maurik (2/3/00) Comments.
</pre>
This class is used to access the 
SCPB
bank data written into the ROOT DST for the E2 run at Jefferson Lab.
It was generated automatically from the ddl file in 
packages/bankdefs/scpb.ddl
and later hand edited to provide comments and additional modifications.
The details on how exactly each variable is defined have to be extraced
from the RECSIS code.
<pre> 
<!-- */ 
// --> End_Html   
////////////////////////////////////////////////////////

#include "TSCRCClass.h"

ClassImp(TSCRCClass)

TSCRCClass::TSCRCClass(TSCRCClass *TmpSCRC){
ID   =   TmpSCRC->ID;
Energy   =   TmpSCRC->Energy;
dEnergy   =   TmpSCRC->dEnergy;
Time   =   TmpSCRC->Time;
dTime   =   TmpSCRC->dTime;
X    =   TmpSCRC->X;
Y    =   TmpSCRC->Y;
Z    =   TmpSCRC->Z;
dX   =   TmpSCRC->dX;
dY   =   TmpSCRC->dY;
dZ   =   TmpSCRC->dZ;
Status   =   TmpSCRC->Status;
}

void   TSCRCClass::Print(){
cout << "ID      " <<  ID  << endl;
cout << "Energy  " <<  Energy  << endl;
cout << "dEnergy " <<  dEnergy  << endl;
cout << "Time    " <<  Time  << endl;
cout << "dTime   " <<  dTime  << endl;
cout << "X       " <<  X  << endl;
cout << "Y       " <<  Y  << endl;
cout << "Z       " <<  Z  << endl;
cout << "dX       " <<  dX  << endl;
cout << "dY       " <<  dY  << endl;
cout << "dZ       " <<  dZ  << endl;

cout << "Status    " <<  Status  << endl;
}


////////////////////////////////////////////////////////////////
//
//  End of File  TSCPBClass.cc
////////////////////////////////////////////////////////////////
