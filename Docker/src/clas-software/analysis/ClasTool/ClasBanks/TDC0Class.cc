// @(#)ClasTool/ClasBanks:$Name:  $:$Id: TDC0Class.cc,v 1.4 2007/03/01 22:26:59 holtrop Exp $
// Author:  Gagik Gavalian
//____________________ 
//Begin_Html <!--
/* -->
<h1>TDC0Class</h1>
<pre>
 Modified:
           Gagik (2/13/17) Comments.
</pre>

<!-- */ 
// --> End_Html   
////////////////////////////////////////////////////////

#include "TDC0Class.h"

ClassImp(TDC0Class)

TDC0Class::TDC0Class(TDC0Class *TmpDC0){
ID   =   TmpDC0->ID;
TDC  =   TmpDC0->TDC;
}

void   TDC0Class::Print(){
cout << "ID    " <<  ID  << endl;
cout << "TDC   " <<  TDC  << endl;
}


////////////////////////////////////////////////////////////////
//
//  End of File  TSCPBClass.cc
////////////////////////////////////////////////////////////////
