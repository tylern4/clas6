// Author: Mike Williams
#ifndef SetCLASdata_H
#define SetCLASdata_H

#include <iostream>
#include "Photons.h"
#include "BOS.h"
#include "EventHeader.h"
#include "Tracks.h"
#include "Charged.h"

using namespace std;

void SetPhotons(BOS*,Photons*,float);
void SetEventHeader(BOS*,EventHeader*);
void SetTracks(BOS*,Tracks*);
void SetCharged(BOS*,Charged*);
int PARTindexFromEVNTindex(int,BOS*);
unsigned long int GetTrigBits(BOS*);

#endif
