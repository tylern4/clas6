#ifndef Pcor_H
#define Pcor_H

#include <kinematics.h>

extern "C" {

void InitPcor();

void initpcor_();

vector4_t DCpcor(vector4_t p4, int sector, int q, int runID);

void dcpcor_(float *p3, int *sector, int *q, int *runid);

float TAGRcor(float epho, float E0, int runID);

void tagrcor_(float *epho, float *E0, int *runID);

vector4_t DCeloss(vector4_t p4, int sector, int runID);

void dceloss_(float *p4, int *sector, int *runid);

vector4_t Pcor(vector4_t p4,int sector, int q, int runID);

void pcor_(float *p4,int *sector,int *q, int *runid);

}

#endif
