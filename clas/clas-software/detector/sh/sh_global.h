
#include <sh.h>

sh_calib_t sh_calib;

int RUNNUMBER;
float xCLAS[72];
float yCLAS[72];
float zCLAS[72];

shraw_t adcev[SH_MAX_ADCHITS];
shraw_t tdcev[SH_MAX_TDCHITS];

sh2et_t adc2ph[SH_MAX_ADCHITS];
sh2et_t tdc2ns[SH_MAX_TDCHITS];

