#ifndef __JEDEFAULTS_H
#define __JEDEFAULTS_H

typedef  enum { c_begin,   // was c_unknown, calibration undefined, should terminate program
		c_gauss,
		c_tdc,
		c_tdcp,
		c_timewalk,
		c_atten,
		c_veff,
		c_gmean,
		c_mass,
		c_p2pdelay,
		c_coupled,
		c_end
} calibration_t;

typedef  enum { i_unknown, 
		i_left, 
		i_right, 
		i_both,     // left and right item equal
		i_p2p,
		i_value
} item_t; 

typedef struct {
  calibration_t caltype;
  int nparFit;
  int nparSave;
  int saveOffs;
  int relative;
  item_t item;
  double defltpar[6];
  double lowlimit[6];
  double upplimit[6];
  char name[20];
} jdefault_t;

static const jdefault_t gDefault[c_end] = {
  {c_begin, 0, 0, 0, 0, i_unknown,
   {    0.,   0.,   0.,   0.,   0.,   0. } ,   // defltpar
   {    0.,   0.,   0.,   0.,   0.,   0. } ,   // lowlimit
   {    0.,   0.,   0.,   0.,   0.,   0. } ,   // upplimit
   "unknown"
  },
  //--- end c_begin/unknown --------

  {c_gauss, 3, 0, 0, 0, i_unknown,
   {    0.,   0.,   0.,   0.,   0.,   0. } ,   // defltpar
   {    0.,   0.,   0.,   0.,   0.,   0. } ,   // lowlimit
   {    0.,   0.,   0.,   0.,   0.,   0. } ,   // upplimit
   "gauss"
  },
  //--- end c_gauss ----------------

  {c_tdc, 3, 3, 0, 1, i_unknown,
   {   0.1, 0.05,4.E-8,   0.,   0.,   0. } ,   // defltpar
   { -200., 0.04,-1E-6,   0.,   0.,   0. } ,   // lowlimit
   {  200., 0.06, 1E-6,   0.,   0.,   0. } ,   // upplimit
   "tdc"
  },
  //--- end c_tdc ------------------

  {c_tdcp, 3, 3, 0, 1, i_unknown,
   {  500., -0.10, 0.0,   0.,   0.,   0. } ,   // defltpar
   {    0., -0.12,-1E-6,   0.,   0.,   0. } ,   // lowlimit
   {  800., -0.08, 1E-6,   0.,   0.,   0. } ,   // upplimit
   "tdcp"
  },
  //--- end c_tdcp ------------------

  {c_timewalk, 4, 3, 1, 0, i_unknown,
   {   15.,  10.,  0.5,  50.,   0.,   0. } ,   // defltpar
   { -100.,  0.1, 0.05,  0.1,   0.,   0. } ,   // lowlimit
   {  100., 200.,  0.9,2.5E3,   0.,   0. } ,   // upplimit
   "timewalk" 
  },
  //--- end c_timewalk -------------

  {c_atten, 2, 2, 0, 0, i_both,
   {    0., 170.,   0.,   0.,   0.,   0. } ,   // defltpar
   {  -10.,   1.,   0.,   0.,   0.,   0. } ,   // lowlimit
   {   10., 999.,   0.,   0.,   0.,   0. } ,   // upplimit
   "atten"
  },
  //--- end c_atten ----------------

  {c_veff, 2, 1, 1, 0, i_both,
   {    0.,  16.,   0.,   0.,   0.,   0. } ,   // defltpar
   {  -30.,  0.1,   0.,   0.,   0.,   0. } ,   // lowlimit
   {   30.,  30.,   0.,   0.,   0.,   0. } ,   // upplimit
   "veff"
  },
  //--- end c_veff -----------------

  {c_gmean, 5, 1, 1, 1, i_both,
   { 1000., 600.,  40.,   1.,-0.001,  0. } ,   // defltpar
   {   0.1,  10.,   4.,   0.,  -1.,   0. } ,   // lowlimit
   {  5.E4,1210., 200., 5.E3,  0.2,   0. } ,   // upplimit
   "gmean"
  },
  //--- end c_gmean ----------------

  {c_mass, 3, 0, 0, 0, i_unknown,
   {  150.,   0.,   0.,   0.,   0.,   0. } ,   // defltpar
   {   0.1, -0.3,0.005,   0.,   0.,   0. } ,   // lowlimit
   {   1E3,  0.3,  0.2,   0.,   0.,   0. } ,   // upplimit
   "mass"
  },
  //--- end c_mass -----------------

  {c_p2pdelay, 3, 1, 1, 1, i_p2p,
   {  600.,   0.,  0.2,   0.,   0.,   0. } ,   // defltpar
   {    1.,  -2., 0.05,   0.,   0.,   0. } ,   // lowlimit
   {   1E4,   2.,  0.5,   0.,   0.,   0. } ,   // upplimit
   "p2pdelay"
  },
  //--- end c_p2pdelay -------------

  {c_coupled, 0, 0, 0, 0, i_unknown,
   {    0.,   0.,   0.,   0.,   0.,   0. } ,   // defltpar
   {    0.,   0.,   0.,   0.,   0.,   0. } ,   // lowlimit 
   {    0.,   0.,   0.,   0.,   0.,   0. } ,   // upplimit 
   "coupled"
  }
  //--- end c_coupled --------------
};


#endif
