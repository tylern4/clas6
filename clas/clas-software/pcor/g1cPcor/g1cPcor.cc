#include <math.h>
#include <stdio.h>
#include <iostream>
#include "kinematics.h"

float GetBdl(int q,float p, float theta, float Itorus);

extern "C"{

  vector4_t g1cPcor(vector4_t p4, int sector, int q);
  
  void g1cpcor_(float *p3, int *sector, int *q);
  
  float g1cTAGRcor(float epho, float E0, int runID);
  
  void g1ctagrcor_(float *epho, float *E0, int *runID);

  vector4_t g1cDCeloss(vector4_t p4, int sector);

  void g1cdceloss_(float *p4, int *sector);

  vector4_t g1cMomCor(vector4_t p4,int sector,int q);

  void g1cmomcor_(float *p4,int *sector,int *q);
}

void g1ctagrcor_(float *epho, float *E0, int *runID){
  float eg;
  eg = g1cTAGRcor(*epho,*E0,*runID);
  *epho = eg;

}

void g1cpcor_(float p3[3], int *sector, int *q){

  vector4_t p4cor,p4m;
  p4m.space.x = p3[0];
  p4m.space.y = p3[1];
  p4m.space.z = p3[2];

  p4m.t = sqrt(p3[0]*p3[0] + p3[1]*p3[1] + p3[2]*p3[2]);

  p4cor = g1cPcor(p4m,*sector,*q);

  p3[0] = p4cor.space.x;
  p3[1] = p4cor.space.y;
  p3[2] = p4cor.space.z;

}

void g1cdceloss_(float *p4, int *sector){

  vector4_t p4cor, p4m;
  p4m.space.x = p4[0];
  p4m.space.y = p4[1];
  p4m.space.z = p4[2];
  p4m.t = p4[3];

  p4cor = g1cDCeloss(p4m, *sector);

  p4[0] = p4cor.space.x;
  p4[1] = p4cor.space.y;
  p4[2] = p4cor.space.z;
  p4[3] = p4cor.t;

}
  

void g1cmomcor_(float *p4,int *sector,int *q){

  vector4_t p4cor,p4m;
  p4m.space.x = p4[0];
  p4m.space.y = p4[1];
  p4m.space.z = p4[2];
  p4m.t = p4[3];

  p4cor = g1cMomCor(p4m,*sector,*q);

  p4[0] = p4cor.space.x;
  p4[1] = p4cor.space.y;
  p4[2] = p4cor.space.z;
  p4[3] = p4cor.t;

}
  


vector4_t g1cPcor(vector4_t p4, int sector, int q){
  // applies momentum corrections to charged particles in CLAS
  // derived for the g1c run period.

  float Bdl,pm,px,py,pz,m,theta,phi,pcor;
  vector4_t p4cor;
  int phibin;

  px = p4.space.x;
  py = p4.space.y;
  pz = p4.space.z;

  pm = sqrt(px*px + py*py + pz*pz);

  m = sqrt(p4.t*p4.t - pm*pm);
  theta = acos(pz/pm);
  phi = atan2(py,px);
 
  Bdl = GetBdl(q,pm,theta,1920.0);

  if(Bdl == 0.0){
    return p4;
  }
  
  // phi bins work as (-25,-20) is bin 0, (-20,-15) is bin 1 ... (20,25) is 
  // bin 9
  float aneg[6][10] = {{-4.48695,-3.74883,-2.79991,-2.34222,-0.853252,-0.319857,1.18798,1.27426,1.26359,1.04627},{-5.59945,-4.69647,-3.60574,-1.95416,-1.70949,-1.46655,-1.94510,-1.10805,-0.916925},{-0.768211,0.426845,-0.514684,-0.373486,-0.316727,-1.31495,-2.41803,-2.37492,-3.41833,-3.81785},{2.06511,3.20744,3.11262,2.91453,2.38069,2.09806,1.06336,0.534416,-0.595552,-1.01872},{-0.026820,0.262493,0.551601,0.8444471,-0.199171,0.367880,-0.101378,0.309209,-0.723196,-1.67610},{7.60884,7.88374,7.65248,4.41801,3.69671,1.40168,-1.39483,-2.30221,-3.67526,-6.23115}};

  float bneg[6][10] = {{0.00123249,0.00139840,0.000347585,-0.000235993,-0.00212308,-0.00354623,-0.00603679,-0.00648359,-0.00676616,-0.00688433},{0.00142145,-0.000972792,-0.00254131,-0.00373451,-0.00662959,-0.00753426,-0.00840490,-0.00818177,-0.00989391,-0.0102452},{0.00462347,0.000592281,0.000335324,0.000418587,0.000175579,0.00133395,0.00229206,0.00178869,0.00175505,-0.00117766},{0.00548663,0.000849534,-0.000592270,-0.00115298,-0.00121905,-0.00185035,-0.00157293,-0.00211409,-0.00247801,-0.00433914},{0.00590055,0.00362922,0.00199951,0.000645485,0.000925869,-0.00112015,-0.00150359,-0.00327762,-0.00315296,-0.00377030},{0.00193828,-0.0028607,-0.00403091,-0.00142564,-0.00228100,-0.000719641,0.00100696,0.000337594,-0.000301571,0.000440365}};

  float cneg[6][5] = {{15.5279,-55.9115,63.3995,-27.7406,4.13267},{4.66412,-25.7774,35.7890,-16.3311,2.48280},{-5.94786,40.3904,-75.7484,51.6833,-11.7097},{11.7423,-59.9989,87.9982,-46.8560,8.40381},{5.26789,-22.7650,28.1304,-12.3584,2.00945},{6.69683,-29.9385,34.1462,-12.3351,1.62178}};

  float apos[6][10] = {{3.08094,2.36445,4.84716,1.40996,1.56156,1.36493,0.899387,0.0397270,0.0320524,-0.00273913},{6.88902,7.65129,8.51941,8.07934,7.61976,7.09939,5.75420,4.88588,3.71089,2.04427},{0.4860347,1.28888,1.87322,1.39567,1.10670,0.745536,1.52524,-0.199171,0.169081,-1.57959},{-0.343763,-0.913237,-0.901479,-0.589175,0.602365,-0.0978728,0.618452,0.395592,1.14669,1.74365},{2.49902,2.76912,3.21036,1.87548,2.84367,1.33166,1.63741,-0.00560407,-0.246814,0.425038},{-7.52874,-5.35393,-6.44887,-4.71433,-4.21594,-3.30512,-1.87963,0.153624,0.196322,1.81755}};

  float bpos[6][10] = {{-0.000256804,-0.000485507,-0.000214371,0.000185955,-0.000780743,-0.00117483,-0.00156018,-0.00154690,-0.00299285,-0.00662343},{-0.00136685,-0.00375091,-0.00572383,-0.00583996,-0.00632324,-0.00657127,-0.00602094,-0.00673949,-0.00719375,-0.00886251},{0.00330722,0.000436964,-0.00110605,-0.00202942,-0.00311298,-0.00423139,-0.00717785,-0.00703845,-0.00837910,-0.00821884},{0.00531096,0.00480925,0.00414197,0.00166822,-0.00120285,-0.00199835,-0.00461049,-0.00627153,-0.00915033,-0.0136073},{0.00180992,0.000569946,-0.000872512,-0.000377804,-0.00269726,-0.00224672,-0.00354342,-0.00258537,-0.00358758,-0.00793604},{0.00333987,0.00169962,0.00315866,0.000783018,0.000133044,-0.00100515,-0.00267094,-0.00293108,-0.00475378,-0.00699526}};

  float cpos[6][5] = {{2.45731,-5.51792,4.03292,-5.07528,2.40164},{-3.36646,11.9747,-9.59951,0.222401,1.54063},{-6.70395,17.7966,-13.8941,8.78835,-2.64496},{0.806461,6.91541,-21.3384,14.0092,-2.07642},{2.87240,-4.87925,-1.04696,3.29361,-0.5969616},{10.9124,-39.0490,50.8838,-31.3906,6.88078}};
  

  if(sector < 4){
    phi -= (3.14159/3.0)*(sector - 1);
  }
  else{
    if(sector == 4){
      if(phi > 0){
	phi -= 3.14159;
      }
      else{
	phi += 3.14159;
      }
    }
    if(sector == 5){
      phi += (3.14159/3.0)*2.0;
    }
    if(sector == 6){
      phi += (3.14159/3.0);
    }
  }

  // convert phi to degrees
  phi *= 180.0/3.14159;
  
  if(phi >= 25.0){
    phibin = 9;
  }
  else{
    if(phi <= -25.0){
      phibin = 0;
    }
    else{
      phibin = (int)((25 + phi)/5);
    }
  }


  if(q < 0){
    pcor = pm + (pm*pm/Bdl)*(aneg[sector -1][phibin] + bneg[sector -1][phibin]*Bdl/pm + cneg[sector - 1][0] + cneg[sector -1][1]*theta + cneg[sector -1][2]*theta*theta + cneg[sector -1][3]*theta*theta*theta + cneg[sector -1][4]*theta*theta*theta*theta);
  }
  else{
    if(q > 0){
      pcor = pm + (pm*pm/Bdl)*(apos[sector -1][phibin] + bpos[sector -1][phibin]*Bdl/pm + cpos[sector - 1][0] + cpos[sector -1][1]*theta + cpos[sector -1][2]*theta*theta + cpos[sector -1][3]*theta*theta*theta + cpos[sector -1][4]*theta*theta*theta*theta);
    }
  }
  p4cor.space.x = px*(pcor/pm);
  p4cor.space.y = py*(pcor/pm);
  p4cor.space.z = pz*(pcor/pm);
  p4cor.t = sqrt(pcor*pcor + m*m);

  return p4cor;

}


float GetBdl(int q, float p, float theta, float Itorus){
  // gets |integral(Bxdl)| for a track with p4 in field with Itorus

  float By,x,z,dpx,dpz,px,pz;
  int ix,iz,index,Niter;
  float Bdl,dBdl,dBdlx,dBdlz;
  float dx,dz,dl;
  float qc;

  Niter = 0;

  extern double Q[(int)1e6],Xmin,Zmin,Dx,Dz;
  extern int Nx,Ny;

  if((Dx == 0.0)||(Dz == 0.0)){
    printf("Warning!!!!! Data Error Dx || Dz==0\n");
    //    cout << "Warning!!!!! Data Error Dx || Dz==0" << endl; 
//    cout << "Warning!!!!! Data Error. It looks like InitPcor() has NOT been 
//n.  This function must be run once prior to using the g1cPcor function.
// Corrections will NOT be made." << endl;
    return 0.0;
  }
  
  Bdl = 0.0;
  qc = (float)q;
  x = 0.0;
  z = 0.0;
  dl = 20.0;
  /*
  px = p4.space.x;
  py = p4.space.y;
  pz = p4.space.z;
  */
  p *= (1.0E9)*(1.782E-36)*(3.0E8);
  px = p*sin(theta);
  pz = p*cos(theta);

  // convert p and q to standard units
  //  p *= (1.0E9)*(1.782E-36)*(3.0E8);
  qc *= (1.602E-19);

  while((z < 400.0)&&(z > -200.0)&&(x < 300.)&&(Niter < 100)){
    Niter++;

    dx = dl*px/p;
    dz = dl*pz/p;
    // calculate closest ix and iz
    ix = (int)((x - Xmin)/Dx);
    iz = (int)((z - Zmin)/Dz);
    index = iz*Nx*Ny*3 + Nx*3 + 3*ix;

    By = (Itorus/3860.0)*Q[index+1];

    dpx = qc*(-1.0*By*dz)*(0.01)*(0.1);
    dpz = qc*(dx*By)*(0.01)*(0.1);

    dBdlx = -1.0*By*dz;
    dBdlz = dx*By;

    dBdl = sqrt(dBdlx*dBdlx + dBdlz*dBdlz);

    Bdl += dBdl;

    px += dpx;
    pz += dpz;

    z += dz;
    x += dx;

  }

  return Bdl;

}

float g1cTAGRcor(float epho, float E0, int runID){
  // beam offsets only calculated for 2.4 and 3.1 from g1c
  // runID = 1 is 3.115 from g1c
  // runID = 2 is 2.4 from g1c
  // runID = 3 is 4.016 from g11a
  // any other runID gets a beam offset of 0.0

  float eg = 0.0;
  float egdivE0 = 0.0;

  egdivE0 = epho/E0;

  if(runID == 1){
    // beam off by -0.2% or -6.23MeV(taken from mcc recorded energy)
    egdivE0 -= 0.002;
  }
  if(runID == 2){
    // beam off by -0.075% or -1.8MeV(needed to make correction match 3.1GeV)
    egdivE0 -= 0.00075;
  }
  if(runID == 3){
    // beam off by + 0.00444 MeV or 0.11% 
    egdivE0 += 0.0011;
    
    if(epho > 2.8){
      eg = -0.264319 + 0.176451*epho - 0.0271662*epho*epho;
    }   
    else if(epho > 1.6){
      eg = -0.0592731 + 0.0586458*epho - 0.0113537*epho*epho;
    }
    else {
      eg = -0.0238095 + 0.0464447*epho - 0.0174077*epho*epho;
    }
    return eg + epho;
    
  }


  if(egdivE0 < 0.401){
    egdivE0 += -0.00599597 + 0.0390912*egdivE0 - 0.0573218*egdivE0*egdivE0;
  }
  if((egdivE0 >= 0.401)&&(egdivE0 <= 0.674)){
    egdivE0 += -0.0181293 + 0.0675599*egdivE0 - 0.0541268*egdivE0*egdivE0;
  }
  if(egdivE0 > 0.674){
    egdivE0 += -0.0511162 + 0.138208*egdivE0 - 0.0867757*egdivE0*egdivE0;
  }

  eg = egdivE0*E0;

  return eg;

}

vector4_t g1cDCeloss(vector4_t p4, int sector){
  //applies corrections for energy loss in the drift chambers
  //derived from the g1c run period.

  float px, py, pz,E, m, pm, pc, B;
  vector4_t p4cor;

  px = p4.space.x;
  py = p4.space.y;
  pz = p4.space.z;
  E = p4.t; 

  float coefs[6][4]={{-.00116104, -.000176058, .00178656, -.000195306}, {-.00792481, .00718844, -.00112648, .000151199}, {-.0120301, .0110054, -.00218759, .000224847}, {.00531744, -.00878848, .00516855, -.000631009}, {-.0000316077, -.00289511, .00323581, -.000398962}, {.0116049, -.0159492, .00788174, -.000916976}};

  pm = sqrt(px*px + py*py + pz*pz);
  m = sqrt(E*E - pm*pm);
  B = pm/E;
  pc = pm + coefs[sector-1][0] + coefs[sector-1][1]/B + coefs[sector-1][2]/(B*B) + coefs[sector-1][3]/(B*B*B);

  p4cor.space.x = px*(pc/pm);
  p4cor.space.y = py*(pc/pm);
  p4cor.space.z = pz*(pc/pm);
  p4cor.t = sqrt(pc*pc + m*m);

  return p4cor;
}

vector4_t g1cMomCor(vector4_t p4, int sector, int q){
  // applies both the g1cPcor momentum corrections for drift chamber 
  // misalignments and field map problems and the g1cDCeloss corrections for
  // energy lost in the drift chambers

  vector4_t p4cor,p4cor1;

  p4cor1 = g1cPcor(p4,sector,q);
  p4cor = g1cDCeloss(p4cor1,sector);

  return p4cor;

}
