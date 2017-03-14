#include <stdlib.h>
#include <string.h>
#include <Lorentz.hxx>

/* units:  Energy, Mass: GeV
   Angles: rad
   dimensions: cm
*/
#define MONTECARLO_PART 2

#define  BEAM_P  18.0
#define  BEAM_MASS 0.13957
#define  NEUTRON_MASS 0.93957
#define  TARGET_MASS 0.93827
#define  PROTON_MASS 0.93827
#define  KZERO_MASS 0.49772
#define  KCHARGED_MASS 0.49367
#define  PI_MASS 0.13957
#define  ETA_MASS 0.54745
#define  PI0_MASS 0.1349764
#define  OMEGA_MASS 0.78194
#define  LAMBDA_MASS 1.11568
#define  ELEC_MASS .000511
#define  GAMMA_MASS 0.0
#define  ETAPRIME_MASS 0.95777

#define  VERTEX_X_CENTER 1.36200
#define  VERTEX_Y_CENTER -0.22760
#define  BEAM_EN_CENTER 18.32000
#define  BEAM_PHI_CENTER 3.73000
#define  BEAM_THETA_CENTER .00507

#define  VERTEX_X_SIGMA 1.08200
#define  VERTEX_Y_SIGMA 0.46180
#define  BEAM_EN_SIGMA 0.18400
#define  BEAM_PHI_SIGMA 0.26270
#define  BEAM_THETA_SIGMA 0.00154

#define  CHARGE_OF_PIMINUS -1
#define  CHARGE_OF_PIZERO 0

double randm(double,double);
double CMmomentum(double,double,double);
int comp_double(const void*,const void*);
void distribute_beam(vector4_t *);
void distribute_vertex(vector3_t *);
float gaussian_gen(void);
float GetBeamP();


void GeneralUsage();
void UsageM1(char  *ProcessName);
void UsageM2(char *ProcessName);
void UsageM3(char *ProcessName);
void UsageM5(char *ProcessName);
void UsageM6(char *ProcessName);
void MUsage(char *ProcessName);



void pipipi(int argc, char *argv[]);
void npip(int argc, char *argv[]);
void npip_gamma(int argc, char *argv[]);
void ppi0(int argc, char *argv[]);
void pippim(int argc, char *argv[]);
void kpkspim(int argc, char *argv[]);
void generateBeamMCinsideTarget(vector3_t *,vector3_t *);

