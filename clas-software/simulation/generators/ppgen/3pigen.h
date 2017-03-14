/* 3pigen.h */

/* units:  Energy, Mass: GeV
   Length: cm 
   dimensions: cm
*/

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
#define  ELEC_MASS .000511
#define  GAMMA_MASS 0.0
#define  ETAPRIME_MASS 0.95777
#define  PHI_MASS 1.020

#define  VERTEX_X_CENTER 0.0
#define  VERTEX_Y_CENTER 0.0
#define  VERTEX_X_SIGMA 0.35 
#define  VERTEX_Y_SIGMA 0.39  

#define  BEAM_EN_CENTER 18.32000
#define  BEAM_PHI_CENTER 3.73000
#define  BEAM_THETA_CENTER .00507

#define  BEAM_EN_SIGMA 0.18400
#define  BEAM_PHI_SIGMA 0.26270
#define  BEAM_THETA_SIGMA 0.00154


/* Function Prototypes */

int  Q(Particle_t pid);
int comp_double(const void*,const void*);

float gaussian_gen(void);
float GetBeamP();
float tmin(float plab,float ma,float mb,float mc,float md);
float pprime(float w, float m, float m3);
float wcm(float p,float m1, float m2);
float e(float p,float m);
float s(float,float,float);
float theta(float QSQ,float W,float E);
float eprime(float theta,float Qsq,float E);

double randm(double,double);
double CMmomentum(double,double,double);
double brem(double brem0,double brem1);
double Mass(Particle_t  pid);
double getT(double tMin,double slope);
double Qsq(double E,double Ep,double theta);
double Wsq(double E,double Ep,double theta);

void cpcmcp (int argc, char *argv[],Particle_t Beam,Particle_t Cplus,Particle_t Cminus,Particle_t Cplus);

void generateBeamMCinsideTarget(vector3_t *,vector3_t *);
void PrintParticleID();
void pParticle_txt2part(Particle_t type,threeVec v,fourVec p);
void pParticle(Particle_t,threeVec,fourVec);
void pParticleGamp(Particle_t,fourVec);
void distribute_beam(vector4_t *);
void distribute_vertex(vector3_t *);
void GeneralUsage();
void UsageM7(char *ProcessName);
void MUsage(char *ProcessName);

string pid2name(Particle_t type);
