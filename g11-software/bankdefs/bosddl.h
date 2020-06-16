#ifndef bosddlH_INCLUDED
#define bosddlH_INCLUDED

/* ------------------------ beam -----------------*/
typedef struct {
	float energy;    /*  Electron beam energy in GeV */
	float itorus;    /*  Torus magnet current in Amps */
	float imini;    /*  Mini-torus magnet current in Amps */
	float itag;    /*  Tagger magnet current in Amps */
} beam_t;

typedef struct {
	bankHeader_t bank;
	beam_t beam[1];
} clasBEAM_t;

/* --------------------- END beam -----------------*/

/* ------------------------ bmpr -----------------*/
typedef struct {
	float q_tot;    /*  Total Charge (Coulomb) */
	float q_tot_lv;    /*  Total charge in Live Time (Coulomb)  */
	float tl_tu;    /*  ( Time Live ) / ( Time Ungated ) */
	float current;    /*  Current in Ampers */
} bmpr_t;

typedef struct {
	bankHeader_t bank;
	bmpr_t bmpr[1];
} clasBMPR_t;

/* --------------------- END bmpr -----------------*/

/* ------------------------ brep -----------------*/
typedef struct {
	float mbsy2c_energy;    /*  beam energy */
	float igt0i00biaset;    /*  thermionic gun */
	float igl1i00dac2;    /*  B polarized gun */
	float smrposa;    /*  A slit position */
	float smrposb;    /*  B slit position */
	float smrposc;    /*  C slit position */
	float harp;    /*  harp */
	float hallb_sf_xy560;    /*  torus current */
	float mtseti;    /*  Mini set current */
	float mtirbck;    /*  Mini current readback */
	float mtvrbck;    /*  Mini voltage readback */
	float tmseti;    /*  Tagger set current */
	float tmirbck;    /*  Tagger current readback */
	float tmvrbck;    /*  Tagger voltage readback */
	float cryo_pressure;    /*  cryotarget pressure */
	float cryo_temperature;    /*  cryotarget temperature */
	float cryo_status;    /*  cryotarget status */
	float vcg2c24;    /*  upstream beam vacuum */
	float vcg2h01;    /*  target vacuum */
	float scalers2o;    /*  Halo UP upstream */
	float scalers3o;    /*  Halo DOWN upstream */
	float scalers4o;    /*  Halo LEFT upstream */
	float scalers5o;    /*  Halo RIGHT upstream */
	float scalers6o;    /*  Halo UP downstream */
	float scalers7o;    /*  Halo DOWN downstream */
	float scalers8o;    /*  Halo LEFT downstream */
	float scalers9o;    /*  Halo RIGHT downstream */
	float ipm2h01_xpos;    /*  bpm 1 x */
	float ipm2h01_ypos;    /*  bpm 1 y */
	float ipm2h01;    /*  bpm 1 current */
	float ipm2c24a_xpos;    /*  bpm 2 x */
	float ipm2c24a_ypos;    /*  bpm 2 y */
	float ipm2c24a;    /*  bpm 2 current */
	float ipm2c22a_xpos;    /*  bpm 3 x */
	float ipm2c22a_ypos;    /*  bpm 3 y */
	float ipm2c22a;    /*  bpm 3 current */
} brep_t;

typedef struct {
	bankHeader_t bank;
	brep_t brep[1];
} clasBREP_t;

/* --------------------- END brep -----------------*/

/* ------------------------ calb -----------------*/
typedef struct {
	float meanrfe;    /*  RF offset for electrons (all sectors) */
	float sigmarfe;    /*  Time resolution for electrons (RF) */
	float sigmarfh;    /*  Time resolution for pions */
	float sigmaect;    /*  Time resolution of EC, tEC(e)-tSC(e)  */
	float sfece;    /*  Sampling fraction E_EC(e)/p(e) */
	float sigmasf;    /*  width of the sampling fraction */
	float ressl1;    /*  DC residuals in R1 (all sectors) */
	float ressl2;    /*  DC residuals in R2 (all sectors) */
	float ressl3;    /*  DC residuals in R3 (all sectors) */
	float ressl4;    /*  DC residuals in R1 (all sectors) */
	float ressl5;    /*  DC residuals in R2 (all sectors) */
	float ressl6;    /*  DC residuals in R3 (all sectors) */
} calb_t;

typedef struct {
	bankHeader_t bank;
	calb_t calb[1];
} clasCALB_t;

/* --------------------- END calb -----------------*/

/* ------------------------ call -----------------*/
typedef struct {
	uint16 id;    /*  catch-all element(RF,laser diode,etc) */
	uint16 tdc;    /*  tdc information (channels) */
	uint16 adc;    /*  adc information (channels) */
} call_t;

typedef struct {
	bankHeader_t bank;
	call_t call[1];
} clasCALL_t;

/* --------------------- END call -----------------*/

/* ------------------------ cc01 -----------------*/
typedef struct {
	int id;    /*  the address of the hit detector element */
	float time;    /*  time(ns)  */
	float n_pe;    /*  number of photoelectrons */
} cc01_t;

typedef struct {
	bankHeader_t bank;
	cc01_t cc01[1];
} clasCC01_t;

/* --------------------- END cc01 -----------------*/

/* ------------------------ cc1 -----------------*/
typedef struct {
	uint16 id;    /*  the address of the hit detector element */
	uint16 tdc;    /*  tdc information (channels) */
	uint16 adc;    /*  adc information (channels) */
} cc1_t;

typedef struct {
	bankHeader_t bank;
	cc1_t cc1[1];
} clasCC1_t;

/* --------------------- END cc1 -----------------*/

/* ------------------------ cc0 -----------------*/
typedef struct {
	uint16 id;    /*  the address of the hit detector element */
	uint16 tdc;    /*  tdc information (channels) */
	uint16 adc;    /*  adc information (channels) */
} cc0_t;

typedef struct {
	bankHeader_t bank;
	cc0_t cc0[1];
} clasCC0_t;

/* --------------------- END cc0 -----------------*/

/* ------------------------ ccdi -----------------*/
typedef struct {
	int crate;    /*  CAMAC crate number */
	int slot;    /*  slot */
	int mask;    /*  mask */
	int threshold;    /*  actual threshold value (mV) */
	int width;    /*  actual width value */
} ccdi_t;

typedef struct {
	bankHeader_t bank;
	ccdi_t ccdi[1];
} clasCCDI_t;

/* --------------------- END ccdi -----------------*/

/* ------------------------ cch -----------------*/
typedef struct {
	int tknum;    /*  track number, 1000*istak+itra */
	int id;    /*  track PDG id */
	int nhits;    /*  number of CC hits per track */
	int sector;    /*  sector number of track */
	int segment;    /*  cc segment number of track */
	int pmom;    /*  track momentum  */
	float xin;    /*  x pos track entry into cerenkov */
	float yin;    /*  y pos */
	float zin;    /*  z pos */
	float xout;    /*  x pos track exit from cerenkov */
	float yout;    /*  y pos */
	float zout;    /*  z pos */
} cch_t;

typedef struct {
	bankHeader_t bank;
	cch_t cch[1];
} clasCCH_t;

/* --------------------- END cch -----------------*/

/* ------------------------ ccmt -----------------*/
typedef struct {
	int mean_high;    /*  mean high threshold (mV) */
	int mean_lo;    /*  mean lo threshold (mV) */
} ccmt_t;

typedef struct {
	bankHeader_t bank;
	ccmt_t ccmt[1];
} clasCCMT_t;

/* --------------------- END ccmt -----------------*/

/* ------------------------ ccpb -----------------*/
typedef struct {
	int scsght;    /*  100*sector + Cluster # in CCRC */
	float nphe;    /*  Number of photo-electrons*10 */
	float time;    /*  Flight time relative to the evnt start time */
	float path;    /*  Path lenght from target (from tracking) */
	float chi2cc;    /*  Geometrical matching: angle between CC hit and */
	int status;    /*  Status word - now 10*(CC segment number)  */
} ccpb_t;

typedef struct {
	bankHeader_t bank;
	ccpb_t ccpb[1];
} clasCCPB_t;

/* --------------------- END ccpb -----------------*/

/* ------------------------ ccpe -----------------*/
typedef struct {
	int id;    /*  the address of the hit detector element */
	int mean;    /*  adc pedestal mean value (channel) */
	int sigma;    /*  sigma of the pedestal distribution (channel) */
} ccpe_t;

typedef struct {
	bankHeader_t bank;
	ccpe_t ccpe[1];
} clasCCPE_t;

/* --------------------- END ccpe -----------------*/

/* ------------------------ ccrc -----------------*/
typedef struct {
	int nrsect;    /*  Sector # */
	int nrsegm;    /*  10 * Mean segment # */
	int nrsegm_p;    /*  10 * Max segment # in the cluster */
	int nrsegm_m;    /*  10 * Min segment # in the cluster */
	int nrphe;    /*  Number of photoelectrons obtained */
	int nrtime;    /*  TOF in channels (50psec/channel) */
	int nrthet;    /*  Estimated angle Theta */
	int nrdthet;    /*  Estimated error of angle Theta */
	int nrphy;    /*  Phy index  */
	int nriec;    /*  Estimated i-coordinate in EC  */
	int nrdiec;    /*  Estimated error of i-coord. in EC */
	int nrstat;    /*  Status  word (yet unclear)  */
} ccrc_t;

typedef struct {
	bankHeader_t bank;
	ccrc_t ccrc[1];
} clasCCRC_t;

/* --------------------- END ccrc -----------------*/

/* ------------------------ ccs -----------------*/
typedef struct {
	int ccs1;    /*  scaler sector 1 */
	int ccs2;    /*  scaler sector 1 */
	int ccs3;    /*  scaler sector 1 */
	int ccs4;    /*  scaler sector 1 */
	int ccs5;    /*  scaler sector 1 */
	int ccs6;    /*  scaler sector 1 */
	int ccs7;    /*  scaler sector 1 */
	int ccs8;    /*  scaler sector 1 */
	int ccs9;    /*  scaler sector 1 */
	int ccs10;    /*  scaler sector 1 */
	int ccs11;    /*  scaler sector 1 */
	int ccs12;    /*  scaler sector 1 */
	int ccs13;    /*  scaler sector 1 */
	int ccs14;    /*  scaler sector 1 */
	int ccs15;    /*  scaler sector 1 */
	int ccs16;    /*  scaler sector 1 */
	int ccs17;    /*  scaler sector 2 */
	int ccs18;    /*  scaler sector 2 */
	int ccs19;    /*  scaler sector 2 */
	int ccs20;    /*  scaler sector 2 */
	int ccs21;    /*  scaler sector 2 */
	int ccs22;    /*  scaler sector 2 */
	int ccs23;    /*  scaler sector 2 */
	int ccs24;    /*  scaler sector 2 */
	int ccs25;    /*  scaler sector 2 */
	int ccs26;    /*  scaler sector 2 */
	int ccs27;    /*  scaler sector 2 */
	int ccs28;    /*  scaler sector 2 */
	int ccs29;    /*  scaler sector 2 */
	int ccs30;    /*  scaler sector 2 */
	int ccs31;    /*  scaler sector 2 */
	int ccs32;    /*  scaler sector 2 */
	int ccs33;    /*  scaler sector 3 */
	int ccs34;    /*  scaler sector 3 */
	int ccs35;    /*  scaler sector 3 */
	int ccs36;    /*  scaler sector 3 */
	int ccs37;    /*  scaler sector 3 */
	int ccs38;    /*  scaler sector 3 */
	int ccs39;    /*  scaler sector 3 */
	int ccs40;    /*  scaler sector 3 */
	int ccs41;    /*  scaler sector 3 */
	int ccs42;    /*  scaler sector 3 */
	int ccs43;    /*  scaler sector 3 */
	int ccs44;    /*  scaler sector 3 */
	int ccs45;    /*  scaler sector 3 */
	int ccs46;    /*  scaler sector 3 */
	int ccs47;    /*  scaler sector 3 */
	int ccs48;    /*  scaler sector 3 */
	int ccs49;    /*  scaler sector 4 */
	int ccs50;    /*  scaler sector 4 */
	int ccs51;    /*  scaler sector 4 */
	int ccs52;    /*  scaler sector 4 */
	int ccs53;    /*  scaler sector 4 */
	int ccs54;    /*  scaler sector 4 */
	int ccs55;    /*  scaler sector 4 */
	int ccs56;    /*  scaler sector 4 */
	int ccs57;    /*  scaler sector 4 */
	int ccs58;    /*  scaler sector 4 */
	int ccs59;    /*  scaler sector 4 */
	int ccs60;    /*  scaler sector 4 */
	int ccs61;    /*  scaler sector 4 */
	int ccs62;    /*  scaler sector 4 */
	int ccs63;    /*  scaler sector 4 */
	int ccs64;    /*  scaler sector 4 */
	int ccs65;    /*  scaler sector 5 */
	int ccs66;    /*  scaler sector 5 */
	int ccs67;    /*  scaler sector 5 */
	int ccs68;    /*  scaler sector 5 */
	int ccs69;    /*  scaler sector 5 */
	int ccs70;    /*  scaler sector 5 */
	int ccs71;    /*  scaler sector 5 */
	int ccs72;    /*  scaler sector 5 */
	int ccs73;    /*  scaler sector 5 */
	int ccs74;    /*  scaler sector 5 */
	int ccs75;    /*  scaler sector 5 */
	int ccs76;    /*  scaler sector 5 */
	int ccs77;    /*  scaler sector 5 */
	int ccs78;    /*  scaler sector 5 */
	int ccs79;    /*  scaler sector 5 */
	int ccs80;    /*  scaler sector 5 */
	int ccs81;    /*  scaler sector 6 */
	int ccs82;    /*  scaler sector 6 */
	int ccs83;    /*  scaler sector 6 */
	int ccs84;    /*  scaler sector 6 */
	int ccs85;    /*  scaler sector 6 */
	int ccs86;    /*  scaler sector 6 */
	int ccs87;    /*  scaler sector 6 */
	int ccs88;    /*  scaler sector 6 */
	int ccs89;    /*  scaler sector 6 */
	int ccs90;    /*  scaler sector 6 */
	int ccs91;    /*  scaler sector 6 */
	int ccs92;    /*  scaler sector 6 */
	int ccs93;    /*  scaler sector 6 */
	int ccs94;    /*  scaler sector 6 */
	int ccs95;    /*  scaler sector 6 */
	int ccs96;    /*  scaler sector 6 */
} ccs_t;

typedef struct {
	bankHeader_t bank;
	ccs_t ccs[1];
} clasCCS_t;

/* --------------------- END ccs -----------------*/

/* ------------------------ chi2 -----------------*/
typedef struct {
	float chi2;    /*  overall chi2 for event   */
	float cl;    /*  percentage (confidence level) */
	int ndf;    /*  number of effective degrees of freedom */
	int iter; 
} chi2_t;

typedef struct {
	bankHeader_t bank;
	chi2_t chi2[1];
} clasCHI2_t;

/* --------------------- END chi2 -----------------*/

/* ------------------------ cl01 -----------------*/
typedef struct {
	int ac_amp;    /*  60 Hz AC amplitude (pedestal subtracted) */
	int fc_diode_amp;    /*  Forward Carriage diode amplitude (ped sub.) */
	float fc_diode_t;    /*  Forward Carriage diode time */
	int nc_diode_amp;    /*  North Clamshell diode amplitude (ped sub.) */
	float nc_diode_t;    /*  Forward  diode time */
	int sc_diode_amp;    /*  Forward Carriage diode amplitude (ped sub.) */
	float sc_diode_t;    /*  Forward Carriage diode time(ns) */
	int sf_diode_amp;    /*  Forward Carriage diode amplitude (ped sub.) */
	float sf_diode_t;    /*  Forward Carriage diode time(ns) */
	float rf1;    /*  RF time 1 (ns) */
	float rf2;    /*  RF time 2 (ns) */
	float rf;    /*  GOOD RF time (ns) */
} cl01_t;

typedef struct {
	bankHeader_t bank;
	cl01_t cl01[1];
} clasCL01_t;

/* --------------------- END cl01 -----------------*/

/* ------------------------ clst -----------------*/
typedef struct {
	int clust;    /*  bit packed,  see: include/bosddl.h, clasCLST_t */
} clst_t;

typedef struct {
	bankHeader_t bank;
	clst_t clst[1];
} clasCLST_t;

/* --------------------- END clst -----------------*/

/* ------------------------ cped -----------------*/
typedef struct {
	int slot;    /*  ADC slot */
	int channel;    /*  ADC channel */
	int mean;    /*  adc pedestal mean value (channel) */
	float sigma;    /*  sigma of the pedestal distribution (channel) */
	int offset;    /*  offset for sparsification threshold calculation */
} cped_t;

typedef struct {
	bankHeader_t bank;
	cped_t cped[1];
} clasCPED_t;

/* --------------------- END cped -----------------*/

/* ------------------------ csql -----------------*/
typedef struct {
	uint32 evid;    /*  Event ID (number of triggers) */
	uint32 nproc;    /*  Number of processed triggers */
	float cpu;    /*  CPU used (sec)  */
	float fc;    /*  Faraday Cup (K) */
	float fcg;    /*  Faraday Cup Gated (K) */
	float tg;    /*  Clock Gated */
	float ibeam;    /*  Beam current  */
	uint32 nes1;    /*   Number of electrons in sect 1 */
	uint32 nes2;    /*   Number of electrons in sect 2 */
	uint32 nes3;    /*   Number of electrons in sect 3  */
	uint32 nes4;    /*   Number of electrons in sect 4  */
	uint32 nes5;    /*   Number of electrons in sect 5   */
	uint32 nes6;    /*   Number of electrons in sect 6 */
	uint32 nhb;    /*  Number of HB  */
	uint32 ntb;    /*  Number of TB */
	uint32 nprot;    /*  Number of protons */
	uint32 npip;    /*  number of pip */
	uint32 ndeut;    /*  number of deutrons */
	uint32 nphot;    /*  number of photons	 */
	uint32 nelhp;    /*  Number of electrons at pos. Helic. */
	uint32 nelhn;    /*  Number of electrons at neg. helic. */
} csql_t;

typedef struct {
	bankHeader_t bank;
	csql_t csql[1];
} clasCSQL_t;

/* --------------------- END csql -----------------*/

/* ------------------------ dc0 -----------------*/
typedef struct {
	uint16 id;    /*  the address of the hit detector element */
	uint16 tdc;    /*  tdc information (channels) */
} dc0_t;

typedef struct {
	bankHeader_t bank;
	dc0_t dc0[1];
} clasDC0_t;

/* --------------------- END dc0 -----------------*/

/* ------------------------ dc1 -----------------*/
typedef struct {
	int id;    /*  the address of the hit detector element */
	float time;    /*  time(ns)  */
} dc1_t;

typedef struct {
	bankHeader_t bank;
	dc1_t dc1[1];
} clasDC1_t;

/* --------------------- END dc1 -----------------*/

/* ------------------------ dcdw -----------------*/
typedef struct {
	int id;    /*  ID_wire  */
	float tidly;    /*  TIme DeLaY (ns) 		  */
	int stat;    /*  wire status word  */
} dcdw_t;

typedef struct {
	bankHeader_t bank;
	dcdw_t dcdw[1];
} clasDCDW_t;

/* --------------------- END dcdw -----------------*/

/* ------------------------ dcgm -----------------*/
typedef struct {
	float x_curve;    /*  x center of curvature (cm) */
	float y_curve;    /*  y center of curvature (cm) */
	float z_curve;    /*  z center of curvature (cm) */
	float r_curve;    /*  radius of curvature (cm) */
	float theta_start;    /*  angle of first logical wire WRT the center of curvature (rad) */
	float d_theta;    /*  delta theta between wires WRT center of curvature */
	float x_nmid;    /*  x normal vector to the midplane */
	float y_nmid;    /*  y normal vector to the midplane */
	float z_nmid;    /*  z normal vector to the midplane */
	float theta_min;    /*  theta of first physical wire (rad) */
	float theta_max;    /*  theta of last physical wire (rad) */
	int min_wire;    /*  minimum physical wire number */
	int max_wire;    /*  maximum physical wire number */
	float stereo;    /*  approximate stereo angle */
	float cell_size;    /*  cell size (cm) */
	float x_norm;    /*  x normal vector to the plane(region 1) */
	float y_norm;    /*  y normal vector to the plane(region 1) */
	float z_norm;    /*  z normal vector to the plane(region 1) */
	float p_dist;    /*  distance of plane to origin(cm) (region 1) */
	float p_sep;    /*  planar separation(cm) (region 1) */
	int max_cylw;    /*  maximum cylindrical wire */
} dcgm_t;

typedef struct {
	bankHeader_t bank;
	dcgm_t dcgm[1];
} clasDCGM_t;

/* --------------------- END dcgm -----------------*/

/* ------------------------ dcgw -----------------*/
typedef struct {
	float x_mid;    /*  x at midplane of wire(cm) */
	float y_mid;    /*  y at midplane of wire(cm) */
	float z_mid;    /*  z at midplane of wire(cm) */
	float x_dir;    /*  x direction cosine along wire (cm) */
	float y_dir;    /*  y direction cosine along wire (cm) */
	float z_dir;    /*  z direction cosine along wire (cm) */
	float w_len;    /*  wire length from midplane to amplifier (cm) */
	float w_len_hv;    /*  wire length from midplane to HV (cm) */
} dcgw_t;

typedef struct {
	bankHeader_t bank;
	dcgw_t dcgw[1];
} clasDCGW_t;

/* --------------------- END dcgw -----------------*/

/* ------------------------ dch -----------------*/
typedef struct {
	float x;    /*  x at layer center */
	float y;    /*  y at layer center */
	float z;    /*  z at layer center */
	float step;    /*  step size through layer */
	float dedx;    /*  energy deposit in layer */
	float pmom;    /*  track momentum at layer center  */
	float time;    /*  time of hit at layer center */
	float cx;    /*  track x dir cosine at layer center */
	float cy;    /*  track y dir cosine at layer center */
	float cz;    /*  track z dir cosine at layer center */
	int track;    /*  track number */
	int id;    /*  track PDG id */
	int layer;    /*  layer number */
} dch_t;

typedef struct {
	bankHeader_t bank;
	dch_t dch[1];
} clasDCH_t;

/* --------------------- END dch -----------------*/

/* ------------------------ dcmn -----------------*/
typedef struct {
	float hbt_evt_1;    /*  Hit Based tracks per event for Sector 1  */
	float tbt_evt_1;    /*  Time Based tracks per event for Sector 1  */
	float hbt_evt_2;    /*  Hit Based tracks per event for Sector 2 */
	float tbt_evt_2;    /*  Time Based tracks per event for Sector 2  */
	float hbt_evt_3;    /*  Hit Based tracks per event for Sector 3  */
	float tbt_evt_3;    /*  Time Based tracks per event for Sector 3  */
	float hbt_evt_4;    /*  Hit Based tracks per event for Sector 4  */
	float tbt_evt_4;    /*  Time Based tracks per event for Sector 4  */
	float hbt_evt_5;    /*  Hit Based tracks per event for Sector 5  */
	float tbt_evt_5;    /*  Time Based tracks per event for Sector 5  */
	float hbt_evt_6;    /*  Hit Based tracks per event for Sector 6  */
	float tbt_evt_6;    /*  Time Based tracks per event for Sector 6  */
	float hbt_evt_all;    /*  Hit Based tracks per event for all sec.   */
	float tbt_evt_all;    /*  Time Based tracks per event for all sec.  */
	float res_s1_sl1;    /*  Residual rms for sec 1, superlayer 1   */
	float res_s1_sl2;    /*  Residual rms for sec 1, superlayer 2   */
	float res_s1_sl3;    /*  Residual rms for sec 1, superlayer 3   */
	float res_s1_sl4;    /*  Residual rms for sec 1, superlayer 4   */
	float res_s1_sl5;    /*  Residual rms for sec 1, superlayer 5   */
	float res_s1_sl6;    /*  Residual rms for sec 1, superlayer 6   */
	float res_s2_sl1;    /*  Residual rms for sec 2, superlayer 1   */
	float res_s2_sl2;    /*  Residual rms for sec 2, superlayer 2   */
	float res_s2_sl3;    /*  Residual rms for sec 2, superlayer 3   */
	float res_s2_sl4;    /*  Residual rms for sec 2, superlayer 4   */
	float res_s2_sl5;    /*  Residual rms for sec 2, superlayer 5   */
	float res_s2_sl6;    /*  Residual rms for sec 2, superlayer 6   */
	float res_s3_sl1;    /*  Residual rms for sec 3, superlayer 1   */
	float res_s3_sl2;    /*  Residual rms for sec 3, superlayer 2   */
	float res_s3_sl3;    /*  Residual rms for sec 3, superlayer 3   */
	float res_s3_sl4;    /*  Residual rms for sec 3, superlayer 4   */
	float res_s3_sl5;    /*  Residual rms for sec 3, superlayer 5   */
	float res_s3_sl6;    /*  Residual rms for sec 3, superlayer 6   */
	float res_s4_sl1;    /*  Residual rms for sec 4, superlayer 1   */
	float res_s4_sl2;    /*  Residual rms for sec 4, superlayer 2   */
	float res_s4_sl3;    /*  Residual rms for sec 4, superlayer 3   */
	float res_s4_sl4;    /*  Residual rms for sec 4, superlayer 4   */
	float res_s4_sl5;    /*  Residual rms for sec 4, superlayer 5   */
	float res_s4_sl6;    /*  Residual rms for sec 4, superlayer 6   */
	float res_s5_sl1;    /*  Residual rms for sec 5, superlayer 1   */
	float res_s5_sl2;    /*  Residual rms for sec 5, superlayer 2   */
	float res_s5_sl3;    /*  Residual rms for sec 5, superlayer 3   */
	float res_s5_sl4;    /*  Residual rms for sec 5, superlayer 4   */
	float res_s5_sl5;    /*  Residual rms for sec 5, superlayer 5   */
	float res_s5_sl6;    /*  Residual rms for sec 5, superlayer 6   */
	float res_s6_sl1;    /*  Residual rms for sec 6, superlayer 1   */
	float res_s6_sl2;    /*  Residual rms for sec 6, superlayer 2   */
	float res_s6_sl3;    /*  Residual rms for sec 6, superlayer 3   */
	float res_s6_sl4;    /*  Residual rms for sec 6, superlayer 4   */
	float res_s6_sl5;    /*  Residual rms for sec 6, superlayer 5   */
	float res_s6_sl6;    /*  Residual rms for sec 6, superlayer 6   */
} dcmn_t;

typedef struct {
	bankHeader_t bank;
	dcmn_t dcmn[1];
} clasDCMN_t;

/* --------------------- END dcmn -----------------*/

/* ------------------------ dcpb -----------------*/
typedef struct {
	int sctr;    /*  100*sector+track_ID in *BTR   */
	float x_sc;    /*  x coordinate of track intersection with SC plane  */
	float y_sc;    /*  y coordinate of track intersection with SC plane */
	float z_sc;    /*  z coordinate of track intersection with SC plane */
	float cx_sc;    /*  X dir cosine at (x_SC,y_SC,z_SC) */
	float cy_sc;    /*  y dir cosine at (x_SC,y_SC,z_SC) */
	float cz_sc;    /*  z dir cosine at (x_SC,y_SC,z_SC) */
	float x_v;    /*  vertex X after fiting to the beam position */
	float y_v;    /*  vertex Y after fiting to the beam position */
	float z_v;    /*  vertex Z after fiting to the beam position */
	float r_v;    /*  distance from production vertex to the bemam.  */
	float chi2;    /*  Chisquare of track fitting */
	int status;    /*  Status word */
} dcpb_t;

typedef struct {
	bankHeader_t bank;
	dcpb_t dcpb[1];
} clasDCPB_t;

/* --------------------- END dcpb -----------------*/

/* ------------------------ dcv1 -----------------*/
typedef struct {
	float tsr1;    /*  Tsmear  */
	float v0r1;    /*  drift velocity (slope) */
	float tmr1;    /*  Maximum drift time (Tmax) */
	float sp1r1;    /*  spare   */
	float sp2r1;    /*  spare	  */
} dcv1_t;

typedef struct {
	bankHeader_t bank;
	dcv1_t dcv1[1];
} clasDCV1_t;

/* --------------------- END dcv1 -----------------*/

/* ------------------------ dcv2 -----------------*/
typedef struct {
	float ts1r2;    /*  Tsmear  */
	float v01r2;    /*  drift velocity (slope) */
	float va1r2;    /*  drift velocity function parameter */
	float vb1r2;    /*  drift velocity function parameter   */
	float tm1r2;    /*  Maximum drift time (Tmax) 		  */
	float ta1r2;    /*  Tmax function parameter */
	float tb1r2;    /*  Tmax function parameter */
	float ts2r2;    /*  Tsmear  */
	float v02r2;    /*  drift velocity (slope) */
	float va2r2;    /*  drift velocity function parameter */
	float vb2r2;    /*  drift velocity function parameter   */
	float tm2r2;    /*  Maximum drift time (Tmax) 		  */
	float ta2r2;    /*  Tmax function parameter */
	float tb2r2;    /*  Tmax function parameter */
} dcv2_t;

typedef struct {
	bankHeader_t bank;
	dcv2_t dcv2[1];
} clasDCV2_t;

/* --------------------- END dcv2 -----------------*/

/* ------------------------ dcv3 -----------------*/
typedef struct {
	float tsr3;    /*  Tsmear  */
	float v0r3;    /*  drift velocity (slope) */
	float tmr3;    /*  Maximum drift time (Tmax) */
	float sp1r3;    /*  spare   */
	float sp2r3;    /*  spare	  */
} dcv3_t;

typedef struct {
	bankHeader_t bank;
	dcv3_t dcv3[1];
} clasDCV3_t;

/* --------------------- END dcv3 -----------------*/

/* ------------------------ ddly -----------------*/
typedef struct {
	int id;    /*  ID_wire  */
	float tidly;    /*  TIme DeLaY (ns) 		  */
	int stat;    /*  wire STATus = 100*C + 10*A + B */
} ddly_t;

typedef struct {
	bankHeader_t bank;
	ddly_t ddly[1];
} clasDDLY_t;

/* --------------------- END ddly -----------------*/

/* ------------------------ dgeo -----------------*/
typedef struct {
	int id_sec;    /*  ID_sector  */
	int id_reg;    /*  ID_region  */
	float xpos;    /*  x misalignment 		  */
	float ypos;    /*  y misalignment 		  */
	float zpos;    /*  z misalignment 		  */
	float sxpos;    /*  sx sine of little x angle 		  */
	float sypos;    /*  sy sine of little y angle  */
	float szpos;    /*  sz sine of little z angle  */
} dgeo_t;

typedef struct {
	bankHeader_t bank;
	dgeo_t dgeo[1];
} clasDGEO_t;

/* --------------------- END dgeo -----------------*/

/* ------------------------ dhcl -----------------*/
typedef struct {
	int sly;    /*  superlayer */
	int btrk;    /*  track_in_sector# (bit set) */
	int trks1;    /*  combinations to track segments */
	int trks2;    /*    "             "       " */
	int wire1;    /*  1.wire# in this cluster in 1.layer */
	int bwir1;    /*  hits in this layer (starting from WIRE1) (bit st) */
	int wire2;    /*  1.wire# in this cluster in 1.layer */
	int bwir2;    /*  hits in this layer (starting from WIRE1) (bit st) */
	int wire3;    /*  1.wire# in this cluster in 1.layer */
	int bwir3;    /*  hits in this layer (starting from WIRE1) (bit st) */
	int wire4;    /*  1.wire# in this cluster in 1.layer */
	int bwir4;    /*  hits in this layer (starting from WIRE1) (bit st) */
	int wire5;    /*  1.wire# in this cluster in 1.layer */
	int bwir5;    /*  hits in this layer (starting from WIRE1) (bit st) */
	int wire6;    /*  1.wire# in this cluster in 1.layer */
	int bwir6;    /*  hits in this layer (starting from WIRE1) (bit st) */
} dhcl_t;

typedef struct {
	bankHeader_t bank;
	dhcl_t dhcl[1];
} clasDHCL_t;

/* --------------------- END dhcl -----------------*/

/* ------------------------ ditm -----------------*/
typedef struct {
	int time;    /*  time of discriminator calibration */
} ditm_t;

typedef struct {
	bankHeader_t bank;
	ditm_t ditm[1];
} clasDITM_t;

/* --------------------- END ditm -----------------*/

/* ------------------------ doca -----------------*/
typedef struct {
	uint16 id;    /*  the address of the hit detector element */
	uint16 doca;    /*  doca from GSIM (micron) */
} doca_t;

typedef struct {
	bankHeader_t bank;
	doca_t doca[1];
} clasDOCA_t;

/* --------------------- END doca -----------------*/

/* ------------------------ dpcp -----------------*/
typedef struct {
	int id;    /*  the address of the hit detector element */
	int mn_mean;    /*  adc pedestal mean value (channel) */
	float mn_sigma;    /*  sigma of the pedestal distribution (channel) */
	int lt_mean;    /*  adc pedestal mean value (channel) */
	float lt_sigma;    /*  sigma of the pedestal distribution (channel) */
	int rb_mean;    /*  adc pedestal mean value (channel) */
	float rb_sigma;    /*  sigma of the pedestal distribution (channel) */
	int lb_mean;    /*  adc pedestal mean value (channel) */
	float lb_sigma;    /*  sigma of the pedestal distribution (channel) */
	int rt_mean;    /*  adc pedestal mean value (channel) */
	float rt_sigma;    /*  sigma of the pedestal distribution (channel) */
	int vt_mean;    /*  adc pedestal mean value (channel) */
	float vt_sigma;    /*  sigma of the pedestal distribution (channel) */
} dpcp_t;

typedef struct {
	bankHeader_t bank;
	dpcp_t dpcp[1];
} clasDPCP_t;

/* --------------------- END dpcp -----------------*/

/* ------------------------ dpsp -----------------*/
typedef struct {
	int id;    /*  the address of the hit detector element */
	int mean;    /*  adc pedestal mean value (channel) */
	float sigma;    /*  sigma of the pedestal distribution (channel) */
} dpsp_t;

typedef struct {
	bankHeader_t bank;
	dpsp_t dpsp[1];
} clasDPSP_t;

/* --------------------- END dpsp -----------------*/

/* ------------------------ dspc -----------------*/
typedef struct {
	uint16 pcid;    /*  Id compelled by DAQ (always 1) */
	uint16 tdcpc;    /*  tdc information ( scintillator) */
	uint16 adcmn;    /*  adc information (main) */
	uint16 adclt;    /*  adc information (left top) */
	uint16 adcrb;    /*  adc information (right bottom) */
	uint16 adclb;    /*  adc information (left bottom) */
	uint16 adcrt;    /*  adc information (right top) */
	uint16 adcve;    /*  adc information (veto) */
} dspc_t;

typedef struct {
	bankHeader_t bank;
	dspc_t dspc[1];
} clasDSPC_t;

/* --------------------- END dspc -----------------*/

/* ------------------------ dsps -----------------*/
typedef struct {
	uint16 id;    /*  paddle ID (left = 1 to 4)( right = 5 to 8) */
	uint16 tdc;    /*  tdc information */
	uint16 adc;    /*  adc information  */
} dsps_t;

typedef struct {
	bankHeader_t bank;
	dsps_t dsps[1];
} clasDSPS_t;

/* --------------------- END dsps -----------------*/

/* ------------------------ dstc -----------------*/
typedef struct {
	uint16 tacid;    /*  ID compelled by DAQ (always 1) */
	uint16 tdctac;    /*  tdc information (on sum) */
	uint16 adclt;    /*  adc information (left top) */
	uint16 adcrt;    /*  adc information (right top) */
	uint16 adclb;    /*  adc information (left bottom) */
	uint16 adcrb;    /*  adc information (right bottom) */
	uint16 adcsum1;    /*  adc information (sum scale1) */
	uint16 adcsum2;    /*  adc information (sum scale2) */
	uint16 adcsum3;    /*  adc information (sum scale3) */
} dstc_t;

typedef struct {
	bankHeader_t bank;
	dstc_t dstc[1];
} clasDSTC_t;

/* --------------------- END dstc -----------------*/

/* ------------------------ dtcp -----------------*/
typedef struct {
	int id;    /*  the address of the hit detector element */
	int lt_mean;    /*  adc pedestal mean value (channel) */
	float lt_sigma;    /*  sigma of the pedestal distribution (channel) */
	int rt_mean;    /*  adc pedestal mean value (channel) */
	float rt_sigma;    /*  sigma of the pedestal distribution (channel) */
	int lb_mean;    /*  adc pedestal mean value (channel) */
	float lb_sigma;    /*  sigma of the pedestal distribution (channel) */
	int rb_mean;    /*  adc pedestal mean value (channel) */
	float rb_sigma;    /*  sigma of the pedestal distribution (channel) */
	int sum1_mean;    /*  adc pedestal mean value (channel) */
	float sum1_sigma;    /*  sigma of the pedestal distribution (channel) */
	int sum2_mean;    /*  adc pedestal mean value (channel) */
	float sum2_sigma;    /*  sigma of the pedestal distribution (channel) */
	int sum3_mean;    /*  adc pedestal mean value (channel) */
	float sum3_sigma;    /*  sigma of the pedestal distribution (channel) */
} dtcp_t;

typedef struct {
	bankHeader_t bank;
	dtcp_t dtcp[1];
} clasDTCP_t;

/* --------------------- END dtcp -----------------*/

/* ------------------------ dtrk -----------------*/
typedef struct {
	float x;    /*  */
	float y;    /*  */
	float z;    /*  */
} dtrk_t;

typedef struct {
	bankHeader_t bank;
	dtrk_t dtrk[1];
} clasDTRK_t;

/* --------------------- END dtrk -----------------*/

/* ------------------------ ec01 -----------------*/
typedef struct {
	int id;    /*  the address of the hit detector element */
	float time;    /*  time for left paddle(ns)  */
	float energy;    /*  energy in left paddle(MeV)  */
} ec01_t;

typedef struct {
	bankHeader_t bank;
	ec01_t ec01[1];
} clasEC01_t;

/* --------------------- END ec01 -----------------*/

/* ------------------------ ec1 -----------------*/
typedef struct {
	uint16 id;    /*  the address of the hit detector element */
	uint16 tdcl;    /*  tdc information (channels) */
	uint16 adcl;    /*  adc information (channels) */
	uint16 tdcr;    /*  tdc information (channels) */
	uint16 adcr;    /*  adc information (channels) */
} ec1_t;

typedef struct {
	bankHeader_t bank;
	ec1_t ec1[1];
} clasEC1_t;

/* --------------------- END ec1 -----------------*/

/* ------------------------ ec1p -----------------*/
typedef struct {
	float n1x;    /*  x component of outward normal to plane */
	float n1y;    /*  y component of outward normal to plane */
	float n1z;    /*  z component of outward normal to plane */
	float r1n;    /*  distanse in cm from origin to plane */
} ec1p_t;

typedef struct {
	bankHeader_t bank;
	ec1p_t ec1p[1];
} clasEC1P_t;

/* --------------------- END ec1p -----------------*/

/* ------------------------ ec1r -----------------*/
typedef struct {
	float e_tot;    /*  cluster energy (sum of inner+outer) */
	float de_tot;    /*   error on the cluster energy */
	float t_tot;    /*  time */
	float dt_tot;    /*  error on the time */
	float x_m;    /*  x-in CLAS frame */
	float y_m;    /*  y-in CLAS frame */
	float z_m;    /*  z-in CLAS frame */
	float dx_m;    /*  error on x */
	float dy_m;    /*  error on y */
	float dz_m;    /*  error on z */
	float e_in;    /*   cluster energy in inner layer */
	float t_in;    /*  time from inner layer */
	float x_in;    /*  lab coordinate , inner layer */
	float y_in;    /*  lab coordinate , inner layer */
	float x_out;    /*  lab coordinate , outer layer */
	float y_out;    /*  lab coordinate , outer layer */
	float x2_in_l;    /*  second moment of x inner left */
	float x2_in_r;    /*  second moment of x inner right  */
	float y2_in_l;    /*  second moment of y inner left */
	float y2_in_r;    /*  second moment of y inner right */
	float x2_out_l;    /*  second moment of x outer hit left */
	float x2_out_r;    /*  second moment of x outer hit right */
	float y2_out_l;    /*  second moment of y outer hit left */
	float y2_out_r;    /*  second moment of y outer hit right */
	int i_in;    /*  cluster center in X inner short layer */
	int j_in;    /*  cluster center in Y inner long layer */
	int i_out;    /*  cluster center in X outer layers */
	int j_out;    /*  cluster center in Y outer layer */
	float a_in_xl;    /*  energy sum in inner short left pmts */
	float a_in_xr;    /*  energy sum in inner short right pmts */
	float a_in_yl;    /*  energy sum in inner long  left pmts */
	float a_in_yr;    /*  energy sum in inner long  right pmts */
	float a_out_xl;    /*  energy sum in outer short left pmts */
	float a_out_xr;    /*  energy sum in outer short right pmts */
	float a_out_yl;    /*  energy sum in outer long left pmts */
	float a_out_yr;    /*  energy sum in outer long right pmts */
	float t_in_xs;    /*  tdc sum in inner short righ+left  for cluster center */
	float t_in_xd;    /*  tdc dif in inner short right-left  */
	float t_in_ys;    /*  tdc sum in inner long  righ+left */
	float t_in_yd;    /*  tdc dif in inner long  right-left */
	float t_out_xs;    /*  tdc sum in outer short righ+left  */
	float t_out_xd;    /*  tdc dif in outer short right-left  */
	float t_out_ys;    /*  tdc sum in outer long righ+left */
	float t_out_yd;    /*  tdc dif in outer long right-left  */
	int ibl;    /*  LAC  block number */
	int ncluster;    /*  1000xNclust4+100xNclust3+10xNclust2+Nclust1  */
	int pmtfired;    /*  Number of fired pmt (more than threshold)  */
	float z_in;    /*  Z in CLAS frame */
	float z_out;    /*  Z out in CLAS frame */
	int istat;    /*  status word */
} ec1r_t;

typedef struct {
	bankHeader_t bank;
	ec1r_t ec1r[1];
} clasEC1R_t;

/* --------------------- END ec1r -----------------*/

/* ------------------------ ecca -----------------*/
typedef struct {
	int id;    /*  PMT ID */
	float aped;    /*  ADC pedestals (channels) */
	float asig;    /*  ADC pedestal variance (channels) */
	float amip;    /*  ADC calibration from MIP (N.I.M.P/ch.) */
	float amipu;    /*  aMIP Error */
	float ashr;    /*  ADC calibration from showers (GeV/ch.) */
	float ashru;    /*  aSHR Error */
	int stat;    /*  4 byte status word     */
} ecca_t;

typedef struct {
	bankHeader_t bank;
	ecca_t ecca[1];
} clasECCA_t;

/* --------------------- END ecca -----------------*/

/* ------------------------ eccl -----------------*/
typedef struct {
	int id;    /*  PMT ID */
	float ldb;    /*  Stack atten length (database)(cm) */
	float ldbu;    /*  lDB Error (cm) */
	float lmip;    /*  Stack atten length (MinIonPart)(cm)  */
	float lmipu;    /*  lMIP Error (cm) */
	float lshr;    /*  Stack atten length (showers)(cm)   */
	float lshru;    /*  lSHR Error (cm)  */
	int stat;    /*  4 byte status word     */
} eccl_t;

typedef struct {
	bankHeader_t bank;
	eccl_t eccl[1];
} clasECCL_t;

/* --------------------- END eccl -----------------*/

/* ------------------------ ecct -----------------*/
typedef struct {
	int id;    /*  PMT ID */
	float toff;    /*  TDC offset (channels) */
	float toffu;    /*  tOFF Error */
	float tgain;    /*  TDC conversion gain (nS/channel) */
	float tgainu;    /*  tGAIN Error */
	float tw0;    /*  Time walk constant (channels) */
	float tw0u;    /*  tWOu Error */
	float tw1;    /*  Time walk correction parameter  */
	float tw1u;    /*  tW1u Error */
	float tvef;    /*  Effective velocity of light (cm/ns) */
	float tvefu;    /*  tVEFu Error */
	int stat;    /*  4 byte status word     */
} ecct_t;

typedef struct {
	bankHeader_t bank;
	ecct_t ecct[1];
} clasECCT_t;

/* --------------------- END ecct -----------------*/

/* ------------------------ ec -----------------*/
typedef struct {
	uint16 id;    /*  the address of the hit detector element */
	uint16 tdc;    /*  tdc information (channels) */
	uint16 adc;    /*  adc information (channels) */
} ec_t;

typedef struct {
	bankHeader_t bank;
	ec_t ec[1];
} clasEC_t;

/* --------------------- END ec -----------------*/

/* ------------------------ ecdi -----------------*/
typedef struct {
	int crate;    /*  CAMAC crate number */
	int slot;    /*  slot */
	int mask;    /*  mask */
	int threshold;    /*  actual threshold value (mV) */
	int width;    /*  actual width value */
} ecdi_t;

typedef struct {
	bankHeader_t bank;
	ecdi_t ecdi[1];
} clasECDI_t;

/* --------------------- END ecdi -----------------*/

/* ------------------------ ecg -----------------*/
typedef struct {
	float l0;    /*  distance from the target  */
	float the0;    /*  angle between the beam and perpendicular */
	float ylow;    /*   */
	float yhi;    /*   */
	float dylow;    /*   */
	float dyhi;    /*   */
	float lrth;    /*  thickness of the single layer */
	float tangr;    /*   */
	int sector;    /*   */
	float phisec;    /*   */
	float x0off;    /*   */
	float y0off;    /*   */
	float z0off;    /*   */
	float rotm11;    /*   */
	float rotm12;    /*   */
	float rotm13;    /*   */
	float rotm21;    /*   */
	float rotm22;    /*   */
	float rotm23;    /*   */
	float rotm31;    /*   */
	float rotm32;    /*   */
	float rotm33;    /*   */
} ecg_t;

typedef struct {
	bankHeader_t bank;
	ecg_t ecg[1];
} clasECG_t;

/* --------------------- END ecg -----------------*/

/* ------------------------ echb -----------------*/
typedef struct {
	int sect;    /*  Sector number & Layer number */
	float e__hit;    /*  energy found  */
	float de_hit;    /*  error on the energy found  */
	float t_hit;    /*  time found  */
	float dt_hi;    /*  error time found  */
	float i_hit;    /*  sector rectangular coordinate  */
	float j_hit;    /*  sector rectangular coordinate  */
	float di_hit;    /*  sector rectangular coordinate error, */
	float dj_hit;    /*  sector rectangular coordinate error, */
	float x_hit;    /*  lab coordinate, */
	float y_hit;    /*  lab coordinate, */
	float z_hit;    /*  lab coordinate, */
	float dx_hit;    /*  lab coordinate error, */
	float dy_hit;    /*  lab coordinate error,  */
	float dz_hit;    /*  lab coordinate error, */
	float u2_hit;    /*  second moment of u  _hit pattern */
	float v2_hit;    /*  second moment of v  _hit pattern */
	float w2_hit;    /*  second moment of w  _hit pattern */
	float u3_hit;    /*  third moment of u  _hit pattern  */
	float v3_hit;    /*  third moment of v  _hit pattern  */
	float w3_hit;    /*  third moment of w  _hit pattern  */
	float u4_hit;    /*  forth moment of u  _hit pattern  */
	float v4_hit;    /*  forth moment of v  _hit pattern  */
	float w4_hit;    /*  forth moment of w  _hit pattern  */
	float centr_u;    /*  peak position on U axis  */
	float centr_v;    /*  peak position on V axis  */
	float centr_w;    /*  peak position on W axis  */
	float path_u;    /*  path length from hit position to U axis  */
	float path_v;    /*  path length from hit position to V axis  */
	float path_w;    /*  path length from hit position to W axis  */
	int nstrp_u;    /*  Number of U strips in the hit */
	int nstrp_v;    /*  Number of V strips in the hit */
	int nstrp_w;    /*  Number of W strips in the hit */
	int matchid1;    /*  Id of matched hit in the layer1 */
	float ch21;    /*  Quality measure of matching with layer1 */
	int matchid2;    /*  Id of matched hit in the layer2 */
	float ch22;    /*  Quality measure of matching with layer2 */
	int istat;    /*  Number of hits & hit ID */
} echb_t;

typedef struct {
	bankHeader_t bank;
	echb_t echb[1];
} clasECHB_t;

/* --------------------- END echb -----------------*/

/* ------------------------ ech -----------------*/
typedef struct {
	float x;    /*  x of hit */
	float y;    /*  y of hit */
	float z;    /*  z of hit */
	float cx;    /*  track x dir cosine */
	float cy;    /*  track y dir cosine */
	float cz;    /*  track z dir cosine */
	float pmom;    /*  track momentum */
	int id;    /*  track PDG id */
	float tof;    /*  time of flight */
	float edepin;    /*  deposit energy (inner part) */
	float edepout;    /*  deposit energy (outer part) */
} ech_t;

typedef struct {
	bankHeader_t bank;
	ech_t ech[1];
} clasECH_t;

/* --------------------- END ech -----------------*/

/* ------------------------ ecmt -----------------*/
typedef struct {
	int in_high;    /*  inner high threshold (mV) */
	int out_high;    /*  outer high threshold (mV) */
	int total_high;    /*  total high threshold (mV) */
	int in_lo;    /*  inner lo threshold (mV) */
	int out_lo;    /*  outer lo threshold (mV) */
	int total_lo;    /*  total lo threshold (mV) */
} ecmt_t;

typedef struct {
	bankHeader_t bank;
	ecmt_t ecmt[1];
} clasECMT_t;

/* --------------------- END ecmt -----------------*/

/* ------------------------ ecp1 -----------------*/
typedef struct {
	int id;    /*  the address of the hit detector element */
	int mean_left;    /*  left adc pedestal mean value (channel) */
	float sigma_left;    /*  sigma of the pedestal distribution for left adc (channel) */
	int mean_right;    /*  right adc pedestal mean value (channel) */
	float sigma_right;    /*  sigma of the pedestal distribution for right adc (channel */
} ecp1_t;

typedef struct {
	bankHeader_t bank;
	ecp1_t ecp1[1];
} clasECP1_t;

/* --------------------- END ecp1 -----------------*/

/* ------------------------ ecpb -----------------*/
typedef struct {
	int scht;    /*  100*sector+Whole_Hit_ID in ECHB  */
	float etot;    /*  Reconstructed total energy */
	float ein;    /*  Inner energy */
	float eout;    /*  Outer energy  */
	float time;    /*  Flight time relative to the evnt start time */
	float path;    /*  Path lenght from target */
	float x;    /*  x coordinate of hit  */
	float y;    /*  y coordinate of hit */
	float z;    /*  z coordinate of hit */
	float m2_hit;    /*  second moment of _hit pattern */
	float m3_hit;    /*  third moment of  _hit pattern  */
	float m4_hit;    /*  forth moment of  _hit pattern  */
	int innstr;    /*  10000*UI+100*VI+WI  */
	int outstr;    /*  10000*UO+100*VO+WO  */
	float chi2ec;    /*  Quality measure of geometrical matching */
	int status;    /*  Status word (not implemented yet) */
} ecpb_t;

typedef struct {
	bankHeader_t bank;
	ecpb_t ecpb[1];
} clasECPB_t;

/* --------------------- END ecpb -----------------*/

/* ------------------------ ecpc -----------------*/
typedef struct {
	int id;    /*  layer(1-3) * 100 + strip ID */
	float tdc;    /*  tdc (channels) */
	float adc;    /*  adc - pedestal (channels) */
} ecpc_t;

typedef struct {
	bankHeader_t bank;
	ecpc_t ecpc[1];
} clasECPC_t;

/* --------------------- END ecpc -----------------*/

/* ------------------------ ecp -----------------*/
typedef struct {
	float n1x;    /*  x component of outward normal to plane */
	float n1y;    /*  y component of outward normal to plane */
	float n1z;    /*  z component of outward normal to plane */
	float r1n;    /*  distanse in cm from origin to plane */
	float plw;    /*  whole plane depth */
	float pli;    /*  inner plane depth */
	float plo;    /*  outer plane depth */
} ecp_t;

typedef struct {
	bankHeader_t bank;
	ecp_t ecp[1];
} clasECP_t;

/* --------------------- END ecp -----------------*/

/* ------------------------ ecpe -----------------*/
typedef struct {
	int id;    /*  the address of the hit detector element */
	int mean;    /*  adc pedestal mean value (channel) */
	float sigma;    /*  sigma of the pedestal distribution (channel) */
} ecpe_t;

typedef struct {
	bankHeader_t bank;
	ecpe_t ecpe[1];
} clasECPE_t;

/* --------------------- END ecpe -----------------*/

/* ------------------------ ecpi -----------------*/
typedef struct {
	int id;    /*  sector number */
	int layer;    /*  layer number, note 1-inner, 2-outer, 3-whole */
	int hitid;    /*  number of hits (first 16 bits) and hit ID (last 16 bits) */
	float iloc;    /*  position of the hit in the local coordinate system */
	float jloc;    /*  position of the hit in the local coordinate system */
	float diloc;    /*  i width of the hit */
	float djloc;    /*  j width of the hit  */
	float r;    /*  radius of the shower */
	float e;    /*  energy */
} ecpi_t;

typedef struct {
	bankHeader_t bank;
	ecpi_t ecpi[1];
} clasECPI_t;

/* --------------------- END ecpi -----------------*/

/* ------------------------ ecpo -----------------*/
typedef struct {
	int uid1;    /*  Pointer to the U-strips 1-18 for hit */
	int uid2;    /*  Pointer to the U-strips 19-3 for hit */
	int vid1;    /*  Pointer to the V-strips 1-18 for hit */
	int vid2;    /*  Pointer to the V-strips 19-3 for hit */
	int wid1;    /*  Pointer to the W-strips 1-18 for hit  */
	int wid2;    /*  Pointer to the W-strips 19-36 for hit  */
	int slh;    /*  Sector*1000+Layer*100+Hit */
} ecpo_t;

typedef struct {
	bankHeader_t bank;
	ecpo_t ecpo[1];
} clasECPO_t;

/* --------------------- END ecpo -----------------*/

/* ------------------------ ecrb -----------------*/
typedef struct {
	float e_in;    /*  energy found for the inner layer */
	float e_out;    /*  energy found for the outer layer */
	float de_in;    /*  error on the energy found for the inner layer */
	float de_out;    /*  error on the energy found for the outer layer */
	float t_in;    /*  time found for the inner layer */
	float t_out;    /*  time found for the outer layer */
	float dt_in;    /*  error on the time found for the inner layer */
	float dt_out;    /*  error on the time found for the outer layer */
	float i_in;    /*  sector rectangular coordinate for the inner layer */
	float j_in;    /*  sector rectangular coordinate for the inner layer */
	float i_out;    /*  sector rectangular coordinate for the outer layer */
	float j_out;    /*  sector rectangular coordinate for the outer layer */
	float di_in;    /*  sector rectangular coordinate error, inner layer */
	float dj_in;    /*  sector rectangular coordinate error, inner layer */
	float di_out;    /*  sector rectangular coordinate error, outer layer */
	float dj_out;    /*  sector rectangular coordinate error, outer layer */
	float x_in;    /*  lab coordinate, inner layer */
	float y_in;    /*  lab coordinate, inner layer */
	float z_in;    /*  lab coordinate, inner layer */
	float x_out;    /*  lab coordinate, outer layer */
	float y_out;    /*  lab coordinate, outer layer */
	float z_out;    /*  lab coordinate, outer layer */
	float dx_in;    /*  lab coordinate error, inner layer */
	float dy_in;    /*  lab coordinate error, inner layer */
	float dz_in;    /*  lab coordinate error, inner layer */
	float dx_out;    /*  lab coordinate error, outer layer */
	float dy_out;    /*  lab coordinate error, outer layer */
	float dz_out;    /*  lab coordinate error, outer layer */
	float u2_in;    /*  second moment of u inner hit pattern */
	float v2_in;    /*  second moment of v inner hit pattern */
	float w2_in;    /*  second moment of w inner hit pattern */
	float u2_out;    /*  second moment of u outer hit pattern */
	float v2_out;    /*  second moment of v outer hit pattern */
	float w2_out;    /*  second moment of w outer hit pattern */
	float u3_in;    /*  third moment of u inner hit pattern  */
	float v3_in;    /*  third moment of v inner hit pattern  */
	float w3_in;    /*  third moment of w inner hit pattern  */
	float u3_out;    /*  third moment of u outer hit pattern  */
	float v3_out;    /*  third moment of v outer hit pattern  */
	float w3_out;    /*  third moment of w outer hit pattern  */
	float i2;    /*  second moment of overall shower, sector coordinates */
	float j2;    /*  second moment of overall shower, sector coordinates */
	float i3;    /*  third moment of overall shower, sector coordinates */
	float j3;    /*  third moment of overall shower, sector coordinates */
	float spare1;    /*  spare */
	float spare2;    /*  spare */
	float spare3;    /*  spare */
	float spare4;    /*  spare */
	int istat;    /*  status word */
} ecrb_t;

typedef struct {
	bankHeader_t bank;
	ecrb_t ecrb[1];
} clasECRB_t;

/* --------------------- END ecrb -----------------*/

/* ------------------------ ecs -----------------*/
typedef struct {
	int ecs1;    /*  scaler sector 1 */
	int ecs2;    /*  scaler sector 1 */
	int ecs3;    /*  scaler sector 1 */
	int ecs4;    /*  scaler sector 1 */
	int ecs5;    /*  scaler sector 1 */
	int ecs6;    /*  scaler sector 1 */
	int ecs7;    /*  scaler sector 1 */
	int ecs8;    /*  scaler sector 1 */
	int ecs9;    /*  scaler sector 1 */
	int ecs10;    /*  scaler sector 1 */
	int ecs11;    /*  scaler sector 1 */
	int ecs12;    /*  scaler sector 1 */
	int ecs13;    /*  scaler sector 1 */
	int ecs14;    /*  scaler sector 1 */
	int ecs15;    /*  scaler sector 1 */
	int ecs16;    /*  scaler sector 1 */
	int ecs17;    /*  scaler sector 2 */
	int ecs18;    /*  scaler sector 2 */
	int ecs19;    /*  scaler sector 2 */
	int ecs20;    /*  scaler sector 2 */
	int ecs21;    /*  scaler sector 2 */
	int ecs22;    /*  scaler sector 2 */
	int ecs23;    /*  scaler sector 2 */
	int ecs24;    /*  scaler sector 2 */
	int ecs25;    /*  scaler sector 2 */
	int ecs26;    /*  scaler sector 2 */
	int ecs27;    /*  scaler sector 2 */
	int ecs28;    /*  scaler sector 2 */
	int ecs29;    /*  scaler sector 2 */
	int ecs30;    /*  scaler sector 2 */
	int ecs31;    /*  scaler sector 2 */
	int ecs32;    /*  scaler sector 2 */
	int ecs33;    /*  scaler sector 3 */
	int ecs34;    /*  scaler sector 3 */
	int ecs35;    /*  scaler sector 3 */
	int ecs36;    /*  scaler sector 3 */
	int ecs37;    /*  scaler sector 3 */
	int ecs38;    /*  scaler sector 3 */
	int ecs39;    /*  scaler sector 3 */
	int ecs40;    /*  scaler sector 3 */
	int ecs41;    /*  scaler sector 3 */
	int ecs42;    /*  scaler sector 3 */
	int ecs43;    /*  scaler sector 3 */
	int ecs44;    /*  scaler sector 3 */
	int ecs45;    /*  scaler sector 3 */
	int ecs46;    /*  scaler sector 3 */
	int ecs47;    /*  scaler sector 3 */
	int ecs48;    /*  scaler sector 3 */
	int ecs49;    /*  scaler sector 4 */
	int ecs50;    /*  scaler sector 4 */
	int ecs51;    /*  scaler sector 4 */
	int ecs52;    /*  scaler sector 4 */
	int ecs53;    /*  scaler sector 4 */
	int ecs54;    /*  scaler sector 4 */
	int ecs55;    /*  scaler sector 4 */
	int ecs56;    /*  scaler sector 4 */
	int ecs57;    /*  scaler sector 4 */
	int ecs58;    /*  scaler sector 4 */
	int ecs59;    /*  scaler sector 4 */
	int ecs60;    /*  scaler sector 4 */
	int ecs61;    /*  scaler sector 4 */
	int ecs62;    /*  scaler sector 4 */
	int ecs63;    /*  scaler sector 4 */
	int ecs64;    /*  scaler sector 4 */
	int ecs65;    /*  scaler sector 5 */
	int ecs66;    /*  scaler sector 5 */
	int ecs67;    /*  scaler sector 5 */
	int ecs68;    /*  scaler sector 5 */
	int ecs69;    /*  scaler sector 5 */
	int ecs70;    /*  scaler sector 5 */
	int ecs71;    /*  scaler sector 5 */
	int ecs72;    /*  scaler sector 5 */
	int ecs73;    /*  scaler sector 5 */
	int ecs74;    /*  scaler sector 5 */
	int ecs75;    /*  scaler sector 5 */
	int ecs76;    /*  scaler sector 5 */
	int ecs77;    /*  scaler sector 5 */
	int ecs78;    /*  scaler sector 5 */
	int ecs79;    /*  scaler sector 5 */
	int ecs80;    /*  scaler sector 5 */
	int ecs81;    /*  scaler sector 6 */
	int ecs82;    /*  scaler sector 6 */
	int ecs83;    /*  scaler sector 6 */
	int ecs84;    /*  scaler sector 6 */
	int ecs85;    /*  scaler sector 6 */
	int ecs86;    /*  scaler sector 6 */
	int ecs87;    /*  scaler sector 6 */
	int ecs88;    /*  scaler sector 6 */
	int ecs89;    /*  scaler sector 6 */
	int ecs90;    /*  scaler sector 6 */
	int ecs91;    /*  scaler sector 6 */
	int ecs92;    /*  scaler sector 6 */
	int ecs93;    /*  scaler sector 6 */
	int ecs94;    /*  scaler sector 6 */
	int ecs95;    /*  scaler sector 6 */
	int ecs96;    /*  scaler sector 6 */
} ecs_t;

typedef struct {
	bankHeader_t bank;
	ecs_t ecs[1];
} clasECS_t;

/* --------------------- END ecs -----------------*/

/* ------------------------ eid0 -----------------*/
typedef struct {
	int jeid0sec;    /*   Sector # for electron candidate */
	int jeid0cc;    /*   Pointer to the hit in CCRC bank */
	int jeid0ec;    /*   Pointer to the hit in ECHB bank */
	int jeid0sc;    /*   Pointer to the hit in SCRX bank */
} eid0_t;

typedef struct {
	bankHeader_t bank;
	eid0_t eid0[1];
} clasEID0_t;

/* --------------------- END eid0 -----------------*/

/* ------------------------ epic -----------------*/
typedef struct {
	float value;    /*  value of the epics channel */
	char char1;    /*  char[32] name; */
	char char2;    /*   */
	char char3;    /*   */
	char char4;    /*   */
	char char5;    /*   */
	char char6;    /*   */
	char char7;    /*   */
	char char8;    /*   */
} epic_t;

typedef struct {
	bankHeader_t bank;
	epic_t epic[1];
} clasEPIC_t;

/* --------------------- END epic -----------------*/

/* ------------------------ evnt -----------------*/
typedef struct {
	int id;    /*  Particle Data Group ID (from SEB)  */
	float pmom;    /*  momentum (from tracking) */
	float mass;    /*  mass squared (from SEB =p**2(1.-betta**2)/beta**2)  */
	int charge;    /*  charge (from tracking) */
	float betta;    /*  Particle velocity in the units of c (=R_trk/TOF/c) */
	float cx;    /*  x dir cosine at track origin  */
	float cy;    /*  y dir cosine at track origin */
	float cz;    /*  z dir cosine at track origin */
	float x;    /*  X coordinate of vertex (cm) */
	float y;    /*  Y coordinate of vertex (cm) */
	float z;    /*  Z coordinate of vertex (cm) */
	int dcstat;    /*  Pointer to DCPB bank (=0 if DC is not involved)  */
	int ccstat;    /*  Pointer to CCPB bank (=0 if CC is not involved)  */
	int scstat;    /*  Pointer to SCPB bank (=0 if SC is not involved)   */
	int ecstat;    /*  Pointer to ECPB bank (=0 if EC is not involved)  */
	int lcstat;    /*  Pointer to LCPB bank (=0 if LAC is not involved)  */
	int ststat;    /*  Pointer to STPB bank (=0 if ST is not involved)  */
	int status;    /*  Status word (=0 out of time particle) */
} evnt_t;

typedef struct {
	bankHeader_t bank;
	evnt_t evnt[1];
} clasEVNT_t;

/* --------------------- END evnt -----------------*/

/* ------------------------ fbpm -----------------*/
typedef struct {
	uint16 id;    /*  the address of the hit detector element */
	uint16 tdc;    /*  tdc information (channels) */
	uint16 adc;    /*  adc information (channels) */
} fbpm_t;

typedef struct {
	bankHeader_t bank;
	fbpm_t fbpm[1];
} clasFBPM_t;

/* --------------------- END fbpm -----------------*/

/* ------------------------ g1sl -----------------*/
typedef struct {
	int g1t1;    /*   */
	int g1t2;    /*   */
	int g1t3;    /*   */
	int g1t4;    /*   */
	int g1t5;    /*   */
	int g1t6;    /*   */
	int g1t7;    /*   */
	int g1t8;    /*   */
	int g1t9;    /*   */
	int g1t10;    /*   */
	int g1t11;    /*   */
	int g1t12;    /*   */
	int g1t13;    /*   */
	int g1t14;    /*   */
	int g1t15;    /*   */
	int g1t16;    /*   */
	int g1t17;    /*   */
	int g1t18;    /*   */
	int g1t19;    /*   */
	int g1t20;    /*   */
	int g1t21;    /*   */
	int g1t22;    /*   */
	int g1t23;    /*   */
	int g1t24;    /*   */
	int g1t25;    /*   */
	int g1t26;    /*   */
	int g1t27;    /*   */
	int g1t28;    /*   */
	int g1t29;    /*   */
	int g1t30;    /*   */
	int g1t31;    /*   */
	int g1t32;    /*   */
	int g1t33;    /*   */
	int g1t34;    /*   */
	int g1t35;    /*   */
	int g1t36;    /*   */
	int g1t37;    /*   */
	int g1t38;    /*   */
	int g1t39;    /*   */
	int g1t40;    /*   */
	int g1t41;    /*   */
	int g1t42;    /*   */
	int g1t43;    /*   */
	int g1t44;    /*   */
	int g1t45;    /*   */
	int g1t46;    /*   */
	int g1t47;    /*   */
	int g1t48;    /*   */
	int g1t49;    /*   */
	int g1t50;    /*   */
	int g1t51;    /*   */
	int g1t52;    /*   */
	int g1t53;    /*   */
	int g1t54;    /*   */
	int g1t55;    /*   */
	int g1t56;    /*   */
	int g1t57;    /*   */
	int g1t58;    /*   */
	int g1t59;    /*   */
	int g1t60;    /*   */
	int g1t61;    /*   */
	int g1t62;    /*   */
	int g1t63;    /*   */
	int g1t64;    /*   */
} g1sl_t;

typedef struct {
	bankHeader_t bank;
	g1sl_t g1sl[1];
} clasG1SL_t;

/* --------------------- END g1sl -----------------*/

/* ------------------------ g2sl -----------------*/
typedef struct {
	int g2t1;    /*   */
	int g2t2;    /*   */
	int g2t3;    /*   */
	int g2t4;    /*   */
	int g2t5;    /*   */
	int g2t6;    /*   */
	int g2t7;    /*   */
	int g2t8;    /*   */
	int g2t9;    /*   */
	int g2t10;    /*   */
	int g2t11;    /*   */
	int g2t12;    /*   */
	int g2t13;    /*   */
	int g2t14;    /*   */
	int g2t15;    /*   */
	int g2t16;    /*   */
	int g2t17;    /*   */
	int g2t18;    /*   */
	int g2t19;    /*   */
	int g2t20;    /*   */
	int g2t21;    /*   */
	int g2t22;    /*   */
	int g2t23;    /*   */
	int g2t24;    /*   */
	int g2t25;    /*   */
	int g2t26;    /*   */
	int g2t27;    /*   */
	int g2t28;    /*   */
	int g2t29;    /*   */
	int g2t30;    /*   */
	int g2t31;    /*   */
	int g2t32;    /*   */
	int g2t33;    /*   */
	int g2t34;    /*   */
	int g2t35;    /*   */
	int g2t36;    /*   */
	int g2t37;    /*   */
	int g2t38;    /*   */
	int g2t39;    /*   */
	int g2t40;    /*   */
	int g2t41;    /*   */
	int g2t42;    /*   */
	int g2t43;    /*   */
	int g2t44;    /*   */
	int g2t45;    /*   */
	int g2t46;    /*   */
	int g2t47;    /*   */
	int g2t48;    /*   */
	int g2t49;    /*   */
	int g2t50;    /*   */
	int g2t51;    /*   */
	int g2t52;    /*   */
	int g2t53;    /*   */
	int g2t54;    /*   */
	int g2t55;    /*   */
	int g2t56;    /*   */
	int g2t57;    /*   */
	int g2t58;    /*   */
	int g2t59;    /*   */
	int g2t60;    /*   */
	int g2t61;    /*   */
	int g2t62;    /*   */
	int g2t63;    /*   */
	int g2t64;    /*   */
} g2sl_t;

typedef struct {
	bankHeader_t bank;
	g2sl_t g2sl[1];
} clasG2SL_t;

/* --------------------- END g2sl -----------------*/

/* ------------------------ g3sl -----------------*/
typedef struct {
	int g3t1;    /*   */
	int g3t2;    /*   */
	int g3t3;    /*   */
	int g3t4;    /*   */
	int g3t5;    /*   */
	int g3t6;    /*   */
	int g3t7;    /*   */
	int g3t8;    /*   */
	int g3t9;    /*   */
	int g3t10;    /*   */
	int g3t11;    /*   */
	int g3t12;    /*   */
	int g3t13;    /*   */
	int g3t14;    /*   */
	int g3t15;    /*   */
	int g3t16;    /*   */
	int g3t17;    /*   */
	int g3t18;    /*   */
	int g3t19;    /*   */
	int g3t20;    /*   */
	int g3t21;    /*   */
	int g3t22;    /*   */
	int g3t23;    /*   */
	int g3t24;    /*   */
	int g3t25;    /*   */
	int g3t26;    /*   */
	int g3t27;    /*   */
	int g3t28;    /*   */
	int g3t29;    /*   */
	int g3t30;    /*   */
	int g3t31;    /*   */
	int g3t32;    /*   */
	int g3t33;    /*   */
	int g3t34;    /*   */
	int g3t35;    /*   */
	int g3t36;    /*   */
	int g3t37;    /*   */
	int g3t38;    /*   */
	int g3t39;    /*   */
	int g3t40;    /*   */
	int g3t41;    /*   */
	int g3t42;    /*   */
	int g3t43;    /*   */
	int g3t44;    /*   */
	int g3t45;    /*   */
	int g3t46;    /*   */
	int g3t47;    /*   */
	int g3t48;    /*   */
	int g3t49;    /*   */
	int g3t50;    /*   */
	int g3t51;    /*   */
	int g3t52;    /*   */
	int g3t53;    /*   */
	int g3t54;    /*   */
	int g3t55;    /*   */
	int g3t56;    /*   */
	int g3t57;    /*   */
	int g3t58;    /*   */
	int g3t59;    /*   */
	int g3t60;    /*   */
	int g3t61;    /*   */
	int g3t62;    /*   */
	int g3t63;    /*   */
	int g3t64;    /*   */
} g3sl_t;

typedef struct {
	bankHeader_t bank;
	g3sl_t g3sl[1];
} clasG3SL_t;

/* --------------------- END g3sl -----------------*/

/* ------------------------ g4sl -----------------*/
typedef struct {
	int g4t1;    /*   */
	int g4t2;    /*   */
	int g4t3;    /*   */
	int g4t4;    /*   */
	int g4t5;    /*   */
	int g4t6;    /*   */
	int g4t7;    /*   */
	int g4t8;    /*   */
	int g4t9;    /*   */
	int g4t10;    /*   */
	int g4t11;    /*   */
	int g4t12;    /*   */
	int g4t13;    /*   */
	int g4t14;    /*   */
	int g4t15;    /*   */
	int g4t16;    /*   */
	int g4t17;    /*   */
	int g4t18;    /*   */
	int g4t19;    /*   */
	int g4t20;    /*   */
	int g4t21;    /*   */
	int g4t22;    /*   */
	int g4t23;    /*   */
	int g4t24;    /*   */
	int g4t25;    /*   */
	int g4t26;    /*   */
	int g4t27;    /*   */
	int g4t28;    /*   */
	int g4t29;    /*   */
	int g4t30;    /*   */
	int g4t31;    /*   */
	int g4t32;    /*   */
	int g4t33;    /*   */
	int g4t34;    /*   */
	int g4t35;    /*   */
	int g4t36;    /*   */
	int g4t37;    /*   */
	int g4t38;    /*   */
	int g4t39;    /*   */
	int g4t40;    /*   */
	int g4t41;    /*   */
	int g4t42;    /*   */
	int g4t43;    /*   */
	int g4t44;    /*   */
	int g4t45;    /*   */
	int g4t46;    /*   */
	int g4t47;    /*   */
	int g4t48;    /*   */
	int g4t49;    /*   */
	int g4t50;    /*   */
	int g4t51;    /*   */
	int g4t52;    /*   */
	int g4t53;    /*   */
	int g4t54;    /*   */
	int g4t55;    /*   */
	int g4t56;    /*   */
	int g4t57;    /*   */
	int g4t58;    /*   */
	int g4t59;    /*   */
	int g4t60;    /*   */
	int g4t61;    /*   */
	int g4t62;    /*   */
	int g4t63;    /*   */
	int g4t64;    /*   */
} g4sl_t;

typedef struct {
	bankHeader_t bank;
	g4sl_t g4sl[1];
} clasG4SL_t;

/* --------------------- END g4sl -----------------*/

/* ------------------------ gpar -----------------*/
typedef struct {
	float fval;    /*  float value */
	int ival;    /*  int value  */
	char char1;    /*  char[20] name; */
	char char2;    /*   */
	char char3;    /*   */
	char char4;    /*   */
	char char5;    /*   */
} gpar_t;

typedef struct {
	bankHeader_t bank;
	gpar_t gpar[1];
} clasGPAR_t;

/* --------------------- END gpar -----------------*/

/* ------------------------ gpid -----------------*/
typedef struct {
	int pid;    /*  particle id (GEANT) */
	float x;    /*  vector3_t vert; Vertex position {x,y,z}  */
	float y;    /*  y */
	float z;    /*  z */
	float e;    /*  vector4_t p; Energy */
	float px;    /*  momentum {x,y,z} */
	float py;    /*   py */
	float pz;    /*  pz */
	int q;    /*  charge */
	int trkid;    /*  index to TBID bank, counting from 1 */
	int sec;    /*  Sector track is in */
	int paddle;    /*  Paddle hit */
	float dedx;    /*  Energy deposited in TOF */
	float beta;    /*  beta pmag/E */
	int sc_stat;    /*  status of hit matching to SC: see sc.h */
	float sc_time;    /*  SC calibrated time for this track (ns) */
	float sc_len;    /*  track length [cm] from origin to SC */
	int st_stat;    /*  ST status */
	float st_time;    /*  ST calibrated time for this track (ns)  */
	float st_len;    /*  track length [cm] from origin to ST */
	float mass;    /*  mass from time-of-flight */
	int mass_ref;    /*  0: mass calc from SC & TAG, 1: SC & ST; -1 neutral or no SC;2:from PART */
	float betam;    /*  beta from time_of-flight */
	float epho;    /*  closest photon energy (GeV) */
	float tpho;    /*  Time of the photon after RF correction */
	int tagrid;    /*  index to TAGR bank, counting from 1 */
	int ngrf;    /*  number of photons in the same RF bucket */
	int ppid;    /*  pid as seen in PART bank */
} gpid_t;

typedef struct {
	bankHeader_t bank;
	gpid_t gpid[1];
} clasGPID_t;

/* --------------------- END gpid -----------------*/

/* ------------------------ gp_x -----------------*/
typedef struct {
	float value;    /*  count rate per channel */
} gp_x_t;

typedef struct {
	bankHeader_t bank;
	gp_x_t gp_x[1];
} clasGP_X_t;

/* --------------------- END gp_x -----------------*/

/* ------------------------ gp_y -----------------*/
typedef struct {
	float value;    /*  count rate per channel */
} gp_y_t;

typedef struct {
	bankHeader_t bank;
	gp_y_t gp_y[1];
} clasGP_Y_t;

/* --------------------- END gp_y -----------------*/

/* ------------------------ hber -----------------*/
typedef struct {
	float q_over_p;    /*  q/p */
	float lambda;    /*  dip angle (pi/2 - theta) */
	float phi;    /*  phi */
	float d0;    /*  min.distance from (x=0,y=0,z=?)   [cm] */
	float z0;    /*  z position of starting point  [cm] */
	float c11;    /*  element C{1,1} */
	float c12;    /*  element C{1,2} */
	float c13;    /*  element C{1,3} */
	float c14;    /*  element C{1,4} */
	float c15;    /*  element C{1,5} */
	float c22;    /*  element C{2,2} */
	float c23;    /*  element C{2,3} */
	float c24;    /*  element C{2,4} */
	float c25;    /*  element C{2,5} */
	float c33;    /*  element C{3,3} */
	float c34;    /*  element C{3,4} */
	float c35;    /*  element C{3,5} */
	float c44;    /*  element C{4,4} */
	float c45;    /*  element C{4,5} */
	float c55;    /*  element C{5,5} */
	float chi2;    /*  Chisquare for this Track */
	int layinfo1;    /*  layerhit info */
	int layinfo2;    /*  layerhit info&sector&track#in sector */
} hber_t;

typedef struct {
	bankHeader_t bank;
	hber_t hber[1];
} clasHBER_t;

/* --------------------- END hber -----------------*/

/* ------------------------ hbid -----------------*/
typedef struct {
	int track;    /*  Track Candidate (ptr to HBTR) */
	int sec;    /*  Sector track is in */
	float beta;    /*  Beta of the track in units of c */
	float vtime;    /*  vertex time of track */
	int sc_stat;    /*  status of hit matching to SC: see sc.h */
	int sc_id;    /*  Pointer to SCRC bank */
	float sc_time;    /*  SC calibrated time for this track (ns) */
	float sc_qual;    /*  quality of match for SC */
	float sc_vtime;    /*  time at vertex for SC(ns) */
	float sc_beta;    /*  Beta calculated from TOF from SC */
	int cc_stat;    /*  status of hit matching to CC: see sc.h */
	int cc_id;    /*  pointer to CC01 bank */
	float cc_time;    /*  CC calibrated time for this track (ns) */
	float cc_qual;    /*  quality of match for CC */
	float cc_vtime;    /*  time at vertex for CC(ns) */
	float cc_beta;    /*  Beta as calculated by the EC */
	int ec_stat;    /*  status of hit matching to ec: see sc.h */
	int ec_id;    /*  Pointer to ECHB bank */
	float ec_time;    /*  EC calibrated time for this track (ns) */
	float ec_qual;    /*  EC quality factor */
	float ec_vtime;    /*  time at vertex for EC(ns) */
	float ec_beta;    /*  Beta as calculated by the EC */
	int st_stat;    /*  status of hit matching to ST */
	int st_id;    /*  Pointer to STR bank */
	float st_time;    /*  ST calibrated time for this track (ns) */
	float st_qual;    /*  ST quality factor */
	float st_vtime;    /*  time at vertex for ST(ns) */
	float st_beta;    /*  Beta as calculated by the ST */
	int lac_stat;    /*  status of hit matching to LAC */
	int lac_id;    /*  Pointer to EC1R bank */
	float lac_time;    /*  LAC calibrated time for this track (ns) */
	float lac_qual;    /*  LAC quality factor */
	float lac_vtime;    /*  time at vertex for LAC(ns) */
	float lac_beta;    /*  Beta as calculated by the LAC */
} hbid_t;

typedef struct {
	bankHeader_t bank;
	hbid_t hbid[1];
} clasHBID_t;

/* --------------------- END hbid -----------------*/

/* ------------------------ hbla -----------------*/
typedef struct {
	int trk_pln;    /*  (track_number) *100 + Trk_plane_number */
	float x;    /*  z coord [cm]  for track in this plane */
	float y;    /*  y coord [cm]  for track in this plane */
	float z;    /*  z coord [cm]  for track in this plane */
	float bx;    /*  B-field in x [kG] at coord.{x,y,z} */
	float by;    /*  B-field in y [kG] at coord.{x,y,z} */
	float bz;    /*  B-field in z [kG] at coord.{x,y,z} */
	float tlen;    /*  track length [cm] from origin to this plane */
	int dc1;    /*  Pointer to DC1 bank */
	int stat;    /*  Status of the hit */
	int wire;    /*  Wire number  */
	float dtime;    /*  drift time [ns]  (not corrected for start ime) */
	float alpha;    /*  track angle (relative to R of SL) [deg] */
	float wlen;    /*  Wire length (hit pos. to preamp)  [cm] */
	float sgdoca;    /*  sigma DOCA */
	float fitdoca;    /*  Fitted DOCA */
} hbla_t;

typedef struct {
	bankHeader_t bank;
	hbla_t hbla[1];
} clasHBLA_t;

/* --------------------- END hbla -----------------*/

/* ------------------------ hbtb -----------------*/
typedef struct {
	int trk;    /*  Track Candidate  */
	int sly;    /*  Superlayer */
	int icl;    /*  Cluster# that matched to the track */
	int isg;    /*  Segment# in this cluster */
	int id1;    /*  Ptr. to DC1 entry  for 1.Layer in SL */
	int id2;    /*  Ptr. to DC1 entry  for 2.Layer in SL */
	int id3;    /*  Ptr. to DC1 entry  for 3.Layer in SL */
	int id4;    /*  Ptr. to DC1 entry  for 4.Layer in SL */
	int id5;    /*  Ptr. to DC1 entry  for 5.Layer in SL */
	int id6;    /*  Ptr. to DC1 entry  for 6.Layer in SL */
} hbtb_t;

typedef struct {
	bankHeader_t bank;
	hbtb_t hbtb[1];
} clasHBTB_t;

/* --------------------- END hbtb -----------------*/

/* ------------------------ hbtr -----------------*/
typedef struct {
	float x;    /*  x */
	float y;    /*  y 'Vertex' position {x,y,z} */
	float z;    /*  z */
	float px;    /*  Px */
	float py;    /*  Py  momentum at 'vertex' {Px,Py,Pz} */
	float pz;    /*  Pz */
	float q;    /*  charge   (straight tracks: set q=0.) */
	float chi2;    /*  Chisquare for this Track */
	int itr_sec;    /*  Trackno_in_Sector + Sector*100 */
} hbtr_t;

typedef struct {
	bankHeader_t bank;
	hbtr_t hbtr[1];
} clasHBTR_t;

/* --------------------- END hbtr -----------------*/

/* ------------------------ hcal -----------------*/
typedef struct {
	int version;    /*  Version number */
	int run_low;    /*  LOW RUN number		  */
	int run_high;    /*  HIGH RUN number */
	int catime;    /*  CAlibration TIME (unix time = 0 1/1/1970)		 */
	int rocca;    /*  32 bit ReadOut Controller CAlibration status */
} hcal_t;

typedef struct {
	bankHeader_t bank;
	hcal_t hcal[1];
} clasHCAL_t;

/* --------------------- END hcal -----------------*/

/* ------------------------ hdpl -----------------*/
typedef struct {
	int trk_pln;    /*  (track_number) *100 + plane_number */
	float x;    /*  vector3_t pos; */
	float y;    /*   (x, y, z coord. for track in on the plane) */
	float z;    /*   */
	float cx;    /*  vector3_t dir; */
	float cy;    /*   direction cosines (x,y,z) for track at coord.{x,y,z} */
	float cz;    /*   */
	float tlen;    /*  track length [cm] from origin to this plane */
} hdpl_t;

typedef struct {
	bankHeader_t bank;
	hdpl_t hdpl[1];
} clasHDPL_t;

/* --------------------- END hdpl -----------------*/

/* ------------------------ head -----------------*/
typedef struct {
	int version;    /*  Version Number */
	int nrun;    /*  Run Number (monotonically increasing) */
	int nevent;    /*  Event Number (starting with 1 at run begin, */
	int time;    /*  Event Time (UNIX time = seconds as of January 1,1970) */
	int type;    /*  Event Type (Defined by on-line system or MC run: */

	int roc;    /*             = 0        - sync status is OK */
	int evtclass;    /*  Event Classification from DAQ: */







	int trigbits;    /*  Level 1 Trigger Latch Word (16 bits) */

} head_t;

typedef struct {
	bankHeader_t bank;
	head_t head[1];
} clasHEAD_t;

/* --------------------- END head -----------------*/

/* ------------------------ hevt -----------------*/
typedef struct {
	int estatus;    /*  Event Statuse after reconstruction */
	int nrun;    /*  Run Number (monotonically increasing) */
	int nevent;    /*  Event Number in the run NRUN  */
	int type;    /*  Event Type (Data or MC) */
	int npgp;    /*  Number of final reconstructed particles*100 + */
	int trgprs;    /*  Trigger type*10000 +  */
	float fc;    /*  Faraday Cup (K) */
	float fcg;    /*  Faraday Cup Gated (K) */
	float tg;    /*  Clock Gated */
	float stt;    /*  Event Start Time  */
	float rf1;    /*  RF Time  */
	float rf2;    /*  RF Time */
	float con1;    /*  Control Rates  */
	float con2;    /*  Control Rates   */
	float con3;    /*  Control Rates  */
	int ptime;    /*  Event Processing Time (UNIX time = seconds) */
} hevt_t;

typedef struct {
	bankHeader_t bank;
	hevt_t hevt[1];
} clasHEVT_t;

/* --------------------- END hevt -----------------*/

/* ------------------------ hls -----------------*/
typedef struct {
	uint32 s1;    /*  10MHz clock */
	uint32 s2;    /*  OTR-1 */
	uint32 s3;    /*  OTR-2 */
	uint32 s4;    /*  SLM */
	uint32 s5;    /*  lvl-1 trigger rate */
	uint32 s6;    /*  L.R Moller coincidences */
	uint32 s7;    /*  L.R Moller accidentals */
	uint32 s8;    /*  F-CUP */
	uint32 s9;    /*  pmt-1 */
	uint32 s10;    /*  pmt-2 */
	uint32 s11;    /*  pmt-3 */
	uint32 s12;    /*  pmt-4 */
	uint32 s13;    /*  reserve */
	uint32 s14;    /*  reserve */
	uint32 s15;    /*  Helicity states accumulating counter */
	uint32 s16;    /*  HLS banks accumulating counter */
} hls_t;

typedef struct {
	bankHeader_t bank;
	hls_t hls[1];
} clasHLS_t;

/* --------------------- END hls -----------------*/

/* ------------------------ ic -----------------*/
typedef struct {
	uint16 id;    /*  the address of the hit detector element */
	uint16 tdc;    /*  tdc information (channels) */
	uint16 adc;    /*  adc information (channels) */
} ic_t;

typedef struct {
	bankHeader_t bank;
	ic_t ic[1];
} clasIC_t;

/* --------------------- END ic -----------------*/

/* ------------------------ ichb -----------------*/
typedef struct {
	float eclust;    /*  Cluster energy  */
	float eclmax;    /*  Max energy in a crystal of the cluster  */
	float tclust;    /*  Reconstructed cluster time */
	float tclmax;    /*  Time in a crystal with Max enrgy */
	float xclust;    /*  lab coordinate X, */
	float yclust;    /*  lab coordinate Y, */
	float zclust;    /*  lab coordinate Z, */
	float xclmax;    /*  lab coordinate X, */
	float yclmax;    /*  lab coordinate Y, */
	float zclmax;    /*  lab coordinate Z, */
	float res1;    /*  lab coordinate error, */
	float res2;    /*  lab coordinate error, */
	float res3;    /*  lab coordinate error, */
	int ncryst;    /*  Number of crystal in the cluster */
} ichb_t;

typedef struct {
	bankHeader_t bank;
	ichb_t ichb[1];
} clasICHB_t;

/* --------------------- END ichb -----------------*/

/* ------------------------ kfit -----------------*/
typedef struct {
	float momenta_f;    /*  fitted momentum parameter (GeV)    */
	float lambda_f;    /*  fitted dip angle (radians)  */
	float phi_f;    /*  fitted phi angle (radians) */
	float d0_f;    /*  fitted d0 (cm) */
	float z0_f;    /*  fitted z0 (cm) */
	float sigma11;    /*    error element of known */
	float sigma12;    /*    error element of known */
	float sigma13;    /*    error element of known */
	float sigma14;    /*    error element of known */
	float sigma15;    /*    error element of known */
	float sigma22;    /*    error element of known    */
	float sigma23;    /*    error element of known */
	float sigma24;    /*    error element of known */
	float sigma25;    /*    error element of known */
	float sigma33;    /*    error element of known  */
	float sigma34;    /*    error element of known  */
	float sigma35;    /*    error element of known  */
	float sigma44;    /*    error element of known  */
	float sigma45;    /*    error element of known  */
	float sigma55;    /*    error element of known  */
	float cov11;    /*    error element bewteen known and unknown */
	float cov12;    /*    error element bewteen known and unknown */
	float cov13;    /*    error element bewteen known and unknown */
	float cov14;    /*    error element bewteen known and unknown */
	float cov15;    /*    error element bewteen known and unknown */
	float cov21;    /*    error element bewteen known and unknown */
	float cov22;    /*    error element bewteen known and unknown */
	float cov23;    /*    error element bewteen known and unknown */
	float cov24;    /*    error element bewteen known and unknown */
	float cov25;    /*    error element bewteen known and unknown */
	float cov31;    /*    error element bewteen known and unknown */
	float cov32;    /*    error element bewteen known and unknown */
	float cov33;    /*    error element bewteen known and unknown */
	float cov34;    /*    error element bewteen known and unknown */
	float cov35;    /*    error element bewteen known and unknown */
	float cov41;    /*    error element bewteen known and unknown */
	float cov42;    /*    error element bewteen known and unknown */
	float cov43;    /*    error element bewteen known and unknown */
	float cov44;    /*    error element bewteen known and unknown */
	float cov45;    /*    error element bewteen known and unknown */
	float cov51;    /*    error element bewteen known and unknown */
	float cov52;    /*    error element bewteen known and unknown */
	float cov53;    /*    error element bewteen known and unknown */
	float cov54;    /*    error element bewteen known and unknown */
	float cov55;    /*    error element bewteen known and unknown */
	float chi2piece;    /*  track contribution to chi2 */
} kfit_t;

typedef struct {
	bankHeader_t bank;
	kfit_t kfit[1];
} clasKFIT_t;

/* --------------------- END kfit -----------------*/

/* ------------------------ l1pg -----------------*/
typedef struct {
	char l1prog;    /*  Level 1 program */
} l1pg_t;

typedef struct {
	bankHeader_t bank;
	l1pg_t l1pg[1];
} clasL1PG_t;

/* --------------------- END l1pg -----------------*/

/* ------------------------ l2h -----------------*/
typedef struct {
	uint16 id;    /*  Superlayer + 256*(hit# in TDC) */
	uint16 tdc;    /*  tdc information (channels) */
} l2h_t;

typedef struct {
	bankHeader_t bank;
	l2h_t l2h[1];
} clasL2H_t;

/* --------------------- END l2h -----------------*/

/* ------------------------ l2s -----------------*/
typedef struct {
	uint16 id;    /*  ID+256*hit# */
	uint16 tdc;    /*  TDC value */
} l2s_t;

typedef struct {
	bankHeader_t bank;
	l2s_t l2s[1];
} clasL2S_t;

/* --------------------- END l2s -----------------*/

/* ------------------------ lasr -----------------*/
typedef struct {
	int id;    /*  identifier  0-1099  TOF 1100-10000 EC laser   */
	int stat1;    /*  status words different for each id */
	int stat2;    /*   " */
	int stat3;    /*   " */
} lasr_t;

typedef struct {
	bankHeader_t bank;
	lasr_t lasr[1];
} clasLASR_t;

/* --------------------- END lasr -----------------*/

/* ------------------------ lcdi -----------------*/
typedef struct {
	int crate;    /*  CAMAC crate number */
	int slot;    /*  slot */
	int mask;    /*  mask */
	int threshold;    /*  actual threshold value (mV) */
} lcdi_t;

typedef struct {
	bankHeader_t bank;
	lcdi_t lcdi[1];
} clasLCDI_t;

/* --------------------- END lcdi -----------------*/

/* ------------------------ lcpb -----------------*/
typedef struct {
	int scht;    /*  100*sector+Hit_ID in EC1R */
	float etot;    /*  Reconstructed total energy */
	float time;    /*  Flight time relative to the evnt start time */
	float path;    /*  Path lenght from target */
	float x;    /*  x coordinate of the hit */
	float y;    /*  y coordinate of the hit */
	float z;    /*  z coordinate of the hit */
	float chi2lc;    /*  Quality measure of geometrical matching */
	int status;    /*  Status word */
	float ein;    /*  Reconstructed energy in the inner part */
} lcpb_t;

typedef struct {
	bankHeader_t bank;
	lcpb_t lcpb[1];
} clasLCPB_t;

/* --------------------- END lcpb -----------------*/

/* ------------------------ lowq -----------------*/
typedef struct {
	int pid;    /*  particle id (GEANT) */
	float x;    /*  vector3_t vert; Vertex position {x,y,z} */
	float y;    /*  y */
	float z;    /*  z */
	float e;    /*  vector4_t p; Energy */
	float px;    /*  momentum {x,y,z} */
	float py;    /*   py */
	float pz;    /*  pz */
	float q;    /*  charge */
	float qpid;    /*  quality factor for the pid */
	float qtrk;    /*  quality factor for the trk */
	int flags;    /*  set of flags defining track (ie, BEAM) */
} lowq_t;

typedef struct {
	bankHeader_t bank;
	lowq_t lowq[1];
} clasLOWQ_t;

/* --------------------- END lowq -----------------*/

/* ------------------------ mcev -----------------*/
typedef struct {
	int i1;    /*  first geant random number seed for event */
	int i2;    /*  second seed */
} mcev_t;

typedef struct {
	bankHeader_t bank;
	mcev_t mcev[1];
} clasMCEV_t;

/* --------------------- END mcev -----------------*/

/* ------------------------ mchd -----------------*/
typedef struct {
	int nrun;    /*  Run Number */
	int nevent;    /*  Event Number */
	int time;    /*  Event Time (UNIX time) */
	int type;    /*  Event Type (MUST be negative) */

	int reactype;    /*  optional: user defined reaction type */
	float weight;    /*  event weight */
	float w;    /*  center_of_mass energy (inv.mass of hadronic states) */
	float q2;    /*  Q2 (photon virtuallity) */
	float e_phot;    /*  energy of (real/virtual) photon */
	float px_phot;    /*  p_x: momentum (in x) of (real/virtual) photon */
	float py_phot;    /*  p_y: momentum (in y) of (real/virtual) photon */
	float pz_phot;    /*  p_z: momentum (in z) of (real/virtual) photon */
	float e_targ;    /*  energy of target particle */
	float px_targ;    /*  p_x: momentum (in x) of target part. */
	float py_targ;    /*  p_y: momentum (in y) of target part. */
	float pz_targ;    /*  p_z: momentum (in z) of target part. */
} mchd_t;

typedef struct {
	bankHeader_t bank;
	mchd_t mchd[1];
} clasMCHD_t;

/* --------------------- END mchd -----------------*/

/* ------------------------ mctk -----------------*/
typedef struct {
	float cx;    /*  x dir cosine at track origin */
	float cy;    /*  y dir cosine */
	float cz;    /*  z dir cosine */
	float pmom;    /*  momentum */
	float mass;    /*  mass */
	float charge;    /*  charge */
	int id;    /*  track Particle Data Group id */
	int flag;    /*  track flag */
	int beg_vtx;    /*  beginning vertex number  */
	int end_vtx;    /*  ending vertex number */
	int parent;    /*  parent track */
} mctk_t;

typedef struct {
	bankHeader_t bank;
	mctk_t mctk[1];
} clasMCTK_t;

/* --------------------- END mctk -----------------*/

/* ------------------------ mcvx -----------------*/
typedef struct {
	float x;    /*  x of vertex */
	float y;    /*  y */
	float z;    /*  z */
	float tof;    /*  secs of flight */
	int flag;    /*  vertex flag */
} mcvx_t;

typedef struct {
	bankHeader_t bank;
	mcvx_t mcvx[1];
} clasMCVX_t;

/* --------------------- END mcvx -----------------*/

/* ------------------------ mtrk -----------------*/
typedef struct {
	int sect;    /*  Sector Number for track */
	int trkl1;    /*  track segment and track cluster for Superlayer 1 (lnk_t) */
	int trkl2;    /*  track segment and track cluster for Superlayer 2 */
	int trkl3;    /*  track segment and track cluster for Superlayer 3 */
	int trkl4;    /*  track segment and track cluster for Superlayer 4 */
	int trkl5;    /*  track segment and track cluster for Superlayer 5 */
	int trkl6;    /*  track segment and track cluster for Superlayer 6 */
} mtrk_t;

typedef struct {
	bankHeader_t bank;
	mtrk_t mtrk[1];
} clasMTRK_t;

/* --------------------- END mtrk -----------------*/

/* ------------------------ mvrt -----------------*/
typedef struct {
	int v_id;    /*  info about track ids */
	float ntrk;    /*  number of tracks used to make vertex */
	float x;    /*  x vector3_t vert{x,y,z} */
	float y;    /*  y  */
	float z;    /*  z */
	float chi2;    /*  chi2 */
	float cxx;    /*  Covariance matrix array element */
	float cxy;    /*  Covariance matrix array element */
	float cxz;    /*  Covariance matrix array element */
	float cyy;    /*  Covariance matrix array element */
	float cyz;    /*  Covariance matrix array element */
	float czz;    /*  Covariance matrix array element */
	int stat;    /*  status integer, not used yet */
} mvrt_t;

typedef struct {
	bankHeader_t bank;
	mvrt_t mvrt[1];
} clasMVRT_t;

/* --------------------- END mvrt -----------------*/

/* ------------------------ part -----------------*/
typedef struct {
	int pid;    /*  particle id (GEANT) */
	float x;    /*  vector3_t vert; Vertex position {x,y,z}  */
	float y;    /*  y */
	float z;    /*  z */
	float e;    /*  vector4_t p; Energy */
	float px;    /*  momentum {x,y,z} */
	float py;    /*   py */
	float pz;    /*  pz */
	float q;    /*  charge */
	int trkid;    /*  index to TBID bank, counting from 1 */
	float qpid;    /*  quality factor for the pid */
	float qtrk;    /*  quality factor for the trk */
	int flags;    /*  set of flags defining track (ie, BEAM) */
} part_t;

typedef struct {
	bankHeader_t bank;
	part_t part[1];
} clasPART_t;

/* --------------------- END part -----------------*/

/* ------------------------ pco -----------------*/
typedef struct {
	float time;    /*   TDC time  */
	float elt;    /*  energy deposit PC = left top */
	float erb;    /*  energy deposit PC = right bottom */
	float elb;    /*  energy deposit PC = left bottom  */
	float ert;    /*  energy deposit PC = right top */
	float emain;    /*  energy deposit PC = MAIN */
	float eveto;    /*  energy deposit PC = veto */
	int tid;    /*  T id of the corresponding T */
} pco_t;

typedef struct {
	bankHeader_t bank;
	pco_t pco[1];
} clasPCO_t;

/* --------------------- END pco -----------------*/

/* ------------------------ phtm -----------------*/
typedef struct {
	uint32 evid;    /*  Event ID (number of triggers) */
	uint32 nsprot;    /*  Number of single proton events */
	uint32 npip;    /*  Number of single pi+ events */
	uint32 npim;    /*  Number of single pi- events */
	uint32 nppippim;    /*  Number of single proton pi+ pi- events */
	uint32 nppip;    /*  Number of single proton pi+ events */
	uint32 npippim;    /*  Number of single pi+ pi- events */
	uint32 nppim;    /*  Number of single proton pi- events */
	uint32 nkp;    /*  Number of single K+ events */
	uint32 npkp;    /*  Number of single proton K+ events */
	float tag_pi_v;    /*  Mean of (tagger_time - pion_vertex_time) */
} phtm_t;

typedef struct {
	bankHeader_t bank;
	phtm_t phtm[1];
} clasPHTM_t;

/* --------------------- END phtm -----------------*/

/* ------------------------ pid1 -----------------*/
typedef struct {
	uint32 nhbpos_1;    /*  Number of pos Hit-Based tracks for sec=1 */
	uint32 nhbneg_1;    /*  Number of neg Hit-Based tracks for sec=1 */
	uint32 ntbpos_1;    /*  Number of pos Time-Based tracks  for sec=1 */
	uint32 ntbneg_1;    /*  Number of neg Time-Based tracks for sec=1 */
	float chi2pos_1;    /*  Chi^2 for positive tracks for sec=1 */
	float chi2neg_1;    /*  Chi^2 for negative tracks for sec=1 */
	uint32 nunknown_1;    /*  Number of unknown particles for sec=1 */
	uint32 ngamma_1;    /*  Number of photons for sec=1 */
	uint32 nelec_1;    /*  Number of electrons for sec=1 */
	uint32 npiplus_1;    /*  Number of pi+ for sec=1 */
	uint32 npiminus_1;    /*  Number of pi- for sec=1 */
	uint32 nprot_1;    /*  Number of proton for sec=1 */
	uint32 nkplus_1;    /*  Number of K+ for sec=1 */
	uint32 nkminus_1;    /*  Number of K- for sec=1 */
	uint32 ndeuteron_1;    /*  Number of deuterons for sec=1 */
	uint32 nneutron_1;    /*  Number of neutrons for sec=1  */
} pid1_t;

typedef struct {
	bankHeader_t bank;
	pid1_t pid1[1];
} clasPID1_t;

/* --------------------- END pid1 -----------------*/

/* ------------------------ pid2 -----------------*/
typedef struct {
	uint32 nhbpos_2;    /*  Number of pos Hit-Based tracks for sec=2 */
	uint32 nhbneg_2;    /*  Number of neg Hit-Based tracks for sec=2 */
	uint32 ntbpos_2;    /*  Number of pos Time-Based tracks  for sec=2 */
	uint32 ntbneg_2;    /*  Number of neg Time-Based tracks for sec=2 */
	float chi2pos_2;    /*  Chi^2 for positive tracks for sec=2 */
	float chi2neg_2;    /*  Chi^2 for negative tracks for sec=2 */
	uint32 nunknown_2;    /*  Number of unknown particles for sec=2 */
	uint32 ngamma_2;    /*  Number of photons for sec=2 */
	uint32 nelec_2;    /*  Number of electrons for sec=2 */
	uint32 npiplus_2;    /*  Number of pi+ for sec=2 */
	uint32 npiminus_2;    /*  Number of pi- for sec=2 */
	uint32 nprot_2;    /*  Number of proton for sec=2 */
	uint32 nkplus_2;    /*  Number of K+ for sec=2 */
	uint32 nkminus_2;    /*  Number of K- for sec=2 */
	uint32 ndeuteron_2;    /*  Number of deuterons for sec=2 */
	uint32 nneutron_2;    /*  Number of neutrons for sec=2 */
} pid2_t;

typedef struct {
	bankHeader_t bank;
	pid2_t pid2[1];
} clasPID2_t;

/* --------------------- END pid2 -----------------*/

/* ------------------------ pid3 -----------------*/
typedef struct {
	uint32 nhbpos_3;    /*  Number of pos Hit-Based tracks for sec=3 */
	uint32 nhbneg_3;    /*  Number of neg Hit-Based tracks for sec=3 */
	uint32 ntbpos_3;    /*  Number of pos Time-Based tracks  for sec=3 */
	uint32 ntbneg_3;    /*  Number of neg Time-Based tracks for sec=3 */
	float chi2pos_3;    /*  Chi^2 for positive tracks for sec=3 */
	float chi2neg_3;    /*  Chi^2 for negative tracks for sec=3 */
	uint32 nunknown_3;    /*  Number of unknown particles for sec=3 */
	uint32 ngamma_3;    /*  Number of photons for sec=3 */
	uint32 nelec_3;    /*  Number of electrons for sec=3 */
	uint32 npiplus_3;    /*  Number of pi+ for sec=3 */
	uint32 npiminus_3;    /*  Number of pi- for sec=3 */
	uint32 nprot_3;    /*  Number of proton for sec=3 */
	uint32 nkplus_3;    /*  Number of K+ for sec=3 */
	uint32 nkminus_3;    /*  Number of K- for sec=3 */
	uint32 ndeuteron_3;    /*  Number of deuterons for sec=3 */
	uint32 nneutron_3;    /*  Number of neutrons for sec=3 */
} pid3_t;

typedef struct {
	bankHeader_t bank;
	pid3_t pid3[1];
} clasPID3_t;

/* --------------------- END pid3 -----------------*/

/* ------------------------ pid4 -----------------*/
typedef struct {
	uint32 nhbpos_4;    /*  Number of pos Hit-Based tracks for sec=4 */
	uint32 nhbneg_4;    /*  Number of neg Hit-Based tracks for sec=4 */
	uint32 ntbpos_4;    /*  Number of pos Time-Based tracks  for sec=4 */
	uint32 ntbneg_4;    /*  Number of neg Time-Based tracks for sec=4 */
	float chi2pos_4;    /*  Chi^2 for positive tracks for sec=4 */
	float chi2neg_4;    /*  Chi^2 for negative tracks for sec=4 */
	uint32 nunknown_4;    /*  Number of unknown particles for sec=4 */
	uint32 ngamma_4;    /*  Number of photons for sec=4 */
	uint32 nelec_4;    /*  Number of electrons for sec=4 */
	uint32 npiplus_4;    /*  Number of pi+ for sec=4 */
	uint32 npiminus_4;    /*  Number of pi- for sec=4 */
	uint32 nprot_4;    /*  Number of proton for sec=4 */
	uint32 nkplus_4;    /*  Number of K+ for sec=4 */
	uint32 nkminus_4;    /*  Number of K- for sec=4 */
	uint32 ndeuteron_4;    /*  Number of deuterons for sec=4 */
	uint32 nneutron_4;    /*  Number of neutrons for sec=4    */
} pid4_t;

typedef struct {
	bankHeader_t bank;
	pid4_t pid4[1];
} clasPID4_t;

/* --------------------- END pid4 -----------------*/

/* ------------------------ pid5 -----------------*/
typedef struct {
	uint32 nhbpos_5;    /*  Number of pos Hit-Based tracks for sec=5 */
	uint32 nhbneg_5;    /*  Number of neg Hit-Based tracks for sec=5 */
	uint32 ntbpos_5;    /*  Number of pos Time-Based tracks  for sec=5 */
	uint32 ntbneg_5;    /*  Number of neg Time-Based tracks for sec=5 */
	float chi2pos_5;    /*  Chi^2 for positive tracks for sec=5 */
	float chi2neg_5;    /*  Chi^2 for negative tracks for sec=5 */
	uint32 nunknown_5;    /*  Number of unknown particles for sec=5 */
	uint32 ngamma_5;    /*  Number of photons for sec=5 */
	uint32 nelec_5;    /*  Number of electrons for sec=5 */
	uint32 npiplus_5;    /*  Number of pi+ for sec=5 */
	uint32 npiminus_5;    /*  Number of pi- for sec=5 */
	uint32 nprot_5;    /*  Number of proton for sec=5 */
	uint32 nkplus_5;    /*  Number of K+ for sec=5 */
	uint32 nkminus_5;    /*  Number of K- for sec=5 */
	uint32 ndeuteron_5;    /*  Number of deuterons for sec=5 */
	uint32 nneutron_5;    /*  Number of neutrons for sec=5  */
} pid5_t;

typedef struct {
	bankHeader_t bank;
	pid5_t pid5[1];
} clasPID5_t;

/* --------------------- END pid5 -----------------*/

/* ------------------------ pid6 -----------------*/
typedef struct {
	uint32 nhbpos_6;    /*  Number of pos Hit-Based tracks for sec=6 */
	uint32 nhbneg_6;    /*  Number of neg Hit-Based tracks for sec=6 */
	uint32 ntbpos_6;    /*  Number of pos Time-Based tracks  for sec=6 */
	uint32 ntbneg_6;    /*  Number of neg Time-Based tracks for sec=6 */
	float chi2pos_6;    /*  Chi^2 for positive tracks for sec=6 */
	float chi2neg_6;    /*  Chi^2 for negative tracks for sec=6 */
	uint32 nunknown_6;    /*  Number of unknown particles for sec=6 */
	uint32 ngamma_6;    /*  Number of photons for sec=6 */
	uint32 nelec_6;    /*  Number of electrons for sec=6 */
	uint32 npiplus_6;    /*  Number of pi+ for sec=6 */
	uint32 npiminus_6;    /*  Number of pi- for sec=6 */
	uint32 nprot_6;    /*  Number of proton for sec=6 */
	uint32 nkplus_6;    /*  Number of K+ for sec=6 */
	uint32 nkminus_6;    /*  Number of K- for sec=6 */
	uint32 ndeuteron_6;    /*  Number of deuterons for sec=6 */
	uint32 nneutron_6;    /*  Number of neutrons for sec=6  */
} pid6_t;

typedef struct {
	bankHeader_t bank;
	pid6_t pid6[1];
} clasPID6_t;

/* --------------------- END pid6 -----------------*/

/* ------------------------ pidt -----------------*/
typedef struct {
	uint32 nunknown;    /*  Number of particles labelled unknown */
	uint32 ngamma;    /*  Number of photons */
	uint32 nelec;    /*  Number of electrons */
	uint32 npiplus;    /*  Number of pi+  */
	uint32 npiminus;    /*  Number of pi- */
	uint32 nprot;    /*  Number of proton */
	uint32 nkplus;    /*  Number of K+ */
	uint32 nkminus;    /*  Number of K- */
	uint32 ndeuteron;    /*  Number of deuterons */
	uint32 nneutron;    /*  Number of neutrons */
} pidt_t;

typedef struct {
	bankHeader_t bank;
	pidt_t pidt[1];
} clasPIDT_t;

/* --------------------- END pidt -----------------*/

/* ------------------------ prtm -----------------*/
typedef struct {
	int time;    /*  time of pretrig calibration */
} prtm_t;

typedef struct {
	bankHeader_t bank;
	prtm_t prtm[1];
} clasPRTM_t;

/* --------------------- END prtm -----------------*/

/* ------------------------ pso -----------------*/
typedef struct {
	int id;    /*  Counter Id () */
	float time;    /*   TDC time  */
	float ener;    /*  energy deposit */
	int tid;    /*  T id of the corresponding T */
} pso_t;

typedef struct {
	bankHeader_t bank;
	pso_t pso[1];
} clasPSO_t;

/* --------------------- END pso -----------------*/

/* ------------------------ ptdb -----------------*/
typedef struct {
	int pbeam;    /*  beam polarization (0 100) */
	int pb_date;    /*  date of measurement */
	int ttype;    /*  Target Type  */
	int b_targ;    /*  Target holding field (Tesla x100) */
	int ptarg;    /*  Target polarization */
	int pt_time;    /*  Time measured (according to PC) */
	int he_level;    /*  Liquid Helium Level(0 100) */
	int eiof;    /*  EIO microwave tube frequency (MHz) */
	int ttemph;    /*  Target temp measured by He Cell (K x100) */
	int ttempc;    /*  Target temp measured by Cernox (K x100) */
	int anealt;    /*  Date of last target anealing */
} ptdb_t;

typedef struct {
	bankHeader_t bank;
	ptdb_t ptdb[1];
} clasPTDB_t;

/* --------------------- END ptdb -----------------*/

/* ------------------------ rcst -----------------*/
typedef struct {
	int down_count;    /*  download count */
	int prestart_count;    /*  prestart count */
	int go_count;    /*  go count */
	int trig_count;    /*  total trigger count */
	int event_count;    /*  phys event count */
	int sync_count;    /*  force-sync event count */
	int run_trig_count;    /*  trigger count current run */
	int run_event_count;    /*  phys event count current run */
	int run_sync_count;    /*  force-sync event count current run */
	int pause_count;    /*  coda pause count */
	int end_count;    /*  end count */
	int illegal_count;    /*  illegal count */
	int run_illegal_count;    /*  illegal count current run */
	int phys_sync_count;    /*  physics-sync event count */
	int run_phys_sync_count;    /*  physics-sync event count current run */
} rcst_t;

typedef struct {
	bankHeader_t bank;
	rcst_t rcst[1];
} clasRCST_t;

/* --------------------- END rcst -----------------*/

/* ------------------------ rf -----------------*/
typedef struct {
	float rf;    /*  Best RF value in ns */
	float rf1;    /*  RF1 in ns */
	float rf2;    /*  RF2 in ns */
} rf_t;

typedef struct {
	bankHeader_t bank;
	rf_t rf[1];
} clasRF_t;

/* --------------------- END rf -----------------*/

/* ------------------------ rglk -----------------*/
typedef struct {
	int iregion;    /*  region */
	float x;    /*  X pos. of hit in CLAS (cm) */
	float y;    /*  Y pos. of hit in CLAS (cm) */
	float z;    /*  Z pos. of hit in CLAS (cm) */
	float theta0;    /*  polar angle of the link position (deg) */
	float phi0;    /*  azim. angle of the link position (deg) */
	float rtheta;    /*  polar angle of the link direction (deg) */
	float rphi;    /*  azim. angle of the link direction (deg) */
	float chi2;    /*  fit chi2         */
	int status;    /*  MINUIT fit status (from 0=bad to 3=ok) */
} rglk_t;

typedef struct {
	bankHeader_t bank;
	rglk_t rglk[1];
} clasRGLK_t;

/* --------------------- END rglk -----------------*/

/* ------------------------ rnlg -----------------*/
typedef struct {
	char sql;    /*  Run log entry SQL statement */
} rnlg_t;

typedef struct {
	bankHeader_t bank;
	rnlg_t rnlg[1];
} clasRNLG_t;

/* --------------------- END rnlg -----------------*/

/* ------------------------ rnpe -----------------*/
typedef struct {
	int run;    /*  calibration run number */
	char bank;    /*  bank name */
} rnpe_t;

typedef struct {
	bankHeader_t bank;
	rnpe_t rnpe[1];
} clasRNPE_t;

/* --------------------- END rnpe -----------------*/

/* ------------------------ rtsl -----------------*/
typedef struct {
	int rawt1;    /*   */
	int rawt2;    /*   */
	int rawt3;    /*   */
	int rawt4;    /*   */
	int rawt5;    /*   */
	int rawt6;    /*   */
	int rawt7;    /*   */
	int rawt8;    /*   */
	int rawt9;    /*   */
	int rawt10;    /*   */
	int rawt11;    /*   */
	int rawt12;    /*   */
	int rawt13;    /*   */
	int rawt14;    /*   */
	int rawt15;    /*   */
	int rawt16;    /*   */
	int rawt17;    /*   */
	int rawt18;    /*   */
	int rawt19;    /*   */
	int rawt20;    /*   */
	int rawt21;    /*   */
	int rawt22;    /*   */
	int rawt23;    /*   */
	int rawt24;    /*   */
	int rawt25;    /*   */
	int rawt26;    /*   */
	int rawt27;    /*   */
	int rawt28;    /*   */
	int rawt29;    /*   */
	int rawt30;    /*   */
	int rawt31;    /*   */
	int rawt32;    /*   */
	int rawt33;    /*   */
	int rawt34;    /*   */
	int rawt35;    /*   */
	int rawt36;    /*   */
	int rawt37;    /*   */
	int rawt38;    /*   */
	int rawt39;    /*   */
	int rawt40;    /*   */
	int rawt41;    /*   */
	int rawt42;    /*   */
	int rawt43;    /*   */
	int rawt44;    /*   */
	int rawt45;    /*   */
	int rawt46;    /*   */
	int rawt47;    /*   */
	int rawt48;    /*   */
	int rawt49;    /*   */
	int rawt50;    /*   */
	int rawt51;    /*   */
	int rawt52;    /*   */
	int rawt53;    /*   */
	int rawt54;    /*   */
	int rawt55;    /*   */
	int rawt56;    /*   */
	int rawt57;    /*   */
	int rawt58;    /*   */
	int rawt59;    /*   */
	int rawt60;    /*   */
	int rawt61;    /*   */
	int rawt62;    /*   */
	int rawt63;    /*   */
	int rawt64;    /*   */
} rtsl_t;

typedef struct {
	bankHeader_t bank;
	rtsl_t rtsl[1];
} clasRTSL_t;

/* --------------------- END rtsl -----------------*/

/* ------------------------ runc -----------------*/
typedef struct {
	int runno;    /*  Run number extracted from map */
	float beam_e;    /*  beam energy in MeV */
	int b_first;    /*  firsttime */
	float q_live;    /*  Fcup * live time (1*E-10C) */
	int ql_first;    /*  firsttime	 */
	float q_all;    /*  Fcup (1*E-10C) */
	int qa_first;    /*  firsttime	 */
	float tor_curr;    /*  Torus Current (A) */
	int t_first;    /*  firsttime	 */
	float mtor_curr;    /*  Minitorus Current (A) */
	int mt_first;    /*  firsttime	 */
	float tag_curr;    /*  Tagger Current (A) */
	int tag_first;    /*  firsttime	 */
} runc_t;

typedef struct {
	bankHeader_t bank;
	runc_t runc[1];
} clasRUNC_t;

/* --------------------- END runc -----------------*/

/* ------------------------ s1st -----------------*/
typedef struct {
	int latch1_bit1_count;    /*  Count trigger bit 1  latched events */
	int latch1_bit2_count;    /*  Count trigger bit 2  latched events */
	int latch1_bit3_count;    /*  Count trigger bit 3  latched events */
	int latch1_bit4_count;    /*  Count trigger bit 4  latched events */
	int latch1_bit5_count;    /*  Count trigger bit 5  latched events */
	int latch1_bit6_count;    /*  Count trigger bit 6  latched events */
	int latch1_bit7_count;    /*  Count trigger bit 7  latched events */
	int latch1_bit8_count;    /*  Count trigger bit 8  latched events */
	int latch1_bit9_count;    /*  Count trigger bit 9  latched events */
	int latch1_bit10_count;    /*  Count trigger bit 10 latched events */
	int latch1_bit11_count;    /*  Count trigger bit 11 latched events */
	int latch1_bit12_count;    /*  Count trigger bit 12 latched events */
	int event_count;    /*  Latched event count this run */
	int latch1_zero_count;    /*  Latch1 zero count (illegal) */
	int latch1_empty_count;    /*  Latch1 empty count (illegal) */
	int latch1_not_empty_count;    /*  Latch1 not empty on sync event (illegal) */
	int latch1_ok_count;    /*  Latch1 ok */
	int level2_sector1;    /*  Level2 sector1 count */
	int level2_sector2;    /*  Level2 sector2 count */
	int level2_sector3;    /*  Level2 sector3 count */
	int level2_sector4;    /*  Level2 sector4 count */
	int level2_sector5;    /*  Level2 sector5 count */
	int level2_sector6;    /*  Level2 sector6 count */
	int level2_pass;    /*  Level2 pass count */
	int level2_fail;    /*  Level2 fail count */
	int latch2_zero_count;    /*  Latch2 zero count (illegal) */
	int latch2_empty_count;    /*  Latch2 empty count (illegal) */
	int latch2_not_empty_count;    /*  Latch2 not empty on sync event (illegal) */
	int latch2_ok_count;    /*  Latch2 ok */
	int roc_13_count;    /*  Roc code 13 count (zero latch) */
	int roc_15_count;    /*  Roc code 15 count (illegal) */
	int l1l2_zero_count;    /*  (latch1==0)&&(latch2==0) */
	int l1zero_13_count;    /*  (latch1==0)&&(roc_code==13) */
	int l2zero_13_count;    /*  (latch2==0)&&(roc_code==13) */
	int l1l2zero_13_count;    /*  (latch1==0)&&(latch2==0)&&(roc_code==13) */
} s1st_t;

typedef struct {
	bankHeader_t bank;
	s1st_t s1st[1];
} clasS1ST_t;

/* --------------------- END s1st -----------------*/

/* ------------------------ sc1 -----------------*/
typedef struct {
	int id;    /*  the address of the hit detector element */
	float time_l;    /*  time for left paddle(ns)  */
	float energy_l;    /*  energy in left paddle(MeV)  */
	float time_r;    /*  time for right paddle(ns)  */
	float energy_r;    /*  energy in right paddle(MeV)  */
	float dtime_l;    /*  uncertainty in time for left paddle(ns)  */
	float denergy_l;    /*  uncertainty in energy in left paddle(MeV)  */
	float dtime_r;    /*  uncertainty in time for right paddle(ns)  */
	float denergy_r;    /*  uncertainty in energy in right paddle(MeV)  */
} sc1_t;

typedef struct {
	bankHeader_t bank;
	sc1_t sc1[1];
} clasSC1_t;

/* --------------------- END sc1 -----------------*/

/* ------------------------ scc -----------------*/
typedef struct {
	int id;    /*  paddle id#  */
	int date;    /*  UNIX univiersal time of calibraton (32 bits)  */
	int version;    /*  sequential version# of calibration  */
	int status;    /*  4 byte status word (see details)  */
	float td0l;    /*  (Left) gives 0 time at center for tube (ns)  */
	float td0lu;    /*  (Left) uncertainty (ns)  */
	float td0r;    /*  (Right) gives 0 time at center for tube (ns)  */
	float td0ru;    /*  (Right) uncertainty (ns)  */
	float td1l;    /*  (Left) TDC**1 coefficient (ns/ch)  */
	float td1lu;    /*  (Left) uncertainty (ns/ch)  */
	float td1r;    /*  (Right) TDC**1 coefficient (ns/ch)  */
	float td1ru;    /*  (Right) uncertainty (ns/ch)  */
	float td2l;    /*  (Left) TDC**2 coefficient  */
	float td2lu;    /*  (Left) uncertainty (ns/ch)  */
	float td2r;    /*  (Right)TDC**2 coefficient  */
	float td2ru;    /*  (Right) uncertainty (ns/ch)  */
	float tw0l;    /*  (Left) time walk constant (ns)  */
	float tw0lu;    /*  (Left) uncertainty (ns)  */
	float tw0r;    /*  (Right) time walk constant (ns)  */
	float tw0ru;    /*  (Right) uncertainty (ns)  */
	float tw1l;    /*  (Left) time walk 1st factor  */
	float tw1lu;    /*  (Left) uncertainty  */
	float tw1r;    /*  (Right) time walk 1st factor  */
	float tw1ru;    /*  (Right) uncertainty  */
	float tw2l;    /*  (Left) time walk 2nd factor  */
	float tw2lu;    /*  (Left) uncertainty  */
	float tw2r;    /*  (Right) time walk 2nd factor  */
	float tw2ru;    /*  (Right) uncertainty  */
	float adpl;    /*  (Left) ADC pedestal (ch)  */
	float adplu;    /*  (Left) uncertainty  */
	float adpr;    /*  (Right) ADC pedestal (ch)  */
	float adpru;    /*  (Right) uncertainty  */
	float m0l;    /*  (Left) nmip adc channel  */
	float m0lu;    /*  (Left) uncertainty (ch)  */
	float m0r;    /*  (Right)nmip adc channel  */
	float m0ru;    /*  (Right) uncertainty(ch)  */
	float vefl;    /*  (Left) effective velocity of light (cm/ns)  */
	float veflu;    /*  (Left) uncertainty (cm/ns)  */
	float vefr;    /*  (Right) effective velocity of light (cm/ns)  */
	float vefru;    /*  (Right) uncertainty (cm/ns)  */
	float atnl;    /*  (Left) attenuation length (cm)  */
	float atnlu;    /*  (Left) uncertainty (cm)  */
	float atnr;    /*  (Right) attenuation length (cm)  */
	float atnru;    /*  (Right) uncertainty (cm)  */
	float tsig0l;    /*  (Left) 1st parameter of measured resolution (ns)  */
	float tsig0r;    /*  (Right) 1st parameter of measured resolution (ns)  */
	float tsig1l;    /*  (Left) 2nd parameter of measured resolution  */
	float tsig1r;    /*  (Right) 2nd parameter of measured resolution  */
} scc_t;

typedef struct {
	bankHeader_t bank;
	scc_t scc[1];
} clasSCC_t;

/* --------------------- END scc -----------------*/

/* ------------------------ sc -----------------*/
typedef struct {
	uint16 id;    /*  the address of the hit detector element */
	uint16 tdcl;    /*  tdc information (channels) */
	uint16 adcl;    /*  adc information (channels) */
	uint16 tdcr;    /*  tdc information (channels) */
	uint16 adcr;    /*  adc information (channels) */
} sc_t;

typedef struct {
	bankHeader_t bank;
	sc_t sc[1];
} clasSC_t;

/* --------------------- END sc -----------------*/

/* ------------------------ scdi -----------------*/
typedef struct {
	int crate;    /*  CAMAC crate number */
	int slot;    /*  slot */
	int mask;    /*  mask */
	int threshold;    /*  actual threshold value (mV) */
	int width;    /*  actual width value */
} scdi_t;

typedef struct {
	bankHeader_t bank;
	scdi_t scdi[1];
} clasSCDI_t;

/* --------------------- END scdi -----------------*/

/* ------------------------ scgd -----------------*/
typedef struct {
	int id;    /*  paddle # inside the plane (1-23(max)) */
	float norm_z;    /*  Z(X') coordinate of unit normal to plane */
	float norm_x;    /*  Z(X') coordinate of unit normal to plane */
	float norm_d;    /*  distance to plane along unit vector */
	float alon_z;    /*  half width in X direction (along id's) */
	float alon_x;    /*  half length of SC in Y direction */
	float beg_pd;    /*  begin point of the paddle along id */
	float end_pd;    /*  end point of the paddle along id */
	float pdl_sh;    /*  shift of the paddle in respect to the Mid Plane */
	float r_beam;    /*  distance from the beam to the counte center in M.P. */
} scgd_t;

typedef struct {
	bankHeader_t bank;
	scgd_t scgd[1];
} clasSCGD_t;

/* --------------------- END scgd -----------------*/

/* ------------------------ scg -----------------*/
typedef struct {
	int id;    /*   paddle id# */
	int panel;    /*  panel number - to which plane (panel) the paddle belongs */
	float xccw;    /*  X center of CCW end of paddle in CLAS system */
	float yccw;    /*  Y center of CCW end  */
	float zccw;    /*  - Z center of CCW end */
	float xcw;    /*  X center of CW end  */
	float ycw;    /*  Y center of CW end */
	float zcw;    /* - Z center of CW end  */
	float width;    /*  width (cm) (~|| CLAS theta)  */
	float thick;    /*  thickness (cm) (~|| CLAS r)  */
	float delta;    /*  the difference in length of the higher-theta slab minus the lower-theta slab divided by 4 (cm)  */
} scg_t;

typedef struct {
	bankHeader_t bank;
	scg_t scg[1];
} clasSCG_t;

/* --------------------- END scg -----------------*/

/* ------------------------ sch -----------------*/
typedef struct {
	float x;    /*  x of hit */
	float y;    /*  y of hit */
	float z;    /*  z of hit */
	float cx;    /*  track x dir cosine */
	float cy;    /*  track y dir cosine */
	float cz;    /*  track z dir cosine */
	float pmom;    /*  track momentum */
	int track;    /*  track number */
	int id;    /*  track PDG id */
} sch_t;

typedef struct {
	bankHeader_t bank;
	sch_t sch[1];
} clasSCH_t;

/* --------------------- END sch -----------------*/

/* ------------------------ scmd -----------------*/
typedef struct {
	int id;    /*  paddle #  */
	int status;    /*  general status information  */
	float x_norm;    /*  X of unit vector normal to paddle */
	float y_norm;    /*  Y of unit vector normal to paddle */
	float d_norm;    /*  normal distance to paddle plane */
	float x_panel;    /*  X of unit vector along the panel */
	float y_panel;    /*  Y of unit vector along the panel */
	float b_pad;    /*  Begin of paddle along the panel */
	float e_pad;    /*  End of paddle along the panel */
	float tof;    /*  time of flight (nS) */
	float edp;    /*  energy deposited (GeV)  */
	float p_time;    /*  position i.r.t. mid.plane from time */
	float p_atln;    /*  position i.r.t. mid.plane from atten. */
} scmd_t;

typedef struct {
	bankHeader_t bank;
	scmd_t scmd[1];
} clasSCMD_t;

/* --------------------- END scmd -----------------*/

/* ------------------------ scmt -----------------*/
typedef struct {
	int mean_thr;    /*  mean threshold (mV) */
} scmt_t;

typedef struct {
	bankHeader_t bank;
	scmt_t scmt[1];
} clasSCMT_t;

/* --------------------- END scmt -----------------*/

/* ------------------------ scmw -----------------*/
typedef struct {
	int mean_width;    /*  mean threshold width */
} scmw_t;

typedef struct {
	bankHeader_t bank;
	scmw_t scmw[1];
} clasSCMW_t;

/* --------------------- END scmw -----------------*/

/* ------------------------ scpb -----------------*/
typedef struct {
	int scpdht;    /*  10000*sector+100*SC_PD_ID+Hit_ID in SCR  */
	float edep;    /*  Deposited energy (dE/dX) */
	float time;    /*  measured time  */
	float path;    /*  Path lenght from target */
	float chi2sc;    /*  Quality measure of geometrical matching */
	int status;    /*  Status word (not defined yet) */
} scpb_t;

typedef struct {
	bankHeader_t bank;
	scpb_t scpb[1];
} clasSCPB_t;

/* --------------------- END scpb -----------------*/

/* ------------------------ scp -----------------*/
typedef struct {
	float n1x;    /*  x component of outward normal to panel 1 */
	float n1y;    /*  y component of outward normal to panel 1 */
	float n1z;    /*  z component of outward normal to panel 1 */
	float r1n;    /*  min.dist.inner(cm)from origin to panel 1 */
	float n2x;    /*  x component of outward normal to panel 2 */
	float n2y;    /*  y component of outward normal to panel 2 */
	float n2z;    /*  z component of outward normal to panel 2 */
	float r2n;    /*  min.dist.inner(cm)from origin to panel 2 */
	float n3x;    /*  x component of outward normal to panel 3 */
	float n3y;    /*  y component of outward normal to panel 3 */
	float n3z;    /*  z component of outward normal to panel 3 */
	float r3n;    /*  min.dist.inner(cm)from origin to panel 3 */
	float n4x;    /*  x component of outward normal to panel 4 */
	float n4y;    /*  y component of outward normal to panel 4 */
	float n4z;    /*  z component of outward normal to panel 4 */
	float r4n;    /*  min.dist.inner(cm)from origin to panel 4 */
} scp_t;

typedef struct {
	bankHeader_t bank;
	scp_t scp[1];
} clasSCP_t;

/* --------------------- END scp -----------------*/

/* ------------------------ scpe -----------------*/
typedef struct {
	int id;    /*  the address of the hit detector element */
	int mean_left;    /*  left adc pedestal mean value (channel) */
	float sigma_left;    /*  sigma of the pedestal distribution for left adc (channel) */
	int mean_right;    /*  right adc pedestal mean value (channel) */
	float sigma_right;    /*  sigma of the pedestal distribution for right adc (channel */
} scpe_t;

typedef struct {
	bankHeader_t bank;
	scpe_t scpe[1];
} clasSCPE_t;

/* --------------------- END scpe -----------------*/

/* ------------------------ scps -----------------*/
typedef struct {
	int id;    /*  Plane #(1-6):1-23,24-34,35-39,40-42,43-46,47-48 */
	int firstpnum;    /*  The first paddle number in the plane */
	int lastpnum;    /*  The last paddle number in the plane */
	float distance;    /*  distance to the center of the plane */
	float begalonshft;    /*  X-shift of Begin Plane poin along id of SC */
	float endalonshft;    /*  X-shift of End of Plane poin along id of SC */
	float lengthshft;    /*  Y-shift of Plane SYS along the SC length */
	float alongidx;    /*  XP.x direction for width in Sector System */
	float alongidy;    /*  XP.y direction for width in Sector System */
	float alongidz;    /*  XP.z direction for width in Sector System */
	float slengthx;    /*  YP.x direction for length in Sector System */
	float slengthy;    /*  YP.y direction for length in Sector System */
	float slengthz;    /*  YP.z direction for length in Sector System */
	float unormalx;    /*  ZP.x direction for thickness in Sector System */
	float unormaly;    /*  ZP.y direction for thickness in Sector System */
	float unormalz;    /*  ZP.z direction for thickness in Sector System */
} scps_t;

typedef struct {
	bankHeader_t bank;
	scps_t scps[1];
} clasSCPS_t;

/* --------------------- END scps -----------------*/

/* ------------------------ scrc -----------------*/
typedef struct {
	int id;    /*  cluster id    */
	float energy;    /*  cluster Energy (MeV)  */
	float denergy;    /*  error in cluster energy (ns) */
	float time;    /*  cluster (energy-weighted) time(ns)  */
	float dtime;    /*  error in cluster time (ns) */
	float x;    /*  x position in sector coordinate system  */
	float y;    /*  y position in sector coordinate system  */
	float z;    /*  z position in sector coordinate system  */
	float dx;    /*  x error in sector coordinate system  */
	float dy;    /*  y error in sector coordinate system  */
	float dz;    /*  z error in sector coordinate system  */
	int status;    /*  status word defined in sc.h */
} scrc_t;

typedef struct {
	bankHeader_t bank;
	scrc_t scrc[1];
} clasSCRC_t;

/* --------------------- END scrc -----------------*/

/* ------------------------ scr -----------------*/
typedef struct {
	int id;    /*  paddle id   */
	float energy;    /*  Energy (MeV)  */
	float time;    /*  time(ns) */
	float x;    /*  x position in sector coodinate system  */
	float y;    /*  y position in sector coodinate system  */
	float z;    /*  z position in sector coodinate system  */
	float dx;    /*  x error in sector coodinate system  */
	float dy;    /*  y error in sector coodinate system  */
	float dz;    /*  z error in sector coodinate system  */
	int status;    /*  status word defined in sc.h */
	float denergy;    /*  uncertainty in Energy (MeV)  */
	float dtime;    /*  uncertainty in time (ns)  */
} scr_t;

typedef struct {
	bankHeader_t bank;
	scr_t scr[1];
} clasSCR_t;

/* --------------------- END scr -----------------*/

/* ------------------------ scs -----------------*/
typedef struct {
	int scs1;    /*  scaler sector 1 */
	int scs2;    /*  scaler sector 1 */
	int scs3;    /*  scaler sector 1 */
	int scs4;    /*  scaler sector 1 */
	int scs5;    /*  scaler sector 1 */
	int scs6;    /*  scaler sector 1 */
	int scs7;    /*  scaler sector 1 */
	int scs8;    /*  scaler sector 1 */
	int scs9;    /*  scaler sector 1 */
	int scs10;    /*  scaler sector 1 */
	int scs11;    /*  scaler sector 1 */
	int scs12;    /*  scaler sector 1 */
	int scs13;    /*  scaler sector 1 */
	int scs14;    /*  scaler sector 1 */
	int scs15;    /*  scaler sector 1 */
	int scs16;    /*  scaler sector 1 */
	int scs17;    /*  scaler sector 1 */
	int scs18;    /*  scaler sector 1 */
	int scs19;    /*  scaler sector 1 */
	int scs20;    /*  scaler sector 1 */
	int scs21;    /*  scaler sector 1 */
	int scs22;    /*  scaler sector 1 */
	int scs23;    /*  scaler sector 1 */
	int scs24;    /*  scaler sector 1 */
	int scs25;    /*  scaler sector 1 */
	int scs26;    /*  scaler sector 1 */
	int scs27;    /*  scaler sector 1 */
	int scs28;    /*  scaler sector 1 */
	int scs29;    /*  scaler sector 1 */
	int scs30;    /*  scaler sector 1 */
	int scs31;    /*  scaler sector 1 */
	int scs32;    /*  scaler sector 1 */
	int scs33;    /*  scaler sector 2 */
	int scs34;    /*  scaler sector 2 */
	int scs35;    /*  scaler sector 2 */
	int scs36;    /*  scaler sector 2 */
	int scs37;    /*  scaler sector 2 */
	int scs38;    /*  scaler sector 2 */
	int scs39;    /*  scaler sector 2 */
	int scs40;    /*  scaler sector 2 */
	int scs41;    /*  scaler sector 2 */
	int scs42;    /*  scaler sector 2 */
	int scs43;    /*  scaler sector 2 */
	int scs44;    /*  scaler sector 2 */
	int scs45;    /*  scaler sector 2 */
	int scs46;    /*  scaler sector 2 */
	int scs47;    /*  scaler sector 2 */
	int scs48;    /*  scaler sector 2 */
	int scs49;    /*  scaler sector 2 */
	int scs50;    /*  scaler sector 2 */
	int scs51;    /*  scaler sector 2 */
	int scs52;    /*  scaler sector 2 */
	int scs53;    /*  scaler sector 2 */
	int scs54;    /*  scaler sector 2 */
	int scs55;    /*  scaler sector 2 */
	int scs56;    /*  scaler sector 2 */
	int scs57;    /*  scaler sector 2 */
	int scs58;    /*  scaler sector 2 */
	int scs59;    /*  scaler sector 2 */
	int scs60;    /*  scaler sector 2 */
	int scs61;    /*  scaler sector 2 */
	int scs62;    /*  scaler sector 2 */
	int scs63;    /*  scaler sector 2 */
	int scs64;    /*  scaler sector 2 */
	int scs65;    /*  scaler sector 3 */
	int scs66;    /*  scaler sector 3 */
	int scs67;    /*  scaler sector 3 */
	int scs68;    /*  scaler sector 3 */
	int scs69;    /*  scaler sector 3 */
	int scs70;    /*  scaler sector 3 */
	int scs71;    /*  scaler sector 3 */
	int scs72;    /*  scaler sector 3 */
	int scs73;    /*  scaler sector 3 */
	int scs74;    /*  scaler sector 3 */
	int scs75;    /*  scaler sector 3 */
	int scs76;    /*  scaler sector 3 */
	int scs77;    /*  scaler sector 3 */
	int scs78;    /*  scaler sector 3 */
	int scs79;    /*  scaler sector 3 */
	int scs80;    /*  scaler sector 3 */
	int scs81;    /*  scaler sector 3 */
	int scs82;    /*  scaler sector 3 */
	int scs83;    /*  scaler sector 3 */
	int scs84;    /*  scaler sector 3 */
	int scs85;    /*  scaler sector 3 */
	int scs86;    /*  scaler sector 3 */
	int scs87;    /*  scaler sector 3 */
	int scs88;    /*  scaler sector 3 */
	int scs89;    /*  scaler sector 3 */
	int scs90;    /*  scaler sector 3 */
	int scs91;    /*  scaler sector 3 */
	int scs92;    /*  scaler sector 3 */
	int scs93;    /*  scaler sector 3 */
	int scs94;    /*  scaler sector 3 */
	int scs95;    /*  scaler sector 3 */
	int scs96;    /*  scaler sector 3 */
	int scs97;    /*  scaler sector 4 */
	int scs98;    /*  scaler sector 4 */
	int scs99;    /*  scaler sector 4 */
	int scs100;    /*  scaler sector 4 */
	int scs101;    /*  scaler sector 4 */
	int scs102;    /*  scaler sector 4 */
	int scs103;    /*  scaler sector 4 */
	int scs104;    /*  scaler sector 4 */
	int scs105;    /*  scaler sector 4 */
	int scs106;    /*  scaler sector 4 */
	int scs107;    /*  scaler sector 4 */
	int scs108;    /*  scaler sector 4 */
	int scs109;    /*  scaler sector 4 */
	int scs110;    /*  scaler sector 4 */
	int scs111;    /*  scaler sector 4 */
	int scs112;    /*  scaler sector 4 */
	int scs113;    /*  scaler sector 4 */
	int scs114;    /*  scaler sector 4 */
	int scs115;    /*  scaler sector 4 */
	int scs116;    /*  scaler sector 4 */
	int scs117;    /*  scaler sector 4 */
	int scs118;    /*  scaler sector 4 */
	int scs119;    /*  scaler sector 4 */
	int scs120;    /*  scaler sector 4 */
	int scs121;    /*  scaler sector 4 */
	int scs122;    /*  scaler sector 4 */
	int scs123;    /*  scaler sector 4 */
	int scs124;    /*  scaler sector 4 */
	int scs125;    /*  scaler sector 4 */
	int scs126;    /*  scaler sector 4 */
	int scs127;    /*  scaler sector 4 */
	int scs128;    /*  scaler sector 4 */
	int scs129;    /*  scaler sector 5 */
	int scs130;    /*  scaler sector 5 */
	int scs131;    /*  scaler sector 5 */
	int scs132;    /*  scaler sector 5 */
	int scs133;    /*  scaler sector 5 */
	int scs134;    /*  scaler sector 5 */
	int scs135;    /*  scaler sector 5 */
	int scs136;    /*  scaler sector 5 */
	int scs137;    /*  scaler sector 5 */
	int scs138;    /*  scaler sector 5 */
	int scs139;    /*  scaler sector 5 */
	int scs140;    /*  scaler sector 5 */
	int scs141;    /*  scaler sector 5 */
	int scs142;    /*  scaler sector 5 */
	int scs143;    /*  scaler sector 5 */
	int scs144;    /*  scaler sector 5 */
	int scs145;    /*  scaler sector 5 */
	int scs146;    /*  scaler sector 5 */
	int scs147;    /*  scaler sector 5 */
	int scs148;    /*  scaler sector 5 */
	int scs149;    /*  scaler sector 5 */
	int scs150;    /*  scaler sector 5 */
	int scs151;    /*  scaler sector 5 */
	int scs152;    /*  scaler sector 5 */
	int scs153;    /*  scaler sector 5 */
	int scs154;    /*  scaler sector 5 */
	int scs155;    /*  scaler sector 5 */
	int scs156;    /*  scaler sector 5 */
	int scs157;    /*  scaler sector 5 */
	int scs158;    /*  scaler sector 5 */
	int scs159;    /*  scaler sector 5 */
	int scs160;    /*  scaler sector 5 */
	int scs161;    /*  scaler sector 6 */
	int scs162;    /*  scaler sector 6 */
	int scs163;    /*  scaler sector 6 */
	int scs164;    /*  scaler sector 6 */
	int scs165;    /*  scaler sector 6 */
	int scs166;    /*  scaler sector 6 */
	int scs167;    /*  scaler sector 6 */
	int scs168;    /*  scaler sector 6 */
	int scs169;    /*  scaler sector 6 */
	int scs170;    /*  scaler sector 6 */
	int scs171;    /*  scaler sector 6 */
	int scs172;    /*  scaler sector 6 */
	int scs173;    /*  scaler sector 6 */
	int scs174;    /*  scaler sector 6 */
	int scs175;    /*  scaler sector 6 */
	int scs176;    /*  scaler sector 6 */
	int scs177;    /*  scaler sector 6 */
	int scs178;    /*  scaler sector 6 */
	int scs179;    /*  scaler sector 6 */
	int scs180;    /*  scaler sector 6 */
	int scs181;    /*  scaler sector 6 */
	int scs182;    /*  scaler sector 6 */
	int scs183;    /*  scaler sector 6 */
	int scs184;    /*  scaler sector 6 */
	int scs185;    /*  scaler sector 6 */
	int scs186;    /*  scaler sector 6 */
	int scs187;    /*  scaler sector 6 */
	int scs188;    /*  scaler sector 6 */
	int scs189;    /*  scaler sector 6 */
	int scs190;    /*  scaler sector 6 */
	int scs191;    /*  scaler sector 6 */
	int scs192;    /*  scaler sector 6 */
} scs_t;

typedef struct {
	bankHeader_t bank;
	scs_t scs[1];
} clasSCS_t;

/* --------------------- END scs -----------------*/

/* ------------------------ sgmp -----------------*/
typedef struct {
	int is;    /*  superlayer number of the segment */
	int sgm1;    /*  data structure see: include/bosddl.h, wire_t */
	int sgm2;    /*  wire_t */
	int sgm3;    /*  wire_t */
	int sgm4;    /*  wire_t */
	int sgm5;    /*  wire_t */
	int sgm6;    /*  wire_t */
} sgmp_t;

typedef struct {
	bankHeader_t bank;
	sgmp_t sgmp[1];
} clasSGMP_t;

/* --------------------- END sgmp -----------------*/

/* ------------------------ spar -----------------*/
typedef struct {
	int slot;    /*  ADC slot */
	int channel;    /*  ADC channel */
	int spar;    /*  sparsification threshold (channel) */
	int pedmean;    /*  pedestal mean value measured, using internal 500ns gate (channel) */
} spar_t;

typedef struct {
	bankHeader_t bank;
	spar_t spar[1];
} clasSPAR_t;

/* --------------------- END spar -----------------*/

/* ------------------------ spin -----------------*/
typedef struct {
	int xspin;    /*  x component of spin */
	int yspin;    /*  y component of spin */
	int zspin;    /*  z component of spin */
} spin_t;

typedef struct {
	bankHeader_t bank;
	spin_t spin[1];
} clasSPIN_t;

/* --------------------- END spin -----------------*/

/* ------------------------ st1 -----------------*/
typedef struct {
	int id;    /*  Pair id */
	int status;    /*  status word */
	float time_1;    /*  Time (ns) for side 1 */
	float adc_1;    /*  Pedestal-subtracted adc for side 1 */
	float time_2;    /*  Time (ns) for side 2 */
	float adc_2;    /*  Pedestal-subtracted ADC  for side 2 */
} st1_t;

typedef struct {
	bankHeader_t bank;
	st1_t st1[1];
} clasST1_t;

/* --------------------- END st1 -----------------*/

/* ------------------------ st -----------------*/
typedef struct {
	uint16 id;    /*  the address of the hit detector element */
	uint16 tdc;    /*  tdc information (channels) */
	uint16 adc;    /*  adc information (channels) */
} st_t;

typedef struct {
	bankHeader_t bank;
	st_t st[1];
} clasST_t;

/* --------------------- END st -----------------*/

/* ------------------------ stg -----------------*/
typedef struct {
	int id;    /*  sector id */
	float leg_x_max;    /*  distance from beam axis to inner face of leg  */
	float leg_x_min;    /*  */
	float leg_y_max;    /*   */
	float leg_y_min;    /*   */
	float leg_z_max;    /*  */
	float let_z_min;    /*   */
	float nose_x_max;    /*    */
	float nose_x_min;    /*   */
	float nose_y_max;    /*  */
	float nose_y_min;    /*  */
	float nose_z_max;    /*  */
	float nose_z_min;    /*  */
	float noseangle;    /*  */
	float lleg;    /*  */
	float lnose;    /*  */
} stg_t;

typedef struct {
	bankHeader_t bank;
	stg_t stg[1];
} clasSTG_t;

/* --------------------- END stg -----------------*/

/* ------------------------ sth -----------------*/
typedef struct {
	float x;    /*  x of hit */
	float y;    /*  y of hit */
	float z;    /*  z of hit */
	float cx;    /*  track x dir cosine */
	float cy;    /*  track y dir cosine */
	float cz;    /*  track z dir cosine */
	float pmom;    /*  track momentum */
	int track;    /*  track number */
	int id;    /*  track PDG id */
	float tof;    /*  flight time */
} sth_t;

typedef struct {
	bankHeader_t bank;
	sth_t sth[1];
} clasSTH_t;

/* --------------------- END sth -----------------*/

/* ------------------------ stn0 -----------------*/
typedef struct {
	uint16 id;    /*  detector element */
	uint16 tdc;    /*  tdc information (channels) */
} stn0_t;

typedef struct {
	bankHeader_t bank;
	stn0_t stn0[1];
} clasSTN0_t;

/* --------------------- END stn0 -----------------*/

/* ------------------------ stn1 -----------------*/
typedef struct {
	uint16 id;    /*  detector element  */
	uint16 adc;    /*  adc information (channels) */
} stn1_t;

typedef struct {
	bankHeader_t bank;
	stn1_t stn1[1];
} clasSTN1_t;

/* --------------------- END stn1 -----------------*/

/* ------------------------ stpb -----------------*/
typedef struct {
	int sthid;    /*  100*sector+Hit_ID   */
	float time;    /*  Flight time relative to the evnt start time */
	float path;    /*  Path lenght from target */
	int charge;    /*  charge (get from tracking) */
	int status;    /*  Status word (not defined yet) */
} stpb_t;

typedef struct {
	bankHeader_t bank;
	stpb_t stpb[1];
} clasSTPB_t;

/* --------------------- END stpb -----------------*/

/* ------------------------ stpe -----------------*/
typedef struct {
	int id;    /*  the address of the hit detector element */
	int mean;    /*  adc pedestal mean value (channel) */
	float sigma;    /*  sigma of the pedestal distribution (channel) */
} stpe_t;

typedef struct {
	bankHeader_t bank;
	stpe_t stpe[1];
} clasSTPE_t;

/* --------------------- END stpe -----------------*/

/* ------------------------ str -----------------*/
typedef struct {
	int id;    /*  sector */
	int trk_no;    /*  Pointer to track in HBTR */
	float st_time;    /*  flight time from ST (ns) */
	float st_l;    /*  flight path from ST (cm) */
	float st_pos;    /*  position within the start counter */
	int status;    /*  Status word */
} str_t;

typedef struct {
	bankHeader_t bank;
	str_t str[1];
} clasSTR_t;

/* --------------------- END str -----------------*/

/* ------------------------ sts -----------------*/
typedef struct {
	int sts1;    /*  trigger rate sector 1-2 */
	int sts2;    /*  trigger rate sector 3-4 */
	int sts3;    /*  trigger rate sector 5-6 */
	int sts4;    /*   */
	int sts5;    /*   */
	int sts6;    /*   */
	int sts7;    /*   */
	int sts8;    /*   */
	int sts9;    /*   */
	int sts10;    /*   */
	int sts11;    /*   */
	int sts12;    /*   */
	int sts13;    /*   */
	int sts14;    /*   */
	int sts15;    /*   */
	int sts16;    /*   */
} sts_t;

typedef struct {
	bankHeader_t bank;
	sts_t sts[1];
} clasSTS_t;

/* --------------------- END sts -----------------*/

/* ------------------------ stsn -----------------*/
typedef struct {
	int stsn1;    /*  */
	int stsn2;    /*  */
	int stsn3;    /*  */
	int stsn4;    /*   */
	int stsn5;    /*   */
	int stsn6;    /*   */
	int stsn7;    /*   */
	int stsn8;    /*   */
	int stsn9;    /*   */
	int stsn10;    /*   */
	int stsn11;    /*   */
	int stsn12;    /*   */
	int stsn13;    /*   */
	int stsn14;    /*   */
	int stsn15;    /*   */
	int stsn16;    /*   */
	int stsn17;    /*   */
	int stsn18;    /*   */
	int stsn19;    /*   */
	int stsn20;    /*   */
	int stsn21;    /*   */
	int stsn22;    /*   */
	int stsn23;    /*   */
	int stsn24;    /*   */
	int stor;    /*   */
	int stmult;    /*   */
	int standmor;    /*   */
	int multandmor;    /*   */
	int res1;    /*   */
	int res2;    /*   */
	int res3;    /*   */
	int res4;    /*   */
} stsn_t;

typedef struct {
	bankHeader_t bank;
	stsn_t stsn[1];
} clasSTSN_t;

/* --------------------- END stsn -----------------*/

/* ------------------------ sync -----------------*/
typedef struct {
	uint16 id;    /*  slot number */
	uint16 tdcl;    /*  count of missing responses */
	uint16 adcl;    /*  count of extra buffers */
} sync_t;

typedef struct {
	bankHeader_t bank;
	sync_t sync[1];
} clasSYNC_t;

/* --------------------- END sync -----------------*/

/* ------------------------ taco -----------------*/
typedef struct {
	int id;    /*  ID  1= TAC, 2 = USLG */
	float time;    /*   TDC time  */
	float elt;    /*  energy deposit TAC = left top */
	float ert;    /*  energy deposit TAC = right top */
	float elb;    /*  energy deposit TAC = left bottom */
	float erb;    /*  energy deposit TAC = right bottom */
	float esum;    /*  energy deposit TAC = sum1 */
	float esum2;    /*  energy deposit TAC = sum2 */
	float esum3;    /*  energy deposit TAC = sum3 */
	int tid;    /*  T id of the corresponding T */
} taco_t;

typedef struct {
	bankHeader_t bank;
	taco_t taco[1];
} clasTACO_t;

/* --------------------- END taco -----------------*/

/* ------------------------ tage -----------------*/
typedef struct {
	uint16 id;    /*  the address of the hit detector element */
	uint16 tdc;    /*  tdc information (channels) [multihit tdc] */
} tage_t;

typedef struct {
	bankHeader_t bank;
	tage_t tage[1];
} clasTAGE_t;

/* --------------------- END tage -----------------*/

/* ------------------------ tagi -----------------*/
typedef struct {
	int idt;    /*  T id */
	float timel;    /*  time information (Left counters channels) */
	float timer;    /*  time information (Right counters channels) */
	int ide;    /*  E id */
	float timee;    /*  time information (E counters) */
	float timemean;    /*  time information (left/right mean value | after alignement) */
	float trf;    /*  time information (mean val - RF | after alignement) */
	float nexttime;    /*  time difference with the next T when in coincidence | after alignment) */
} tagi_t;

typedef struct {
	bankHeader_t bank;
	tagi_t tagi[1];
} clasTAGI_t;

/* --------------------- END tagi -----------------*/

/* ------------------------ tagm -----------------*/
typedef struct {
	float energy;    /*  Energy of the photon in GeV */
	float t;    /*  T-counter time (ns) */
	float e_t;    /*  E-counter time (ns) */
	int status;    /*  Status (not yet used)  */
	int tid;    /*  T channel Id */
	int eid;    /*  E channel Id */
} tagm_t;

typedef struct {
	bankHeader_t bank;
	tagm_t tagm[1];
} clasTAGM_t;

/* --------------------- END tagm -----------------*/

/* ------------------------ tagr -----------------*/
typedef struct {
	float erg;    /*  Energy of the photon in GeV */
	float ttag;    /*  Time of the photon has reconstructed in the Tagger */
	float tpho;    /*  Time of the photon after RF correction */
	int stat;    /*  Status ( 7 or 15 are Good) other values have problems (see tag_process_TAGR.F)  */
	int t_id;    /*  T counter Id */
	int e_id;    /*  E counter Id */
} tagr_t;

typedef struct {
	bankHeader_t bank;
	tagr_t tagr[1];
} clasTAGR_t;

/* --------------------- END tagr -----------------*/

/* ------------------------ tagt -----------------*/
typedef struct {
	uint16 id;    /*  the address of the hit detector element */
	uint16 tdcl;    /*  tdc information (Left counters channels) */
	uint16 tdcr;    /*  tdc information (Right counters channels) */
} tagt_t;

typedef struct {
	bankHeader_t bank;
	tagt_t tagt[1];
} clasTAGT_t;

/* --------------------- END tagt -----------------*/

/* ------------------------ tber -----------------*/
typedef struct {
	float q_over_p;    /*  q/p */
	float lambda;    /*  dip angle (pi/2 - theta) */
	float phi;    /*  phi */
	float d0;    /*  min.distance from (x=0,y=0,z=?)  [cm] */
	float z0;    /*  z position of starting point     [cm]  */
	float c11;    /*  element C{1,1} */
	float c12;    /*  element C{1,2} */
	float c13;    /*  element C{1,3} */
	float c14;    /*  element C{1,4} */
	float c15;    /*  element C{1,5} */
	float c22;    /*  element C{2,2} */
	float c23;    /*  element C{2,3} */
	float c24;    /*  element C{2,4} */
	float c25;    /*  element C{2,5} */
	float c33;    /*  element C{3,3} */
	float c34;    /*  element C{3,4} */
	float c35;    /*  element C{3,5} */
	float c44;    /*  element C{4,4} */
	float c45;    /*  element C{4,5} */
	float c55;    /*  element C{5,5} */
	float chi2;    /*  Chisquare for this Track */
	int layinfo1;    /*  layerhit info */
	int layinfo2;    /*  layerhit info&sector&track#in hber */
} tber_t;

typedef struct {
	bankHeader_t bank;
	tber_t tber[1];
} clasTBER_t;

/* --------------------- END tber -----------------*/

/* ------------------------ tbid -----------------*/
typedef struct {
	int track;    /*  Track (index to TBTR) or zero if neutral */
	int sec;    /*  Sector track is in */
	float beta;    /*  Beta of the track in units of c */
	float vtime;    /*  vertex time of track */
	int sc_stat;    /*  status of hit matching to SC: see sc.h */
	int sc_id;    /*  Pointer to SCRC bank */
	float sc_time;    /*  SC calibrated time for this track (ns) */
	float sc_qual;    /*  quality of match for SC */
	float sc_vtime;    /*  time at vertex for SC(ns) */
	float sc_beta;    /*  Beta calculated from TOF from SC */
	int cc_stat;    /*  status of hit matching to CC: see sc.h */
	int cc_id;    /*  pointer to CC01 bank */
	float cc_time;    /*  CC calibrated time for this track (ns) */
	float cc_qual;    /*  quality of match for CC */
	float cc_vtime;    /*  time at vertex for CC(ns) */
	float cc_beta;    /*  Beta as calculated by the EC */
	int ec_stat;    /*  status of hit matching to ec: see sc.h */
	int ec_id;    /*  Pointer to ECHB bank */
	float ec_time;    /*  EC calibrated time for this track (ns) */
	float ec_qual;    /*  EC quality factor */
	float ec_vtime;    /*  time at vertex for EC(ns) */
	float ec_beta;    /*  Beta as calculated by the EC */
	int st_stat;    /*  status of hit matching to ST */
	int st_id;    /*  Pointer to STR bank */
	float st_time;    /*  ST calibrated time for this track (ns) */
	float st_qual;    /*  ST quality factor */
	float st_vtime;    /*  time at vertex for ST(ns) */
	float st_beta;    /*  Beta as calculated by the ST */
	int lac_stat;    /*  status of hit matching to LAC */
	int lac_id;    /*  Pointer to EC1R bank */
	float lac_time;    /*  LAC calibrated time for this track (ns) */
	float lac_qual;    /*  LAC quality factor */
	float lac_vtime;    /*  time at vertex for LAC(ns) */
	float lac_beta;    /*  Beta as calculated by the LAC */
} tbid_t;

typedef struct {
	bankHeader_t bank;
	tbid_t tbid[1];
} clasTBID_t;

/* --------------------- END tbid -----------------*/

/* ------------------------ tbla -----------------*/
typedef struct {
	int trk_pln;    /*  (track_number) *100 + Trk_plane_number */
	float x;    /*  z coord [cm]  for track in this plane */
	float y;    /*  y coord [cm]  for track in this plane */
	float z;    /*  z coord [cm]  for track in this plane */
	float bx;    /*  B-field in x [kG] at coord.{x,y,z} */
	float by;    /*  B-field in y [kG] at coord.{x,y,z} */
	float bz;    /*  B-field in z [kG] at coord.{x,y,z} */
	float tlen;    /*  track length [cm] from origin to this plane */
	int dc1;    /*  Pointer to DC1 bank */
	int stat;    /*  Status of the hit */
	int wire;    /*  Wire number  */
	float dtime;    /*  drift time  [ns] */
	float alpha;    /*  track angle (relative to R of SL) [deg] */
	float wlen;    /*  Wire length (hit pos. to preamp)  [cm] */
	float sgdoca;    /*  sigma DOCA  [cm] */
	float fitdoca;    /*  Fitted DOCA [cm] */
	float calcdoca;    /*  calculated DOCA (via dtime)  [cm] */
} tbla_t;

typedef struct {
	bankHeader_t bank;
	tbla_t tbla[1];
} clasTBLA_t;

/* --------------------- END tbla -----------------*/

/* ------------------------ tbtr -----------------*/
typedef struct {
	float x;    /*  x */
	float y;    /*  y 'Vertex' position {x,y,z} */
	float z;    /*  z */
	float px;    /*  Px */
	float py;    /*  Py  momentum at 'vertex' {Px,Py,Pz} */
	float pz;    /*  Pz */
	float q;    /*  charge   (straight tracks: set q=0.)  */
	float chi2;    /*  Chisquare for this Track */
	int itr_sec;    /*  Trackno_in_Sector + Sector*100 */
	int itr_hbt;    /*  Trackno in HBTR for this track */
} tbtr_t;

typedef struct {
	bankHeader_t bank;
	tbtr_t tbtr[1];
} clasTBTR_t;

/* --------------------- END tbtr -----------------*/

/* ------------------------ tcsb -----------------*/
typedef struct {
	float xpos;    /*  x misalignment of TCS in HCS 		  */
	float ypos;    /*  y misalignment of TCS in HCS	  */
	float zpos;    /*  z misalignment of TCS in HCS	  */
	float sxpos;    /*  sx sine of x-axis misorientation of TCS vs HCS		  */
	float sypos;    /*  sy sine of y-axis misorientation of TCS vs HCS */
	float szpos;    /*  sz sine of z-axis misorientation of TCS vs HCS */
} tcsb_t;

typedef struct {
	bankHeader_t bank;
	tcsb_t tcsb[1];
} clasTCSB_t;

/* --------------------- END tcsb -----------------*/

/* ------------------------ tdpl -----------------*/
typedef struct {
	int trk_pln;    /*  (track_number) *100 + plane_number */
	float x;    /*  vector3_t pos; */
	float y;    /*   (x, y, z coord. for track in on the plane) */
	float z;    /*   */
	float cx;    /*  vector3_t dir; */
	float cy;    /*   direction cosines (x,y,z) for track at coord.{x,y,z} */
	float cz;    /*   */
	float tlen;    /*  track length [cm] from origin to this plane */
} tdpl_t;

typedef struct {
	bankHeader_t bank;
	tdpl_t tdpl[1];
} clasTDPL_t;

/* --------------------- END tdpl -----------------*/

/* ------------------------ tesc -----------------*/
typedef struct {
	float value;    /*  count rate per E-counter channel */
} tesc_t;

typedef struct {
	bankHeader_t bank;
	tesc_t tesc[1];
} clasTESC_t;

/* --------------------- END tesc -----------------*/

/* ------------------------ tgbi -----------------*/
typedef struct {
	uint32 latch1;    /*  level1 trigger latch word (16 bits) */





	uint32 helicity_scaler;    /*  helicity interval count */
	uint32 interrupt_time;    /*  interrupt time from microsec clock */
	uint32 latch2;    /*  level2 trigger latch word (16 bits) */






	uint32 level3;    /*  level3 trigger word (32 bits) */



} tgbi_t;

typedef struct {
	bankHeader_t bank;
	tgbi_t tgbi[1];
} clasTGBI_t;

/* --------------------- END tgbi -----------------*/

/* ------------------------ tgeo -----------------*/
typedef struct {
	float x;    /*  X position of target */
	float y;    /*  Y position of target */
	float z;    /*  Z position of target */
	float radius;    /*  radius of target */
	float lenght;    /*  lenght of target */
	int material;    /*  material of target: 0=empty; 1=Hydrogen;  */
} tgeo_t;

typedef struct {
	bankHeader_t bank;
	tgeo_t tgeo[1];
} clasTGEO_t;

/* --------------------- END tgeo -----------------*/

/* ------------------------ tgpb -----------------*/
typedef struct {
	int pointer;    /*  pointer to TAGR   */
	float time;    /*  starttime_TAG at interaction point(ns) */
	float energy;    /*  photon energy(GeV) */
	float dt;    /*  starttime_ST - starttime_TAG (ns) */
} tgpb_t;

typedef struct {
	bankHeader_t bank;
	tgpb_t tgpb[1];
} clasTGPB_t;

/* --------------------- END tgpb -----------------*/

/* ------------------------ tgs -----------------*/
typedef struct {
	int rawt1;    /*   */
	int rawt2;    /*   */
	int rawt3;    /*   */
	int rawt4;    /*   */
	int rawt5;    /*   */
	int rawt6;    /*   */
	int rawt7;    /*   */
	int rawt8;    /*   */
	int rawt9;    /*   */
	int rawt10;    /*   */
	int rawt11;    /*   */
	int rawt12;    /*   */
	int rawt13;    /*   */
	int rawt14;    /*   */
	int rawt15;    /*   */
	int rawt16;    /*   */
	int rawt17;    /*   */
	int rawt18;    /*   */
	int rawt19;    /*   */
	int rawt20;    /*   */
	int rawt21;    /*   */
	int rawt22;    /*   */
	int rawt23;    /*   */
	int rawt24;    /*   */
	int rawt25;    /*   */
	int rawt26;    /*   */
	int rawt27;    /*   */
	int rawt28;    /*   */
	int rawt29;    /*   */
	int rawt30;    /*   */
	int rawt31;    /*   */
	int rawt32;    /*   */
	int rawt33;    /*   */
	int rawt34;    /*   */
	int rawt35;    /*   */
	int rawt36;    /*   */
	int rawt37;    /*   */
	int rawt38;    /*   */
	int rawt39;    /*   */
	int rawt40;    /*   */
	int rawt41;    /*   */
	int rawt42;    /*   */
	int rawt43;    /*   */
	int rawt44;    /*   */
	int rawt45;    /*   */
	int rawt46;    /*   */
	int rawt47;    /*   */
	int rawt48;    /*   */
	int rawt49;    /*   */
	int rawt50;    /*   */
	int rawt51;    /*   */
	int rawt52;    /*   */
	int rawt53;    /*   */
	int rawt54;    /*   */
	int rawt55;    /*   */
	int rawt56;    /*   */
	int rawt57;    /*   */
	int rawt58;    /*   */
	int rawt59;    /*   */
	int rawt60;    /*   */
	int rawt61;    /*   */
	int notused62;    /*   */
	int notused63;    /*   */
	int notused64;    /*   */
	int bnk1t1;    /*  */
	int bnk1t2;    /*  */
	int bnk1t3;    /*  */
	int bnk1t4;    /*  */
	int bnk1t5;    /*  */
	int bnk1t6;    /*  */
	int bnk1t7;    /*  */
	int bnk1t8;    /*  */
	int bnk1t9;    /*  */
	int bnk1t10;    /*  */
	int bnk1t11;    /*  */
	int bnk1t12;    /*  */
	int bnk1t13;    /*  */
	int bnk1t14;    /*  */
	int bnk1t15;    /*  */
	int bnk1t16;    /*  */
	int bnk1t17;    /*  */
	int bnk1t18;    /*  */
	int bnk1t19;    /*  */
	int bnk1t20;    /*  */
	int bnk1t21;    /*  */
	int bnk1t22;    /*  */
	int bnk1t23;    /*  */
	int bnk1t24;    /*  */
	int bnk1t25;    /*  */
	int bnk1t26;    /*  */
	int bnk1t27;    /*  */
	int bnk1t28;    /*  */
	int bnk1t29;    /*  */
	int bnk1t30;    /*  */
	int bnk1t31;    /*  */
	int bnk1t32;    /*  */
	int bnk1t33;    /*  */
	int bnk1t34;    /*  */
	int bnk1t35;    /*  */
	int bnk1t36;    /*  */
	int bnk1t37;    /*  */
	int bnk1t38;    /*  */
	int bnk1t39;    /*  */
	int bnk1t40;    /*  */
	int bnk1t41;    /*  */
	int bnk1t42;    /*  */
	int bnk1t43;    /*  */
	int bnk1t44;    /*  */
	int bnk1t45;    /*  */
	int bnk1t46;    /*  */
	int bnk1t47;    /*  */
	int bnk1t48;    /*  */
	int bnk1t49;    /*  */
	int bnk1t50;    /*  */
	int bnk1t51;    /*  */
	int bnk1t52;    /*  */
	int bnk1t53;    /*  */
	int bnk1t54;    /*  */
	int bnk1t55;    /*  */
	int bnk1t56;    /*  */
	int bnk1t57;    /*  */
	int bnk1t58;    /*  */
	int bnk1t59;    /*  */
	int bnk1t60;    /*  */
	int bnk1t61;    /*  */
	int notused126;    /*  */
	int notused127;    /*  */
	int notused128;    /*  */
	int bnk2t1;    /*  */
	int bnk2t2;    /*  */
	int bnk2t3;    /*  */
	int bnk2t4;    /*  */
	int bnk2t5;    /*  */
	int bnk2t6;    /*  */
	int bnk2t7;    /*  */
	int bnk2t8;    /*  */
	int bnk2t9;    /*  */
	int bnk2t10;    /*  */
	int bnk2t11;    /*  */
	int bnk2t12;    /*  */
	int bnk2t13;    /*  */
	int bnk2t14;    /*  */
	int bnk2t15;    /*  */
	int bnk2t16;    /*  */
	int bnk2t17;    /*  */
	int bnk2t18;    /*  */
	int bnk2t19;    /*  */
	int bnk2t20;    /*  */
	int bnk2t21;    /*  */
	int bnk2t22;    /*  */
	int bnk2t23;    /*  */
	int bnk2t24;    /*  */
	int bnk2t25;    /*  */
	int bnk2t26;    /*  */
	int bnk2t27;    /*  */
	int bnk2t28;    /*  */
	int bnk2t29;    /*  */
	int bnk2t30;    /*  */
	int bnk2t31;    /*  */
	int bnk2t32;    /*  */
	int bnk2t33;    /*  */
	int bnk2t34;    /*  */
	int bnk2t35;    /*  */
	int bnk2t36;    /*  */
	int bnk2t37;    /*  */
	int bnk2t38;    /*  */
	int bnk2t39;    /*  */
	int bnk2t40;    /*  */
	int bnk2t41;    /*  */
	int bnk2t42;    /*  */
	int bnk2t43;    /*  */
	int bnk2t44;    /*  */
	int bnk2t45;    /*  */
	int bnk2t46;    /*  */
	int bnk2t47;    /*  */
	int bnk2t48;    /*  */
	int bnk2t49;    /*  */
	int bnk2t50;    /*  */
	int bnk2t51;    /*  */
	int bnk2t52;    /*  */
	int bnk2t53;    /*  */
	int bnk2t54;    /*  */
	int bnk2t55;    /*  */
	int bnk2t56;    /*  */
	int bnk2t57;    /*  */
	int bnk2t58;    /*  */
	int bnk2t59;    /*  */
	int bnk2t60;    /*  */
	int bnk2t61;    /*  */
	int notused190;    /*  */
	int notused191;    /*  */
	int notused192;    /*  */
} tgs_t;

typedef struct {
	bankHeader_t bank;
	tgs_t tgs[1];
} clasTGS_t;

/* --------------------- END tgs -----------------*/

/* ------------------------ tlv1 -----------------*/
typedef struct {
	int id;    /*  id = Detector * 256 + channel */


























	float time;    /*  time(ns)  */
} tlv1_t;

typedef struct {
	bankHeader_t bank;
	tlv1_t tlv1[1];
} clasTLV1_t;

/* --------------------- END tlv1 -----------------*/

/* ------------------------ trcf -----------------*/
typedef struct {
	char trigcfg;    /*  Trigger config file */
} trcf_t;

typedef struct {
	bankHeader_t bank;
	trcf_t trcf[1];
} clasTRCF_t;

/* --------------------- END trcf -----------------*/

/* ------------------------ trgs -----------------*/
typedef struct {
	int clock_ug;    /*  Clock ungated */
	int fcup_ug;    /*  FCUP ungated */
	int microsec;    /*  Microsecond clock (will overflow during normal run) */
	int random_ug;    /*  Random pulser ungated */
	int mor_st;    /*  MOR.ST */
	int mor_pc;    /*  MOR.PC */
	int mor_ps;    /*  MOR.PS */
	int mor_tac;    /*  MOR.TAC */
	int mor;    /*  Master OR */
	int pc;    /*  Pair Counter */
	int ps;    /*  Pair Spectrometer */
	int tac;    /*  Total Absorption Counter */
	int st;    /*  ST */
	int hel_sync;    /*  Helicity sync signal */
	int clock_ug_2;    /*  Duplicate of channel 1 */
	int fcup_ug_2;    /*  Duplicate of channel 2 */
	int clock_g1;    /*  Clock with run gatei */
	int fcup_g1;    /*  FCUP with Run Gate */
	int notused19;    /*  Currently not used */
	int random_g1;    /*  Random pulser run gated */
	int mor_st_rg;    /*  MOR.ST  with run gate */
	int mor_pc_rg;    /*  MOR.PC with run gate */
	int mor_ps_rg;    /*  MOR.PS with run gate */
	int mor_tac_rg;    /*  MOR.TAC with run gate */
	int mor_rg;    /*  MASTER_OR with run gate */
	int pc_rg;    /*  PC with run gate */
	int ps_rg;    /*  PS with run gate */
	int tac_rg;    /*  TAC with run gate */
	int st_rg;    /*  ST  with run gate */
	int random_g1_2;    /*  duplicate of channel 20 */
	int clock_g1_2;    /*  duplicate of channel 17 */
	int fcup_g1_2;    /*  duplicate of channel 18 */
	int clock_g2;    /*  CLOCK live gated */
	int fcup_g2;    /*  FCUP live gated */
	int trig_or_g2;    /*  Trigger OR of 1-12 live gated */
	int random_g2;    /*  Random pulser live gated */
	int notused37;    /*  Currently not used */
	int notused38;    /*  Currently not used */
	int notused39;    /*  Currently not used */
	int notused40;    /*  Currently not used */
	int mor_lg;    /*  MASTER_OR live gated */
	int notused42;    /*  Currently not used */
	int notused43;    /*  Currently not used */
	int notused44;    /*  Currently not used */
	int notused45;    /*  Currently not used */
	int random_g2_2;    /*  duplicate of channel 36 */
	int clock_g2_2;    /*  duplicate of channel 33 */
	int fcup_g2_2;    /*  duplicate of channel 34 */
	int trig1_ug;    /*  Trigger 1 ungated, prescaled */
	int trig2_ug;    /*  Trigger 2 ungated, prescaled */
	int trig3_ug;    /*  Trigger 3 ungated, prescaled */
	int trig4_ug;    /*  Trigger 4 ungated, prescaled */
	int trig5_ug;    /*  Trigger 5 ungated, prescaled */
	int trig6_ug;    /*  Trigger 6 ungated, prescaled */
	int trig7_ug;    /*  Trigger 7 ungated, prescaled */
	int trig8_ug;    /*  Trigger 8 ungated, prescaled */
	int trig9_ug;    /*  Trigger 9 ungated, prescaled */
	int trig10_ug;    /*  Trigger 10 ungated, prescaled */
	int trig11_ug;    /*  Trigger 11 ungated, prescaled */
	int trig12_ug;    /*  Trigger 12 ungated, prescaled */
	int trig_or_ug;    /*  Trigger OR of 1-12 ungated, */
	int l1accept;    /*  Level 1 accept */
	int notused63;    /*  Currently not used */
	int notused64;    /*  Currently not used */
	int l2fail;    /*  Level2 fail */
	int l2pass;    /*  Level2 pass */
	int l2start;    /*  Level2 start */
	int l2clear;    /*  Level2 clear */
	int l2accept;    /*  Level2 accept */
	int l3accept;    /*  Level3 accept */
	int notused71;    /*  Currently not used */
	int notused72;    /*  Currently not used */
	int l2sec1_g;    /*  Level2 sec1 gated */
	int l2sec2_g;    /*  Level2 sec2 gated */
	int l2sec3_g;    /*  Level2 sec3 gated */
	int l2sec4_g;    /*  Level2 sec4 gated */
	int l2sec5_g;    /*  Level2 sec5 gated */
	int l2sec6_g;    /*  Level2 sec6 gated */
	int l2_or_g;    /*  OR level2 gated */
	int l2_ok_g;    /*  Level 2 OK gated */
	int trig1_lg;    /*  Trigger 1 live gated */
	int trig2_lg;    /*  Trigger 2 live gated */
	int trig3_lg;    /*  Trigger 3 live gated */
	int trig4_lg;    /*  Trigger 4 live gated */
	int trig5_lg;    /*  Trigger 5 live gated */
	int trig6_lg;    /*  Trigger 6 live gated */
	int trig7_lg;    /*  Trigger 7 live gated */
	int trig8_lg;    /*  Trigger 8 live gated */
	int trig9_lg;    /*  Trigger 9 live gated */
	int trig10_lg;    /*  Trigger 10 live gated */
	int trig11_lg;    /*  Trigger 11 live gated */
	int trig12_lg;    /*  Trigger 12 live gated */
	int notused93;    /*  not used */
	int notused94;    /*  not used */
	int ignore95;    /*  ignore */
	int ignore96;    /*  ignore */
} trgs_t;

typedef struct {
	bankHeader_t bank;
	trgs_t trgs[1];
} clasTRGS_t;

/* --------------------- END trgs -----------------*/

/* ------------------------ trks -----------------*/
typedef struct {
	int trk_lev;    /*  Tracknr + analysis_level*100 */
	float beta;    /*  Beta of the track in unit of c */
	float st_time;    /*  Start time of the event (track: photon trigg) */
	float cc_time;    /*  CC calibrated time for this track (ns) */
	float sc_time;    /*  SC calibrated time for this track (ns) */
	float ec_time;    /*  EC calibrated time for this track (ns) */
	int st_id;    /*  ST hit id (ptr. to STx: photon trigg) */
	int cc_id;    /*  CC hit id (ptr. to CCRC) */
	int sc_id;    /*  SC hit id (ptr. to SCRW) */
	int ec_id;    /*  EC Cluster-id (ptr. to ECHB) */
} trks_t;

typedef struct {
	bankHeader_t bank;
	trks_t trks[1];
} clasTRKS_t;

/* --------------------- END trks -----------------*/

/* ------------------------ trl1 -----------------*/
typedef struct {
	int fit_flags;    /*  flags used in track fitting */
	int hits_hbt;    /*  hits used in HBT fit */
	int hits_tbt;    /*  hits used in TBT fit */
	float x;    /*  x */
	float y;    /*  y   position in first DC layer  */
	float z;    /*  z */
	float cx;    /*  cx */
	float cy;    /*  cy  dir.cosine at first DC layer */
	float cz;    /*  cz */
	float tlen;    /*  track length to this layer (starting */
} trl1_t;

typedef struct {
	bankHeader_t bank;
	trl1_t trl1[1];
} clasTRL1_t;

/* --------------------- END trl1 -----------------*/

/* ------------------------ trpb -----------------*/
typedef struct {
	int hits_tbt;    /*  hits used in TBT fit */
	float x;    /*  x */
	float y;    /*  y   position in first DC layer  */
	float z;    /*  z */
	float cx;    /*  cx */
	float cy;    /*  cy  dir.cosine at first DC layer */
	float cz;    /*  cz */
	float tlen;    /*  track length to this layer (starting */
} trpb_t;

typedef struct {
	bankHeader_t bank;
	trpb_t trpb[1];
} clasTRPB_t;

/* --------------------- END trpb -----------------*/

/* ------------------------ tspr -----------------*/
typedef struct {
	uint32 mem;    /*  memory value */
} tspr_t;

typedef struct {
	bankHeader_t bank;
	tspr_t tspr[1];
} clasTSPR_t;

/* --------------------- END tspr -----------------*/

/* ------------------------ tsrg -----------------*/
typedef struct {
	uint32 reg;    /*  register value */
} tsrg_t;

typedef struct {
	bankHeader_t bank;
	tsrg_t tsrg[1];
} clasTSRG_t;

/* --------------------- END tsrg -----------------*/

/* ------------------------ unus -----------------*/
typedef struct {
	int ndcun;    /*  Number of unused DC tracks */
	int idcun;    /*  Trek ID   */
	int nscun;    /*  Number of unused SC hits */
	int iscun;    /*  SC hit ID */
	int nccun;    /*  Number of unused CC hits */
	int iccun;    /*  CC hit ID */
	int necun;    /*  Number of unused EC hits */
	int iecun;    /*  EC hit ID */
} unus_t;

typedef struct {
	bankHeader_t bank;
	unus_t unus[1];
} clasUNUS_t;

/* --------------------- END unus -----------------*/

/* ------------------------ vert -----------------*/
typedef struct {
	int vertex;    /*  vertex id */
	int trk1;    /*  track #1 */
	int trk2;    /*  track #2 */
	float x;    /*  x vector3_t vert{x,y,z} */
	float y;    /*  y  */
	float z;    /*  z */
	float sepd;    /*  seperation distance */
} vert_t;

typedef struct {
	bankHeader_t bank;
	vert_t vert[1];
} clasVERT_t;

/* --------------------- END vert -----------------*/


#endif

/* end of file */
