#include "physEVNT.h"


extern RunControl* PhyAna ;


physEVNT_c::physEVNT_c() {
  int n_elec ;
  float Epr, nu, teta, vieh; 
  TLorentzVector Elec ;
  
  Target.SetXYZT(0., 0., 0., PROTON_MASS );
  Beam.SetXYZT( 0., 0., PhyAna->GetBeamEnergy() ,PhyAna->GetBeamEnergy() );
 
  // World = &(PhyAna->World);
  BField = PhyAna->GetBField();
  
  nparts = 0;
  
  Event = NULL;
  Event = (clasEVNT_t *) getBank(&bcs_,"EVNT"); 
  Hevt = NULL ;
  Hevt = (clasHEVT_t *) getBank(&bcs_,"HEVT");
  SCPB = NULL;
  SCPB = (clasSCPB_t *) getBank(&bcs_,"SCPB");
  ECPB = NULL;
  ECPB = (clasECPB_t *) getBank(&bcs_,"ECPB");
  DCPB = NULL;
  DCPB = (clasDCPB_t *) getBank(&bcs_,"DCPB");
  
  
  if ( ( Event == NULL ) || ( Hevt == NULL ) )  {
      StatusFlag = 0;
      return  ;
  }
  else StatusFlag = 1 ;
  
  
  n_elec = Electron_ID();
  if ( n_elec > 0 ) {
    Proton_ID();
    Neutron_ID();
    Pi_Plus_ID();
    Pi_Minus_ID();
    K_Plus_ID();
    K_Minus_ID(); 
    Deutron_ID();   
    
    Elec = GetPart(11,1 );
    
    Epr  = Elec.E();
    
    teta = Elec.Theta();
    vieh = Elec.Phi();
    
    nu = Beam.E() - Epr; 
    
    Q2  = 4. * Epr * Beam.E() * sin(teta/2) * sin(teta/2);
    W   = sqrt(PROTON_MASS*PROTON_MASS + 2.*PROTON_MASS*( Beam.E()- Epr) - Q2);
    Eps = 1./(1.+2.*((nu*nu + Q2)/Q2)*
	      tan(teta/2.)*tan(teta/2.));
    Gamma = (ALPHA/(2.*acos(-1.)*acos(-1.)))*(Epr/Beam.E())*
      ((W*W - PROTON_MASS*PROTON_MASS)/(2.*PROTON_MASS*Q2))
      *(1./(1.-Eps));
  }
}


physEVNT_c::~physEVNT_c() {
}


void physEVNT_c::Insert(int Pid, int evnt_ind) {
  parts[nparts].part_id  = Pid;
  parts[nparts].part_ind = evnt_ind;
  nparts++;
}



int physEVNT_c::GetNpart() {
 return nparts;
}



int physEVNT_c::GetNpartID(int Pid) {
  int ipart;
  int pcount = 0;
  
  for(ipart=0;ipart<nparts;ipart++) {
    if(parts[ipart].part_id == Pid) pcount++;
  }
  return pcount;
}



int physEVNT_c::IsOK() {
 return StatusFlag ;
}


TLorentzVector physEVNT_c::GetPart(int Pid, int wich ) {
  int ip = 0;
  int pcount =  0;
  int cind;
  TLorentzVector pvec;

  while( (ip<nparts) && (pcount!=wich) ) {
    if(parts[ip].part_id == Pid) pcount++;
    ip++;
  }

  if( pcount==wich ) {

    cind  = parts[ip-1].part_ind;
 
    if( ( Event != NULL ) && ( cind < Event->bank.nrow ) ) {

      TVector3 u_dir(Event->evnt[cind].dir_cos.x,
		     Event->evnt[cind].dir_cos.y,
		     Event->evnt[cind].dir_cos.z);
      pvec.SetVectM( Event->evnt[cind].pmom * u_dir, Mass_Of_ID(Pid) );

    } 
  }
  return pvec; 
}


TLorentzVector physEVNT_c::GetPart(int Pid, int wich , const char* what ) {
  int ip = 0;
  int pcount =  0;
  int cind;
  TLorentzVector pvec;

  while( (ip<nparts) && (pcount!=wich) ) {
    if(parts[ip].part_id == Pid) pcount++;
    ip++;
  }
  
  if( pcount==wich ) {
    
    cind  = parts[ip-1].part_ind;
    
    if( ( Event != NULL ) && ( cind < Event->bank.nrow ) ) {
      switch  ( what[0] ) {
      case 'p':
	{
	  TVector3 u_dir(Event->evnt[cind].dir_cos.x,
			 Event->evnt[cind].dir_cos.y,
		       Event->evnt[cind].dir_cos.z);
	  pvec.SetVectM( Event->evnt[cind].pmom * u_dir, Mass_Of_ID(Pid) );
	}
	break;
      case 'v':
	pvec.SetXYZT(Event->evnt[cind].vert.x,
		     Event->evnt[cind].vert.y,
		     Event->evnt[cind].vert.z,
		     Hevt->hevt[0].stt) ;
	break;    
      default :
      break;
      }                    
    } 
  } 
  return pvec; 
}



void physEVNT_c::Reset() {
  nparts = 0;
}


void physEVNT_c::Print() {
  int ipart;
  cout << "________________________________________________________" << endl;
  for(ipart = 0; ipart < nparts; ipart++) {
    cout << "[" << ipart << "]" << "ID = " << parts[ipart].part_id 
	 << "  IND = " << parts[ipart].part_ind << endl ;
  }
}



int physEVNT_c::GetInv(float *iW, float *iQ2, float *iEps, float *iGamma) {
  *iW     = W;
  *iQ2    = Q2;
  *iEps   = Eps;
  *iGamma = Gamma;
  return 1;
}


float physEVNT_c::GetECEnergy( int PID, int which ) {
  int ip = 0;
  int pcount = 0;
  float Energy_EC = 0. ;
  
  while( (ip<nparts) && (pcount!=which) )  {
    if(parts[ip].part_id == PID) pcount++;
    ip++;
  }
  
  if( pcount == which ) {
    int cind  = parts[ip-1].part_ind;
    int indEC = Event->evnt[cind].ecstat - 1;
    if ( indEC >= 0 )
      Energy_EC = ECPB->ecpb[indEC].etot ;
  }
  return Energy_EC ;
}




int physEVNT_c::BuildEVNT(  ) {
  int n_elec ;
  float Epr, nu, teta, vieh; 
  TLorentzVector Elec ;
  
  Event = NULL;
  Event = (clasEVNT_t *) getBank(&bcs_,"EVNT"); 
  Hevt = NULL ;
  Hevt = (clasHEVT_t *) getBank(&bcs_,"HEVT");
  SCPB = NULL;
  SCPB = (clasSCPB_t *) getBank(&bcs_,"SCPB");
  ECPB = NULL;
  ECPB = (clasECPB_t *) getBank(&bcs_,"ECPB");
  DCPB = NULL;
  DCPB = (clasDCPB_t *) getBank(&bcs_,"DCPB");
  
  
  if ( ( Event == NULL ) || ( Hevt == NULL ) ) return -1 ;
  
  Target.SetXYZT(0., 0., 0., PROTON_MASS );
  Beam.SetXYZT( 0., 0., PhyAna->GetBeamEnergy() ,PhyAna->GetBeamEnergy() );
  
  // World = &(PhyAna->World);
  BField = PhyAna->GetBField();

  n_elec = Electron_ID();
  if ( n_elec > 0 )  {
    Proton_ID();
    Neutron_ID();
    Pi_Plus_ID();
    Pi_Minus_ID();
    K_Plus_ID();
    K_Minus_ID();    
    
    Elec = GetPart(11,1 );
    
    Epr = Elec.E();
  
    teta = Elec.Theta();
    vieh = Elec.Phi();
    
    nu = Beam.E() - Epr; 
    
    Q2  = 4. * Epr * Beam.E() * sin(teta/2) * sin(teta/2);
    W   = sqrt(PROTON_MASS*PROTON_MASS + 2.*PROTON_MASS*( Beam.E()- Epr) - Q2);
    Eps = 1./(1.+2.*((nu*nu + Q2)/Q2)*
	      tan(teta/2.)*tan(teta/2.));
    Gamma = (ALPHA/(2.*acos(-1.)*acos(-1.)))*(Epr/Beam.E())*
	      ((W*W - PROTON_MASS*PROTON_MASS)/(2.*PROTON_MASS*Q2))
      *(1./(1.-Eps));
    return 0;	     
  }
  return -2;
}


float physEVNT_c::GetBField() {
  return BField;
}


int physEVNT_c::Electron_ID() {
  int irow ;
  int nelectron ;
  
  nelectron = 0;
  for(irow = 0; irow < Event->bank.nrow; irow++) {
    if(Event->evnt[irow].id == 11) {
      Insert(11,irow);
      nelectron++;
    }
  } 
  return nelectron;
}


int physEVNT_c::Proton_ID() {
  int irow ;
  int nproton ;
  
  nproton = 0;
  for(irow = 0; irow < Event->bank.nrow; irow++) {
    if(Event->evnt[irow].id == 2212) {
      Insert(2212,irow);
      nproton++;
    }
  } 
  return nproton;
}


int physEVNT_c::Neutron_ID() {
  int irow ;
  int nneutron ;
  
  nneutron = 0;
  for(irow = 0; irow < Event->bank.nrow; irow++) {
    if(Event->evnt[irow].id == 2112) {
      Insert(2112,irow);
      nneutron++;
    }
  } 
  return nneutron;
}


int physEVNT_c::Pi_Plus_ID() {
  int irow ;
  int npiplus ;
  
  const float c_light = 29.98;
 
  npiplus = 0;
  
  
  if ( Event->evnt[0].id != 11 || Event->evnt[0].scstat < 1 ) return npiplus;
  
  float TDC_el = SCPB->scpb[Event->evnt[0].scstat-1].time ;
  float Path_el = SCPB->scpb[Event->evnt[0].scstat-1].path ; 
  float TOF_el =  Path_el / c_light ;
  float T_start = TDC_el - TOF_el ;
  
  
  for(irow = 1; irow < Event->bank.nrow; irow++) { 
    
    if ( Event->evnt[irow].dcstat > 0 && 			// particle hit DC
         Event->evnt[irow].scstat > 0 &&			// particle hit SC
         Event->evnt[irow].charge > 0. &&
         DCPB->dcpb[Event->evnt[irow].dcstat-1].status > 0 &&	// TBD check
         Event->evnt[irow].pmom > 0.02 				// 20 MeV cutoff
	 )	 							
      {     
	float TDC = SCPB->scpb[Event->evnt[irow].scstat-1].time ;
	float Path = SCPB->scpb[Event->evnt[irow].scstat-1].path ; 
	float p_pi = Event->evnt[irow].pmom ;
	float Beta_pi = 1. / sqrt( 1. + pow( ( PI_CH_MASS / p_pi ), 2) ) ;
	float Beta_k  = 1. / sqrt( 1. + pow( ( KA_CH_MASS / p_pi ), 2) ) ;
	float TOF = TDC - T_start ;
	float Beta_part = Path / ( TOF * c_light );
	if ( fabs( Beta_part - Beta_pi )  <= fabs( Beta_part - Beta_k ) ) {
	  Insert(211,irow);
	  npiplus++;
	}  
      }
    
    /* 
       if( 
       ( Event->evnt[irow].id == 211 ) || 
      ( ( Event->evnt[irow].id == -11 ) && ( Event->evnt[irow].pmom > 0.300 ) ) 
// Make sure that theres no positrons with P > 300 MeV, must be pi plus
    )
  {
        Insert(211,irow);
        npiplus++;
  }
*/    
  } 
  return npiplus;
}


int physEVNT_c::Pi_Minus_ID() {
  int irow ;
  int npiminus ;
  
  npiminus= 0;
  for(irow = 0; irow < Event->bank.nrow; irow++) {
    if(Event->evnt[irow].id == -211) {
      Insert(-211,irow);
      npiminus++;
    }
  } 
 return npiminus;
}


int physEVNT_c::K_Plus_ID() {
  int irow ;
  int nkplus ;
  
  nkplus= 0;
  for(irow = 0; irow < Event->bank.nrow; irow++) {
    if(Event->evnt[irow].id == 321) {
      Insert(321,irow);
      nkplus++;
    }
  } 
  return nkplus;
}


int physEVNT_c::K_Minus_ID() {
  int irow ;
  int nkminus ;
  
  nkminus= 0;
  for(irow = 0; irow < Event->bank.nrow; irow++) {
    if(Event->evnt[irow].id == -321) {
      Insert(-321,irow);
      nkminus++;
    }
  } 
  return nkminus;
}


int physEVNT_c::Deutron_ID() {
  int irow ;
  int ndeutron ;
  
  ndeutron= 0;
  for(irow = 0; irow < Event->bank.nrow; irow++) {
    if(Event->evnt[irow].id == 45) {
      //
      //	Applying DE/DX vs P cut also to separate from protons
      //   
      float p_d = ( float ) Event->evnt[irow].pmom ;
      int sc_pt = (int) Event->evnt[irow].scstat - 1 ;
      float de = 0. ;
      if ( sc_pt >= 0 ) 
	de = (float) SCPB->scpb[sc_pt].edep ;
      if ( ( 0.4 < p_d && p_d < 1.5 )  )
	if ( de > ( 9./( p_d - 0.35 ) +11. ) ) 
	  {
	    Insert(45,irow);
	    ndeutron++;
	  }   
    }
  } 
  return ndeutron;
}



float physEVNT_c::Mass_Of_ID(int Pid) {
  switch ( (Pid) < 0 ? -(Pid) : (Pid) ) {
  case 11:
    return ELECTRON_MASS;
    break;
  case 22:
    return 0. ;
    break;
  case 211:
    return  PI_CH_MASS ;
    break;
  case 111: 
    return PI_N_MASS ;
    break;
  case 321:
    return KA_CH_MASS ;
    break;
  case 311:
    return KA_N_MASS ;
    break; 
  case 2212:
    return PROTON_MASS ;
    break;
  case 2112:
    return NEUTRON_MASS ;
    break;
  case 45:
    return DEUTRON_MASS ;
    break;    
  default:
    return 2.*PROTON_MASS + NEUTRON_MASS ;
   break;
  }
}


#define sqr(x) ((x)*(x))  

int get_scstat(evnt_t *evnt) {
  return evnt->scstat;
} 

float physEVNT_c::Get_Mass_Hadron2(int Pid, int num) {
  int irow;  
  evnt_t *event;
  int sc_index = 0;
  float p = 0;
  float tof_e,tdc_e;
  float edep_h, l_h, tdc_h;
  float tcorr =.1, beta_h, mass_h2 = 0;
  int current_num(0);
  
  //check to make sure there are evnt and scpb banks
  if ((SCPB != NULL) && (Event != NULL)) {
    
    //loop through evnt bank looking for electron
    for(irow = 0; irow< Event->bank.nrow; irow++) {
      event = &(Event->evnt[irow]);
      if (Event->evnt[irow].id == 11) {

	//set index for sc bank from evnt bank, and then get p, tof, tdc for electron
	sc_index = get_scstat(event) - 1;
	p = Event->evnt[irow].pmom;
	tof_e = SCPB->scpb[sc_index].path/30; 
	tdc_e = SCPB->scpb[sc_index].time;    
	for(int j = 1; j < nparts; j ++) {
	  if (Event->evnt[j].id = Pid) { 
	    current_num++;
	    if (current_num == num) {
	      //set index for hadron and then determine mass from p, l_h, tof and tdc for hadron
	      event = &(Event->evnt[j]);
	      sc_index = get_scstat(event) - 1;
	      p = Event->evnt[j].pmom;
	      l_h = SCPB->scpb[sc_index].path;
	      tdc_h = SCPB->scpb[sc_index].time;
	      beta_h = (l_h/(tof_e - tdc_e + tdc_h + tcorr))/30;
	      mass_h2   = (sqr(p/beta_h))*(1. -(sqr(beta_h)));
	      break;
	    }
	  }
	}
      }
    }
  }
  if (mass_h2 == 0) return 50;
  return mass_h2;
}

float physEVNT_c::Get_Mass_Hadron2(int pnum) {
  int irow;  
  evnt_t *event;
  int sc_index = 0;
  float p = 0;
  float tof_e,tdc_e;
  float edep_h, l_h, tdc_h;
  float tcorr =.1, beta_h, mass_h2 = 0;
  
  pnum--; //since c++ arrays start at 0; 
  //check to make sure there are evnt and scpb banks
  if ((SCPB != NULL) && (Event != NULL)) {
    
    //loop through evnt bank looking for electron
    for(irow = 0; irow< Event->bank.nrow; irow++) {
      event = &(Event->evnt[irow]);
      if (Event->evnt[irow].id == 11) {
	
	//set index for sc bank from evnt bank, and then get p, tof, tdc for electron
	sc_index = get_scstat(event) - 1;
	p = Event->evnt[irow].pmom;
	tof_e = SCPB->scpb[sc_index].path/30; 
	tdc_e = SCPB->scpb[sc_index].time;    
	if (pnum < nparts) {
	  
	  //set index for hadron and then determine mass from p, l_h, tof and tdc for hadron;
	  event = &(Event->evnt[pnum]);
	  sc_index = get_scstat(event) - 1;
	  p = Event->evnt[pnum].pmom;
	  l_h = SCPB->scpb[sc_index].path;
	  tdc_h = SCPB->scpb[sc_index].time;
	  beta_h = (l_h/(tof_e - tdc_e + tdc_h + tcorr))/30;
	  mass_h2   = (sqr(p/beta_h))*(1. -(sqr(beta_h)));
	}
      }
    }
  }
  if (mass_h2 == 0) {return 50;}
  return mass_h2; 
}
