#include "BOS_Interface.h"
#include <cmath>
//_____________________________________________________________________________

/** @file ListOfBanks.lst 
 *  @brief File Containing lists of required BOS banks
 */
//_____________________________________________________________________________

/** @file BOS_Interface.C
 * @brief BOS file interface to ClasEvent source file.
 */
#include "../include/Constants.h"
//_____________________________________________________________________________

template<> void ClasEvent::SetClasEvent(Bos *__bos){
  /// See SetClasEvent(Bos*,int) for details
  this->SetClasEvent(__bos,-1);
}
//_____________________________________________________________________________

template<> void ClasEvent::SetClasEvent(Bos *__bos,int __runNumber){
  /// Bos specification of ClasEvent::SetClasEvent.
  /**
   * @param data Bos pointer (should allready be storing desired event)
   * @param runNumber CLAS run number (defaults to -1)
   * 
   *  If @a runNumber is not specified, then the run number is obtained from 
   *  the file. The run number should be passed in for files where the stored
   *  run number isn't a valid run number for the CLAS run period. For example,
   *  g11a monte carlo (as of when this was written) uses a run number in the
   *  range 1-10 (this is needed for the new start counter code to work 
   *  properly). Since these aren't valid g11a run numbers, the code will fail
   *  to obtain some g11a quantities (such as target offset) from ClasRuns.
   * 
   *  Part of the @a BOS file interface with ClasEvent.
   *  This function should be called once, after a new event has been loaded 
   *  into memory for @a Bos, to intialize ClasEvent for the new event.
   * 
   *  Gets the run/event number directly from the file. Sets @a _vertex to be
   *  the @a MVRT vertex. The event vertex time is set to be @a stt when using
   *  the @a seb id scheme and @a vtime otherwise.
   *  See Index::SetIndex(Bos*,float,float) for more details.
   *
   *  Note: This function calls Bos::BankCheck, if any banks are missing then
   *        the event is set to have no good combos of particles and photons.
   *
   *  <b> Example Usage: </b>
   *
   *  \include ClasEvent_SetClasEvent_Bos.ex
   *
   * 
   */
  head_t head = ((clasHEAD_t*)__bos->GetBank("HEAD"))->head[0];

  if(__runNumber > 0) this->SetRunNumber(__runNumber);
  else
    this->SetRunNumber(head.nrun);
  this->SetEventNumber(head.nevent);

  // check if this is monte carlo
  if(head.type < 0) this->SetIsMC(true);
  else this->SetIsMC(false);

  if(!__bos->BankCheck()){
    // some banks are missing, skip this event
    _index.Reset();
    return;
  }

  mvrt_t mvrt = ((clasMVRT_t*)__bos->GetBank("MVRT"))->mvrt[0];
  this->SetVertex(TVector3(mvrt.vert.x,mvrt.vert.y,mvrt.vert.z));

  float vtime;
  if(this->IdScheme() == "seb")
    vtime = ((clasHEVT_t*)__bos->GetBank("HEVT"))->hevt->stt;
  else
    vtime = ((clasTBID_t*)__bos->GetBank("TBID"))->tbid[0].vtime;
  this->SetVertexTime(vtime);

  _chi2 = 666.;
  _ndf = 0;
  float targOffset = this->GetRunPeriod().TargetOffset();
  _index.SetIndex(__bos,targOffset,vtime);
}
//_____________________________________________________________________________

template<> bool ClasEvent::GetEvent(Bos *__bos){

  /// Bos specification of ClasEvent::GetEvent.
  /**
   *
   *  Part of the @a Bos file interface with ClasEvent.
   *  This function gets the next combination of detected charged particles and
   *  tagged photons. Returns @a true if a combo was found and @a false 
   *  otherwise.
   *
   *  See Index::SetParticles(Bos*,ParticleArray<Particle>&,float) and
   *  and Index::SetPhoton(Bos*,Photon&,float) for details on setting
   *  particle and tagged photon info. See ClasEvent::_GetCovMatTrack for 
   *  details on how the tracking covariance matrix is set up.
   *
   *  Note: This function also resets the particle names if they've been
   *  changed.
   *
   *  <b>Example Usage: </b>
   *
   *  \include ClasEvent_SetClasEvent_Bos.ex
   *
   */
  if(!_index.IsValid()) return false;

  // reset particles name (in case they've been changed)
  this->ResetParticleNames();
  _momentumCor = false; // no momentum corrections on this combo
  if(_isMC) _momentumCor = true; // mc shouldn't need momentum corrections

  _index.SetParticles(__bos,this->_particles,this->_ignored_particles,
		      _vertexTime);
  double targOffset = this->GetRunPeriod().TargetOffset();
  _index.SetPhoton(__bos,this->_photon,targOffset);
  // set up the tracking covariance matrix
  this->_GetCovMatTrack(__bos);
  _index++;
  return true;
}
//_____________________________________________________________________________

template<> void ClasEvent::_GetCovMatTrack(Bos *__bos){
  /// Get the tracking covariance matrix from a BOS file.
  /** Part of the @a BOS file interface with ClasEvent*/
  std::vector<int> inds = _index.GetIndicies();
  int data_ind,q;
  int dim = 3*_particles.Size()+1;
  _covMatTrack.ResizeTo(dim,dim);
  _covMatTrack.Zero();
  // get tagged photon resolution
  _covMatTrack(0,0) = pow(0.001*(this->GetRunPeriod().BeamEnergy()),2)/3.; 
  double p_mag;
  tber_t *tber = ((clasTBER_t*)__bos->GetBank("TBER"))->tber;

  for(int i = 0; i < _particles.Size(); i++){
    // now get detected particle tracking errors
    if(_index.IdScheme() == "seb"){
      data_ind = ((clasEVNT_t*)__bos->GetBank("EVNT"))->evnt[inds[i]].dcstat-1;
      data_ind = ((clasDCPB_t*)__bos->GetBank("DCPB"))->dcpb[data_ind].status-1;
    }
    else {
      data_ind = ((clasPART_t*)__bos->GetBank("PART"))->part[inds[i]].trkid-1;
      data_ind = ((clasTBID_t*)__bos->GetBank("TBID"))->tbid[data_ind].track-1;
    }

    p_mag = _particles[i].P4().P();
    q = _particles[i].Q();
    
    _covMatTrack(3*i+1,3*i+1) = tber[data_ind].c11*pow(p_mag,4);
    _covMatTrack(3*i+2,3*i+2) = tber[data_ind].c22;
    _covMatTrack(3*i+3,3*i+3) = tber[data_ind].c33;
    _covMatTrack(3*i+1,3*i+2) = -(tber[data_ind].c12)*q*p_mag*p_mag;
    _covMatTrack(3*i+2,3*i+1) = _covMatTrack(3*i+1,3*i+2);
    _covMatTrack(3*i+1,3*i+3) = -(tber[data_ind].c13)*q*p_mag*p_mag;
    _covMatTrack(3*i+3,3*i+1) = _covMatTrack(3*i+1,3*i+3);
    _covMatTrack(3*i+2,3*i+3) = tber[data_ind].c23;
    _covMatTrack(3*i+3,3*i+2) = _covMatTrack(3*i+2,3*i+3);
  }
}
//_____________________________________________________________________________

template<> bool Index::SetIndex(Bos *__bos,float __targOffset,float __vtime){
  
  /// Bos specification of Index::SetIndex.
  /**
   * @param data Pointer to a Bos object
   * @param targOffset Target offset from CLAS center
   * @param vtime Event start time
   *
   *  Part of the @a BOS file interface with ClasEvent.
   *  This function intializes the Index object for the current event. It is
   *  called by ClasEvent::SetClasEvent.
   *
   */
  int i;
  this->Reset();  // reset for the new event

  int num_part;
  if(this->IdScheme() == "seb"){
    num_part = ((clasEVNT_t*)__bos->GetBank("EVNT"))->bank.nrow;
  }
  else{
    num_part = ((clasPART_t*)__bos->GetBank("PART"))->bank.nrow;
  }
  if(num_part < _particles.Size()) return false;
  
  // get indicies for ALL good photons
  /** Any tagged photon with \f$| t_{\gamma}^{vertex} - vtime| < dt \f$
   *  is considered @a good, and is its index in the Bos object is added
   *  to @a _photIndex. Here, 
   *  \f$ t_{\gamma}^{vertex} = Tpho + (V_z - targOffset)/c \f$ and 
   *  \f$ V_z = MVRT.z \f$. If there are no @a good photons, the function
   *  returns @a false.
   * 
   */
  double photVtime;
  double vertCor = (((clasMVRT_t*)__bos->GetBank("MVRT"))->mvrt->vert.z  
		    - __targOffset)/__c_cm_per_ns__;
  int ntagr = ((clasTAGR_t*)__bos->GetBank("TAGR"))->bank.nrow;
  int stat;
  for(i = 0; i < ntagr; i++){
    stat = ((clasTAGR_t*)__bos->GetBank("TAGR"))->tagr[i].stat;    
    photVtime = ((clasTAGR_t*)__bos->GetBank("TAGR"))->tagr[i].tpho + vertCor;
    if(std::abs(photVtime - __vtime) < _dt && (stat == 15 || stat == 7)) 
      _photIndex.push_back(i); // add it
  }
  if(this->NumPhotons() == 0) return false;
  
  // now get charged particle indicies
  /**
   * Next it gets charged particle indicies. It loops over all detected charged
   * particles stored in @a data. If we're using @a seb, then indicies will be
   * to EVNT, otherwise it's PART. For each of these particles, it
   * loops over all of @a _particles (what we're looking for). If @a id matches
   * the @a _particles id, this index is added to the @a _particles set of
   * indicies. If the particle we're looking for is @a +,@a - or @a 0 (generic
   * charged particles), then it only checks to see if the charge is correct.
   *
   * It then returns the result of Index::GetIndexCombos.
   *
   */
  std::vector<std::vector<int> > p_index(_particles.Size());
  int pid;
  float q;
  bool add = false;
  for(i = 0; i < num_part; i++){
    if(_id_scheme == "seb"){
      pid = ((clasEVNT_t*)__bos->GetBank("EVNT"))->evnt[i].id;
      q = ((clasEVNT_t*)__bos->GetBank("EVNT"))->evnt[i].charge;
    }
    else{
      pid = ((clasPART_t*)__bos->GetBank("PART"))->part[i].pid;
      q = ((clasPART_t*)__bos->GetBank("PART"))->part[i].q;
    }
    if(q != 0) _numCharged++;

    for(int j = 0; j < _particles.Size(); j++){
      add = false;
      if(_id_scheme == "seb"){
	if(_particles[j].PDG_Id() == pid && pid != 0) add = true;
      }
      else
	if(_particles[j].GEANT_Id() == pid && pid != 0) add = true;
      
      if(!add){
	// check the special generic charged cases
	if(_particles[j].Name() == "+") if(q > 0) add = true;
	else if(_particles[j].Name() == "0") if(q == 0) add = true;
	if(_particles[j].Name() == "-") if(q < 0) add = true;
      }
      if(add) p_index[j].push_back(i);
    }
  }

  bool hasEvent = this->_GetIndexCombos(p_index);

#ifdef __DEBUG__
  // If we're debugging, print out a bunch o' crap    
#endif /* __DEBUG__ */

  return hasEvent;
}
//_____________________________________________________________________________

template<> void Index::SetParticles(Bos *__bos,
				    ParticleArray<Particle> &__parts,
				    ParticleArray<Particle> &__ignored,
				    float __vtime) {
  /// Bos specification of Index::SetParticles.
  /**
   * @param data Bos pointer
   * @param parts Array of Particle objects to be set
   * @param ignored Array of Particle objects to be set (ignored particles)
   * @param vtime Event start time
   *
   *  Part of the @a BOS file interface with ClasEvent.
   *  This function sets all detected charged particle info.
   *
   *  All info is taken straight from the Bos object except the particle's
   *  mass (in the energy component of the 4-momentum) which is taken to be
   *  the mass of the global particle that Index was looking for (ex. if 
   *  the 1st entry in Index::_particles is @a pi+, then it uses 0.13957 as the
   *  mass) and the calculated mass is \f$ |\vec{p}|/(\beta\gamma) \f$ where
   *  \f$ \beta \f$ = (path length)/(sc time - @a vtime).
   *
   */  
  TLorentzVector p4;
  TVector3 p3;
  int size = _particles.Size();
  std::vector<int> cur_combo = _indexCombos[_currentIndexPointer];
  int which,q;
  String name;
  double mass,sctime,path_len;
  part_t part;
  tbid_t tbid;
  evnt_t evnt;
  dcpb_t dcpb;
  mvrt_t mvrt = ((clasMVRT_t*)__bos->GetBank("MVRT"))->mvrt[0];
  TVector3 vertex(mvrt.vert.x,mvrt.vert.y,mvrt.vert.z);
  int sc_id;
  double p_mag;

  for(int i = 0; i < size; i++){
    name = _particles[i].Name();
    which = _particles[i];
    mass = _particles[i].Mass();

    if(this->IdScheme() == "seb"){
      evnt = ((clasEVNT_t*)__bos->GetBank("EVNT"))->evnt[cur_combo[i]];
      dcpb = ((clasDCPB_t*)__bos->GetBank("DCPB"))->dcpb[evnt.dcstat-1];

      // set the 4-momentum
      p_mag = evnt.pmom;
      p3.SetXYZ(evnt.dir_cos.x*p_mag,evnt.dir_cos.y*p_mag,evnt.dir_cos.z*p_mag);
      p4.SetPxPyPzE(p3.X(),p3.Y(),p3.Z(),sqrt(p3.Mag2() + mass*mass));
      __parts(name,which).SetP4(p4);  
      // set vertex
      __parts(name,which).SetVertex(vertex);
      // set the vtime
      __parts(name,which).SetVertexTime(0); // not sure where to get this?

      // set sector
      __parts(name,which).SetSector(dcpb.sctr/100);
      // set sc id and status flag
      sc_id = 0;
      if(__bos->GetBank("SCPB") && evnt.scstat > 0){
	sc_id = ((clasSCPB_t*)__bos->GetBank("SCPB"))->scpb[evnt.scstat-1].scpdht;
	__parts(name,which).SetSC_Id(sc_id,evnt.scstat);
	// set path length
	__parts(name,which).SetPathLength(((clasSCPB_t*)__bos->GetBank("SCPB"))->scpb[evnt.scstat-1].path);	
	// set time-of-flight
	__parts(name,which).SetTOF(((clasSCPB_t*)__bos->GetBank("SCPB"))->scpb[evnt.scstat-1].time);
      }
      else {
	__parts(name,which).SetSC_Id(0,0);
	__parts(name,which).SetPathLength(0.);
	__parts(name,which).SetTOF(0.);
      }
      // set calculated mass
      __parts(name,which).SetCalcMass(evnt.mass);
    }
    else{
      part = ((clasPART_t*)__bos->GetBank("PART"))->part[cur_combo[i]];
      tbid = ((clasTBID_t*)__bos->GetBank("TBID"))->tbid[part.trkid-1];
      
      // set the 4-momentum
      p3.SetXYZ(part.p.space.x,part.p.space.y,part.p.space.z);
      p4.SetPxPyPzE(p3.X(),p3.Y(),p3.Z(),sqrt(p3.Mag2() + mass*mass));
      __parts(name,which).SetP4(p4);  
      // set vertex
      __parts(name,which).SetVertex(vertex);
      // set the vtime
      __parts(name,which).SetVertexTime(tbid.st_vtime);
      // set sector
      __parts(name,which).SetSector(tbid.sec);
      // set sc id and status flag
      sc_id = 0;
      if(__bos->GetBank("SCRC",tbid.sec) && tbid.sc_stat > 0){
	sc_id = ((clasSCRC_t*)__bos->GetBank("SCRC",tbid.sec))->scrc[tbid.sc_id-1].id;
	__parts(name,which).SetSC_Id(sc_id,tbid.sc_stat);
      }
      else __parts(name,which).SetSC_Id(0,0);
      // set path length
      path_len = 0.;
      if(__bos->GetBank("SCG",tbid.sec) && __bos->GetBank("TDPL",tbid.sec)
	 && sc_id > 0){
	path_len = ((clasTDPL_t*)__bos->GetBank("TDPL",tbid.sec))->tdpl[((clasSCG_t*)__bos->GetBank("SCG",tbid.sec))->scg[sc_id-1].panel + 3].tlen;
      }
      __parts(name,which).SetPathLength(path_len);
      // set time-of-flight
      sctime = tbid.sc_time;
      __parts(name,which).SetTOF(sctime - __vtime);
      // set calculated mass
      __parts(name,which).SetCalcMass(part.qpid);
    }
  }
  
  // now get any ignored particles
  __ignored.Clear();
  int num_parts;
  if(this->IdScheme() == "seb"){
    num_parts = ((clasEVNT_t*)__bos->GetBank("EVNT"))->bank.nrow;
    for(int i = 0; i < num_parts; i++){

      bool used = false;
      for(int j = 0; j < size; j++) {
	if(i == cur_combo[j]) {
	  used = true;
	  break;
	}
      }
      if(used) continue;

      evnt = ((clasEVNT_t*)__bos->GetBank("EVNT"))->evnt[i];
      dcpb = ((clasDCPB_t*)__bos->GetBank("DCPB"))->dcpb[evnt.dcstat-1];
      
      q = evnt.charge;
      if(q > 0) __ignored.Add("+");
      else if(q < 0) __ignored.Add("-");
      else continue; // we only want charged particles here
      int pind = __ignored.Size() - 1;
      
      // set vertex
      __ignored[pind].SetVertex(vertex);
      // set the vtime
      __ignored[pind].SetVertexTime(0);
      
      // set sector
      __ignored[pind].SetSector(dcpb.sctr/100);
      // set sc id and status flag
      sc_id = 0;
      if(__bos->GetBank("SCPB") && evnt.scstat > 0){
	sc_id = ((clasSCPB_t*)__bos->GetBank("SCPB"))->scpb[evnt.scstat-1].scpdht;
	__ignored[pind].SetSC_Id(sc_id,evnt.scstat);
	// set path length
	__ignored[pind].SetPathLength(((clasSCPB_t*)__bos->GetBank("SCPB"))->scpb[evnt.scstat-1].path);	
	// set time-of-flight
	__ignored[pind].SetTOF(((clasSCPB_t*)__bos->GetBank("SCPB"))->scpb[evnt.scstat-1].time);
      }
      else {
	__ignored[pind].SetSC_Id(0,0);
	__ignored[pind].SetPathLength(0.);
	__ignored[pind].SetTOF(0.);
      }
      // set calculated mass
      __ignored[pind].SetCalcMass(evnt.mass);
      
      // set the 4-momentum
      mass = evnt.mass;
      p_mag = evnt.pmom;
      p3.SetXYZ(evnt.dir_cos.x*p_mag,evnt.dir_cos.y*p_mag,evnt.dir_cos.z*p_mag);
      p4.SetPxPyPzE(p3.X(),p3.Y(),p3.Z(),sqrt(p3.Mag2() + mass*mass));
      __ignored[pind].SetP4(p4);  
    }
  }
  else{
    num_parts = ((clasPART_t*)__bos->GetBank("PART"))->bank.nrow;
    for(int i = 0; i < num_parts; i++){

      bool used = false;
      for(int j = 0; j < size; j++) {
	if(i == cur_combo[j]) {
	  used = true;
	  break;
	}
      }
      if(used) continue;
      
      part = ((clasPART_t*)__bos->GetBank("PART"))->part[i];
      tbid = ((clasTBID_t*)__bos->GetBank("TBID"))->tbid[part.trkid-1];
      q = (int)part.q;
      
      if(q > 0) __ignored.Add("+");
      else if(q < 0) __ignored.Add("-");
      else continue; // we only want charged particles here
      int pind = __ignored.Size() - 1;
      
      // set vertex
      __ignored[pind].SetVertex(vertex);
      // set the vtime
      __ignored[pind].SetVertexTime(tbid.st_vtime);
      // set sector
      __ignored[pind].SetSector(tbid.sec);
      // set sc id and status flag
      sc_id = 0;
      if(__bos->GetBank("SCRC",tbid.sec) && tbid.sc_stat > 0){
	sc_id = ((clasSCRC_t*)__bos->GetBank("SCRC",tbid.sec))->scrc[tbid.sc_id-1].id;
	__ignored[pind].SetSC_Id(sc_id,tbid.sc_stat);
      }
      else __ignored[pind].SetSC_Id(0,0);
      // set path length
      path_len = 0.;
      if(__bos->GetBank("SCG",tbid.sec) && __bos->GetBank("TDPL",tbid.sec)
	 && sc_id > 0){
	path_len = ((clasTDPL_t*)__bos->GetBank("TDPL",tbid.sec))->tdpl[((clasSCG_t*)__bos->GetBank("SCG",tbid.sec))->scg[sc_id-1].panel + 3].tlen;
      }
      __ignored[pind].SetPathLength(path_len);
      // set time-of-flight
      sctime = tbid.sc_time;
      __ignored[pind].SetTOF(sctime - __vtime);
      // set calculated mass
      __ignored[pind].SetCalcMass(part.qpid);
      
      // set the 4-momentum
      mass = __ignored[pind].CalcMass();
      p3.SetXYZ(part.p.space.x,part.p.space.y,part.p.space.z);
      p4.SetPxPyPzE(p3.X(),p3.Y(),p3.Z(),sqrt(p3.Mag2() + mass*mass));
      __ignored[pind].SetP4(p4);  
    }
  }  
}
//_____________________________________________________________________________

template<> void Index::SetPhoton(Bos *__bos,Photon &__photon,
				 float __targOffset) const {
  /// Bos specification of Index::SetPhoton.
  /**
   * @param data Bos pointer
   * @param photon Photon object to be set
   * @param targOffset Target offset from the @a CLAS center
   *
   *
   *  Part of the @a BOS file interface with ClasEvent.
   *  This function sets all tagged photon info.
   *
   *  Everything is taken straight from the Bos object except the photon's
   *  vertex time is set to be Tpho + (MVRT.z - @a targOffset)/c.
   *
   */
  tagr_t tagr = ((clasTAGR_t*)__bos->GetBank("TAGR"))->tagr[_photIndex[_currentPhotIndexPointer]];
  double vtime = tagr.tpho
    + (((clasMVRT_t*)__bos->GetBank("MVRT"))->mvrt->vert.z 
       - __targOffset)/__c_cm_per_ns__;
  __photon.SetVertexTime(vtime);
  __photon.SetE_Id(tagr.e_id);
  __photon.SetT_Id(tagr.t_id);
  __photon.SetP4(tagr.erg);  
}
//_____________________________________________________________________________
