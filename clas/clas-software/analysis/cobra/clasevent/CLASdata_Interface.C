#include "CLASdata_Interface.h"
/** @file CLASdata_Interface.C
 * @brief CLASdata (compressed ROOT files) interface to ClasEvent source file.
 */
#include "../include/Constants.h"
//_____________________________________________________________________________

template<> void ClasEvent::SetClasEvent(CLASdata *__data){
  /// See SetClasEvent(CLASdata*,int) for details
  this->SetClasEvent(__data,-1);
}
//_____________________________________________________________________________

template<> void ClasEvent::SetClasEvent(CLASdata *__data,int __runNumber){
  /// CLASdata specification of ClasEvent::SetClasEvent.
  /**
   * @param data CLASdata pointer (should allready be storing desired event)
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
   *  Part of the @a CLASdata (compressed ROOT files) interface with ClasEvent.
   *  This function should be called once, after a new event has been loaded 
   *  into memory for @a CLASdata, to intialize ClasEvent for the new event.
   * 
   *  Gets the run/event number directly from the file. Sets @a _vertex to be
   *  the @a MVRT vertex. The event vertex time is set to be @a stt when using
   *  the @a seb id scheme and @a vtime otherwise.
   *  See Index::SetIndex(CLASdata*,float,float) for more details.
   *
   *  <b> Example Usage: </b>
   *
   *  \include ClasEvent_SetClasEvent_CLASdata.ex
   *
   */
  if(__runNumber > 0)
    this->SetRunNumber(__runNumber);
  else
    this->SetRunNumber(__data->RunNumber());
  this->SetEventNumber(__data->EventNumber());
  this->SetVertex(__data->Vertex());

  // check whether this is monte carlo
  if(__data->Type() < 0) this->SetIsMC(true);
  else this->SetIsMC(false);

  float vtime;
  if(this->IdScheme() == "seb") vtime = __data->STT();
  else vtime = __data->Vtime();
  this->SetVertexTime(vtime);
  _chi2 = 666.;
  _ndf = 0;
  float targOffset = this->GetRunPeriod().TargetOffset();
  _index.SetIndex(__data,targOffset,vtime);
}
//_____________________________________________________________________________

template<> bool ClasEvent::GetEvent(CLASdata *__data){

  /// CLASdata specification of ClasEvent::GetEvent.
  /**
   *
   *  Part of the @a CLASdata (compressed ROOT files) interface with ClasEvent.
   *  This function gets the next combination of detected charged particles and
   *  tagged photons. Returns @a true if a combo was found and @a false 
   *  otherwise.
   *
   *  See Index::SetParticles(CLASdata*,ParticleArray<Particle>&,float) and
   *  and Index::SetPhoton(CLASdata*,Photon&,float) for details on setting
   *  particle and tagged photon info. See ClasEvent::_GetCovMatTrack for 
   *  details on how the tracking covariance matrix is set up.
   *
   *  Note: This function also resets the particle names if they've been
   *  changed.
   *
   *  <b>Example Usage: </b>
   *
   *  \include ClasEvent_SetClasEvent_CLASdata.ex
   *
   */
  if(!_index.IsValid()) return false;

  // reset particles name (in case they've been changed)
  this->ResetParticleNames();

  _momentumCor = false; // no momentum corrections on this combo
  if(_isMC) _momentumCor = true; // mc shouldn't need momentum corrections

  _index.SetParticles(__data,this->_particles,this->_ignored_particles,
      _vertexTime);
  double targOffset = this->GetRunPeriod().TargetOffset();
  _index.SetPhoton(__data,this->_photon,targOffset);

  // set up the tracking covariance matrix
  this->_GetCovMatTrack(__data);

  _index++;
  return true;
}
//_____________________________________________________________________________

template<> void ClasEvent::_GetCovMatTrack(CLASdata *__data){
  /// Get the tracking covariance matrix from a compressed ROOT file.
  /** Part of the @a CLASdata (compressed ROOT file) interface with ClasEvent*/
  std::vector<int> inds = _index.GetIndicies();
  int data_ind,q;
  int dim = 3*_particles.Size()+1;
  _covMatTrack.ResizeTo(dim,dim);
  _covMatTrack.Zero();
  // get tagged photon resolution
  _covMatTrack(0,0) = pow(0.001*(this->GetRunPeriod().BeamEnergy()),2)/3.; 
  double p_mag;
  ATrack trackInfo;
  for(int i = 0; i < _particles.Size(); i++){
    // now get detected particle tracking errors
    data_ind = inds[i];
    p_mag = __data->GetParticle(data_ind).P();
    q = __data->GetParticle(data_ind).Q();
    trackInfo = __data->GetTrack(data_ind);
    _covMatTrack(3*i+1,3*i+1) = trackInfo.C11()*pow(p_mag,4);
    _covMatTrack(3*i+2,3*i+2) = trackInfo.C22();
    _covMatTrack(3*i+3,3*i+3) = trackInfo.C33();
    _covMatTrack(3*i+1,3*i+2) = -(trackInfo.C12())*q*p_mag*p_mag;
    _covMatTrack(3*i+2,3*i+1) = _covMatTrack(3*i+1,3*i+2);
    _covMatTrack(3*i+1,3*i+3) = -(trackInfo.C13())*q*p_mag*p_mag;
    _covMatTrack(3*i+3,3*i+1) = _covMatTrack(3*i+1,3*i+3);
    _covMatTrack(3*i+2,3*i+3) = trackInfo.C23();
    _covMatTrack(3*i+3,3*i+2) = _covMatTrack(3*i+2,3*i+3);
  }
}
//_____________________________________________________________________________

template<> bool Index::SetIndex(CLASdata *__data,float __targOffset,float __vtime){

  /// CLASdata specification of Index::SetIndex.
  /**
   * @param data Pointer to a CLASdata object
   * @param targOffset Target offset from CLAS center
   * @param vtime Event start time
   *
   *  Part of the @a CLASdata (compressed ROOT files) interface with ClasEvent.
   *  This function intializes the Index object for the current event. It is
   *  called by ClasEvent::SetClasEvent.
   *
   */
  int i;
  this->Reset();  // reset for the new event

  _numCharged = __data->Ncharged(); // get # of charged particles detected
  if(_numCharged < _particles.Size()) return false;

  // get indicies for ALL good photons
  /** Any tagged photon with \f$| t_{\gamma}^{vertex} - vtime| < dt \f$
   *  is considered @a good, and is its index in the CLASdata object is added
   *  to @a _photIndex. Here, 
   *  \f$ t_{\gamma}^{vertex} = Tpho + (V_z - targOffset)/c \f$ and 
   *  \f$ V_z = MVRT.z \f$. If there are no @a good photons, the function
   *  returns @a false.
   * 
   */
  double photVtime;
  double vertCor = (__data->Vertex().Z() - __targOffset)/__c_cm_per_ns__;
  for(i = 0; i < __data->Nphotons(); i++){
    photVtime = __data->GetPhoton(i).Tpho() + vertCor;
    if(abs(photVtime - __vtime) < _dt) _photIndex.push_back(i); // add it
  }
  if(this->NumPhotons() == 0) return false;

  // now get charged particle indicies
  /**
   * Next it gets charged particle indicies. It loops over all detected charged
   * particles stored in @a data. If we're using @a seb, then we'll define 
   * @a id to be SEBid, otherwise it's PARTid. For each of these particles, it
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
  for(i = 0; i < _numCharged; i++){
    if(_id_scheme == "seb")
      pid = __data->GetParticle(i).SEBid();
    else
      pid = __data->GetParticle(i).PARTid();
    q = __data->GetParticle(i).Q();
    for(int j = 0; j < _particles.Size(); j++){
      add = false;
      /*
         if(_id_scheme == "seb"){
         if(_particles[j].PDG_Id() == pid && pid != 0) add = true;
         }
         else
       */
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
  std::cout << "<Index::SetIndex> Event: " << __data->EventNumber()<<std::endl;
  std::cout << "Vertex Time: " << __data->Vtime() << std::endl;
  std::cout << "Photons detected: ";
  for(i = 0; i < __data->Nphotons(); i++)
    std::cout << "[" << i << "," << __data->GetPhoton(i).Tpho() << "] ";
  std::cout << std::endl;

  std::cout << "Index \'good\' photons: ";
  for(i = 0; i < (int)_photIndex.size(); i++) std::cout << _photIndex[i] <<" ";
  std::cout << std::endl;

  std::cout << "Charged particles detected: " ;
  for(i = 0; i < __data->Ncharged(); i++) 
    std::cout << __data->GetParticle(i).PARTid() << " ";
  std::cout << std::endl;

  std::cout << "Index combos [";
  for(i = 0; i < _particles.Size() - 1; i++) 
    std::cout << _particles[i].Name() << " ";
  std::cout << _particles[_particles.Size()-1].Name() << "]" << std::endl;

  for(i = 0; i < (int)_indexCombos.size(); i++){
    for(int j = 0; j < (int)_indexCombos[i].size(); j++){
      std::cout << _indexCombos[i][j] << " ";
    }
    std::cout << std::endl;
  }
  std::cout << std::endl;
#endif /* __DEBUG__ */

  return hasEvent;
}
//_____________________________________________________________________________

template<> void Index::SetParticles(CLASdata *__data,
    ParticleArray<Particle> &__parts,
    ParticleArray<Particle> &__ignored,
    float __vtime) {
  /// CLASdata specification of Index::SetParticles.
  /**
   * @param data CLASdata pointer
   * @param parts Array of Particle objects to be set
   * @param ignored Array of Particle objects to be set (ignored particles)
   * @param vtime Event start time
   *
   *  Part of the @a CLASdata (compressed ROOT files) interface with ClasEvent.
   *  This function sets all detected charged particle info.
   *
   *  All info is taken straight from the CLASdata object except the particle's
   *  mass (in the energy component of the 4-momentum) which is taken to be
   *  the mass of the global particle that Index was looking for (ex. if 
   *  the 1st entry in Index::_particles is @a pi+, then it uses 0.13597 as the
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
  double mass,sctime,path_len,beta,gamma;
  AParticle apart;
  for(int i = 0; i < size; i++){
    name = _particles[i].Name();
    which = _particles[i];
    mass = _particles[i].Mass();
    apart = __data->GetParticle(cur_combo[i]);
    // set the 4-momentum
    p3 = apart.P3();
    p4.SetPxPyPzE(p3.X(),p3.Y(),p3.Z(),sqrt(p3.Mag2() + mass*mass));
    __parts(name,which).SetP4(p4);    
    // set the vertex
    __parts(name,which).SetVertex(__data->Vertex());
    // set the vtime
    __parts(name,which).SetVertexTime(apart.STvtime());
    // set sector
    __parts(name,which).SetSector(apart.Sector());
    // set sc id and status flag
    __parts(name,which).SetSC_Id(apart.SCid(_id_scheme.c_str()),
        apart.SCstatus(_id_scheme.c_str()));
    // set path length
    path_len = apart.SCpath(_id_scheme.c_str());
    __parts(name,which).SetPathLength(path_len);
    // set time-of-flight    
    sctime = apart.SCtime(_id_scheme.c_str());
    __parts(name,which).SetTOF(sctime - __vtime);
    // set the calculated mass
    if(sctime - __vtime != 0. && path_len != 0.){
      beta = (path_len/(sctime - __vtime))/__c_cm_per_ns__;      
      gamma = 1./sqrt(1. - beta*beta);
      __parts(name,which).SetCalcMass(p3.Mag()/(beta*gamma));
    }
    else __parts(name,which).SetCalcMass(0.);
  }

  // now get any detected particles ignored by ClasEvent
  int num_charged = __data->Ncharged();
  __ignored.Clear();

  for(int i = 0; i < num_charged; i++){
    bool used = false;
    for(int j = 0; j < size; j++) {
      if(i == cur_combo[j]) {
        used = true;
        break;
      }
    }
    if(used) continue;

    apart = __data->GetParticle(i);
    q = apart.Q();
    if(q > 0) __ignored.Add("+");
    else if(q < 0) __ignored.Add("-");
    else continue; // this shouldn't be possible
    int pind = __ignored.Size() - 1;

    // set the vertex
    __ignored[pind].SetVertex(__data->Vertex());
    // set the vtime
    __ignored[pind].SetVertexTime(apart.STvtime());
    // set sector
    __ignored[pind].SetSector(apart.Sector());
    // set sc id and status flag
    __ignored[pind].SetSC_Id(apart.SCid(_id_scheme.c_str()),
        apart.SCstatus(_id_scheme.c_str()));
    // set path length
    path_len = apart.SCpath(_id_scheme.c_str());
    __ignored[pind].SetPathLength(path_len);
    // set time-of-flight    
    sctime = apart.SCtime(_id_scheme.c_str());
    __ignored[pind].SetTOF(sctime - __vtime);
    // set the calculated mass
    if(sctime - __vtime != 0. && path_len != 0.){
      beta = (path_len/(sctime - __vtime))/__c_cm_per_ns__;      
      gamma = 1./sqrt(1. - beta*beta);
      __ignored[pind].SetCalcMass(p3.Mag()/(beta*gamma));
    }
    else __ignored[pind].SetCalcMass(0.);

    // set the 4-momentum
    mass = __ignored[pind].CalcMass();
    p3 = apart.P3();
    p4.SetPxPyPzE(p3.X(),p3.Y(),p3.Z(),sqrt(p3.Mag2() + mass*mass));
    __ignored[pind].SetP4(p4); 
  }  
}
//_____________________________________________________________________________

template<> void Index::SetPhoton(CLASdata *__data,Photon &__photon,
    float __targOffset) const {
  /// CLASdata specification of Index::SetPhoton.
  /**
   * @param data CLASdata pointer
   * @param photon Photon object to be set
   * @param targOffset Target offset from the @a CLAS center
   *
   *
   *  Part of the @a CLASdata (compressed ROOT files) interface with ClasEvent.
   *  This function sets all tagged photon info.
   *
   *  Everything is taken straight from the CLASdata object except the photon's
   *  vertex time is set to be Tpho + (MVRT.z - @a targOffset)/c.
   *
   */
  TAGRentry tag = __data->GetPhoton(_photIndex[_currentPhotIndexPointer]);
  double vtime = tag.Tpho()
    + (__data->Vertex().Z() - __targOffset)/__c_cm_per_ns__;
  __photon.SetVertexTime(vtime);
  __photon.SetE_Id(tag.Eid());
  __photon.SetT_Id(tag.Tid());
  __photon.SetP4(tag.E());  
}
//_____________________________________________________________________________

template<> TVector3 ClasEvent::GetVertex(CLASdata *__data, const std::vector<int> &__inds)
{

  const int num_tracks = (int)__inds.size();
  std::vector<int> inds = _index.GetIndicies();
  TVector3 p3,vertex;
  vector3_t vert;
  tber_t tber[num_tracks];
  float cov[9],chi2;
  int iter;
  AParticle part;
  ATrack track;

  for(int n = 0; n < num_tracks; n++){

    int cur_ind = __inds[n];
    part = __data->GetParticle(cur_ind);
    track = __data->GetTrack(cur_ind);

    int sector = _particles[cur_ind].Sector();

    tber[n].c11 = track.C11();
    tber[n].c12 = track.C12();
    tber[n].c13 = track.C13();
    tber[n].c14 = track.C14();
    tber[n].c15 = track.C15();
    tber[n].c22 = track.C22();    
    tber[n].c33 = track.C33();
    tber[n].c44 = track.C44();
    tber[n].c55 = track.C55();
    tber[n].c23 = track.C23();
    tber[n].c24 = track.C24();
    tber[n].c25 = track.C25();
    tber[n].c34 = track.C34();
    tber[n].c35 = track.C35();
    tber[n].c45 = track.C45();

    tber[n].q_over_p = part.Q()/part.P();
    tber[n].phi = part.Phi();
    tber[n].lambda = part.Lambda();
    tber[n].d0 = part.D0();
    tber[n].z0 = part.Z0();

    tber[n].chi2 = track.Chi2();
    tber[n].layinfo1 = track.LayInfo1();

    tber[n].layinfo2 = (int)pow(256.,3)*sector;
  }
  // Get the vertex using the appropriate MVRT routines
  if(num_tracks>1)
  {
    mtv_nobos(tber,num_tracks,&vert,cov,&chi2,&iter);
    vertex.SetXYZ(vert.x,vert.y,vert.z);
  }
  else 
  {/*Call the approppriate function from $CLAS_PACK/vertex*/
    vert = get_1part_vert_nobos(tber);
    vertex.SetXYZ(vert.x, vert.y, vert.z);
  }

  return vertex;
}
//_____________________________________________________________________________

template<> float ClasEvent::GetDOCA(CLASdata *__data, const std::vector<int> &__inds)
{

  const int num_tracks = (int)__inds.size();
  std::vector<int> inds = _index.GetIndicies();
  TVector3 p3,vertex;
  vector3_t vert[2];
  line_t line[2];
  float sepd, R;
  tber_t tber[1];
  AParticle part;
  ATrack track;
  sepd = 999; 
  if(num_tracks!=2) 
  {
    cerr << "Called GetDOCA with not 2 tracks!" << endl;
    cerr << "Not returning valid parameters for this call." << endl;
    return sepd;
  }

  for(int n = 0; n < num_tracks; n++)
  {

    int cur_ind = __inds[n];
    part = __data->GetParticle(cur_ind);
    track = __data->GetTrack(cur_ind);

    int sector = _particles[cur_ind].Sector();

    tber[0].c11 = track.C11();
    tber[0].c12 = track.C12();
    tber[0].c13 = track.C13();
    tber[0].c14 = track.C14();
    tber[0].c15 = track.C15();
    tber[0].c22 = track.C22();    
    tber[0].c33 = track.C33();
    tber[0].c44 = track.C44();
    tber[0].c55 = track.C55();
    tber[0].c23 = track.C23();
    tber[0].c24 = track.C24();
    tber[0].c25 = track.C25();
    tber[0].c34 = track.C34();
    tber[0].c35 = track.C35();
    tber[0].c45 = track.C45();

    tber[0].q_over_p = part.Q()/part.P();
    tber[0].phi = part.Phi();
    tber[0].lambda = part.Lambda();
    tber[0].d0 = part.D0();
    tber[0].z0 = part.Z0();

    tber[0].chi2 = track.Chi2();
    tber[0].layinfo1 = track.LayInfo1();

    tber[0].layinfo2 = (int)pow(256.,3)*sector;

    //cerr << n << "\t" << tber[0].q_over_p << " " << tber[0].d0 << " " << tber[0].z0 << "\t";
    //cerr << part.Q() << " " << part.P() << endl;
    vert[n] = get_1part_vert_nobos(tber);
    //cerr << n << "\t" << vert[n].x << " " << vert[n].y << " " << vert[n].z << "\t"; 
    line[n].point = vert[n];
    line[n].dir.x = part.Px();
    line[n].dir.y = part.Py();
    line[n].dir.z = part.Pz();
    //cerr << n << "\t" << line[n].point.x << " " << line[n].point.y << " " << line[n].point.z << "\t"; 
    //cerr <<              line[n].dir.x << " " << line[n].dir.y << " " << line[n].dir.z << endl;
  }

  vertex_doca(&line[0],&line[1],&sepd,&R);
  //cerr << "sepd/R: " << sepd << " " << R << endl;

  return sepd;
}
