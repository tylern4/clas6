#ifndef ROOT_CLASdata
#define ROOT_CLASdata

#include <TROOT.h>
#include <TTree.h>
#include <EventHeader.h>
#include <Photons.h>
#include <Tracks.h>
#include <Charged.h>

using namespace std;

class CLASdata {

 private:

  TTree *_Tree; // Pointer to current TTree
  Photons *_Photons; // Pointer to Photons object in _Tree
  EventHeader *_Header; // Pointer to EventHeader object in _Tree
  Tracks *_Tracks; // Pointer to Tracks object in _Tree
  Charged *_Charged; // Pointer to Charged object in _Tree

  TBranch *_HeaderBranch; // Pointer to EventHeader Branch
  TBranch *_TracksBranch; // Pointer to Tracks Branch
  TBranch *_ChargedBranch; // Pointer to Charged Branch 
  TBranch *_PhotonsBranch; // Pointer to Photons Branch

 public:

  // Constructors/Destructors
  CLASdata(TTree *tree = NULL);
  virtual ~CLASdata();

  // Getters
  const EventHeader& GetEventHeader() const {return *_Header;};
  const Photons& GetPhotons() const {return *_Photons;};
  const Tracks& GetTracks() const {return *_Tracks;};
  const Charged& GetCharged() const {return *_Charged;};

  // Functions
  Int_t GetEntry(Int_t entry);
  void Init(TTree*);
  Bool_t GetBranches();
  void Show(Int_t entry = -1);

  // Photons Functions
  inline Int_t Nphotons() const {return _Photons->N();};
  inline const TAGRentry& GetPhoton(Int_t i) const {return (*_Photons)(i);};
  inline const TAGRentry& GetSEBphoton() const {return (*_Photons)[_Photons->SEBphot()];};
  inline Float_t SEBdt() const {return _Photons->SEBdt();};
  // EventHeader Functions
  inline Int_t RunNumber() const {return _Header->RunNumber();};
  inline Int_t EventNumber() const {return _Header->EventNumber();};
  inline Int_t TimeProcessed() const {return _Header->TimeProcessed();};
  inline Int_t Type() const {return _Header->Type();};
  inline Int_t EvtClass() const {return _Header->EvtClass();};
  inline Float_t RFtime1() const {return _Header->RFtime1();};
  inline Float_t RFtime2() const {return _Header->RFtime2();};
  inline Float_t RFtime() const {return _Header->RFtime();};
  inline Float_t SEBtime() const {return _Header->SEBtime();};
  inline Float_t STT() const {return _Header->SEBtime();};
  inline UInt_t TrigBits() const {return _Header->TrigBits();};
  inline Bool_t IsTrigger(Int_t bit) const {return _Header->IsTrigger(bit);};
  inline Bool_t IsFluxEvent() const {return _Header->IsFluxEvent();};
  // Charged Functions
  inline Int_t Ncharged() const {return _Charged->N();};
  inline const AParticle& GetParticle(Int_t i) const {return (*_Charged)[i];};
  inline const TVector3& Vertex() const {return _Charged->Vertex();};
  inline Float_t Vtime(Int_t i = 1) const {return _Charged->Vtime(i);};
  // Tracks Functions
  inline Int_t Ntracks() const {return _Tracks->N();};
  inline const ATrack& GetTrack(Int_t i) const {return (*_Tracks)[i];};

  // Functions
  Float_t CalcMass(Int_t,const char*,Int_t group = 1) const;

  ClassDef(CLASdata,1)
};

#endif
