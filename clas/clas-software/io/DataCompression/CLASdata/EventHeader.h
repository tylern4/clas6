#ifndef ROOT_EventHeader
#define ROOT_EventHeader

#include "TObject.h"
#include <iostream>
#include <vector>
#include <bitset>

using namespace std;

class EventHeader : public TObject {

 private:

  Int_t _RunNumber; //  Run Number
  Int_t _EventNumber; //  Event Number
  Int_t _TimeProcessed; //  Time the event was processed (unix time)
  Int_t _Mode; //  Bit packed Type (0 for mc 1 for data) and Class
  UInt_t _TrigBits; // Trigger bits from HEAD bank
  Float_t _RFtime1; //  RF time 1(ns) (From CL01 bank)
  Float_t _RFtime2; //  RF time 2(ns) (From CL01 bank)
  Float_t _RFtime; //  Good RF time(ns) (From CL01 bank)
  Float_t _SEBtime; // Event start time as defined by SEB (From HEVT bank) 
  Bool_t _IsFluxEvent; // Is a good gflux event?
  vector<Int_t> _Neutrals; // Bit packed Neutral particle info

 public:

  // Constructors/Destructors
  EventHeader();
  EventHeader(const EventHeader&);

  virtual ~EventHeader();

  // Setters
  inline void SetRunNumber(Int_t nrun) {_RunNumber = nrun;};
  inline void SetEventNumber(Int_t nevnt) {_EventNumber = nevnt;};
  inline void SetTimeProcessed(Int_t tproc) {_TimeProcessed = tproc;};
  inline void SetMode(Int_t type,Int_t evtclass) {_Mode = 1000*type+evtclass;};
  inline void SetRFtime1(Float_t rf1) {_RFtime1 = (rf1);};
  inline void SetRFtime2(Float_t rf2) {_RFtime2 = (rf2);};
  inline void SetRFtime(Float_t rf) {_RFtime = (rf);};
  inline void SetSEBtime(Float_t stt) {_SEBtime = (stt);};
  inline void SetIsFluxEvent(Bool_t is) {_IsFluxEvent = is;};
  inline void SetTrigBits(Int_t trgbts) {_TrigBits = trgbts;};
  void AddNeutral(const char *idscheme,Int_t group,Int_t id,Int_t sec);

  // Getters
  inline Int_t RunNumber() const {return _RunNumber;};
  inline Int_t EventNumber() const {return _EventNumber;};
  inline Int_t TimeProcessed() const {return _TimeProcessed;};
  inline Int_t Mode() const {return _Mode;};
  inline Int_t Type() const {return _Mode/1000;};
  inline Int_t EvtClass() const {return (_Mode - (_Mode/1000)*1000);};
  inline UInt_t TrigBits() const {return _TrigBits;};
  inline Float_t RFtime1() const {return (_RFtime1);};
  inline Float_t RFtime2() const {return (_RFtime2);};
  inline Float_t RFtime() const {return (_RFtime);};
  inline Float_t SEBtime() const {return (_SEBtime);};
  inline Float_t STT() const {return (_SEBtime);};
  inline Bool_t IsFluxEvent() const {return _IsFluxEvent;};
  inline Int_t Sector(Int_t) const;
  inline Int_t ID(Int_t) const;
  inline Int_t IDscheme(Int_t n) const {return _Neutrals[n]/1000;};
  Bool_t IsTrigger(Int_t) const;

  // Functions
  void Print(std::ostream &os = std::cout) const;
  void Reset();

  // Operators
  EventHeader& operator=(const EventHeader&);

  ClassDef(EventHeader,1) // Stores info from the HEAD,CL01 and HEVT banks
};
//_____________________________________________________________________________

inline Int_t EventHeader::Sector(Int_t n) const {
  Int_t tmp = _Neutrals[n];
  return (tmp - (tmp/1000)*1000)/100;
}

inline Int_t  EventHeader::ID(Int_t n) const {
  Int_t tmp = _Neutrals[n];
  tmp -= (tmp/1000)*1000;
  return (tmp - (tmp/100)*100);
}
//_____________________________________________________________________________

std::ostream& operator<<(std::ostream &os,const EventHeader& head);

#endif
