#include "EventHeader.h"
///////////////////////////////////////////////////////////////////////////////
//                                                                           //
// The EventHeader class stores event information from the HEAD, CL01 and    //
// HEVT banks.                                                               //
//                                                                           //
// EventHeader Return Functions: 
//     RunNumber().........Returns the run number (nrun in HEAD/HEVT banks)   
//     EventNumber().......Returns the event number (nevent in HEAD/HEVT banks)
//     TimeProcessed().....Returns Processing time, UNIX time or seconds as of 
//                         January 1,1970 (time in HEAD/ptime in HEVT banks)
//     Type()..............data = 1 monte carlo = 0 (type in HEAD/HEVT banks)
//     EvtClass()..........DAQ classification (evtclass in HEAD bank)
//     RFtime1()...........RF time 1 (ns) (rf1 in CL01 bank)
//     RFtime2()...........RF time 2 (ns) (rf2 in CL01 bank)
//     RFtime()............Good RF time (ns) (rf in CL01 bank/rf1 in HEVT bank)
//     SEBtime()...........Returns event start time as determined by SEB (ns)
//                         (stt in HEVT bank) [same as EventHeader::STT]
//                                                                           //
///////////////////////////////////////////////////////////////////////////////

ClassImp(EventHeader)

EventHeader::EventHeader():TObject() {
  // **********Constructor**********
}
//_____________________________________________________________________________

EventHeader::EventHeader(const EventHeader &head):TObject() {
  // **********Copy Constructor**********
  _RunNumber = head._RunNumber;
  _EventNumber = head._EventNumber;
  _TimeProcessed = head._TimeProcessed;
  _Mode = head._Mode;
  _RFtime1 = head._RFtime1;
  _RFtime2 = head._RFtime2;
  _RFtime = head._RFtime;  
  _SEBtime = head._SEBtime;
  _IsFluxEvent = head._IsFluxEvent;
  _TrigBits = head._TrigBits;

  _Neutrals.clear();
  for(Int_t i = 0; i < (int)head._Neutrals.size(); i++){
    _Neutrals.push_back(head._Neutrals[i]);
  }
}
//_____________________________________________________________________________

EventHeader::~EventHeader() {
  // **********Destructor**********
  _Neutrals.clear();
}
//_____________________________________________________________________________

EventHeader& EventHeader::operator=(const EventHeader &head){
  // Assignment operator
  _RunNumber = head._RunNumber;
  _EventNumber = head._EventNumber;
  _TimeProcessed = head._TimeProcessed;
  _Mode = head._Mode;
  _RFtime1 = head._RFtime1;
  _RFtime2 = head._RFtime2;
  _RFtime = head._RFtime;  
  _SEBtime = head._SEBtime;
  _IsFluxEvent = head._IsFluxEvent;
  _TrigBits = head._TrigBits;

  _Neutrals.clear();
  for(Int_t i = 0; i < (int)head._Neutrals.size(); i++){
    _Neutrals.push_back(head._Neutrals[i]);
  }
  return *this;
} 
//_____________________________________________________________________________

void EventHeader::AddNeutral(const char *idscheme,Int_t group,Int_t id,Int_t sec){
  // Add a neutral particle from "idscheme" (group = 1 or 2 is the PART sector)
  // with particle ID id and in sector sec.
  Int_t packed;
  if(!strcmp(idscheme,"seb")) packed = 0;
  if(!strcmp(idscheme,"part")){
    if(group == 1) packed = 1000;
    else if(group == 2) packed = 2000;
    else packed = 3000;
  }
  if(id >= 100 || id < 0){
    id = 99;
    cout << "Warning <EventHeader::AddNeutral> " << id << " does NOT appear to be a PDG id flag, 99 will be stored." << endl;
  }
  packed += id;
  if(sec > 6 || sec < 1) sec = 0;
  packed += sec*100;
  _Neutrals.push_back(packed);
}
//_____________________________________________________________________________

void EventHeader::Reset(){
  // Reset the EventHeader object for the next event.
  _Neutrals.clear();
  _RunNumber = 0;
  _EventNumber = 0;
  _TimeProcessed = 0;
  _Mode = 0;
  _RFtime1 = 0.;
  _RFtime2 = 0.;
  _RFtime = 0.;
  _SEBtime = 0.;
  _IsFluxEvent = false;
}
//_____________________________________________________________________________

void EventHeader::Print(std::ostream &os) const {
  // Print to os the current values stored in this EventHeader object.
  // os defaults to std::cout, thus printing the info to the screen.
  Int_t i;
  os << "EventHeader: " << endl;
  os << "\t Run Number: " << _RunNumber << " Event Number: " << _EventNumber << " Is Flux Event?: " << _IsFluxEvent << endl;
  os << " Time Processed: " << _TimeProcessed << endl;
  os << "\t Type: " << this->Type() << " Class: " << this->EvtClass() << " TrigBits: " << _TrigBits << endl;
  os << "\t rf1: " << this->RFtime1() << " rf2: " << this->RFtime2() << " rf: " << this->RFtime() << " STT: " << this->SEBtime() << endl;
  os << "\t Neutrals:" << endl;
  for(Int_t i = 0; i < (int)_Neutrals.size(); i++){
    os << "\t " << this->IDscheme(i) << ": ID: " << this->ID(i) << " Sector: " << this->Sector(i) << endl;
  }
}
//_____________________________________________________________________________

std::ostream& operator<<(std::ostream &os,const EventHeader& head){
  // Overload of the ostream << operator, this makes the following legal:
  // EventHeader head;
  // cout << head << endl;
  head.Print(os);
  return os;
}
//_____________________________________________________________________________

Bool_t EventHeader::IsTrigger(Int_t bit) const {
  // Returns true if bit is set, false otherwise. Bits 1-6...

  bitset<32> bits(_TrigBits);
  return bits.test(bit-1);
}
