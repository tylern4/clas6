#include "Bos.h"
/** @file Bos.C
 * @brief Bos class source file.
 */
//_____________________________________________________________________________
/** @class Bos
 *  @brief Provides a CLAS BOS file interface for ClasEvent.
 *
 */
//_____________________________________________________________________________

/* Allocate memory for the static data members of Bos */

bool Bos::_init = true; // it has to start out as true
std::vector<String> Bos::_SebList;
std::vector<String> Bos::_PartList;

//_____________________________________________________________________________

Bos::Bos(){
  /// Default Constructor
  /** Runs initbos() */

  if(_init){
    // this is 1st Bos object created...initialize the global bank lists
    
    // parse the ListOfBanks.lst file to get banks
    String fileName = getenv("COBRASYS");
    fileName += "/clasevent/ListOfBanks.lst";
    std::ifstream inFile(fileName.c_str());
    String s_dummy;
  
    // skip over comment/blank lines 
    while(inFile >> s_dummy){
      if(s_dummy[0] == '#' || s_dummy == String()){
	// this is a comment or a blank line
	inFile.ignore(1000,'\n'); // skip to end of this line
      }
      else break;
    }
    // Get PART banks
    _PartList.push_back(s_dummy);
    while(inFile >> s_dummy){
      if(s_dummy[0] == '#' || s_dummy == String()){
	// this is a comment or a blank line
	inFile.ignore(1000,'\n'); // skip to end of this line
	break;
      }
      else _PartList.push_back(s_dummy);
    }
    // Get the SEB banks
    while(inFile >> s_dummy){
      if(s_dummy[0] == '#' || s_dummy == String()){
	// this is a comment or a blank line
	inFile.ignore(1000,'\n'); // skip to end of this line
      }
      else _SebList.push_back(s_dummy);
    }
    inFile.close();
    _init = false;
  }

  _runNumber = 0;
  _banksList = _PartList;
  /*
  std::cout << "_banksList:" << std::endl;
  for(int i = 0; i < (int)_banksList.size(); i++) std::cout << _banksList[i] << " ";
  std::cout << std::endl;
  */
  initbos(); // initialize the BOS crap
}
//_____________________________________________________________________________

Bos::~Bos(){
  /// Destructor
  _banksList.clear();
}
//_____________________________________________________________________________

void Bos::RemakeBanks(int __pidGroup) const {
  /// Remake BOS banks with corrected calibration data
  /** @param pidGroup Which BOS bank group should be remade
   *  Calibration info is in CLAS_PARMS and banks are remade using CLAS 
   *  software in packages/pid
   */
  
  MakeMVRT(); // make MVRT

  // remake the bid panks
  dropBank(&bcs_,BID_BANKS,__pidGroup);
  bankList(&bcs_,"E+",BID_BANKS); 
  make_BID_banks(__pidGroup);
    
  // remake part banks
  dropBank(&bcs_,"PART",__pidGroup);
  bankList(&bcs_,"E+","PART");
  make_PART_group(__pidGroup) ;   
}
//_____________________________________________________________________________

void Bos::MakeMVRT(bool __isMC) const {
  /// Remake the MVRT bank 
  /** @param isMC Is this a monte carlo file? */
  int flag = 1;
  static int prev_run_num = -1;
  if(__isMC) flag = 0;
  ConfigEvent(_runNumber,flag);
  if(_runNumber != prev_run_num){
    vertex_brun(_runNumber);
    prev_run_num = _runNumber;
  }
  make_mvrt();
}
//_____________________________________________________________________________

bool Bos::BankCheck() const {
  /// Check if @a ALL BOS banks stored in @a banksList exist for the event
  bool bankCheck = true;
  for(int i = 0; i < (int)_banksList.size(); i++){
    if(!getBank(&bcs_,_banksList[i].c_str())){
      bankCheck = false;
      break;
    }
  }
  return bankCheck;
}
//_____________________________________________________________________________
