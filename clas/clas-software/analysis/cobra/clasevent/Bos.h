// Bos class header file. -*- C++ -*-
// Author: Mike Williams
/** @file clasevent/Bos.h
 * @brief Bos class definition file.
 */
#ifndef _Bos_H
#define _Bos_H
// System Headers:
#include <iostream>
#include <string>
#include <vector>
#include <fstream>
#include <cstdio>
// CLAS Headers:
extern "C" {
#include <signal.h>
#include <errno.h>
#include <ntypes.h>
#include <bostypes.h>
#include <ec.h>
#include <clas_cern.h>
#include <ctype.h>
#include <kinematics.h>
#include <map_manager.h>
#include <trk.h>
#include <clasmdl.h>
#include <utility.h>
#include <pid.h>
#include <makebanks.h>
#include <call.h>
#include <bosddl.h>
#include <tagtnorm.h>
#include <vertex.h>
}
//_____________________________________________________________________________

typedef std::string String; 

//_____________________________________________________________________________

class Bos {

  private:
    // Data Members:
    int _runNumber; ///< CLAS run number
    std::vector<String> _banksList; ///< List of Banks required

    // Functions(private):
    void _Copy(const Bos &__bos){
      /// Private function for copying data members
      _runNumber = __bos._runNumber;
      _banksList = __bos._banksList;
    }

    static bool _init; ///< Private initialization flag
    static std::vector<String> _SebList; ///< Global list of SEB banks
    static std::vector<String> _PartList; ///< Global list of PART banks

  public:

    // Create/Copy/Destroy:
    Bos();
    virtual ~Bos();

    Bos(const Bos &__bos){
      /// Copy Constructor
      this->_Copy(__bos);
    }

    Bos& operator=(const Bos &__bos){
      /// Assignment operator
      this->_Copy(__bos);
      return *this;
    }

    inline bool OutFile(const String &__outFile) const {
      /// Open a BOS output file @a outFile
      //    unlink(__outFile.c_str());
      remove(__outFile.c_str());
      char message[100];
      sprintf(message,
          "OPEN BOSOUTPUT UNIT=7 FILE=\"%s\" WRITE STATUS=NEW RECL=3600",
          __outFile.c_str());

      return fparm_c(message);
    }

    inline bool InFile(const String &__inFile) const {
      /// Open a BOS input file @a inFile
      char message[100];
      sprintf(message,"OPEN BOSINPUT UNIT=1 FILE=\"%s\" READ",__inFile.c_str());
      return fparm_c(message);
    }

    // Remake BOS banks using corrected calibration data 
    void RemakeBanks(int __pidGroup) const;

    // Remake the MVRT bank
    void MakeMVRT(bool __isMC = false) const;

    inline bool GetBOS(int __pidGroup = 1,int __remakeFlag = 0) {
      /// Get BOS banks for next event (calls getBOS)
      /** @param pidGroup Flag passed to RemakeBanks() if @a remakeFlag is 1
       *  @param remakeFlag 1 calls RemakeBanks, 2 calls MakeMVRT(), 0 is neither
       */
      if(getBOS(&bcs_,1,"E")){
        if(getBank(&bcs_, "HEAD")) 
          _runNumber = ((clasHEAD_t*)getBank(&bcs_, "HEAD"))->head[0].nrun;
        if(__remakeFlag == 1) RemakeBanks(__pidGroup);
        else if(__remakeFlag == 2) MakeMVRT();
        return true;
      }
      else return false;
    }

    inline void CloseInFile() const {
      /// Close BOS input file
      char message[100];
      sprintf(message,"CLOSE BOSINPUT UNIT=1");  
      fparm_c(message);  
    }

    inline void CloseOutFile() const {
      /// Close BOS output file
      char message[100];
      sprintf(message,"CLOSE BOSINPUT UNIT=7");  
      fparm_c(message);  
    }

    // Check if the Required BOS banks exist for the event
    bool BankCheck() const;

    inline void Write() const {
      /// Write to BOS output file
      putBOS(&bcs_, 7, "E");
    }

    inline void CleanBOS() {
      /// Clear banks to get ready for next event
      dropAllBanks(&bcs_,"E");
      cleanBanks(&bcs_);
    }

    inline void AddMVRT() {
      /// Add MVRT to the list of banks to be written out to BOS output file
      formatBank("MVRT","2I,10F,I");
      bankList(&bcs_,"E+","MVRT");
    }

    inline void AddBankToList(const String &__bank) {
      /// Add @a bank to list of banks required for each event
      _banksList.push_back(__bank);
    }

    inline void *GetBank(const String &__bank,int __group = -1) const {
      /// Return a pointer to @a bank
      void *BANKret = NULL;
      String bankname = __bank;
      if(bankname.length() == 3) 
      {
        bankname = bankname + " ";
      }
      else if(bankname.length() == 2) 
      {
        bankname = bankname + "  ";
      }
      if(__group < 0)
      {
        BANKret =  getBank(&bcs_,bankname.c_str());
        if(BANKret == NULL)
        {
          BANKret =  getBank(&wcs_,bankname.c_str());
        }
      }
      else
      {
        BANKret = getGroup(&bcs_,bankname.c_str(),__group);
        if(BANKret == NULL)
        {
          BANKret = getGroup(&wcs_,bankname.c_str(),__group);
        }
      }
      return BANKret;
    }


    inline int GetTBERIndexFromPARTIndex(int __index) const {
      /// Returns the TBER bank index of track with PART index @a index
      int data_ind;
      data_ind = ((clasPART_t*)this->GetBank("PART"))->part[__index].trkid-1;
      data_ind = ((clasTBID_t*)this->GetBank("TBID"))->tbid[data_ind].track-1;
      return data_ind;
    }
};
//_____________________________________________________________________________

#endif /* Bos_H */
