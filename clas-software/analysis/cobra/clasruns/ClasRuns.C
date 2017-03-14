#include "ClasRuns.h"
#include <fstream>
/** @file ClasRuns.C
 * @brief ClasRuns class source file.
 */
//_____________________________________________________________________________
// document ClasRunInfo.lst file here also
/** @file ClasRunInfo.lst
 * @brief Read at run time by ClasRuns to get run period info.
 *
 * This file has a list of all valid run periods that the ClasRuns class will
 * have access to at run time. The format for adding a run period's info is:
 *
 * \include ClasRunInfo_Format.ex
 *
 * The beam energy should be given in \f$ GeV \f$, the torus current in @a A.
 * The target flags follow the @a eloss convention. The target offset should be
 * given in @a cm and is the distant the center of the target is from the 
 * center of CLAS (- means upstream).
 *
 * <b> Example: </b>
 *
 * The @a g1c entry looks like,
 *
 * \include ClasRunInfo_g1c.ex
 *
 */
//_____________________________________________________________________________

/** @class ClasRuns
 *  @brief Singleton class that stores CLAS run period information.
 *
 *  A @a Singleton class is a class that can only ever have 1 instance in
 *  existance (these are very useful when writing gui's since there's only
 *  1 mouse...). The class itself insures that <em> there can be only one</em>,
 *  the user can grab as many pointers to the instance as he/she wants. This is
 *  done using the Instance() static member function using a statement like,
 *
 *  <!--
 *    ClasRuns *ptypes = ClasRuns::Instance();
 *  -->
 *
 *  \include ClasRuns_Instance.ex
 *
 *  Once the user has a pointer to the instance of ClasRuns, they can 
 *  access any information from the list of @a valid run periods (which is 
 *  obtained at run time by reading ClasRunInfo.lst). For example, to get
 *  the torus current for run 20926,
 *
 *  <!--
 *    ClasRuns *runs = ClasRuns::Instance();
 *    double i_torus = runs->GetRunPeriod(20926).TorusCurrent();
 *  -->
 *
 *  \include ClasRuns_GetRunPeriod.ex
 *
 *  For a list of valid run periods see ClasRunInfo.lst. For a list of what
 *  information is stored for each run period see ClasRunPeriod.
 *
 *  Note: This class is ROOT compitable, however it can @a NOT be written to
 *  a ROOT file (it doesn't inherit from TObject).
 *
 */
//_____________________________________________________________________________

//ClassImp(ClasRuns); // ROOT implementation macro

//_____________________________________________________________________________

/* Allocate memory for the static data members of ClasRuns */

ClasRuns* ClasRuns::_instance = 0;

//_____________________________________________________________________________

ClasRuns::ClasRuns():_default_run(){
  /// Default Constructor
  /** Reads CLAS_RunInfo.lst to get known run periods' information */
  String fileName = getenv("COBRASYS");
  fileName += "/clasruns/ClasRunInfo.lst";
  ifstream inFile(fileName.c_str());
  String name,s_dummy,s_tmp;
  float e_beam,i_torus,targOffset;
  int runLow,runHigh,targMat,targCell;
  ClasRunPeriod clasRun;

  while(inFile >> s_dummy){
    if(s_dummy[0] == '#' || s_dummy == String()){
      // this is a comment or a blank line
      inFile.ignore(1000,'\n'); // skip to end of this line
    }
    else{
      // if not a comment/blank line, then s_dummy is the run period name
      name = s_dummy;
      inFile >> s_dummy; // this should be 'runLow-runHigh'
      String::size_type dash_pos = s_dummy.find("-");
      s_tmp.assign(s_dummy,0,dash_pos);
      runLow = atoi(s_tmp.c_str());
      s_tmp.assign(s_dummy,(int)dash_pos+1,s_dummy.size()-dash_pos);
      runHigh = atoi(s_tmp.c_str());
      
      // get the rest of the run info
      inFile >> e_beam >> i_torus >> targMat >> targCell >> targOffset;

      clasRun.SetRunPeriod(name,runLow,runHigh,e_beam,i_torus,targMat,
			   targCell,targOffset);
      _runs.push_back(clasRun); // add this run 

    }
  }
  inFile.close();
}
