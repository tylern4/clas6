#include "ParticleTypes.h"
/** @file ParticleTypes.C
 *  @brief ParticleTypes class source file.
 */
//_____________________________________________________________________________
// Also document the particle list file here
/** @file ListOfParticles.lst
 *  @brief Read at run time by ParticleTypes to get all valid particle info.
 *
 *  This file has all valid particle names and info that will be available to 
 *  the ParticleTypes class, and thus the ParticleArray class as well, at run
 *  time. The format for adding a particle's information is,
 * 
 *  \include ListOfParticles_format.ex
 *
 *  Particle names should be all lower case letters and contain no white space
 *  or -'s (except for the last character in the case of a negative particle).
 *  Charge information in the name should be done using a @a +,@a - or @a 0.
 *  The mass and width should be in @a GeV and the charge should be in units of
 *  @a e. Any line in this file that starts with a # will be treated as a 
 *  comment field.
 *
 *  <b> Example </b>
 *
 *  The \f$ \pi^+ \f$ entry looks like,
 *
 *  \include ListOfParticles_pip.ex
 *
 */
//_____________________________________________________________________________

/** @class ParticleTypes
 *  @brief Singleton class that stores global particle information.
 *
 *  ParticleTypes is a @a Singleton class that stores global particle info.
 *  A @a Singleton class is a class that can only ever have 1 instance in
 *  existance (these are very useful when writing gui's since there's only
 *  1 mouse...). The class itself insures that <em> there can be only one</em>,
 *  the user can grab as many pointers to the instance as he/she wants. This is
 *  done using the Instance() static member function using a statement like,
 *
 *  <!--
 *    ParticleTypes *ptypes = ParticleTypes::Instance();
 *  -->
 *
 *  \include ParticleTypes_Instance.ex
 *
 *  Once the user has a pointer to the instance of ParticleTypes, they can 
 *  access any information from the list of @a valid particles (which is 
 *  obtained at run time by reading ListOfParticles.lst). For example, to get
 *  the mass of the \f$ \pi^+ \f$, use,
 *
 *  <!--
 *    ParticleTypes *ptypes = ParticleTypes::Instance();
 *    double m_pip = ptypes->GetParticle("pi+").Mass();
 *  -->
 *
 *  \include ParticleTypes_GetParticle.ex
 *
 *  For a list of valid particles see ListOfParticles.lst. For a list of what
 *  information is stored for each particle see ParticleInfo.
 *
 *  Note: This class is ROOT compitable, however it can @a NOT be written to
 *  a ROOT file (it doesn't inherit from TObject).
 *
 */
//_____________________________________________________________________________

//ClassImp(ParticleTypes); // ROOT class implementation macro
//_____________________________________________________________________________

/* Allocate memory for the static data members of ParticleTypes */

ParticleTypes* ParticleTypes::_instance = 0; // it has to start out as null!

//_____________________________________________________________________________

ParticleTypes::ParticleTypes(){
  /// Default Constructor
  /**
   * Parses the @a ListOfParticles.lst file found in
   * COBRASYS/ptypes. The @a COBRASYS enviornment variable
   * must be set so that the text file can be found.
   *
   * Each particle found in @a ListOfParticles.lst is added to the list of 
   * known particles along with 3 generic charged entries: @a +,@a - and @a 0.
   *
   * Note: All names and types are converted to all lower case letters.
   *
   */

  // parse the ListOfParticles.lst file to get particle info
  String fileName = getenv("COBRASYS");
  fileName += "/ptypes/ListOfParticles.lst";
  ifstream inFile(fileName.c_str());
  String s_dummy,name,type,jp,texName;
  double mass,width;
  int q,pdgId,geantId;
  
  while(inFile >> s_dummy){
    if(s_dummy[0] == '#' || s_dummy == String()){
      // this is a comment or a blank line
      inFile.ignore(1000,'\n'); // skip to end of this line
    }
    else{
      name = s_dummy;
      inFile >> type >> q >> mass >> width >> jp >> pdgId >> geantId 
	     >> texName;
      // convert name and type to lower case
      transform(name.begin(),name.end(),name.begin(),tolower);
      transform(type.begin(),type.end(),type.begin(),tolower);
      
      // add it to the _particles vector
      _particles.push_back(ParticleInfo(name,q,mass,jp,width,pdgId,geantId,
					type,texName));
    }
  }
  inFile.close();
  
  // provide generic charged particles...these can be used as place holders
  _particles.push_back(ParticleInfo("+"));
  _particles.push_back(ParticleInfo("0"));
  _particles.push_back(ParticleInfo("-"));  
}
//_____________________________________________________________________________

ParticleTypes::~ParticleTypes(){
  /// Destructor
  _particles.clear();
  _instance = 0; // in case someone manages to delete the Singleton, null the 
                 // pointer so Instance knows to recreate it if needed. 
}
//_____________________________________________________________________________

bool ParticleTypes::ConvertName(const String &__name,String &__convName) const{
  /// Converts @a name to a known particle name.
  /**
   * @param name Name to be converted
   * @param convName The converted name (result of the conversion)
   *
   * Returns @a true if the conversion was successful, @a false otherwise.
   *
   * If @a name is allready valid, then @a convName is set equal to @a name and
   * the function returns @a true. Otherwise, the 1st conversion attempt is to
   * simply make all letters lowercase and remove any white space or -'s (-'s 
   * are allowed and not removed for the last character obviously). If
   * after doing this @a name still isn't valid, then the function searches for
   * some common substrings in @a name. The following is a list of substrings
   * that will be converted if found:
   *
   *
   * @a plus \f$ \rightarrow \f$ @a +
   *
   * @a minus \f$ \rightarrow \f$ @a -
   *
   * @a zero \f$ \rightarrow \f$ @a 0
   *
   * @a long \f$ \rightarrow \f$ @a l
   *
   * @a short \f$ \rightarrow \f$ @a s
   *
   * Thus, if @a piplus (or @a pi @a plus since white space has allready been
   * removed) is entered, the function will convert this to @a pi+ which is a
   * valid name.
   *
   * If @a name still isn't valid, then the function will check to see if the 
   * last letter is @a p or @a m and try to replace this with @a + or @a - to
   * see if that would be a valid name. So this allows a successfull conversion
   * from @a pip to @a pi+. If this doesn't result in a valid name, then the 
   * last character isn't changed.
   *
   * The last attempt is to convert some special cases to their proper names.
   * @a proton is converted to @a p and @a electron to @a e. Then the 
   * followintg matches are tried:
   *
   * @a e \f$ \rightarrow \f$ @a e-
   *
   * @a positron or @a antie \f$ \rightarrow \f$ @a e+
   *
   * @a p+ \f$ \rightarrow \f$ @a p
   *
   * @a neutron \f$ \rightarrow \f$ @a n 
   *
   * @a pbar or @a antip \f$ \rightarrow \f$ @a p-
   *
   * Note that at this point, @a antie would match @a anti-electron or 
   * @a anti @a electron.
   *
   * If we still can't find a match, then return false.
   *
   */
  String tempName;
  __convName = __name;
  // 1st check if it's already a valid name
  if(this->IsValidName(__name)) return true;
  // make all letters lower case
  transform(__convName.begin(),__convName.end(),__convName.begin(),tolower);
  // remove all white space 
  for(String::size_type pos = 0; pos < __convName.size(); pos++){
    if(__convName[pos] == ' '){
      __convName.erase(pos,1);
      pos--;
    }
  }
  // remove any -'s not at the end (which would be a negative particle)
  for(String::size_type pos = 0; pos < __convName.size() - 1; pos++){
    if(__convName[pos] == '-'){
      __convName.erase(pos,1);
      pos--;
    }
  }

  // now see if it's a good name
  if(this->IsValidName(__convName)) return true;

  // try to convert some general patterns like plus or minus.
  String::size_type pos = __convName.find("plus");
  if(pos != String::npos){
    __convName.replace(pos,4,"+",0,1);
    if(this->IsValidName(__convName)) return true;
  }
  pos = __convName.find("minus");
  if(pos != String::npos){
    __convName.replace(pos,5,"-",0,1);
    if(this->IsValidName(__convName)) return true;
  }
  pos = __convName.find("zero");
  if(pos != String::npos){
    __convName.replace(pos,4,"0",0,1);
    if(this->IsValidName(__convName)) return true;
  }
  pos = __convName.find("long");
  if(pos != String::npos){
    __convName.replace(pos,4,"l",0,1);
    if(this->IsValidName(__convName)) return true;
  }
  pos = __convName.find("short");
  if(pos != String::npos){
    __convName.replace(pos,5,"s",0,1);
    if(this->IsValidName(__convName)) return true;
  }

  // if the last letter is p or m, try replacing them by + or -
  int last_index = (int)__convName.size()-1;
  if(__convName[last_index] == 'p'){
    tempName = __convName;
    tempName[last_index] = '+';
    if(this->IsValidName(tempName)){
      __convName = tempName;
      return true;
    }
  }
  if(__convName[last_index] == 'm'){
    tempName = __convName;
    tempName[last_index] = '-';
    if(this->IsValidName(tempName)){
      __convName = tempName;
      return true;
    }
  }
  
  // ok...try a few special names...then give up
  pos = __convName.find("proton");
  if(pos != String::npos){
    __convName.replace(pos,6,"p",0,1);
    if(this->IsValidName(__convName)) return true;
  }
  pos = __convName.find("electron");
  if(pos != String::npos){
    __convName.replace(pos,8,"e",0,1);
    if(this->IsValidName(__convName)) return true;
  }

  if(__convName == "e"){
    __convName = "e-";
    return true;
  }
  if(__convName == "positron" || __convName == "antie"){
    __convName = "e+";
    return true;
  }
  if(__convName == "p+"){
    __convName = "p";
    return true;
  }
  if(__convName == "neutron"){
    __convName = "n";
    return true;
  }
  if(__convName == "pbar" || __convName == "antip"){
    __convName = "p-";
    return true;
  }
  __convName = __name;
  return false;
}
//_____________________________________________________________________________

const ParticleInfo& ParticleTypes::GetParticle(const String &__name) const {
  /// Return a constant reference to ParticleInfo object @a name.
  /**
   * <b> Asserts: </b> @a name is a @a valid particle name.
   *
   * If @a name isn't a valid particle name, ParticleTypes will attempt to 
   * convert it to a valid name with ConvertName(). If this is unsuccessful,
   * then the program will be aborted.
   *
   */

  // see if we can find it...
  int i;
  for(i = 0; i < this->Size(); i++){
    if(_particles[i].Name() == __name) return _particles[i];
  }

  String convName;
  // if the name isn't valid, try and convert it to something that is
  if(!this->ConvertName(__name,convName)){
    // the name couldn't be converted
    std::cout << "Error! <ParticleTypes::GetParticle> Could NOT convert "
	      << __name << " to any known particle name." << std::endl;
    this->PrintValidNames();
    abort(); // pro-choice baby!
  }
  // if we get here, the name is now valid
  for(i = 0; i < this->Size(); i++){
    if(_particles[i].Name() == __name) break;
  }
  return _particles[i];
}
//_____________________________________________________________________________

void ParticleTypes::PrintValidNames() const {
  /// Print to the screen a list of valid particle names
  int i;
  std::cout << "Valid particle names:" << std::endl;
  std::cout << "\tLeptons:";
  for(i = 0; i < this->Size(); i++){    
    if(_particles[i].IsLepton()) std::cout << " " << _particles[i].Name();
  }
  std::cout << std::endl;
  std::cout << "\tMesons:";
  for(i = 0; i < this->Size(); i++){    
    if(_particles[i].IsMeson()) std::cout << " " << _particles[i].Name();
  }
  std::cout << std::endl;
  std::cout << "\tBaryons:";
  for(i = 0; i < this->Size(); i++){    
    if(_particles[i].IsBaryon()) std::cout << " " << _particles[i].Name();
  }
  std::cout << std::endl;
}
//_____________________________________________________________________________

void ParticleTypes::Print(std::ostream &__os) const {
  /// Print to @a os all known particles' info.
  __os << "Particle Types: " << std::endl;
  for(int i = 0; i < this->Size(); i++) _particles[i].Print(__os);
  __os << std::endl;
}
//_____________________________________________________________________________
