// ParticleArray class template functions. -*- C++ -*-
#ifndef _ParticleArray_TCC
#define _ParticleArray_TCC
/** @file ParticleArray.tcc
 *  @brief ParticleArray template functions file.
 */
//_____________________________________________________________________________

/** @class ParticleArray
 *  @brief Variable length templated array indexed by particle names.
 *
 *  ParticleArray is a dynamic array that maps data to particle names. The 
 *  particle names are used as indicies to the ParticleTypes singleton class
 *  that stores a list of known particle information (See ParticleTypes for 
 *  details on what information is stored and accessed). A complete list of 
 *  known particles can be found in ListOfParticles.lst. ParticleArray can 
 *  store data of any type that can be stored by the stl vector class.
 *
 *  An element can only be added to the array with a valid particle name. See
 *  the Add functions for details on adding elements. If all particle names are
 *  known at creation time, a convienent creation method is to call 
 *  ParticleArray("a:b:c") that will store data for particles a,b and c.
 *
 *  <b>Example:</b>
 *
 *  \include ParticleArray_Ctor.ex
 *
 *  The elements in the array are of the type PArrayElement<_Tp>. This class
 *  has wrappers to all of the ParticleInfo public member functions which 
 *  provide access to the global particle info. A PArrayElement<_Tp> object 
 *  also functions like an object of type @a _Tp. If @a _Tp is an object, 
 *  PArrayElement<_Tp> inherits from it, thus, inherits its public methods.
 *  So, as long as the method isn't the same name as one in ParticleInfo, it 
 *  can be accessed directly from the PArrayElement object.
 *
 *  <b>Example Usage: </b>
 *
 *  \include ParticleArray_Methods.ex
 *
 *  PArrayElement<_Tp> also has conversion operators to type @a _Tp. Thus, it
 *  can be passed to a function as a @a _Tp object. So, in essence, it can be 
 *  treated as if it were a @a _Tp object.
 *
 *  \include ParticleArray_Conversion.ex
 *
 *  See PArrayElement for details on using ParticleArray elements.
 *
 *  If the array contains identical particles (ex. 2 protons), they can be 
 *  accessed by using a subscript in addition to the particle name.
 *
 *  <b> Example: </b>
 *
 *  \include ParticleArray_MultipleParts.ex
 *
 */
//_____________________________________________________________________________

template<typename _Tp> ParticleArray<_Tp>::ParticleArray(const String &__p_names):TObject(){
  /// Constructor
  /** @param p_names String of particle names of form <em>"a:b:c:..."</em>
   *
   *  Parses the string @a p_names to get particle names. The form of the 
   *  string should be <em>"name1:name2:name3:..."</em> and so on for as many
   *  particles as needed. See ListOfParticles.lst for a list of valid particle
   *  names, along with ParticleTypes::ConvertName() for aliases that will 
   *  work.
   *
   *  Once a vector<string> of names is parsed out of @a p_names, then the 
   *  particles are added to the array using Add(const vector<string>&).
   *
   */
  std::vector<String> p_names;
  String name;
  // parse the string, anything inbetween colons take as a particle name
  String::size_type input_size = __p_names.size();
  String::size_type start_pos,colon_pos;
  start_pos = 0;
  while(start_pos < input_size){
    // find next colon
    colon_pos = __p_names.find(":",start_pos);
    if(colon_pos != String::npos){
      name.assign(__p_names,start_pos,(colon_pos - start_pos));
      p_names.push_back(name);
      start_pos = colon_pos + 1;
    }
    else break;
  }
  // get whatever's left
  if(start_pos < input_size){
    name.assign(__p_names,start_pos,(input_size - start_pos));
    // check that it's not just white space
    bool not_white_space = false;
    for(String::size_type n = 0; n < name.size(); n++){
      if(name[n] != ' ') not_white_space = true;
    }
    if(not_white_space){
      p_names.push_back(name);
    }
  }

  *this = ParticleArray(p_names);
}
//_____________________________________________________________________________

template<typename _Tp>
void  ParticleArray<_Tp>::_Copy(const ParticleArray<_Tp> &__p){
  /// Private member function used to copy another ParticleArray's elements.
  /** @param p ParticleArray object of same type as this.
   *
   * The array will be resized if needed.
   *
   */
  int size = __p.Size();
  if(this->Size() != size) this->_Resize(size);
  for(int i = 0; i < size; i++) _data[i] = __p._data[i];
}
//_____________________________________________________________________________

template<typename _Tp>
int ParticleArray<_Tp>::GetIndex(const String &__name,int __n) const {
  /// Get the index for @a name in the array.
  /** 
   * @param name Particle name 
   * @param n Which @a name particle to get (defaults to 0)
   *
   * Note: @a n is a @a C-index, so <em> n = 0 </em> is the first @a name
   * particle, <em> n = 1 </em> is the second, etc...
   *
   * If the array has no <em>(name,n)</em> entry, @a -1 is returned.
   *
   * Note: @a name is run thru ParticleTypes::ConvertName.
   */
  int num = 0;
  String good_name;
  String name;
  if(ParticleTypes::Instance()->ConvertName(__name,good_name)){
    name = good_name;
  }
  else return -1;
  for(int i = 0; i < this->Size(); i++){
    if(name == _data[i].Name()){
      if(num == __n) return i;
      num++;
    }
  }
  return -1;
}
//_____________________________________________________________________________

template<typename _Tp>
bool ParticleArray<_Tp>::Add(const String &__name,typename TypeInfo<_Tp>::ParamType __value){
  /// Add a particle to the array.
  /**
   * @param name Name of the particle.
   * @param value Value associated with name (defaults to _Tp()).
   *
   * If @a name is a valid particle name or is able to be converted to 
   * one by ParticleTypes (see ListOfParticles.lst for a list of valid particle
   * names and info and ParticleTypes::ConvertName() to see what variations
   * of valid names are able to be converted) it will be added to the array and
   * the function will return @a true. Otherwise, the particle will @a NOT be
   * added to the array and Add will return @a false.
   *
   */
  String good_name;
  if(ParticleTypes::Instance()->ConvertName(__name,good_name)){
    PArrayElement<_Tp> p_elem(good_name,__value);
    _data.push_back(p_elem);
    return true;
  }
  else return false;
}
//_____________________________________________________________________________

template<typename _Tp>
int ParticleArray<_Tp>::Add(const std::vector<String> &__names,const std::vector<_Tp> &__values){
  /// Add a number of elements to the array.
  /**
   * @param names Vector of particle names
   * @param values Vector of values to be associated with names
   *
   * <b> Returns: </b> Number of particles successfully added to the array.
   *
   * See the 1 particle Add function for details on whether or not a particle
   * will be added.
   *
   * Note: If @a values has fewer elements than @a names, then the default 
   * value of @a _Tp is paired with the extra elements in @a names.
   *
   */
  String good_name;
  int size = (int)__names.size();
  int v_size = (int)__values.size();
  int num = 0;
  this->_Resize(size); // set to max size needed
  for(int i = 0; i < size; i++){ 
    if(ParticleTypes::Instance()->ConvertName(__names[i],good_name)){
      _data[num].SetName(good_name);
      if(i < v_size) _data[num] = __values[i];
      else _data[num] = _Tp();
      num++;
    }
  }
  this->_Resize(num);
  return num;
}
//_____________________________________________________________________________

#endif /* _ParticleArray_TCC */
