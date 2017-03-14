// ParticleArray class header file. -*- C++ -*-
#ifndef _ParticleArray_H
#define _ParticleArray_H
/** @file parray/ParticleArray.h
 *  @brief ParticleArray template class definition file
 */
//_____________________________________________________________________________
// System Headers:
#include <vector>
#include <iostream>
#include <string>
// Local Headers:
#include "TypeInfo.h"
#include "PArrayElement.h"
//_____________________________________________________________________________

// Global Types:
typedef std::string String; 

//_____________________________________________________________________________

// class description is in ParticleArray.tcc

template<typename _Tp> class ParticleArray : public TObject {

private:
  // Data Members (private):  
  std::vector<PArrayElement<_Tp> > _data; ///< Data vector (see PArrayElement)

  // Functions (private):
  
  inline void _Resize(int __size){
    /// Private function for quick memory grabs.
    /** @param size Number of elements to reserve memory for. */
    _data.resize(__size);
  }

  inline bool _CheckIndexValidity(int __i) const {
    /// Checks whether @a i is a valid index for this array.
    if(__i >= this->Size() || __i < 0){
      std::cout << "Error! ParticleArray index " << __i << " is out of bounds."
		<< " Array size = " << this->Size() << std::endl;
      return false;
    }
    return true;
  }    
  
  // copies the _data vector from p to this
  void _Copy(const ParticleArray &__p);

public:
  // Create/Copy/Destroy: 

  ParticleArray():TObject(){
    /// Default Constructor (constructs an empty ParticleArray object)
  }

  // ctor to build array from string like "p1:p2:p3:..." with pn valid names
  ParticleArray(const String &__p_names);

  // the next 2 ctor's build arrays from vector<string>'s
  ParticleArray(const std::vector<String> &__names,
		typename TypeInfo<_Tp>::ParamType __value = _Tp()):TObject(){
    /// Constructor 
    /** See Add(const vector<string>&,TypeInfo<_Tp>::ParamType) for details */
    this->Add(__names,__value);
  }

  ParticleArray(const std::vector<String> &__names,
		const std::vector<_Tp> &__values):TObject(){
    /// Constructor 
    /** See Add(const vector<string>&,const vector<_Tp>&) for details */
    this->Add(__names,__values);
  }

  ParticleArray(const ParticleArray &__p):TObject(__p){
    /// Copy Constructor
    /** @param p ParticleArray of same type as this. */
    this->_Copy(__p);
  }

  ~ParticleArray(){
    /// Destructor
    /** See ParticleArray::Clear() for memory management details. */
    this->Clear();
  }
  
  ParticleArray& operator=(const ParticleArray &__p){
    /// Assignment Operator.
    /**
     * @param p ParticleArray object of same type as this.
     *
     * If this ParticleArray is @a NOT the same size as @a __p, then it will
     * be resized (see _Copy() for details).
     *
     */
    this->TObject::operator=(__p);
    this->_Copy(__p);
    return *this;
  }
    
  // Comparison operators:

  inline bool operator==(const ParticleArray &__p) const {
    /// Comparison operator.
    return (_data == __p._data);
  }

  inline bool operator!=(const ParticleArray &__p) const {
    /// Comparison operator.
    return !(*this == __p);
  }
  
  // Getter operators:

  inline PArrayElement<_Tp>& operator()(const String &__name,int __n = 0) {
    /// Returns a reference to the <em>(name,n)</em> entry in the array
    /** See ParticleArray::GetIndex() for details */
    return const_cast<PArrayElement<_Tp> &>(this->GetEntry(__name,__n));
  }

  inline const PArrayElement<_Tp>& operator()(const String &__name,int __n=0)const{
    /// Returns constant reference to the <em>(name,n)</em> entry in the array
    /** See ParticleArray::GetIndex() for details.*/
    return this->GetEntry(__name,__n);
  }

  inline PArrayElement<_Tp>& operator[](int __i) {
    /// Returns a reference to the @a i'th element in the array.
    /** <b> Asserts: </b> @a i < Size()*/
    if(!(this->_CheckIndexValidity(__i))) abort(); 
    return _data[__i];
  }

  inline const PArrayElement<_Tp>& operator[](int __i) const {
    /// Returns a constant reference to the @a i'th element in the array.
    /** <b> Asserts: </b> @a i < Size()*/
    if(!(this->_CheckIndexValidity(__i))) abort(); 
    return _data[__i];
  }    
  
  // Functions:

  // gets the index of name particle number n
  int GetIndex(const String &__name,int __n = 0) const;

  // add a particle to the array
  bool Add(const String &__name,typename TypeInfo<_Tp>::ParamType __value=_Tp());

  // the next 2 add multiple particles to the array
  int Add(const std::vector<String> &__names,const std::vector<_Tp> &__values);

  int Add(const std::vector<String> &__names,
	  typename TypeInfo<_Tp>::ParamType __value = _Tp()){
    /// Add a number of elements to the array.
    /** Same as Add(vector,vector) but @a ALL elements added equal @a value */
    std::vector<_Tp> vect_tp(__names.size(),__value);
    return this->Add(__names,vect_tp);
  }
    
  void Remove(const String &__name,int __n = 0){
    /// Remove the <em>(name,n)</em> entry from the array.
    /** See GetIndex() for details on named indexing in ParticleArray */
    this->Remove(this->GetIndex(__name,__n));
  }

  void Remove(int __i){
    /// Remove element @a i from the array.
    if(__i < 0 || __i >= this->Size()) return;
    typename std::vector<PArrayElement<_Tp> >::iterator iter = _data.begin();
    iter += __i;
    _data.erase(iter);
  }

  const PArrayElement<_Tp>& GetEntry(const String &__name,int __n = 0) const {
    /// Returns constant reference to the <em>(name,n)</em> entry in the array
    /** See ParticleArray::GetIndex() for details.*/ 
    int index = this->GetIndex(__name,__n);
    if(index < 0){ 
      std::cout << "Error! No (" << __name << "," << __n << ") entry in "
		<< "ParticleArray." << std::endl;
      abort();
    }
    return _data[index];
  }

  inline bool HasParticle(const String &__name,int __n = 0) const {
    /// Is the array storing a <em>(name,n)</em> particle?
    return (this->GetIndex(__name,__n) >= 0);
  }

  inline int Size() const {
    /// Returns the number of entries in the array.
    return (int)_data.size();
  }

  inline bool Empty() const {
    /// Returns @a true if the array is empty, @a false otherwise.
    return _data.empty();
  }

  inline int Count(const String &__name) const {
    /// Counts how many @a name entries there are in the array.
    String good_name;
    int num = 0;
    if(ParticleTypes::Instance()->ConvertName(__name,good_name)){
      for(int i = 0; i < this->Size(); i++)
	if(_data[i].Name() == __name) num++;
    }
    return num;
  }

  void Print() const {
    /// Prints ParticleArray to @a cout.
    /** Not implemented to generic @a ostream due to interference of having a
     *  templated @a << operator with ROOT dictionary generation.
     */
    for(int i = 0; i < this->Size(); i++) _data[i].Print();
  }

  inline void Clear(){
    /// Remove @a ALL elements from the array (see Remove() for details).
    for(int i = (this->Size() - 1); i >= 0; i--) this->Remove(i);
  }

  inline std::vector<String> GetNames() const {
    /// Returns a vector of just the particle names in the array
    std::vector<String> names(this->Size());
    for(int i = 0; i < this->Size(); i++) names[i] = _data[i].Name();
    return names;
  }

  inline String GetNamesString() const {
    /// Returns a string with the particle names using same format as @a ctor
    String names;
    for(int i = 0; i < this->Size(); i++) names += _data[i].Name() + ":";
    names.erase((int)names.size()-1,1);
    return names;
  }

  //ClassDef(ParticleArray,1); // ROOT definition of ALL instantiations 
};
//_____________________________________________________________________________

#include "ParticleArray.tcc"

#endif /* _ParticleArray_H */
