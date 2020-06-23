// PArrayElement_Base class header file. -*- C++ -*- 
#ifndef _PArrayElement_Base_H
#define _PArrayElement_Base_H
/** @file PArrayElement_Base.h 
 *  @brief PArrayElement_Base internal template class definition file.
 */
//_____________________________________________________________________________
// Includes:
#include <iostream>
#include "TObject.h" // to make it ROOT compatible
#include "TypeInfo.h"
//_____________________________________________________________________________

/** @class PArrayElement_Base
 *  @brief Internal base class for primitive instantiations of PArrayElement.
 *
 *  When PArrayElement is instantiated with a primitive type (or pointer) this
 *  class serves as its base class. See PArrayElement for details.
 *
 *  The class inherits from TObject and is fully ROOT compitable. It is 
 *  equipped with a conversion operator to the instantiating class, thus it
 *  acts like the primitive @a _Tp except it can be inherited from
 *.
 */
//_____________________________________________________________________________

template<typename _Tp> class PArrayElement_Base : public TObject {
  
private:
  // Data Members (private):
  _Tp _data; ///< Data of instantiating type

protected:
  // protected access to data:
  
  _Tp& _Data() {
    /// Protected function to obtain a reference to @a _data.
    return _data;
  }
  
  typename TypeInfo<_Tp>::ParamType _Const_Data() const {
    /// Protected @a const function to obtain the value of @a data.    
    return _data;
  }
  
public:
  
  // Create/Copy/Destory:

  PArrayElement_Base():TObject(){
    /// Default Constructor
    _data = _Tp();
  }
  
  PArrayElement_Base(typename TypeInfo<_Tp>::ParamType __value) {
    /// Constructor
    _data = __value;
  }  
  
  PArrayElement_Base(const PArrayElement_Base &__base):TObject(__base){
    /// Copy Constructor
    _data = __base._data;
  }

  virtual ~PArrayElement_Base(){
    /// Destructor
  }
  
  PArrayElement_Base& operator=(const PArrayElement_Base &__base){
    /// Assignment operator
    this->TObject::operator=(__base);
    _data = __base._data;
    return *this;
  }
  
  PArrayElement_Base& operator=(typename TypeInfo<_Tp>::ParamType __value){
    /// Assignment operator
    _data = __value;
    return *this;
  }
    
  operator _Tp () {
    /// Conversion operator to type @a _Tp.    
    return _data;
  }
  
  inline bool operator==(const PArrayElement_Base &__base) const {
    /// Comparison operator
    return (_data == __base._data);
  }

  inline void Print(std::ostream &__os = std::cout) const {
    /// Wrapper to << operator (but with an endl)
    __os << _data << std::endl;
  }

  //ClassDef(PArrayElement_Base,1); // ROOT class defintion macro
};
//_____________________________________________________________________________

#endif /* _PArrayElement_Base_H */
