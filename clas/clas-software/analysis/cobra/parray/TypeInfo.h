// TypeInfo class header file. -*- C++ -*- 
// Author: Mike Williams (9/15/2004)
#ifndef _TypeInfo_H
#define _TypeInfo_H
//_____________________________________________________________________________
/** @file TypeInfo.h 
 *  @brief Internal class TypeInfo definition file
 */
//_____________________________________________________________________________
/** @class TypeInfo<_Tp>
 *  @brief Internal template class used to determine type properties.
 *  
 *  TypeInfo is a template class used to determine properties of the type used
 *  to instantiate it. Each instantiation contains 2 typedefs and 2 static 
 *  boolean members. The typedef ParamType is how the instantiating type should
 *  be passed as an argument. For basic types, it's the type itself. For
 *  example, TypeInfo<float>::ParamType is @a float. For pointers, it's the
 *  pointer and for objects it's a constant reference to the object. So,
 *  TypeInfo<SomeObject>::ParamType would be <em> const SomeObject& </em>.
 *
 *  <b>Exmaple Use:</b>                          
 * <!--
 * template<typename _Tp> void foo(typename TypeInfo<_Tp>::ParamType __val){
 *    ...
 *   };                                                                      
 * -->                                                        
 * \include TypeInfo_ParamType.ex
 *
 *  The second typedef, PArrayElement_BaseType, is what the base class of
 *  PArrayElement should be. See PArrayElement for details.
 *
 *  The two static boolean members give the user access to basic info about the
 *  instantiating type. IsPointer is @a true if the type was a pointer and 
 *  @a false otherwise. IsPrimitive is @a true if the type was a basic type
 *  and @a false otherwise.
 *  
 * <b> Specifications provided for: </b><br>
 *                         float,double,long double,int,long int,short int,  
 *                         signed char,unsigned char,unsigned short int,     
 *                         unsigned int,unsigned long int,bool    
 *
 *  Any non-pointer type not listed will instantiate the default @a object
 *  template. Thus, it'll have ParamType as a constant reference, 
 *  PArrayElement_BaseType will be it's own type, IsPointer and IsPrimitive
 *  will both be false. If this isn't the desired behavior, then define a
 *  specification for the type in its header file and make sure you remember
 *  to declare its static members in its source file.
 *
 *  Note: This class is ROOT compitable (but can't be written to a file).
 */                                                                           
//_____________________________________________________________________________

#include "TObject.h" // for //ClassDef macro
//_____________________________________________________________________________

// the base class for PArrayElement instantiated with any primitive type
template<typename _Tp> class PArrayElement_Base;
//_____________________________________________________________________________

// default template: 
template <typename _Tp> class TypeInfo {
  
 public:

  typedef _Tp PArrayElement_BaseType; ///< Base type for PArrayElement<_Tp>

  typedef const _Tp& ParamType; ///< How _Tp should be passed as an argument

  const static bool IsPointer;  ///< Is _Tp a pointer?

  const static bool IsPrimitive; ///< Is _Tp a primitive?

  //ClassDef(TypeInfo,0); // ROOT class definition macro for ALL instances 
};
//________________________________________________________________________
#ifndef DOXYGEN_SKIP_THIS

// primitive specifications:

template<> class TypeInfo<float> {
  
 public:
  
  typedef PArrayElement_Base<float> PArrayElement_BaseType;

  typedef float ParamType;

  const static bool IsPointer;

  const static bool IsPrimitive;
};
//________________________________________________________________________

template<> class TypeInfo<double> {
  
 public:
  
  typedef PArrayElement_Base<double> PArrayElement_BaseType;

  typedef double ParamType;

  const static bool IsPointer;

  const static bool IsPrimitive;
};
//________________________________________________________________________

template<> class TypeInfo<int> {
  
 public:
  
  typedef PArrayElement_Base<int> PArrayElement_BaseType;

  typedef int ParamType;

  const static bool IsPointer;

  const static bool IsPrimitive;
};
//________________________________________________________________________

#ifndef _generate_ROOT_dictionary_ // root doesn't like these types

template<> class TypeInfo<long double> {

 public:

  typedef PArrayElement_Base<long double> PArrayElement_BaseType;

  typedef long double ParamType;

  const static bool IsPointer;

  const static bool IsPrimitive;
};
//________________________________________________________________________

template<> class TypeInfo<long int> {
  
 public:

  typedef PArrayElement_Base<long int> PArrayElement_BaseType;  

  typedef long int ParamType;

  const static bool IsPointer;

  const static bool IsPrimitive;
};
//________________________________________________________________________

template<> class TypeInfo<short int> {
  
 public:
  
  typedef PArrayElement_Base<short int> PArrayElement_BaseType;

  typedef short int ParamType;

  const static bool IsPointer;

  const static bool IsPrimitive;
};
//________________________________________________________________________

template<> class TypeInfo<signed char> {
  
 public:
  
  typedef PArrayElement_Base<signed char> PArrayElement_BaseType;

  typedef signed char ParamType;

  const static bool IsPointer;

  const static bool IsPrimitive;
};
//________________________________________________________________________

template<> class TypeInfo<unsigned char> {
  
 public:
  
  typedef PArrayElement_Base<unsigned char> PArrayElement_BaseType;

  typedef unsigned char ParamType;

  const static bool IsPointer;

  const static bool IsPrimitive;
};
//________________________________________________________________________

template<> class TypeInfo<unsigned short int> {
  
 public:
  
  typedef PArrayElement_Base<unsigned short int> PArrayElement_BaseType;

  typedef unsigned short int ParamType;

  const static bool IsPointer;

  const static bool IsPrimitive;
};
//________________________________________________________________________

template<> class TypeInfo<unsigned int> {
  
 public:
  
  typedef PArrayElement_Base<unsigned int> PArrayElement_BaseType;

  typedef unsigned int ParamType;

  const static bool IsPointer;

  const static bool IsPrimitive;
};
//________________________________________________________________________

template<> class TypeInfo<unsigned long int> {
  
 public:
  
  typedef PArrayElement_Base<unsigned long int> PArrayElement_BaseType;

  typedef unsigned long int ParamType;

  const static bool IsPointer;

  const static bool IsPrimitive;
};
#endif /* _generate_ROOT_dictionary_ */
//________________________________________________________________________

template<> class TypeInfo<bool> {

 public:

  typedef PArrayElement_Base<bool> PArrayElement_BaseType;

  typedef bool ParamType;

  const static bool IsPointer;

  const static bool IsPrimitive;
};
//________________________________________________________________________

// partial specialization for pointers:

template<typename _Tp> class TypeInfo<_Tp*> {

 public:
 
  typedef PArrayElement_Base<_Tp*> PArrayElement_BaseType;

  typedef _Tp* ParamType;
  
  const static bool IsPointer;

  const static bool IsPrimitive;
};
//________________________________________________________________________
#endif /* DOXYGEN_SKIP_THIS */

#include "TypeInfo.tcc"

#endif /* _Type_H */
