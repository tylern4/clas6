// TypeInfo class static member initializations. -*- C++ -*- 
#ifndef _TypeInfo_TCC
#define _TypeInfo_TCC

// Note: primitive's are in TypeInfo.C

// Default template:
template <typename _Tp> const  bool TypeInfo<_Tp>::IsPointer = false;
template <typename _Tp> const  bool TypeInfo<_Tp>::IsPrimitive = false;

// Pointer specification:
template <typename _Tp> const  bool TypeInfo<_Tp*>::IsPointer = true;
template <typename _Tp> const  bool TypeInfo<_Tp*>::IsPrimitive = false;

#endif /* _Type_TCC */
