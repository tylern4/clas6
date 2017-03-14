#ifdef __CINT__

#pragma link off all globals;
#pragma link off all classes;
#pragma link off all functions;

// link ParticleArray of basic types:

// float:
#pragma link C++ class PArrayElement_Base<float>;
#pragma link C++ class TypeInfo<float>;
#pragma link C++ class PArrayElement<float>;
#pragma link C++ class ParticleArray<float>;

// double:
#pragma link C++ class PArrayElement_Base<double>;
#pragma link C++ class TypeInfo<double>;
#pragma link C++ class PArrayElement<double>;
#pragma link C++ class ParticleArray<double>;

// int:
#pragma link C++ class PArrayElement_Base<int>;
#pragma link C++ class TypeInfo<int>;
#pragma link C++ class PArrayElement<int>;
#pragma link C++ class ParticleArray<int>;


#endif
