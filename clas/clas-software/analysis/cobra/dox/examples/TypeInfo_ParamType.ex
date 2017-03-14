// define a template function to take an argument of type _Tp
template<typename _Tp> void foo(typename TypeInfo<_Tp>::ParamType __val){
	std::cout << "Value = " << __val << std::endl;
}