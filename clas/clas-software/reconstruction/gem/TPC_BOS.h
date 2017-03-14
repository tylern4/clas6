// TPC_BOS.h: interface for the TPC_BOS.cc 
// V1.00 Jixie Zhang, Bonus Exp
/////////////////////////////////////////////////////////////////////


#if !defined(AFX_TPC_BOS_H_)
#define AFX_TPC_BOS_H_

 #include <stdio.h>


#ifdef __cplusplus
  extern "C"
  {
#endif
  #include <ntypes.h>
  #include <bostypes.h>
  #include <bosddl.h>

#ifdef __cplusplus
  }
  using namespace std;
  #include <vector>
  extern vector<tpc_t> raw_data;
#endif

  //////////////////////////////////////////////////
  extern int           imTrigType; 
  //////////////////////////////////////////////////
  extern int  ReadTPCBank();
  extern int  ReadOneTPCEvent(); 
  extern int  ReadTGBIBank();

#endif // !defined(AFX_TPC_BOS_H_)


