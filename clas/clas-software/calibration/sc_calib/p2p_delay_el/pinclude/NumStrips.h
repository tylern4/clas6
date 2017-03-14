//Author: Paul Mattione (12/14/2008)

#ifndef NumStrips_h
#define NumStrips_h

#include <sstream>
#include <stdlib.h>
#include <iostream>

using namespace std;

class NumStrips {

  public: 

    static NumStrips& Instance();

    void Initialize_NumStrips(int locRunNumber);

    inline int Get_NumStrips() const{return nsNumStrips;};
    inline int Get_NumTotalStrips() const{return nsNumTotalStrips;};

  protected:

    NumStrips();
    NumStrips(const NumStrips& locNumStrips);
    virtual ~NumStrips();
    NumStrips& operator = (const NumStrips& locNumStrips);

  private:

    void Set_Defaults();
    int nsNumStrips;
    int nsNumTotalStrips;
};

#endif
