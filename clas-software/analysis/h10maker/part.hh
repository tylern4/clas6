#ifndef PART_HH
#define PART_HH

#include <TTree.h>

const int maxpart = 10;

extern "C" {
  struct partnt_ {
    int nprt;
    int pidpart[maxpart];
    float xpart[maxpart];
    float ypart[maxpart];
    float zpart[maxpart];
    float epart[maxpart];
    float pxpart[maxpart];
    float pypart[maxpart];
    float pzpart[maxpart];
    float qpart[maxpart];
    int flagspart[maxpart];
  };

  extern struct partnt_ partnt_;
  
  extern void fill_part_nt_();
}

void part_branches(TTree* tree);

#endif
