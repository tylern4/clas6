#ifndef __SPECTRA_H
#define __SPECTRA_H
#include <stdlib.h>
#include <stdio.h>
#include <string>
#include "JMainMenu.h"
#include "TdcHistogram.h"
#include "Expect.h"
#include "SlotAverage.h"
#include "ReadRoc.h"
#include "JBosFile.h"

typedef struct {
  bankHeader_t head;
  uint16       data[2];
} rawtdcbank_t;


class Spectra {
  TdcHistogram***  hist;
  ReadRoc**        tt;     // Translation tables
  void InitRoc();          // function to initialize Translation tables
  
  TH1F** hcount;
  TH1F*  bankcount;

  TCanvas* Cs;                           
public:
  Spectra ();
  ~Spectra ();
  void AverageT0();
  void DumpValues ();
  void Fill (JBosEvent*, Expect*);        // Process a single (non sync) event
  void GaussFit (double texp);            // obvious
  void MainMenu ();
  void WriteHist (const char* filename);
  void PolynomialFit();
  void HandOverResults(JMainMenu* mm);
};

#endif
