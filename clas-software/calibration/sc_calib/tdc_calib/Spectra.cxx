#include "Spectra.h"
#include "BankDescription.h"

extern vector<BankDescription> gBankDescr;

extern bool useXwindows;
extern int SC_Version_Flag;

int ecIndex(int sector, int id) {
  int layer = id >> 8;
  if (layer <= 0 || layer > 6) {
    cout << "Invalid layer " << layer << endl;
    throw -1;
  }
  int index = id & 0xFF;
  if (index <=0 || index > 36) {
    cout << "invalid index " << index << endl;
    throw -2;
  }
  return (sector - 1) * 6 * 36 + (layer - 1) * 36 + (index - 1);
}

int scIndex(int sector, int id, int side) {
  if (side < 0) {
    int highbyte = id >> 8;
    side = highbyte;
  }

  if (side < 0 || side > 1) {
    cout << "Invalid side " << side << endl;
    throw -1;
  }
 
  int index = id & 0xFF;

  if(SC_Version_Flag == 2){
    if (index <=0 || index > 57) {
      cout << "invalid index " << index << endl;
      throw -2;
    }
    return side * 342 + (sector- 1) * 57 + (index - 1);
  }else{
    if (index <=0 || index > 57) {
      cout << "invalid index " << index << endl;
      throw -2;
    }
    return side * 288 + (sector- 1) * 48 + (index - 1);
  }
}

int ccIndex(int sector, int id) {
  return (sector - 1) *36 + (id-1);
}

Spectra::Spectra () {
  char name[80];
//cout << "make spectra, version flag = " << SC_Version_Flag << endl;
  InitRoc ();
//cout << "ROC done, make histos" << endl;
  // book histogram

  bankcount = new TH1F("bankcount", "bankcount", 
		       gBankDescr.size(), -0.5,
		       gBankDescr.size()-0.5);
  TAxis* bcax = bankcount->GetXaxis();
  hcount = new TH1F* [gBankDescr.size()];
  hist   = new TdcHistogram** [gBankDescr.size()];
  for (unsigned int i = 0; i<gBankDescr.size(); i++) {
//cout << "make histo group " << i << endl;
    bcax->SetBinLabel(i+1, gBankDescr[i].GetBankName().c_str());
    hist[i] = new TdcHistogram* [gBankDescr[i].GetMaxIndex()];
    for (int j=0; j<gBankDescr[i].GetMaxIndex(); j++) {
      hist[i][j] = new TdcHistogram(j, &gBankDescr[i]);
    }
    char hstname[80];
    sprintf (hstname, "%scount", gBankDescr[i].GetBankName().c_str());
    hcount[i] = new TH1F(hstname, hstname, gBankDescr[i].GetMaxIndex(),
			 -0.5, gBankDescr[i].GetMaxIndex()-0.5);
  }
//cout << "hists made" << endl;

  // overview histograms
  for (int i=0; i< 6; i++) { 
    sprintf (name, "gs%d", i);

    //    gs[i] = new TH2F (name, title[i], SC_MAX/2, -0.5, 287.5, 349, -373.2555, 373.2555);
  }

  if (useXwindows) {
    gStyle->SetOptStat(0);
    gStyle->SetPadRightMargin(0.13);
    Cs = new TCanvas ("Cs", "running data files...", 1200, 800);

    for (int i=0; i< 6; i++) { 
      //      gs[i]->GetXaxis()->SetTitle("SC index");
      //      gs[i]->GetYaxis()->SetTitle("t   [ns]");
    }

    Cs->Divide(4,3);
    for (unsigned int i=0; i<gBankDescr.size(); i++) {
      Cs->cd(i+1);
      hcount[i]->Draw();
    }
    Cs->cd(12);
    bankcount->Draw();
//cout << "stuff drawn" << endl;
    gSystem->ProcessEvents();
cout << "events processed" << endl;
    for (unsigned int i=0; i<12; i++) {
      Cs->cd(i+1);
      gPad->Modified();
    }
    Cs->Update ();
  }
//cout << "done with init" << endl;
}

Spectra::~Spectra () {
}

void Spectra::InitRoc () {
  // initialize ROC slot structure
  try {
    tt = new ReadRoc*[gBankDescr.size()];
    for (unsigned int i = 0; i<gBankDescr.size(); i++) {
      tt[i] = new ReadRoc(&gBankDescr[i]);
    }
  }
  catch (const char* e) {
    cerr << "Error <" << e << ">" << endl; exit (2);
  }
  catch (int e) {
    cerr << "Error " << e << endl; exit (2);
  }
  catch (...) {
    cerr << "Error (unknown type) " << endl; exit (2);
  }
}

void Spectra::PolynomialFit () {
  ofstream locOFstream, locOFstream_IU, locOFstream_IV, locOFstream_IW, locOFstream_OU, locOFstream_OV, locOFstream_OW;
  for (unsigned int i = 0; i<gBankDescr.size(); i++){
    if(gBankDescr[i].GetBankName() == "ECT"){
      locOFstream_IU.open("ECT_TDC_Inner_U.dat");
      locOFstream_IV.open("ECT_TDC_Inner_V.dat");
      locOFstream_IW.open("ECT_TDC_Inner_W.dat");
      locOFstream_OU.open("ECT_TDC_Outer_U.dat");
      locOFstream_OV.open("ECT_TDC_Outer_V.dat");
      locOFstream_OW.open("ECT_TDC_Outer_W.dat");
    }

    for (int j=0; j<gBankDescr[i].GetMaxIndex(); j++){
      hist[i][j]-> PolynomialFit(gBankDescr[i].IsPipeline());
      if(gBankDescr[i].GetBankName() == "LSRT"){
        cout << "t1 for LSRT pmt index " << j << " is " << hist[i][j]->GetT1() << endl;
        cout << "t1 error for LSRT pmt index " << j << " is " << hist[i][j]->GetT1Error() << endl;
      }
      if(gBankDescr[i].GetBankName() == "ECT"){
        string locFileName;
        string locOFStream;
        switch(j/216){
          case 0: locOFstream_IU << -1.0*(hist[i][j]->GetT1()) << "\n"; break;
          case 1: locOFstream_IV << -1.0*(hist[i][j]->GetT1()) << "\n"; break;
          case 2: locOFstream_IW << -1.0*(hist[i][j]->GetT1()) << "\n"; break;
          case 3: locOFstream_OU << -1.0*(hist[i][j]->GetT1()) << "\n"; break;
          case 4: locOFstream_OV << -1.0*(hist[i][j]->GetT1()) << "\n"; break;
          case 5: locOFstream_OW << -1.0*(hist[i][j]->GetT1()) << "\n"; break;
          default: break;
        }
      }
    }
    if(gBankDescr[i].GetBankName() == "ECT"){
      locOFstream_IU.close();
      locOFstream_IV.close();
      locOFstream_IW.close();
      locOFstream_OU.close();
      locOFstream_OV.close();
      locOFstream_OW.close();
    }
  }
}

void Spectra::AverageT0 () {
  /*
  map<int,SlotAverage> a;

  for (int i = 0; i < SC_MAX; i++) 
    a[ttsc->GetSlot(i)] += sctdc[i]->GetT0();

  for (int i = 0; i < SC_MAX; i++) 
    sctdc[i]->SubtractAverageT0(a[ttsc->GetSlot(i)].getAverage());
  */
} 

void Spectra::HandOverResults (JMainMenu* mm) {
  if (useXwindows) mm->SetCanvas(Cs);
  //  mm->SetGaussHisto(gs);
  //  for (int i = 0; i < SC_MAX; i++) 
    // swap tdc according to swapindex table from database
    /*
    mm->SetChannel(i, i%288, i/288, 
		   sctdc[i]->GetGraph(), sctdc[i]->GetHisto(), 
		   sctdc[i]->GetPar(), sctdc[i]->GetErr());
    */
  mm->SetOverview();
}

void Spectra::DumpValues () {
  for (unsigned int i = 0; i<gBankDescr.size(); i++) {
    for (int j=0; j<gBankDescr[i].GetMaxIndex(); j++) {
      cout << (*hist[i][j]) << endl;
    }
  }
}

void Spectra::GaussFit (double texp) {
  bool isModified = false;
  GaussFitResult* r ;
  gStyle->SetPalette(1);
  for (unsigned int i = 0; i<gBankDescr.size(); i++) {
    for (int j=0; j<gBankDescr[i].GetMaxIndex(); j++) {
      if ((r = hist[i][j]->GaussFit (texp, j))) {  // do the fitting 
	/*
	float expecTime =  (i > 288 ? -1. : 1.) * r->t;
	gs[0]-> Fill ( (float) i, expecTime, (r->bad(bd->type!=1872) ? 2.: 1.));
	gs[1]-> Fill ( (float) i, expecTime, r->mean);
	gs[2]-> Fill ( (float) i, expecTime, r->sigma);
	gs[3]-> Fill ( (float) i, expecTime, r->area);
	gs[4]-> Fill ( (float) i, expecTime, r->chiSquare);
	gs[5]-> Fill ( (float) i, expecTime, r->errMean);
	*/
	isModified = true;
	delete r;
      }
    }
  }

  if (useXwindows && isModified) {
    gSystem->ProcessEvents();
    for (unsigned int i=0; i<12; i++) {
      Cs->cd(i+1);
      gPad->Modified();
    }
    Cs->Update();
  }
}

void Spectra::WriteHist(const char* filename) {
  if (filename == NULL) filename = "tdc_calib.root";
  TFile ff (filename, "recreate");
  
  bankcount->Write();
  for (unsigned int i = 0; i<gBankDescr.size(); i++) {
    ff.cd();
    TDirectory* dir =  // separate directories for EC, SC, SCT, ...
      ff.mkdir(gBankDescr[i].GetBankName().c_str(), 
	       gBankDescr[i].GetBankName().c_str());
    dir->cd();
    hcount[i]->Write();
    for (int j=0; j<gBankDescr[i].GetMaxIndex(); j++) {
      hist[i][j]->Write();
    }
  }

  // ff destructor closes file
}

void Spectra::Fill (JBosEvent* be, Expect* expct) {

  try {
  for (unsigned int i=0; i<gBankDescr.size(); i++) {
    char bankname[5];
    sprintf (bankname, "%-4s", gBankDescr[i].GetBankName().c_str()); 
    for (int j=gBankDescr[i].GetMinSector(); 
	 j<=gBankDescr[i].GetMaxSector(); j++) {
      rawtdcbank_t* rtb = 
	(rawtdcbank_t*) (gBankDescr[i].GetMinSector() ?
			 getGroup(&bcs_, bankname, j) :
			 getBank(&bcs_, bankname));
      if (rtb) {
	if (rtb->head.ncol != gBankDescr[i].GetColumns()) {
	  cerr << gBankDescr[i].GetBankName() << " col exp:"
	       << gBankDescr[i].GetColumns() << "\t"
	       << rtb->head.ncol << endl;
	  throw "wrong number of columns";
	}
	bankcount->Fill(i);

	for (int k=0; k<rtb->head.nrow; k++) {
	  int kk = k * gBankDescr[i].GetColumns();
	  int index = gBankDescr[i].CalculateIndex(j,rtb->data[kk]);
	  int tdcval = rtb->data[kk+1];
//cout << "fill hist; bank index, sector, id, index, tdcvalue = " << i << ", " << j << ", " << rtb->data[kk] << ", " << index << ", " << tdcval << endl;
	  if (index <  gBankDescr[i].GetMaxIndex()) {
//cout << "filling left " << index << " hist" << endl;
	    hist[i][index]->Fill(tdcval, gBankDescr[i].IsPipeline());
	    hcount[i]->Fill(index);
	  }
	  if (gBankDescr[i].IsLeftRight()) { //if using SC bank, LR data in same entry (old method) (SCT bank is separate entries)
	    index = gBankDescr[i].CalculateIndex(j,rtb->data[kk],1);
	    tdcval = rtb->data[kk+3];
//cout << "fill right hist; bank index, sector, id, index, tdcvalue = " << i << ", " << j << ", " << rtb->data[kk] << ", " << index << ", " << tdcval << endl;
	    if (index <  gBankDescr[i].GetMaxIndex()) {
//cout << "filling right " << index << " hist" << endl;
	      hist[i][index]->Fill(tdcval, gBankDescr[i].IsPipeline());
	      hcount[i]->Fill(index);
	    }
	  }
	} //  for (k = )  rows in bank
      }   //  if (rtb)    bank found
    }     //  for (j = )  loop sector
  }       //  for (i = )  loop banks

  }       //  try
  catch (char* excpt) {
    cout << "Spectra fill Cought: <" << excpt << ">" << endl;
  }
  catch (...) {
    cout << "unknown exception cought" << endl;
  }
}
