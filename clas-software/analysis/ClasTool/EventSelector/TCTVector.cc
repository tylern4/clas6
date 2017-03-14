// Autogenerated Class (Source File)
// Author : G.Gavalian
// Date   : Tue Mar 27 16:45:59 EST 2007
//

#include "TCTVector.h"



ClassImp(TCTVector)

TCTVector::TCTVector (const char *name,TLorentzVector v)
{
  fVECT_Name = name;
  fVect = v;
  fVECT_Type = CTVEC_STATIC;
  SetName(name);
}

TCTVector::TCTVector (const char *name,const char *partname)
{
  fVECT_Name  = name;
  fVECT_Type  = CTVEC_PARTICLE;
  fPartName_1 = partname;
  SetName(name);
}

TCTVector::TCTVector (const char *name,const char *oper,const char *part1,const char *part2)
{
  fVECT_Name  = name;
  fVECT_Type  = CTVEC_VIRTUAL;
  fPartName_1 = part1;
  fPartName_2 = part2;
  fOperation  = oper;
  SetName(name);
}


TCTVector::~TCTVector ()
{

}

//-----------------
//-----------------
//-----------------
TLorentzVector TCTVector::GetV()
{
  TLorentzVector  vct;
  if(fVECT_Type==CTVEC_STATIC) return fVect;
  return vct;
}

TLorentzVector TCTVector::GetPartVector(const char *name)
{
  TLorentzVector  vct;
  //  Int_t  nobj = ;
  TCTParticle *ptr = (TCTParticle *) fObjects->FindObject(name);
  if(ptr==NULL){
    printf("ERROR: finding object with name [%s] has failed\n",name);
    return vct;
  }
  vct = ptr->GetV();
  return vct;
}

void TCTVector::PrintV()
{
  printf("%12s | (%6.3f,%6.3f,%6.3f,%6.3f) (%7.4f)\n",GetName(),fVect.Px(),fVect.Py(),fVect.Pz(),fVect.E(),fVect.P());
}

void TCTVector::Print(){
  if(fVECT_Type==CTVEC_STATIC){
    printf("%12s  |  %9s | [%9.5f , %9.5f , %9.5f , %9.5f]\n",
	   GetName(),"STATIC",
	   fVect.Px(),fVect.Py(),fVect.Pz(),fVect.E());
  }
  if(fVECT_Type==CTVEC_PARTICLE){
    printf("%12s  |  %9s | [%12s] |  [%9.5f , %9.5f , %9.5f , %9.5f]\n",
	   GetName(),"PARTICL",fPartName_1.Data(),
	   fVect.Px(),fVect.Py(),fVect.Pz(),fVect.E());
  }

  if(fVECT_Type==CTVEC_VIRTUAL){
    printf("%12s  |  %9s | [%6s] ( ",
	   GetName(),"VIRTUAL",
	   fOperation.Data());
    for(int kk=0;kk<fNOperand;kk++) printf(", [%8s]",fOperVects[kk].Data()); 
    printf(" )\n");
  }
}

Double_t       TCTVector::Get(const char *name)
{
  double  var = -1000.;
  TString  fsys = name;
  if(fsys.CompareTo("M")==0) return fVect.M();
  if(fsys.CompareTo("M2")==0) return fVect.M2();
  if(fsys.CompareTo("E")==0) return fVect.E();
  if(fsys.CompareTo("P")==0) return fVect.P();
  if(fsys.CompareTo("Px")==0) return fVect.Px();
  if(fsys.CompareTo("Py")==0) return fVect.Py();
  if(fsys.CompareTo("Pz")==0) return fVect.Pz();
  if(fsys.CompareTo("Theta")==0) return fVect.Theta();
  if(fsys.CompareTo("Phi")==0) return fVect.Phi();

  return var;
}

void       TCTVector::Fill(TObjArray *farr, const char *system)
{

  if(fVECT_Type==CTVEC_STATIC) return;
  TString  fsys = system;

  //_______ Particle Vectors ____________
  if(fsys.CompareTo("PARTICLE")==0&&fVECT_Type==CTVEC_PARTICLE){
    TCTParticle *fptr = static_cast<TCTParticle*> (farr->FindObject(fPartName_1.Data()));
    if(fptr==NULL){
      //  fVarValue = 0.;
    } else {
      fVect = fptr->GetV();
    }
  }
  //______________________________________
  //______ Vector Variable _________________
  if(fsys.CompareTo("VECTOR")==0&&fVECT_Type==CTVEC_VIRTUAL){
    for(int i=0;i<fNOperand;i++) {
      TCTVector *fptr_1 = static_cast<TCTVector*> (farr->FindObject(fOperVects[i].Data()));
      if(fptr_1==NULL){
	printf("TCTVector::Fill::ERROR vector with name (%s) was not found\n",fOperVects[i].Data());
      } else {
	fOperBuff[i] = fptr_1->GetV();
      }
      
    }

    //    printf("Processing Vector: %s\n",GetName());
//     printf("%f %f %f %f  %s %f %f %f %f \n",fOperBuff[0].Px()
// 	   ,fOperBuff[0].Px(),fOperBuff[0].Pz(),fOperBuff[0].E(),
// 	   fOperation.Data(),fOperBuff[1].Px()
// 	   ,fOperBuff[1].Px(),fOperBuff[1].Pz(),fOperBuff[1].E());
    fVect = fOper.GetOperV(fOperation.Data(),fOperBuff);
  }

}

void       TCTVector::DoOperation(TLorentzVector v1,TLorentzVector v2)
{
  if(fOperation.CompareTo("add")  ==0) fVect = v1 + v2;
  if(fOperation.CompareTo("sub")  ==0) fVect = v1 - v2;
  //  if(fOperation.CompareTo("cross")==0){ fVect = v1; fVect.Cross(v2))};
  
}