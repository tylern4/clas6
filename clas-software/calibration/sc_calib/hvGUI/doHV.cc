// GUI application to calibrate TOF High Voltages
// This is the code that reads in parm and BURT files
// and calculates new HV
// File : doHV.C
// Author : Joe Santoro (Catholic U.)
// Mod. Author :  Paul Mattione (Rice)
// Creation Date  : 11/29/2004
//*****************************

#include "doHV.h"

double locMaxDeltaV = 250.0;
//****** MEMBER FUNCTION TO PROCESS FILES AND CALCULATE NEW HV
doHV::doHV(int locAngleFlag, TGTextView *CalVals) {
  TGFileDialog *openBurt = new TGFileDialog(gClient->GetRoot(),gClient->GetRoot(),kFDOpen,&fileinfo);
  TTimeStamp   *Time     = new TTimeStamp();
  burtFile=fileinfo.fFilename;
  cout<<"BURT FILE IS:"<<burtFile<<endl;
  BURTFILE.open(burtFile);
  Date = TString(Time->AsString("cl"));
  Date = Date.Remove(19,Date.Length());
  Filename  = "SC_"+Date;
  Filename += "_HV_GAINS.calib";
  NEW_BURTFILE.open(Filename.Data());

  for(i=1;i<=3431;i++){ //Loop over BURT file to create TString Array
    BURTFILE.getline(line,200);
    if(i>=1&&i<=11){
      header_line = TString(line);
      header[i-1] = header_line;
    }
    if(i>=12){ //consistent with 57*6 = 342, 342*2(Left/Right)*5(voltage every 5lines) + 11(header) = 3431
      in_from_burt = TString(line);
      burtArray[i-12] = in_from_burt;
    }
  }

  BURTFILE.close();

  //Write out header to new BURT file
  NEW_BURTFILE<<header[0]<<endl;
  NEW_BURTFILE<<"Time:     "<<DateString(Time)<<endl;
  for(i=2;i<=10;i++)
    NEW_BURTFILE<<header[i]<<endl;

  //Loop to create array of BURT named objects
  for(i=0;i<=683;i++) //57*6*2 = 684
    COUNTER[i] = new BurtNamedObj(i,burtArray);

  //Loop to create array of PARM named objects for the Large angles
  if((locAngleFlag==1) || (locAngleFlag==2)){
    PARMFILE.open("fit_parametersLA.dat");
    for(i=0;i<=203;i++){ //34(LA counters)*6 = 204
      PARMFILE>>Paddle>>GAIN>>CENTROID;
      PADDLE_LA[i] = new ParmNamedObj(Paddle,GAIN,CENTROID);
    }
    PARMFILE.close();
  }

  //Loop to create array of PARM named objects for the Forward angles
  if((locAngleFlag==0) || (locAngleFlag==2)){
    PARMFILE.open("fit_parametersFA.dat");
    for(i=0;i<=137;i++){ //23(FA counters)*6 = 138
      PARMFILE>>Paddle>>GAIN>>CENTROID;
      PADDLE_FA[i] = new ParmNamedObj(Paddle,GAIN,CENTROID);
    }
    PARMFILE.close();
  }

  // ********* Calculate new HV for LA and write out BURT FILE
  if((locAngleFlag==1) || (locAngleFlag==2)){
    for(i=0;i<=683;i++){ //57*6*2 = 684
      name_comp1 = TString(COUNTER[i]->GetName());
      OLD_HV     = COUNTER[i]->VOLTAGE;
      for(j=0;j<=203;j++){ //34(LA counters)*6 = 204
        name_comp2 = TString(PADDLE_LA[j]->GetName());
        if(name_comp1.CompareTo(name_comp2)==0){
          GAIN       = PADDLE_LA[j]->Gain;
          CENTROID   = PADDLE_LA[j]->Centroid;
//          cout<<COUNTER[i]->GetTitle()<<endl;
          NEW_HV     = CalNewHV(OLD_HV,GAIN,CENTROID,COUNTER[i]->LEFT_RIGHT);
          COUNTER[i]->NEW_VOLTAGE = NEW_HV;

          //WRITE OUT NEW BURT FILE
          NEW_BURTFILE<<COUNTER[i]->lineONOFF<<endl;
          if( (COUNTER[i]->PAD_NUM>=24)&&(COUNTER[i]->PAD_NUM<=57) )
            NEW_BURTFILE<<COUNTER[i]->GetTitle()<<""<<"1"<<" "<<scientific<<NEW_HV<<endl;
          NEW_BURTFILE<<COUNTER[i]->lineRAMPDOWN<<endl;
          NEW_BURTFILE<<COUNTER[i]->lineRAMPUP<<endl;
          NEW_BURTFILE<<COUNTER[i]->lineREQ<<endl;
        }
      }

      if(locAngleFlag == 1){
        if(COUNTER[i]->PAD_NUM<=23){
          NEW_HV = OLD_HV;
          COUNTER[i]->NEW_VOLTAGE = NEW_HV;
          NEW_BURTFILE<<COUNTER[i]->lineONOFF<<endl;
          NEW_BURTFILE<<COUNTER[i]->lineVAL2<<endl;
          NEW_BURTFILE<<COUNTER[i]->lineRAMPDOWN<<endl;
          NEW_BURTFILE<<COUNTER[i]->lineRAMPUP<<endl;
          NEW_BURTFILE<<COUNTER[i]->lineREQ<<endl;
        }
      }
    }
  }
  if((locAngleFlag==0) || (locAngleFlag==2)){
// ************ Calculate new HV for FA and write out BURT FILE *************
    for(i=0;i<=683;i++){
      name_comp1 = TString(COUNTER[i]->GetName());
      OLD_HV     = COUNTER[i]->VOLTAGE;
      for(j=0;j<=137;j++){
        name_comp2 = TString(PADDLE_FA[j]->GetName());
        if(name_comp1.CompareTo(name_comp2)==0){
          GAIN       = PADDLE_FA[j]->Gain;
          CENTROID   = PADDLE_FA[j]->Centroid;
//          cout<<COUNTER[i]->GetTitle()<<endl;
          NEW_HV     = CalNewHV(OLD_HV,GAIN,CENTROID,COUNTER[i]->LEFT_RIGHT);
          COUNTER[i]->NEW_VOLTAGE = NEW_HV;

          //WRITE OUT NEW BURT FILE
          NEW_BURTFILE<<COUNTER[i]->lineONOFF<<endl;
          NEW_BURTFILE<<COUNTER[i]->GetTitle()<<" "<<"1"<<" "<<scientific<<NEW_HV<<endl;
          NEW_BURTFILE<<COUNTER[i]->lineRAMPDOWN<<endl;
          NEW_BURTFILE<<COUNTER[i]->lineRAMPUP<<endl;
          NEW_BURTFILE<<COUNTER[i]->lineREQ<<endl;
        }
      }
      if(locAngleFlag == 0){
        if(COUNTER[i]->PAD_NUM>=24){
          NEW_HV = OLD_HV;
          COUNTER[i]->NEW_VOLTAGE = NEW_HV;
          NEW_BURTFILE<<COUNTER[i]->lineONOFF<<endl;
          NEW_BURTFILE<<COUNTER[i]->lineVAL2<<endl;
          NEW_BURTFILE<<COUNTER[i]->lineRAMPDOWN<<endl;
          NEW_BURTFILE<<COUNTER[i]->lineRAMPUP<<endl;
          NEW_BURTFILE<<COUNTER[i]->lineREQ<<endl;
        }
      }
    }
  }
  this->ReturnInOrder(CalVals);
  NEW_BURTFILE.close();
}

double doHV::CalNewHV(double old_HV,double GAIN_IN,double centroid,int LEFT_RIGHT) {
  double G,delG,delV,newHV;
  double alpha = 7.2;

  old_HV = fabs(old_HV);

  //cout<<"OLDV:"<<old_HV<<"\tGAIN_IN:"<<GAIN_IN<<"\tCENTROID:"<<centroid<<"\tLEFT_RIGHT:"<<LEFT_RIGHT<<endl;

  //Left Counters
  if(LEFT_RIGHT == 1)
    G = GAIN_IN/exp(centroid);
  //Right Counters
  if(LEFT_RIGHT == 0)
    G = GAIN_IN*exp(centroid);

  delG   = (600.0-G);
  delV   = (old_HV*delG)/(G*alpha);
  newHV  = (old_HV+delV);
  //cout<<"G:"<<G<<"\tDELG:"<<delG<<"\tDELV:"<<delV<<"\tnewHV:"<<newHV<<endl;

  //SOME CHECKS
  if((GAIN_IN==600.0)||(GAIN_IN<=50.0))
    newHV = old_HV;
  if(delV>locMaxDeltaV)
    newHV = old_HV+250.;
  if(delV<-1.0*locMaxDeltaV)
    newHV = old_HV-250.;
  if(newHV>2500.0)
    newHV = old_HV;
  if(G>=575.&&G<=625.)
    newHV = old_HV;
  if(GAIN_IN>=575.&&GAIN_IN<=625.)
    newHV = old_HV;

  //cout<<"newHV:"<<newHV<<endl;
  newHV = -1.0*newHV;
  //cout<<"newHV:"<<newHV<<endl;
  //cout<<"_____________________________"<<endl;
  return newHV;
}

//********* Print out Date String *********
TString doHV::DateString(TTimeStamp *Time){
  TString TheDateString;
  Date  = TString(Time->AsString("l"));
  Date  = Date.Remove(26,Date.Length());
  DAY   = Date.Remove(3,Date.Length());
  Date  = TString(Time->AsString("l"));
  DATE  = Date.Remove(0,5);
  DATE  = DATE.Remove(3,DATE.Length());
  Date  = TString(Time->AsString("l"));
  MONTH = Date.Remove(12,Date.Length());
  MONTH = MONTH.Remove(0,8);
  Date  = TString(Time->AsString("l"));
  YEAR  = Date.Remove(17,Date.Length());
  YEAR  = YEAR.Remove(0,12);
  Date  = TString(Time->AsString("l"));
  TIME  = Date.Remove(0,17);
  TIME  = Date.Remove(9,TIME.Length());
  TheDateString= DAY+" ";
  TheDateString+=MONTH;
  TheDateString+=DATE;
  TheDateString+=TIME;
  TheDateString+=YEAR;
  cout<<TheDateString<<endl;
  return(TheDateString);
}

//********* WRITE OUT VALUES TO WINDOW IN ORDER ********
void doHV::ReturnInOrder(TGTextView *CalVals) {

  for(i=1;i<=6;i++){
    for(j=1;j<=57;j++){
      for(k=0;k<=683;k++){
        if(COUNTER[k]->SECTOR==i){
          if(COUNTER[k]->PAD_NUM==j){

            //cout<<COUNTER[k]->GetTitle()<<"\tOLD_V:"<<COUNTER[k]->VOLTAGE<<"\tNEW_V:"<<COUNTER[k]->NEW_VOLTAGE<<endl;
            char *dum_string = new char[50];
            sprintf(dum_string,"%4.1f",COUNTER[k]->VOLTAGE);
            WindowInfo=TString(COUNTER[k]->GetTitle()) + "  ";
            WindowInfo+=TString(dum_string);
            delete dum_string;
            WindowInfo+=" ";

            char *dum_string2 = new char[50];
            sprintf(dum_string2,"%4.1f",COUNTER[k]->NEW_VOLTAGE);
            WindowInfo+=TString(dum_string2);
            delete dum_string2;
            WindowInfo+=" ";

            char *dum_string3 = new char[50];
            DELTA_V = fabs(COUNTER[k]->NEW_VOLTAGE)-fabs(COUNTER[k]->VOLTAGE);
            sprintf(dum_string3,"%f",DELTA_V);
            WindowInfo+=TString(dum_string3);
            delete dum_string3;

            CalVals->AddLine(WindowInfo.Data());
          }
        }
      }
    }
  }
}

//******** TNamed object containing BURT file info
BurtNamedObj::BurtNamedObj(int mult,TString burtArray[3500]):TNamed() {

  beg_loop   = 5*mult;
  end_loop   = beg_loop+4;

  for(i=beg_loop;i<=end_loop;i++){
    local_loop = i-5*mult;
    local_array[local_loop] = burtArray[i];
  }

  lineONOFF    = local_array[0];
  lineVAL      = local_array[1];
  lineRAMPDOWN = local_array[2];
  lineRAMPUP   = local_array[3];
  lineREQ      = local_array[4];

  SECTOR = atoi(TString(lineONOFF.Data(),10).Remove(0,9));
  Paddle = TString(lineONOFF.Data(),14);
  if(Paddle.EndsWith("_")==1){
    Paddle.Remove(13,14);
    this->SetName(Paddle.Data());
  }else{this->SetName(Paddle.Data());}

  lineVAL2 = lineVAL;
  Counter  = TString(lineVAL.Data(),19);
  Counter3 = Counter;
  this->SetTitle(Counter.Data());
  VALLINE  = lineVAL.Remove(0,21);
  VOLTAGE  = atof(VALLINE.Data());
  DUM      = Counter.Remove(0,11);
  DUM      = DUM.Remove(2,DUM.Length());
  PAD_NUM  = atoi(DUM.Data());

  Counter2 = Counter3.Remove(15,17);

  Bool_t L = Counter2.EndsWith("L");
  Bool_t R = Counter2.EndsWith("R");
  if((L==kTRUE))
    LEFT_RIGHT = 1; //Left
  if((R==kTRUE))
    LEFT_RIGHT = 0; //Right
}

//******** TNamed object containing PARM file info
ParmNamedObj::ParmNamedObj(char Paddle[100],double GAIN,double CENTROID):TNamed() {
  this->SetName(Paddle);
  Gain     = GAIN;
  Centroid = CENTROID;
}


