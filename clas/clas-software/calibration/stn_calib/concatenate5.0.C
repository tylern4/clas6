//$Id: concatenate5.0.C,v 1.2 2008/03/27 14:22:50 pasyuk Exp $
/************************************************************
* concatenate5.0.C: This program concatenate root files     *
*                   with the same structure into one.       *
*                                                           *
*                                                           *
*                                                           *
* Usage:   root [] .x concatenate.C                         *
*                                                           *
*                                                           *
* Input:  list of root files from the called "list.txt"     *
*                                                           *
*                                                           *
* Output: one.root. It contains a list of histograms which  *
*          are the same as a single root file               *
*                                                           *
*                                                           *
* Author: Julian Salamanca                                  *
*         Department of Physics                             *
*         Idaho State University                            *
*         salamanc@jlab.org  (DIC 2005)                     *
************************************************************/          


{
  gROOT->Reset();
  #include<fstream>
  #include<cstdlib>
  #include<string>
  #include<cstdio>
  #include<assert.h>

  TFile* out_root_file = new TFile("one.root","RECREATE");   // opening a file where the histograms will be
  TFile* f_init;                                             // initial file in list.txt
  TFile* f;                                                  // the following files in list.txt
  
  TDirectory* dir_init[100];
  TDirectory* dir[100];

  TH1F* name_dir_init1[1000][1000];
  TH1F* name_dir1[1000][1000];
  
  TH2F* name_dir_init2[1000][1000];
  TH2F* name_dir2[1000][1000];
  

  TObject* obj_dir_init;
  TObject* obj_dir;

  TH1F* name_init[1000];  
  TH1F* name[1000];   
 
  TH2F* name_init2[1000];
  TH2F* name2[1000];

  TObject* obj_init;
  TObject* obj;
  fstream inlist;                        //list of root files
  char file_init[40];                    // first file in the list
  char file[40];                         // root file name in the list
  char hist[50];                         
  inlist.open("list.txt",ios::in);
  
  int n1=0;
  int n2=0;
  
  int j1=0;
  int j2=0;
 

  int d = 0;
  int dd = 0;
  
  int nd1 = 0;
  int nd2 = 0;
  
  int jd1 = 0;
  int jd2 = 0;
  
  if (inlist.fail()){                          //checking if the root list file exist
    cout << "Could not open: list.txt " << '\n';
    exit(1);
  }   
 

  inlist >> file_init;                         //taking reference for all histograms by using the first
                                               // file in list.txt
  f_init = new TFile(file_init);
  TIter next_init(f_init->GetListOfKeys());    
  TKey *key_init;

  while ((key_init=(TKey*)next_init())) {      //getting list of keys (see root manual) 
    obj_init = key_init->ReadObj();
    
    if(obj_init->IsA()->InheritsFrom("TH1F")){ //looking for TH1D histograms
      n1++;
      name_init[n1] = (TH1F*)obj_init;
    }
    
    if(obj_init->IsA()->InheritsFrom("TH2F")){ //looking for TH2D histograms 
      n2++;
      name_init2[n2] = (TH2F*)obj_init;
    }
    
    /******************************************************/
    /******************STARTING DIRECTORIES****************/
    /******************************************************/
        
    
      
    if(obj_init->IsA()->InheritsFrom("TDirectory")){  //looking for Directories
      d++;
      dir_init[d] = (TDirectory*)obj_init;
      out_root_file->mkdir(dir_init[d]->GetName());
      
      TIter next_dir_init(dir_init[d]->GetListOfKeys());
      TKey *key_dir_init;
      
      nd1=0;                                         //counter for TH1F histograms in directory
      nd2=0;                                         //   "     " TH2F      "      "   "
   
  
      while ((key_dir_init=(TKey*)next_dir_init())) {
	obj_dir_init = key_dir_init->ReadObj();
		
	if(obj_dir_init->IsA()->InheritsFrom("TH1F")){
	  nd1++;
	  name_dir_init1[d][nd1] = (TH1F*)obj_dir_init;
	}  	  	  
	
	if(obj_dir_init->IsA()->InheritsFrom("TH2F")){
	  nd2++;
	  name_dir_init2[d][nd2] = (TH2F*)obj_dir_init;
	}	
      }
    }
  }
      

  while (!inlist.eof()){                //loop over files in list.txt
  
    inlist >> file;                     
    f = new TFile(file);
    TIter next(f.GetListOfKeys());
    TKey *key;	
    
    j1=0;                       //counter for TH1F histograms
    j2=0;                       //   "     "  TH2F   "
    dd=0;                       //counter for directories
    
    
    while ((key=(TKey*)next())) {	     
      obj = key->ReadObj();
      if(obj->IsA()->InheritsFrom("TH1F")){
	j1++;	
	name[j1] = (TH1F*)obj;
	name_init[j1].Add(name[j1]);             //concatenating TH1F histograms      
      } 
      
      if(obj->IsA()->InheritsFrom("TH2F")){
	j2++;	
	name2[j2] = (TH2F*)obj;
	name_init2[j2].Add(name2[j2]);            //concatenating TH2F histograms
      } 
            
      if(obj->IsA()->InheritsFrom("TDirectory")){ //concatenating directories
	dd++;
	dir[dd] = (TDirectory*)obj;
	TIter next_dir(dir[dd]->GetListOfKeys());
	TKey *key_dir;
	
	jd1=0;
	jd2=0;
      
	while ((key_dir=(TKey*)next_dir())) {                 //getting directory key
	  obj_dir = key_dir->ReadObj();
	  
	  if(obj_dir->IsA()->InheritsFrom("TH1F")){
	    jd1++;
	    name_dir1[dd][jd1] = (TH1F*)obj_dir;              //concatenating TH1F histograms in directory
	  }
	  
	  if(obj_dir->IsA()->InheritsFrom("TH2F")){
	    jd2++;	    	    
	    name_dir2[dd][jd2] = (TH2F*)obj_dir;
	    name_dir_init2[dd][jd2].Add(name_dir2[dd][jd2]);  //concatenating TH2F histogramas in directory	    
	  }
	}
      }
    }
    f->Close();
  }

  for(int i1 = 1; i1<=j1; i1++){                        //writting TH1F histograms in output file
    out_root_file->WriteTObject(name_init[i1]);
  }

  for(int i2 = 1; i2<=j2; i2++){                        //writting TH2F histograms in output file
    out_root_file->WriteTObject(name_init2[i2]);
  }
  
 
  char subdir[50];
  int q1 = 0;
  int q2 = 0;
  int r = 0;

  while (1){                                          //writting  histograms in each directory
    r++;
    sprintf(subdir,"%s",dir_init[r]->GetName());
    
    while (1){
      q1++;
      if(name_dir_init1[r][q1]==NULL){
	q1=0;
	break;
      }     
      out_root_file->cd(subdir);      
      name_dir_init1[r][q1]->Write();      
    }                
    
    while (1){
      q2++;
      if(name_dir_init2[r][q2]==NULL){
	q2=0;
	break;
      }     
      out_root_file->cd(subdir);      
      name_dir_init2[r][q2]->Write();      
    }             
    if(r==4)
      break;
    
  }

  f_init->Close();                     //closing files
  inlist.close();
  out_root_file->Close();
}

