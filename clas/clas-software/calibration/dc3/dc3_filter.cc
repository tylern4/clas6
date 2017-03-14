

#include "dc3.h"


ntuple_row_t ISCUT,CUTMAX,CUTMIN;

void UpdateCutListboxEntry(int field);


//===================================================
// Compare if two filter structures are equivalent
//===================================================
int operator==(filter_t f1,filter_t f2)
{
   int i;

/*
	cout<<"SUP="<<SUP<<"\n";
	cout<<"f1.stale="<<f1.stale<<"\n";
	cout<<"f2.stale="<<f2.stale<<"\n";
	cout<<"f1.recalculated_residuals="<<f1.recalculated_residuals<<"\n";
	cout<<"f2.recalculated_residuals="<<f2.recalculated_residuals<<"\n";
	cout<<"f1.recalculate_method="<<f1.recalculate_method<<"\n";
	cout<<"f2.recalculate_method="<<f2.recalculate_method<<"\n";
	cout<<"f1.N="<<f1.N<<"\n";
	cout<<"f2.N="<<f2.N<<"\n";
*/
   if(f1.stale || f2.stale)return 0;
   if(f1.recalculated_residuals!=f2.recalculated_residuals)return 0;
   if(f1.N!=f2.N)return 0;
   if(f1.std_tcut!=f2.std_tcut)return 0;
   if(f1.used_corrected_time!=f2.used_corrected_time)return 0;
   
   if(f1.recalculated_residuals)
      if(f1.recalculate_method!=f2.recalculate_method)return 0;

	if(f1.std_tcut){
		if(f1.autocut_t!=f2.autocut_t)return 0;
		if(f1.autocut_t){
			if(f1.autocut_t_frac!=f2.autocut_t_frac)return 0;
		}
	}
   
   for(i=0;i<f1.N;i++){
      if( f1.cut[i].pos != f2.cut[i].pos )return 0;
      if( f1.cut[i].min != f2.cut[i].min )return 0;
      if( f1.cut[i].max != f2.cut[i].max )return 0;
   }

//   cout<<"filter matched sec="<<f1.sec<<" sup="<<f1.sup<<"\n";

   return 1;
}

//====================
// Opposite of above
//====================
int operator!=(filter_t f1,filter_t f2)
{
   return !(f1==f2);
}

//=====================================================
// See if an event from the ntuple passes this filter
//=====================================================
int operator*(ntuple_row_t n,filter_t f)
{
   float *p=&n.sector;
   int i;

	if(f.std_tcut){
		if(n.time>STD_TCUT_HI[f.sec][f.sup])return -1;
		if(n.time<STD_TCUT_LOW[f.sec][f.sup])return -1;
	}
   
   for(i=0;i<f.N;i++){
      if(p[f.cut[i].pos]>f.cut[i].max)return -1;
      if(p[f.cut[i].pos]<f.cut[i].min)return -1;
   }
   return 0;
}

//===========================================================
// Compare xvst parameters in dc3 dc_parms_t class with
// those in the dc library's xvst_parms_t structure.
//===========================================================
int CompareXvsTParms(dc_parms_t *a, xvst_parms_t *b)
{
	return 1;
}

//===========================================================
// Compare timewalk parameters in dc3 dc_parms_t class with
// those in the dc library's xvst_parms_t structure.
//===========================================================
int CompareTimewalkParms(dc_parms_t *a, xvst_parms_t *b)
{
	return 1;
}

//===========================================================
// Get current filter settings from GUI and data structures
//===========================================================
filter_t GetFilter(void)
{
	int i;
   filter_t f;

                        f.N = 0;
                    f.stale = 0;
   f.recalculated_residuals = RECALC_RESIDUALS   && EXPERT;
       f.recalculate_method = RECALC_METHOD      && EXPERT;
      f.used_corrected_time = USE_CORRECTED_TIME || !EXPERT;
                      f.sup = SUP;
                      f.sec = 0;
                 f.std_tcut = STD_TCUT;
                f.autocut_t = AUTOCUT_T;
           f.autocut_t_frac = AUTOCUT_T_FRAC;

	for(i=0;i<NUM_NTUPLE_FIELDS;i++){
		if(((float*)&ISCUT)[i]!=0.0){
			f.cut[f.N].pos=i;
			f.cut[f.N].max=((float*)&CUTMAX)[i];
			f.cut[f.N].min=((float*)&CUTMIN)[i];
			f.N++;
		}
	}
   
   return f;
}

//====================================================================
// This routine just updates the "Cut" button in the GUI to reflect
// whether the cut for the currently chosen Ntuple field is on .
//====================================================================
int SetCutButton(ClientData clientData,Tcl_Interp *interp,int argc, const char *argv[])
{
   int field=-1;
   char cmd[256];
   
   
   // Get currently selected field
   Tcl_GlobalEval(interp,"$filter_path.lbf.lb curselection");
   sscanf(interp->result,"%d",&field);
   if(field<0||field>=NUM_NTUPLE_FIELDS){
      strcpy(cmd,"tk_dialog .bc Cut ");
      strcat(cmd,"\"You must select a field!\" ");
      strcat(cmd,"\"\" 0 OK");
      Tcl_Evaluate(cmd);
      return TCL_OK;
   }
   
   sprintf(cmd,"set cutbutton %d",((float*)&ISCUT)[field]==0.0 ? 0:1);
   Tcl_Evaluate(cmd);
   

   return TCL_OK;
}

//========================================================================
// Open a dialog box so that the currently selected field can be edited
//========================================================================
int SetCuts(ClientData clientData,Tcl_Interp *interp,int argc, const char *argv[])
{
   int field=-1;
   char cmd[256];
   float upper,lower;
   int cutbutton;
   
   // Get currently selected field
   Tcl_GlobalEval(interp,"$filter_path.lbf.lb curselection");
   sscanf(interp->result,"%d",&field);
   if(field<0||field>=NUM_NTUPLE_FIELDS){
      strcpy(cmd,"tk_dialog .bc Cut ");
      strcat(cmd,"\"You must select a field!\" ");
      strcat(cmd,"\"\" 0 OK");
      Tcl_Evaluate(cmd);
      return TCL_OK;
   }
	cutbutton=atoi(Tcl_GetVar(interp,"cutbutton",TCL_GLOBAL_ONLY));
   
   ((float*)&ISCUT)[field]=0;
   if(!cutbutton){
      UpdateCutListboxEntry(field);
      return TCL_OK;
   }
   
   // open dialog
	sprintf(cmd,"SetCutDialog \"%s\" %f %f %f %f"
		,NTUPLE_FIELD_NAMES[field]
		,((float*)&CUTMAX)[field]
		,((float*)&CUTMIN)[field]
		,NTUPLE_FIELD[field].max
		,NTUPLE_FIELD[field].min);
	Tcl_Evaluate(cmd);
   
   // wait for user to interact with dialog
   Tcl_Evaluate("tkwait window .c");
   
   if(atoi(Tcl_GetVar(interp,"cut_cancel",TCL_GLOBAL_ONLY))){
      printf("Canceled.\n");
      return TCL_OK;
   }

	// Get final values
	((float*)&CUTMAX)[field]=atof(Tcl_GetVar(interp,"cut_upper",TCL_GLOBAL_ONLY));
	((float*)&CUTMIN)[field]=atof(Tcl_GetVar(interp,"cut_lower",TCL_GLOBAL_ONLY));
	((float*)&ISCUT)[field]=1;
   
   // update entry in listbox
   UpdateCutListboxEntry(field);
   
   return TCL_OK;
}

//=====================================================
// Updates the cut listbox entry by marking/removing
// the "*" beside the cut/uncut ntuple field
//=====================================================
void UpdateCutListboxEntry(int field)
{
   char cmd[256];
   
   /* update entry in GUI listbox */
   sprintf(cmd,"$filter_path.lbf.lb delete %d",field);
   Tcl_Evaluate(cmd);
   
   sprintf(cmd,"$filter_path.lbf.lb insert %d \"%c%s\""
   ,field
   ,((float*)&ISCUT)[field]!=0.0 ? '*':' '
   ,NTUPLE_FIELD_NAMES[field]);
   Tcl_Evaluate(cmd);

   sprintf(cmd,"$filter_path.lbf.lb selection set %d",field);
   Tcl_Evaluate(cmd);
}



