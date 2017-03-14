void initWireLimit(Wlimits_t *WireLimits)
{
 //actual number of wires in each layer
 //130 130 130 130   0   0 142 142 142 126 121 120
 //184 185 186 187 188 189 189 189 190 191 192 192
 //192 192 192 192 192 192 192 192 192 192 192 192

 // Region 1 -----------------------------------------------------------------------------
 WireLimits[0].Min  = WireLimits[6].Min = 4;
 WireLimits[1].Min  = WireLimits[2].Min = WireLimits[7].Min  = WireLimits[8].Min = 3;
 WireLimits[3].Min  = WireLimits[9].Min = WireLimits[10].Min = 2;
 WireLimits[11].Min = 1;

 for(int Layer=0; Layer<4; Layer++) WireLimits[Layer].Max = 130;
 for(int Layer=6; Layer<9; Layer++) WireLimits[Layer].Max = 142;
 WireLimits[9].Max  = 126;
 WireLimits[10].Max = 121;
 WireLimits[11].Max = 120;

 // the phantom layers in region 1:
 WireLimits[4].Min = WireLimits[4].Max = WireLimits[5].Min = WireLimits[5].Max = 0;

 // Region 2 -----------------------------------------------------------------------------
 for(int Layer = 12; Layer<24; Layer++) WireLimits[Layer].Min = 1;
 WireLimits[12].Max = 184;
 WireLimits[13].Max = 185;
 WireLimits[14].Max = 186;
 WireLimits[15].Max = 187;
 WireLimits[16].Max = 188;
 WireLimits[17].Max = WireLimits[18].Max = WireLimits[19].Max = 189;
 WireLimits[20].Max = 190;
 WireLimits[21].Max = 191;
 WireLimits[22].Max = WireLimits[23].Max = 192;

 // Region 3
 for (int Layer=24; Layer<36; Layer++)
 {
  WireLimits[Layer].Min =   1;
  WireLimits[Layer].Max = 192;
 }
}


double eff(double w, double occu[18], int flag[18], double *average)
{
 //this is Maurizio Ungaro's algorithm for findind the 'good' wires
 //that go into the calculation of the average occupancy

 // if opt > 0 verbose mode
 // w is wire occupancy of wire you want efficiency of
 // occu is the array of the neighborough wires
 // flag is the ignore array. flag[i] = 0 means ignore ith wire

 string  ans[18];
 int     max = 0;
 double mean = 0;

 TH1F Neigh("Neigh","Neigh",18,1,19);
 Neigh.SetNdivisions(18);
 Neigh.Reset();
 // looking at similarities...
 for(int n=0; n<18; n++)
 {
  if(occu[n]==0) ++occu[n];  // don't like to divide by zero :-)
  for(int s=0; s<18; s++) if(s!=n && fabs(occu[s]-occu[n])/occu[n] <= buddies && flag[s] && flag[n]) Neigh.Fill(s+1);
 }
 max = Neigh.GetMaximumBin(); // choosing maximum ensemble
 
 // calculating mean and efficiency
 int h = 0;
 for(int s=0; s<18; s++)
 {
  if(fabs(occu[s]-occu[max-1])/occu[max-1] > buddies  || !flag[s]) ans[s] = "NO";
  if(fabs(occu[s]-occu[max-1])/occu[max-1] <= buddies && flag[s] )
  {
   ans[s] = "YES";
   h++;
   mean += occu[s];
  }
 }
 mean = mean / h;
 *average = mean;

 if(opt)
 {
  cout << endl ;
  for(int s=0; s<18; s++)
  {
   cout << " Wire: " << setw(2) << s+1 << "  -  Occ: " << setw(14) << occu[s] ;
   cout << "  -  buddy " << ans[s]  << "\t  -  wire exist = " << flag[s] ;
   if(w == occu[s]) cout << "  <--- wire under current analysis" ;
   cout << endl;
  }
  cout << endl;
  cout << " Max bin = " << max << "  which has " << Neigh.GetBinContent(max) << " elements" ;
  cout << endl;
  cout << " Mean = " << mean << "  Efficiency = " << w/mean << endl;
  cout << " Press Enter continue" << endl;
  cout << endl;
  getchar();
 }

 return w/mean;
}

