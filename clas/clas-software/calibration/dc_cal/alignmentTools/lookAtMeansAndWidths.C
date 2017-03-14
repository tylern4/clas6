void lookAtMeansAndWidths(char *file1, char *file2="", char *file3="", char*file4="")
{
  gROOT->Reset ();

  /////////// Style /////////////////////////
  gStyle->SetStatW (0.30);
  gStyle->SetStatH (0.20);
  gStyle->SetStatColor (0);
  gStyle->SetStatFont (40);

  gStyle->SetTitleW (0.60);
  gStyle->SetTitleH (0.10);
  gStyle->SetTitleColor (1);
  gStyle->SetTitleFont (40);

  gStyle->SetTitleSize (0.08, "X");
  gStyle->SetTitleFont (42);
  gStyle->SetTitleSize (0.07, "Y");
  gStyle->SetLabelSize (0.08, "X");

  //gStyle->SetOptStat (10);
  gStyle->SetOptStat (111);
  gStyle->SetOptFit (110);

  gSystem->Load ("libPhysics");
  /////////// Style /////////////////////////
  
  char tag[256];
  sprintf(tag,"comparison");

  string filename;
  char tag[256];

  int i, j, k;
  char name[256];
  char part[256];
  char diff[256];
  char title[256];
  char command[256];
  char cut[256];

  int numcan = 4;
  int numbins, maxx, minx;
  TCanvas *can[4];
  TPad *top[4];
  TPad *bottom[4]; 
  TLegend *leg[4];
  //TPaveLabel *laba[108];
  //TPaveLabel *labsec[108];
  TPaveText *laba[108];
  TPaveText *labsec[108];



  for (i = 0; i < numcan; i++)
  {
    sprintf (name, "can%d", i);
    sprintf (title, "Canvas %d", i);
    can[i] = new TCanvas (name, title, 10 + 10 * i, 10 + 10 * i, 800, 1000);
    can[i]->SetFillColor (0);
    top[i] = new TPad("top", "The Top", 0.01, 0.90, 0.99, 0.99);
    top[i]->SetFillColor(0);
    top[i]->Draw();
    bottom[i] = new TPad("bottom", "The bottom", 0.01, 0.01, 0.99, 0.90);
    bottom[i]->SetFillColor(0);
    bottom[i]->Draw();
    bottom[i]->Divide(3, 6, 0.0, 0.0);
  }

  TGraph *gr[24][3][6];
  TGraph *grrms[24][3][6];
  TGraph *grm[24][3][6];
  TGraph *grmrms[24][3][6];
  TLine *line[36];
  TLine *slline[512];
  float x0[24][3][6][36], y0[24][3][6][36];
  float rmsx[24][3][6][6], rmsy[24][3][6][6];
  float mx[24][3][6][6], my[24][3][6][6];
  float mrmsx[24][3][6][6], mrmsy[24][3][6][6];
  int max = 0;
  float mean, width;
  int angle, sec, lr, superlr;
  int filecount = 0;
  float sum;

  float layermeans[16][3][6][36]; // file/angle/sector/layer
  float superlayermeans[16][3][6][6]; // file/angle/sector/layer
  float layer[16][3][6][36]; // file/angle/sector/layer
  float superlayer[16][3][6][6]; // file/angle/sector/layer

  float layermeansRMS[16][3][6][6]; // file/angle/sector/superlayer
  float superlayermeansRMS[16][3][6][1]; // file/angle/sector/layer
  float layerRMS[16][3][6][6]; // file/angle/sector/superlayer
  float superlayerRMS[16][3][6][1]; // file/angle/sector/layer

  filecount = 0;

  int numfiles = 1;

  if(strcmp(file2,"")!=0) numfiles++;
  if(strcmp(file3,"")!=0) numfiles++;
  if(strcmp(file4,"")!=0) numfiles++;

  ifstream IN[4];

  for(int i=0;i<numfiles;i++)
  {
    if(i==0) IN[0].open(file1);
    else if(i==1) IN[1].open(file2);
    else if(i==2) IN[2].open(file3);
    else if(i==3) IN[3].open(file4);
  }


  cerr << "numfiles: " << numfiles << endl;
  for(int ifile=0;ifile<numfiles;ifile++)
  {
    // Read in the means/widths of the layers
    for(int i=0;i<3;i++) // loops over angles
    {
      for(int j=0;j<6;j++) // loop over sectors
      {
        for(int k=0;k<36;k++) // loop over layers
        {
          IN[ifile] >> angle >> sec >> lr >> mean >> width;
          layermeans[ifile][angle][sec-1][lr-1] = mean;
          layer[ifile][angle][sec-1][lr-1] = lr;
        }
      }
    } 

    // Read in the means/widths of the superlayers
    for(int i=0;i<3;i++) // loop over angles
    {
      for(int j=0;j<6;j++) // loop over sectors
      {
        for(int k=0;k<6;k++) // loop over superlayers
        {
          IN[ifile] >> angle >> sec >> superlr >> mean >> width;
          superlayermeans[ifile][angle][sec-1][superlr-1] = mean;
          superlayer[ifile][angle][sec-1][superlr-1] = superlr;
        }
      }
    }


    float rms = 0;
    int count = 0;

    for(int i=0;i<3;i++) // loops over angles
    {
      for(int j=0;j<6;j++) // loop over sectors
      {
        for(int k=0;k<36;k++) // loop over layers
        {
          if(k%6==0)
          {
            rms = 0;
            count = 0;
          }
          mean = layermeans[ifile][i][j][k];
          if(mean!=0)
          {
            rms += mean*mean;
            count++;
          }
          if(k%6==5)
          {
            if(count!=0) rms = rms/(float)(count); 
            else         rms = 0.0;
            layermeansRMS[ifile][i][j][k/6] = sqrt(rms);
            layerRMS[ifile][i][j][k/6] = k/6 + 1;
          }
        }
      }
    }

    for(int i=0;i<3;i++) // loops over angles
    {
      for(int j=0;j<6;j++) // loop over sectors
      {
        for(int k=0;k<6;k++) // loop over layers
        {
          if(k%6==0)
          {
            rms = 0;
            count = 0;
          }
          mean = superlayermeans[ifile][i][j][k];
          if(mean!=0)
          {
            rms += mean*mean;
            count++;
          }
          if(k%6==5)
          {
            if(count!=0) rms = rms/(float)(count); 
            else         rms = 0; 
            superlayermeansRMS[ifile][i][j][k/6] = sqrt(rms);
            superlayerRMS[ifile][i][j][k/6] = k/6 + 1;
          }
        }
      }
    } 
  }


  int grcolor = 1;
  for(int i=0;i<numfiles;i++)
  {
    for(int j=0;j<3;j++)
    {
      for(int k=0;k<6;k++)
      {
        grcolor = i + 1;
        gr[i][j][k] = new TGraph(36,layermeans[i][j][k],layer[i][j][k]);
        gr[i][j][k]->SetTitle();
        gr[i][j][k]->SetMarkerStyle(20+i);
        gr[i][j][k]->SetMarkerSize(0.4);
        gr[i][j][k]->SetMarkerColor(grcolor);
        gr[i][j][k]->SetFillColor(grcolor);
        gr[i][j][k]->GetXaxis()->SetLimits(-250,+250);
        gr[i][j][k]->GetHistogram()->GetYaxis()->SetTitle("Layer");
        gr[i][j][k]->GetHistogram()->GetYaxis()->CenterTitle();
        gr[i][j][k]->GetHistogram()->GetXaxis()->SetTitle("Mean of residuals");
        gr[i][j][k]->GetHistogram()->GetXaxis()->CenterTitle();
        gr[i][j][k]->GetHistogram()->GetXaxis()->SetNdivisions(6);

        grm[i][j][k] = new TGraph(6,superlayermeans[i][j][k],superlayer[i][j][k]);
        grm[i][j][k]->SetTitle();
        grm[i][j][k]->SetMarkerStyle(20+i);
        grm[i][j][k]->SetMarkerSize(0.8);
        grm[i][j][k]->SetMarkerColor(grcolor);
        grm[i][j][k]->SetFillColor(grcolor);
        grm[i][j][k]->GetXaxis()->SetLimits(-200,+200);
        grm[i][j][k]->GetHistogram()->GetYaxis()->SetTitle("Superlayer");
        grm[i][j][k]->GetHistogram()->GetYaxis()->CenterTitle();
        grm[i][j][k]->GetHistogram()->GetXaxis()->SetTitle("Mean of residuals over superlayers");
        grm[i][j][k]->GetHistogram()->GetXaxis()->CenterTitle();
        grm[i][j][k]->GetHistogram()->GetXaxis()->SetNdivisions(6);

        grrms[i][j][k] = new TGraph(6,layermeansRMS[i][j][k],layerRMS[i][j][k]);
        grrms[i][j][k]->SetTitle();
        grrms[i][j][k]->SetMarkerStyle(20+i);
        grrms[i][j][k]->SetMarkerSize(0.8);
        grrms[i][j][k]->SetMarkerColor(grcolor);
        grrms[i][j][k]->SetFillColor(grcolor);
        grrms[i][j][k]->GetXaxis()->SetLimits(0, +200);
        grrms[i][j][k]->GetHistogram()->GetYaxis()->SetTitle("Superlayer");
        grrms[i][j][k]->GetHistogram()->GetYaxis()->CenterTitle();
        grrms[i][j][k]->GetHistogram()->GetXaxis()->SetTitle("RMS of layer means over superlayer");
        grrms[i][j][k]->GetHistogram()->GetXaxis()->CenterTitle();
        grrms[i][j][k]->GetHistogram()->GetXaxis()->SetNdivisions(6);

        grmrms[i][j][k] = new TGraph(1,superlayermeansRMS[i][j][k],superlayerRMS[i][j][k]);
        grmrms[i][j][k]->SetTitle();
        grmrms[i][j][k]->SetMarkerStyle(20+i);
        grmrms[i][j][k]->SetMarkerSize(1.8);
        grmrms[i][j][k]->SetMarkerColor(grcolor);
        grmrms[i][j][k]->SetFillColor(grcolor);
        grmrms[i][j][k]->GetXaxis()->SetLimits(0,+200);
        grmrms[i][j][k]->GetYaxis()->SetLimits(0,2.0);
        grmrms[i][j][k]->GetHistogram()->GetYaxis()->SetTitle("Arbitrary");
        grmrms[i][j][k]->GetHistogram()->GetYaxis()->CenterTitle();
        grmrms[i][j][k]->GetHistogram()->GetXaxis()->SetTitle("RMS of means of superlayers for sector");
        grmrms[i][j][k]->GetHistogram()->GetXaxis()->CenterTitle();
        grmrms[i][j][k]->GetHistogram()->GetXaxis()->SetNdivisions(6);
      }
    }
  }

  for(int i=0;i<4;i++)
  {
    for(int k=0;k<6;k++)
    {
      for(int j=0;j<3;j++)
      {
        bottom[i]->cd((3*k) + j+1);
        gPad->SetBottomMargin (0.18);
        gPad->SetLeftMargin (0.18);
        if(i==0) gr[0][j][k]->Draw("ap");
        else if(i==1) grrms[0][j][k]->Draw("ap");
        else if(i==2) grm[0][j][k]->Draw("ap");
        else if(i==3) grmrms[0][j][k]->Draw("ap");
        for(int m=1;m<numfiles;m++)
        {
          if(i==0) gr[m][j][k]->Draw("p");
          else if(i==1) grrms[m][j][k]->Draw("p");
          else if(i==2) grm[m][j][k]->Draw("p");
          else if(i==3) grmrms[m][j][k]->Draw("p");
        }

        float lx1, lx2, ly1, ly2;
        int lstyle;
        int loffset;
        if(i==0)      {lx1=0.0; ly1=0.0; lx2=0.0; ly2=40.0; lstyle=2; loffset=0;}
        else if(i==1) {lx1=50.0; ly1=0.0; lx2=50.0; ly2=6.5; lstyle=2; loffset=18;}
        else if(i==2) {lx1=50.0; ly1=0.0; lx2=50.0; ly2=6.5; lstyle=2; loffset=0;}
        else if(i==3) {lx1=50.0; ly1=0.0; lx2=50.0; ly2=6.5; lstyle=2; loffset=18;}

        line[(3*k) + j + loffset] = new TLine(lx1,ly1,lx2,ly2);
        line[(3*k) + j + loffset]->SetLineStyle(lstyle);
        line[(3*k) + j + loffset]->Draw();


        
        for(int l=0;l<6;l++)
        {
          float xpos = ((l+1)*6)+0.5;
          slline[l + 6*j + 18*k] = new TLine(-250, xpos, 250,xpos);
          slline[l + 6*j + 18*k]->SetLineStyle(3);
          slline[l + 6*j + 18*k]->Draw();
        }
        

        if(j==0) sprintf(command,"8< #theta <20");
        else if(j==1) sprintf(command,"40< #theta <65");
        else if(j==2) sprintf(command,"75< #theta <115");
        laba[(3*k) + j] = new TPaveText(0.7, 0.85, 0.98, 0.98,"brNDC");
        laba[(3*k) + j]->AddText(command);
        if(j==0)      laba[(3*k) + j]->SetFillColor(41);
        else if(j==1) laba[(3*k) + j]->SetFillColor(16);
        else if(j==2) laba[(3*k) + j]->SetFillColor(29);
        laba[(3*k) + j]->Draw();

        sprintf(command,"Sec: %d",k+1);
        labsec[(3*k) + j] = new TPaveText(0.01, 0.80, 0.25, 0.98,"brNDC");
        labsec[(3*k) + j]->AddText(command);
        if(k==0)      labsec[(3*k) + j]->SetFillColor(5);
        else if(k==1) labsec[(3*k) + j]->SetFillColor(18);
        else if(k==2) labsec[(3*k) + j]->SetFillColor(21);
        else if(k==3) labsec[(3*k) + j]->SetFillColor(29);
        else if(k==4) labsec[(3*k) + j]->SetFillColor(42);
        else if(k==5) labsec[(3*k) + j]->SetFillColor(33);
        labsec[(3*k) + j]->Draw();

      }
    }
  }

  char passnames[24][256];
  for(int j=0;j<4;j++)
  {
    top[j]->cd();
    leg[j] = new TLegend(0.3, 0.01, 0.99, 0.99);
    for(int i=0;i<numfiles;i++)
    {
      if(i==0 && strcmp(file1,"")!=0)      sprintf(passnames[i],"%s",file1);
      if(i==1 && strcmp(file2,"")!=0)      sprintf(passnames[i],"%s",file2);
      if(i==2 && strcmp(file3,"")!=0)      sprintf(passnames[i],"%s",file3);
      if(i==3 && strcmp(file4,"")!=0)      sprintf(passnames[i],"%s",file4);
      leg[j]->AddEntry(gr[i][0][0], passnames[i], "fill");
    }
    top[j]->cd();
    leg[j]->Draw();
  }

  cerr << "Saving the plots...." << endl;

  sprintf(name,"Plots/layers.%s.pdf",tag);
  can[0]->SaveAs(name);
  sprintf(name,"Plots/layers.RMS.%s.pdf",tag);
  can[1]->SaveAs(name);
  sprintf(name,"Plots/superlayers.%s.pdf",tag);
  can[2]->SaveAs(name);
  sprintf(name,"Plots/superlayers.RMS.%s.pdf",tag);
  can[3]->SaveAs(name);

}
