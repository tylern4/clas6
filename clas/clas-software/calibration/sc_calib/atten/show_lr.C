TCanvas *c;

void show_lr(char *rootfile){

// gROOT->Reset();
gStyle->SetPalette(1);

string plotindex;

int xx=3,yy=2;
const int n=6;
char * hstname[n]={"h16","h26","h36","h46","h56","h66"};
// plotindex="id";

TFile* file=new TFile(rootfile);

// TFile* file=new TFile("atten_45625_before.root");
// TFile* file=new TFile("atten_45625_after.root");
// TFile* file=new TFile("atten_45625_final.root");
// TFile* file=new TFile("atten_45625_minus.root");

// TFile* file=new TFile("atten_45812_before.root");
// TFile* file=new TFile("atten_x.root");

TDirectory * dir;
// dir=(TDirectory*) file->Get("EC");

c=new TCanvas("c","c",1000,800);
c->SetGrid();
c->Divide(xx,yy);
gStyle->SetOptStat(1111111);
gStyle->SetPalette(1);
for (int i=0; i < n; i++) {
// cout << "try " <<hstname[i] <<endl;
  c->cd(i+1);
  string theClassName;
  if (dir) theClassName = string(dir->Get(hstname[i])->ClassName());
  else theClassName = string(file->Get(hstname[i])->ClassName());
  int dimension;
  if (theClassName == "TH1F") {dimension=1;}
  if (theClassName == "TH2F") {dimension=2;}
//  cout << theClassName <<endl;
 	switch (dimension){
		case 1:
			break;
		case 2:
			if (dir)	TH2F * h2 = (TH2F*) dir->Get(hstname[i]);
			else TH2F * h2 = (TH2F*) file->Get(hstname[i]);
			
 			h2->Draw("colz");
//			h2->Draw();


			break;
		default:
			cout <<"Not found"<<endl;
			break;
}
}

c->Update();
c->Modified();


}

