#include <iostream>
#include <stdio.h>
#include <TGuiStudio.h>
#include <TApplication.h>

int main(int argc, char **argv)
{
  char prjname[521];

  if(argc<2){
    printf("\n\n Usage: \n");
    printf("\t %s [projectname] \n\n--\n",argv[0]);
    exit(1);
  }

  sprintf(prjname,"%s",argv[1]);

   TApplication *theApp;
   theApp = new TApplication("App", &argc, argv);
   TGuiStudio theStudio(gClient->GetRoot(), 600, 600);

   theStudio.LoadFromXML(prjname);

   theApp->Run();
   delete theApp;
   return 0;
}
