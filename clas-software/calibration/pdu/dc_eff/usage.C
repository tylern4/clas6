void usage(int argc, char **argv)
{
 void PrintUsage(char *processName); 
 char *argptr = NULL;
 cout << endl;
 for (int i = 1; i < argc; i++)
 {
  argptr = argv[i];
  if(*argptr == '-')
  {
   argptr++;
   switch (*argptr)
   {
    case 'h':
     PrintUsage(argv[0]);
     exit(0);
    break;

    case 'o':
     WriteROOT   = true;
     ROOToutfile = (++argptr);
     cout << " Writing ROOT output: " << ROOToutfile << endl;
     rootout = TFile::Open(ROOToutfile, "RECREATE", "WireEfficiency");
    break;

    case 'a':
     WriteASCII = true;
     ASCIIoutfile = (++argptr);
     cout << " Writing ASCII output: " << ASCIIoutfile << endl;
     OUT.open(ASCIIoutfile, ios::out);
    break;

    case 'm':
     minLayers = atoi(++argptr);
    break;

    case 'M':
     maxLayers = atoi(++argptr);
    break;

    case 'l':
     lowLimit = atof(++argptr);
    break;

    case 'H':
     highLimit = atof(++argptr);
    break;

    case 'B':
     buddies = atof(++argptr)/100.0;
    break;
    
    case 'v':
     opt = 1;
     cout << " Verbose mode !" <<  endl;
   break;
   }
  }
  else
  {
   inputFile = argptr;
   cout << " Analyzing input file: " << inputFile << endl;
  }
 }
 //error trapping
 if(maxLayers > LAYERS || minLayers < 1) 
 {
  cerr << " specified layer out of range" << endl;
  exit(1);
 }

 if(maxLayers < minLayers)
 {
  cerr << " maxLayers must be greater/equal minLayers" << endl;
  exit(1);
 }
 if(buddies < .0 || buddies >100)
 {
  cerr << " buddies value out of range" << endl;
 }

 cout << endl;
 cout << " Min layer to analyze: "       << minLayers   << endl;
 cout << " Max layer to analyze: "       << maxLayers   << endl;
 cout << " Lower limit for efficiency: " << lowLimit    << endl;
 cout << " Upper limit for efficiency: " << highLimit   << endl;
 cout << " Buddies percentage value: "   << buddies*100 << endl;
 cout << endl;
}

void PrintUsage(char *processName)
{
 cout << processName << " <options> <filename>" << endl;
 cout << "\toptions are:" << endl;
 cout << "\t-h\t\tPrint this message.\n";
 cout << "\t-o<filename>\tOutput ROOT file name." << endl;
 cout << "\t-a<filename>\tOutput ASCII file name." << endl;
 cout << "\t-m[#]\t\tMin. layer number to be analyzed (default = 1)." << endl;
 cout << "\t-M[#]\t\tMax. layer number to be analyzed (default = 36)." << endl;
 cout << "\t-l[#]\t\tif efficiency less than this,    it will be written as 0 (default = 1e-3)." << endl;
 cout << "\t-H[#]\t\tif efficiency greater than this, it will be written as 1 (default = 0.9)." << endl;
 cout << "\t-B[#]\t\twires with occupancy within this percentage are considered buddies (default = 16)." << endl;
 cout << "\t-v\t\tVerbose mode." << endl;
}














