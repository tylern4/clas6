#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <bostypes.h>
#include <utility.h>

int configure_banks(FILE *out, int verbose){

formatBank("BMPR","4F");
if(verbose) fprintf(out,"BMPR bank defined with format: 4F\n");

formatBank("BREP","36F");
if(verbose) fprintf(out,"BREP bank defined with format: 36F\n");

formatBank("CALL","B16");
if(verbose) fprintf(out,"CALL bank defined with format: B16\n");

formatBank("CC  ","B16");
if(verbose) fprintf(out,"CC   bank defined with format: B16\n");

formatBank("CC01","I,2F");
if(verbose) fprintf(out,"CC01 bank defined with format: I,2F\n");

formatBank("CC1 ","B16");
if(verbose) fprintf(out,"CC1  bank defined with format: B16\n");

formatBank("CCDI","5I");
if(verbose) fprintf(out,"CCDI bank defined with format: 5I\n");

formatBank("CCH ","6I,6F");
if(verbose) fprintf(out,"CCH  bank defined with format: 6I,6F\n");

formatBank("CCMT","2I");
if(verbose) fprintf(out,"CCMT bank defined with format: 2I\n");

formatBank("CCPB","I,4F,I");
if(verbose) fprintf(out,"CCPB bank defined with format: I,4F,I\n");

formatBank("CCPE","3I");
if(verbose) fprintf(out,"CCPE bank defined with format: 3I\n");

formatBank("CCRC","12I");
if(verbose) fprintf(out,"CCRC bank defined with format: 12I\n");

formatBank("CCS ","96I");
if(verbose) fprintf(out,"CCS  bank defined with format: 96I\n");

formatBank("CHI2","2F,2I");
if(verbose) fprintf(out,"CHI2 bank defined with format: 2F,2I\n");

formatBank("CL01","2I,F,I,F,I,F,I,4F");
if(verbose) fprintf(out,"CL01 bank defined with format: 2I,F,I,F,I,F,I,4F\n");

formatBank("BMPR","4F");
if(verbose) fprintf(out,"BMPR bank defined with format: 4F\n");

formatBank("BREP","36F");
if(verbose) fprintf(out,"BREP bank defined with format: 36F\n");

formatBank("CALL","B16");
if(verbose) fprintf(out,"CALL bank defined with format: B16\n");

formatBank("CC  ","B16");
if(verbose) fprintf(out,"CC   bank defined with format: B16\n");

formatBank("CC01","I,2F");
if(verbose) fprintf(out,"CC01 bank defined with format: I,2F\n");

formatBank("CC1 ","B16");
if(verbose) fprintf(out,"CC1  bank defined with format: B16\n");

formatBank("CCDI","5I");
if(verbose) fprintf(out,"CCDI bank defined with format: 5I\n");

formatBank("CCH ","6I,6F");
if(verbose) fprintf(out,"CCH  bank defined with format: 6I,6F\n");

formatBank("CCMT","2I");
if(verbose) fprintf(out,"CCMT bank defined with format: 2I\n");

formatBank("CCPB","I,4F,I");
if(verbose) fprintf(out,"CCPB bank defined with format: I,4F,I\n");

formatBank("CCPE","3I");
if(verbose) fprintf(out,"CCPE bank defined with format: 3I\n");

formatBank("CCRC","12I");
if(verbose) fprintf(out,"CCRC bank defined with format: 12I\n");

formatBank("CCS ","96I");
if(verbose) fprintf(out,"CCS  bank defined with format: 96I\n");

formatBank("CHI2","2F,2I");
if(verbose) fprintf(out,"CHI2 bank defined with format: 2F,2I\n");

formatBank("CL01","2I,F,I,F,I,F,I,4F");
if(verbose) fprintf(out,"CL01 bank defined with format: 2I,F,I,F,I,F,I,4F\n");

formatBank("CLST","I");
if(verbose) fprintf(out,"CLST bank defined with format: I\n");

formatBank("CPED","3I,F,I");
if(verbose) fprintf(out,"CPED bank defined with format: 3I,F,I\n");

formatBank("DC0 ","B16");
if(verbose) fprintf(out,"DC0  bank defined with format: B16\n");

formatBank("DC1 ","I,F");
if(verbose) fprintf(out,"DC1  bank defined with format: I,F\n");

formatBank("DCDW","I,F,I");
if(verbose) fprintf(out,"DCDW bank defined with format: I,F,I\n");

formatBank("DCGM","11F,2I,7F,I");
if(verbose) fprintf(out,"DCGM bank defined with format: 11F,2I,7F,I\n");

formatBank("DCGW","8F");
if(verbose) fprintf(out,"DCGW bank defined with format: 8F\n");

formatBank("DCH ","10F,3I");
if(verbose) fprintf(out,"DCH  bank defined with format: 10F,3I\n");

formatBank("DCPB","I,11F,I");
if(verbose) fprintf(out,"DCPB bank defined with format: I,11F,I\n");

formatBank("DCV1","5F");
if(verbose) fprintf(out,"DCV1 bank defined with format: 5F\n");

formatBank("DCV2","14F");
if(verbose) fprintf(out,"DCV2 bank defined with format: 14F\n");

formatBank("DCV3","5F");
if(verbose) fprintf(out,"DCV3 bank defined with format: 5F\n");

formatBank("DDLY","I,F,I");
if(verbose) fprintf(out,"DDLY bank defined with format: I,F,I\n");

formatBank("DGEO","2I,6F");
if(verbose) fprintf(out,"DGEO bank defined with format: 2I,6F\n");

formatBank("DHCL","B08");
if(verbose) fprintf(out,"DHCL bank defined with format: B08\n");

formatBank("DITM","I");
if(verbose) fprintf(out,"DITM bank defined with format: I\n");

formatBank("DOCA","B16");
if(verbose) fprintf(out,"DOCA bank defined with format: B16\n");

formatBank("DPCP","2I,F,I,F,I,F,I,F,I,F,I,F");
if(verbose) fprintf(out,"DPCP bank defined with format: 2I,F,I,F,I,F,I,F,I,F,I,F\n");

formatBank("DPSP","2I,F");
if(verbose) fprintf(out,"DPSP bank defined with format: 2I,F\n");

formatBank("DSPC","B16");
if(verbose) fprintf(out,"DSPC bank defined with format: B16\n");

formatBank("DSPS","B16");
if(verbose) fprintf(out,"DSPS bank defined with format: B16\n");

formatBank("DSTC","B16");
if(verbose) fprintf(out,"DSTC bank defined with format: B16\n");

formatBank("DTCP","2I,F,I,F,I,F,I,F,I,F,I,F,I,F");
if(verbose) fprintf(out,"DTCP bank defined with format: 2I,F,I,F,I,F,I,F,I,F,I,F,I,F\n");

formatBank("DTRK","3F");
if(verbose) fprintf(out,"DTRK bank defined with format: 3F\n");

formatBank("EC  ","B16");
if(verbose) fprintf(out,"EC   bank defined with format: B16\n");

formatBank("EC01","I,2F");
if(verbose) fprintf(out,"EC01 bank defined with format: I,2F\n");

formatBank("EC1 ","B16");
if(verbose) fprintf(out,"EC1  bank defined with format: B16\n");

formatBank("EC1P","4F");
if(verbose) fprintf(out,"EC1P bank defined with format: 4F\n");

formatBank("EC1R","24F,4I,16F,3I,2F,I");
if(verbose) fprintf(out,"EC1R bank defined with format: 24F,4I,16F,3I,2F,I\n");

formatBank("ECCA","I,6F,I");
if(verbose) fprintf(out,"ECCA bank defined with format: I,6F,I\n");

formatBank("ECCL","I,6F,I");
if(verbose) fprintf(out,"ECCL bank defined with format: I,6F,I\n");

formatBank("ECCT","I,10F,I");
if(verbose) fprintf(out,"ECCT bank defined with format: I,10F,I\n");

formatBank("ECDI","5I");
if(verbose) fprintf(out,"ECDI bank defined with format: 5I\n");

formatBank("ECG ","8F,I,13F");
if(verbose) fprintf(out,"ECG  bank defined with format: 8F,I,13F\n");

formatBank("ECH ","7F,I,3F");
if(verbose) fprintf(out,"ECH  bank defined with format: 7F,I,3F\n");

formatBank("ECHB","I,29F,4I,F,I,F,I");
if(verbose) fprintf(out,"ECHB bank defined with format: I,29F,4I,F,I,F,I\n");

formatBank("ECMT","6I");
if(verbose) fprintf(out,"ECMT bank defined with format: 6I\n");

formatBank("ECP ","7F");
if(verbose) fprintf(out,"ECP  bank defined with format: 7F\n");

formatBank("ECP1","2I,F,I,F");
if(verbose) fprintf(out,"ECP1 bank defined with format: 2I,F,I,F\n");

formatBank("ECPB","I,11F,2I,F,I");
if(verbose) fprintf(out,"ECPB bank defined with format: I,11F,2I,F,I\n");

formatBank("ECPC","I,2F");
if(verbose) fprintf(out,"ECPC bank defined with format: I,2F\n");

formatBank("ECPE","2I,F");
if(verbose) fprintf(out,"ECPE bank defined with format: 2I,F\n");

formatBank("ECPI","3I,6F");
if(verbose) fprintf(out,"ECPI bank defined with format: 3I,6F\n");

formatBank("ECPO","7I");
if(verbose) fprintf(out,"ECPO bank defined with format: 7I\n");

formatBank("ECRB","48F,I");
if(verbose) fprintf(out,"ECRB bank defined with format: 48F,I\n");

formatBank("ECS ","96I");
if(verbose) fprintf(out,"ECS  bank defined with format: 96I\n");

formatBank("EID0","4I");
if(verbose) fprintf(out,"EID0 bank defined with format: 4I\n");

formatBank("EPIC","F,8A");
if(verbose) fprintf(out,"EPIC bank defined with format: F,8A\n");

formatBank("EVNT","I,2F,I,7F,7I");
if(verbose) fprintf(out,"EVNT bank defined with format: I,2F,I,7F,7I\n");

formatBank("FBPM","B16");
if(verbose) fprintf(out,"FBPM bank defined with format: B16\n");

formatBank("G1SL","64I");
if(verbose) fprintf(out,"G1SL bank defined with format: 64I\n");

formatBank("G2SL","64I");
if(verbose) fprintf(out,"G2SL bank defined with format: 64I\n");

formatBank("G3SL","64I");
if(verbose) fprintf(out,"G3SL bank defined with format: 64I\n");

formatBank("G4SL","64I");
if(verbose) fprintf(out,"G4SL bank defined with format: 64I\n");

formatBank("GCPB","I,12F,I,4F,I,F");
if(verbose) fprintf(out,"GCPB bank defined with format: I,12F,I,4F,I,F\n");

formatBank("GGEO","I,6F");
if(verbose) fprintf(out,"GGEO bank defined with format: I,6F\n");

formatBank("GPAR","F,I,5A");
if(verbose) fprintf(out,"GPAR bank defined with format: F,I,5A\n");

formatBank("GPID","I,7F,4I,2F,I,2F,I,3F,I,3F,3I");
if(verbose) fprintf(out,"GPID bank defined with format: I,7F,4I,2F,I,2F,I,3F,I,3F,3I\n");

formatBank("HBER","21F,2I");
if(verbose) fprintf(out,"HBER bank defined with format: 21F,2I\n");

formatBank("HBID","2I,2F,2I,4F,2I,4F,2I,4F,2I,4F,2I,4F");
if(verbose) fprintf(out,"HBID bank defined with format: 2I,2F,2I,4F,2I,4F,2I,4F,2I,4F,2I,4F\n");

formatBank("HBLA","I,7F,3I,5F");
if(verbose) fprintf(out,"HBLA bank defined with format: I,7F,3I,5F\n");

formatBank("HBTB","10I");
if(verbose) fprintf(out,"HBTB bank defined with format: 10I\n");

formatBank("HBTR","8F,I");
if(verbose) fprintf(out,"HBTR bank defined with format: 8F,I\n");

formatBank("HCAL","5I");
if(verbose) fprintf(out,"HCAL bank defined with format: 5I\n");

formatBank("HDPL","I,7F");
if(verbose) fprintf(out,"HDPL bank defined with format: I,7F\n");

formatBank("HEAD","8I");
if(verbose) fprintf(out,"HEAD bank defined with format: 8I\n");

formatBank("HEVT","6I,9F,I");
if(verbose) fprintf(out,"HEVT bank defined with format: 6I,9F,I\n");

formatBank("HLB ","B32");
if(verbose) fprintf(out,"HLB  bank defined with format: B32\n");

formatBank("HLS ","B32");
if(verbose) fprintf(out,"HLS  bank defined with format: B32\n");

formatBank("IC  ", "B16");
if(verbose) fprintf(out, "IC    bank defined with format: B16\n");
  
formatBank("ICHB", "F,F,F,F,F,F,F,F,F,F,F,F,F,I");
if(verbose) fprintf(out, "ICHB  bank defined with format: F,F,F,F,F,F,F,F,F,F,F,F,F,I\n");
  
formatBank("ICPB", "F,F,F,F,F,F,F,F,F,I");
if(verbose) fprintf(out, "ICPB  bank defined with format: F,F,F,F,F,F,F,F,F,I\n");

formatBank("SH  ", "B16");
//if(verbose) 
fprintf(out, "SH    bank defined with format: B16\n");

formatBank("SHHB", "I,F,F,F,F,F,I");
if(verbose) fprintf(out, "SHHB  bank defined with format: I,F,F,F,F,F,I\n");

formatBank("SHPB", "I,F,F,F,F,F,I");
if(verbose) fprintf(out, "SHPB  bank defined with format: I,F,F,F,F,F,I\n");

formatBank("KFIT","46F");
if(verbose) fprintf(out,"KFIT bank defined with format: 46F\n");

formatBank("L1PG","A");
if(verbose) fprintf(out,"L1PG bank defined with format: A\n");

formatBank("L2H ","B16");
if(verbose) fprintf(out,"L2H  bank defined with format: B16\n");

formatBank("L2S ","B16");
if(verbose) fprintf(out,"L2S  bank defined with format: B16\n");

formatBank("LASR","4I");
if(verbose) fprintf(out,"LASR bank defined with format: 4I\n");

formatBank("LCDI","4I");
if(verbose) fprintf(out,"LCDI bank defined with format: 4I\n");

formatBank("LCPB","I,7F,I,F");
if(verbose) fprintf(out,"LCPB bank defined with format: I,7F,I,F\n");

formatBank("MCEV","2I");
if(verbose) fprintf(out,"MCEV bank defined with format: 2I\n");

formatBank("MCHD","5I,11F");
if(verbose) fprintf(out,"MCHD bank defined with format: 5I,11F\n");

formatBank("MCTK","6F,5I");
if(verbose) fprintf(out,"MCTK bank defined with format: 6F,5I\n");

formatBank("MCVX","4F,I");
if(verbose) fprintf(out,"MCVX bank defined with format: 4F,I\n");

formatBank("MTRK","7I");
if(verbose) fprintf(out,"MTRK bank defined with format: 7I\n");

formatBank("MVRT","I,11F,I");
if(verbose) fprintf(out,"MVRT bank defined with format: I,11F,I\n");

formatBank("LOWQ","I,9F,I");
if(verbose) fprintf(out,"LOWQ bank defined with format: I,9F,I\n");

formatBank("PART","I,8F,I,2F,I");
if(verbose) fprintf(out,"PART bank defined with format: I,8F,I,2F,I\n");

formatBank("PCO ","7F,I");
if(verbose) fprintf(out,"PCO  bank defined with format: 7F,I\n");

formatBank("PRTM","I");
if(verbose) fprintf(out,"PRTM bank defined with format: I\n");

formatBank("PSO ","I,2F,I");
if(verbose) fprintf(out,"PSO  bank defined with format: I,2F,I\n");

formatBank("PTDB","11I");
if(verbose) fprintf(out,"PTDB bank defined with format: 11I\n");

formatBank("RCST","15I");
if(verbose) fprintf(out,"RCST bank defined with format: 15I\n");

formatBank("RC26","B32");
if(verbose) fprintf(out,"RC26 bank defined with format: B32\n");

formatBank("RF  ","3F");
if(verbose) fprintf(out,"RF   bank defined with format: 3F\n");

formatBank("RGLK","I,8F,I");
if(verbose) fprintf(out,"RGLK bank defined with format: I,8F,I\n");

formatBank("RNLG","A");
if(verbose) fprintf(out,"RNLG bank defined with format: A\n");

formatBank("RNPE","I,A");
if(verbose) fprintf(out,"RNPE bank defined with format: I,A\n");

formatBank("RTSL","64I");
if(verbose) fprintf(out,"RTSL bank defined with format: 64I\n");

formatBank("RUNC","I,F,I,F,I,F,I,F,I,F,I,F,I");
if(verbose) fprintf(out,"RUNC bank defined with format: I,F,I,F,I,F,I,F,I,F,I,F,I\n");

formatBank("S1ST","17I");
if(verbose) fprintf(out,"S1ST bank defined with format: 17I\n");

formatBank("SC  ","B16");
if(verbose) fprintf(out,"SC   bank defined with format: B16\n");

formatBank("SC1 ","I,8F");
if(verbose) fprintf(out,"SC1  bank defined with format: I,8F\n");

formatBank("SCC ","4I,44F");
if(verbose) fprintf(out,"SCC  bank defined with format: 4I,44F\n");

formatBank("SCDI","5I");
if(verbose) fprintf(out,"SCDI bank defined with format: 5I\n");

formatBank("SCG ","2I,9F");
if(verbose) fprintf(out,"SCG  bank defined with format: 2I,9F\n");

formatBank("SCGD","I,9F");
if(verbose) fprintf(out,"SCGD bank defined with format: I,9F\n");

formatBank("SCH ","7F,2I");
if(verbose) fprintf(out,"SCH  bank defined with format: 7F,2I\n");

formatBank("SCMD","2I,11F");
if(verbose) fprintf(out,"SCMD bank defined with format: 2I,11F\n");

formatBank("SCMT","I");
if(verbose) fprintf(out,"SCMT bank defined with format: I\n");

formatBank("SCMW","I");
if(verbose) fprintf(out,"SCMW bank defined with format: I\n");

formatBank("SCP ","16F");
if(verbose) fprintf(out,"SCP  bank defined with format: 16F\n");

formatBank("SCPB","I,4F,I");
if(verbose) fprintf(out,"SCPB bank defined with format: I,4F,I\n");

formatBank("SCPE","2I,F,I,F");
if(verbose) fprintf(out,"SCPE bank defined with format: 2I,F,I,F\n");

formatBank("SCPS","3I,13F");
if(verbose) fprintf(out,"SCPS bank defined with format: 3I,13F\n");

formatBank("SCR ","I,8F,I,2F");
if(verbose) fprintf(out,"SCR  bank defined with format: I,8F,I,2F\n");

formatBank("SCRC","I,10F,I");
if(verbose) fprintf(out,"SCRC bank defined with format: I,10F,I\n");

formatBank("SCS ","192I");
if(verbose) fprintf(out,"SCS  bank defined with format: 192I\n");

formatBank("SGMP","7I");
if(verbose) fprintf(out,"SGMP bank defined with format: 7I\n");

formatBank("SPAR","4I");
if(verbose) fprintf(out,"SPAR bank defined with format: 4I\n");

formatBank("SPIN","3I");
if(verbose) fprintf(out,"SPIN bank defined with format: 3I\n");

formatBank("ST  ","B16");
if(verbose) fprintf(out,"ST   bank defined with format: B16\n");

formatBank("ST1 ","2I,4F");
if(verbose) fprintf(out,"ST1  bank defined with format: 2I,4F\n");

formatBank("STH ","7F,2I,F");
if(verbose) fprintf(out,"STH  bank defined with format: 7F,2I,F\n");

formatBank("STPB","I,2F,2I");
if(verbose) fprintf(out,"STPB bank defined with format: I,2F,2I\n");

formatBank("STPE","2I,F");
if(verbose) fprintf(out,"STPE bank defined with format: 2I,F\n");

formatBank("STR ","2I,3F,I");
if(verbose) fprintf(out,"STR  bank defined with format: 2I,3F,I\n");

formatBank("STRE","2I,3F,I,2F");
if(verbose) fprintf(out,"STRE bank defined with format: 2I,3F,I,2F\n");

formatBank("STS ","16I");
if(verbose) fprintf(out,"STS  bank defined with format: 16I\n");

formatBank("SYNC","B16");
if(verbose) fprintf(out,"SYNC bank defined with format: B16\n");

formatBank("TACO","I,8F,I");
if(verbose) fprintf(out,"TACO bank defined with format: I,8F,I\n");

formatBank("TAGE","B16");
if(verbose) fprintf(out,"TAGE bank defined with format: B16\n");

formatBank("TAGI","I,2F,I,4F");
if(verbose) fprintf(out,"TAGI bank defined with format: I,2F,I,4F\n")
;
formatBank("TAGM","3F,3I");
if(verbose) fprintf(out,"TAGM bank defined with format: 3F,3I\n");

formatBank("TAGR","3F,3I");
if(verbose) fprintf(out,"TAGR bank defined with format: 3F,3I\n");

formatBank("TAGT","B16");
if(verbose) fprintf(out,"TAGT bank defined with format: B16\n");

formatBank("TBER","21F,2I");
if(verbose) fprintf(out,"TBER bank defined with format: 21F,2I\n");

formatBank("TBID","2I,2F,2I,4F,2I,4F,2I,4F,2I,4F,2I,4F");
if(verbose) fprintf(out,"TBID bank defined with format: 2I,2F,2I,4F,2I,4F,2I,4F,2I,4F,2I,4F\n");

formatBank("TBLA","I,7F,3I,6F");
if(verbose) fprintf(out,"TBLA bank defined with format: I,7F,3I,6F\n");

formatBank("TBTR","8F,2I");
if(verbose) fprintf(out,"TBTR bank defined with format: 8F,2I\n");

formatBank("TCSB","6F");
if(verbose) fprintf(out,"TCSB bank defined with format: 6F\n");

formatBank("TDPL","I,7F");
if(verbose) fprintf(out,"TDPL bank defined with format: I,7F\n");

formatBank("TGBI","B32");
if(verbose) fprintf(out,"TGBI bank defined with format: B32\n");

formatBank("TGPB","I,3F");
if(verbose) fprintf(out,"TGPB bank defined with format: I,3F\n");

formatBank("TGS ","192I");
if(verbose) fprintf(out,"TGS  bank defined with format: 192I\n");

formatBank("TRGS","64I");
if(verbose) fprintf(out,"TRGS bank defined with format: 64I\n");

formatBank("TRKS","I,5F,4I");
if(verbose) fprintf(out,"TRKS bank defined with format: I,5F,4I\n");

formatBank("TRL1","3I,7F");
if(verbose) fprintf(out,"TRL1 bank defined with format: 3I,7F\n");

formatBank("TRPB","I,7F");
if(verbose) fprintf(out,"TRPB bank defined with format: I,7F\n");

formatBank("TSPR","B32");
if(verbose) fprintf(out,"TSPR bank defined with format: B32\n");

formatBank("TSRG","B32");
if(verbose) fprintf(out,"TSRG bank defined with format: B32\n");

formatBank("UNUS","8I");
if(verbose) fprintf(out,"UNUS bank defined with format: 8I\n");

formatBank("VERT","3I,4F");
if(verbose) fprintf(out,"VERT bank defined with format: 3I,4F\n");

formatBank("CLST","I");
if(verbose) fprintf(out,"CLST bank defined with format: I\n");

formatBank("CPED","3I,F,I");
if(verbose) fprintf(out,"CPED bank defined with format: 3I,F,I\n");

formatBank("DC0 ","B16");
if(verbose) fprintf(out,"DC0  bank defined with format: B16\n");

formatBank("DC1 ","I,F");
if(verbose) fprintf(out,"DC1  bank defined with format: I,F\n");

formatBank("DCDW","I,F,I");
if(verbose) fprintf(out,"DCDW bank defined with format: I,F,I\n");

formatBank("DCGM","11F,2I,7F,I");
if(verbose) fprintf(out,"DCGM bank defined with format: 11F,2I,7F,I\n");

formatBank("DCGW","8F");
if(verbose) fprintf(out,"DCGW bank defined with format: 8F\n");

formatBank("DCH ","10F,3I");
if(verbose) fprintf(out,"DCH  bank defined with format: 10F,3I\n");

formatBank("DCPB","I,11F,I");
if(verbose) fprintf(out,"DCPB bank defined with format: I,11F,I\n");

formatBank("DCV1","5F");
if(verbose) fprintf(out,"DCV1 bank defined with format: 5F\n");

formatBank("DCV2","14F");
if(verbose) fprintf(out,"DCV2 bank defined with format: 14F\n");

formatBank("DCV3","5F");
if(verbose) fprintf(out,"DCV3 bank defined with format: 5F\n");

formatBank("DDLY","I,F,I");
if(verbose) fprintf(out,"DDLY bank defined with format: I,F,I\n");

formatBank("DGEO","2I,6F");
if(verbose) fprintf(out,"DGEO bank defined with format: 2I,6F\n");

formatBank("DHCL","B08");
if(verbose) fprintf(out,"DHCL bank defined with format: B08\n");

formatBank("DITM","I");
if(verbose) fprintf(out,"DITM bank defined with format: I\n");

formatBank("DOCA","B16");
if(verbose) fprintf(out,"DOCA bank defined with format: B16\n");

formatBank("DPCP","2I,F,I,F,I,F,I,F,I,F,I,F");
if(verbose) fprintf(out,"DPCP bank defined with format: 2I,F,I,F,I,F,I,F,I,F,I,F\n");

formatBank("DPSP","2I,F");
if(verbose) fprintf(out,"DPSP bank defined with format: 2I,F\n");

formatBank("DSPC","B16");
if(verbose) fprintf(out,"DSPC bank defined with format: B16\n");

formatBank("DSPS","B16");
if(verbose) fprintf(out,"DSPS bank defined with format: B16\n");

formatBank("DSTC","B16");
if(verbose) fprintf(out,"DSTC bank defined with format: B16\n");

formatBank("DTCP","2I,F,I,F,I,F,I,F,I,F,I,F,I,F");
if(verbose) fprintf(out,"DTCP bank defined with format: 2I,F,I,F,I,F,I,F,I,F,I,F,I,F\n");

formatBank("DTRK","3F");
if(verbose) fprintf(out,"DTRK bank defined with format: 3F\n");

formatBank("EC  ","B16");
if(verbose) fprintf(out,"EC   bank defined with format: B16\n");

formatBank("EC01","I,2F");
if(verbose) fprintf(out,"EC01 bank defined with format: I,2F\n");

formatBank("EC1 ","B16");
if(verbose) fprintf(out,"EC1  bank defined with format: B16\n");

formatBank("EC1P","4F");
if(verbose) fprintf(out,"EC1P bank defined with format: 4F\n");

formatBank("EC1R","24F,4I,16F,3I,2F,I");
if(verbose) fprintf(out,"EC1R bank defined with format: 24F,4I,16F,3I,2F,I\n");

formatBank("ECCA","I,6F,I");
if(verbose) fprintf(out,"ECCA bank defined with format: I,6F,I\n");

formatBank("ECCL","I,6F,I");
if(verbose) fprintf(out,"ECCL bank defined with format: I,6F,I\n");

formatBank("ECCT","I,10F,I");
if(verbose) fprintf(out,"ECCT bank defined with format: I,10F,I\n");

formatBank("ECDI","5I");
if(verbose) fprintf(out,"ECDI bank defined with format: 5I\n");

formatBank("ECG ","8F,I,13F");
if(verbose) fprintf(out,"ECG  bank defined with format: 8F,I,13F\n");

formatBank("ECH ","7F,I,3F");
if(verbose) fprintf(out,"ECH  bank defined with format: 7F,I,3F\n");

formatBank("ECHB","I,29F,4I,F,I,F,I");
if(verbose) fprintf(out,"ECHB bank defined with format: I,29F,4I,F,I,F,I\n");

formatBank("ECMT","6I");
if(verbose) fprintf(out,"ECMT bank defined with format: 6I\n");

formatBank("ECP ","7F");
if(verbose) fprintf(out,"ECP  bank defined with format: 7F\n");

formatBank("ECP1","2I,F,I,F");
if(verbose) fprintf(out,"ECP1 bank defined with format: 2I,F,I,F\n");

formatBank("ECPB","I,11F,2I,F,I");
if(verbose) fprintf(out,"ECPB bank defined with format: I,11F,2I,F,I\n");

formatBank("ECPC","I,2F");
if(verbose) fprintf(out,"ECPC bank defined with format: I,2F\n");

formatBank("ECPE","2I,F");
if(verbose) fprintf(out,"ECPE bank defined with format: 2I,F\n");

formatBank("ECPI","3I,6F");
if(verbose) fprintf(out,"ECPI bank defined with format: 3I,6F\n");

formatBank("ECPO","7I");
if(verbose) fprintf(out,"ECPO bank defined with format: 7I\n");

formatBank("ECRB","48F,I");
if(verbose) fprintf(out,"ECRB bank defined with format: 48F,I\n");

formatBank("ECS ","96I");
if(verbose) fprintf(out,"ECS  bank defined with format: 96I\n");

formatBank("EID0","4I");
if(verbose) fprintf(out,"EID0 bank defined with format: 4I\n");

formatBank("EPIC","F,8A");
if(verbose) fprintf(out,"EPIC bank defined with format: F,8A\n");

formatBank("EVNT","I,2F,I,7F,7I");
if(verbose) fprintf(out,"EVNT bank defined with format: I,2F,I,7F,7I\n");

formatBank("FBPM","B16");
if(verbose) fprintf(out,"FBPM bank defined with format: B16\n");

formatBank("G1SL","64I");
if(verbose) fprintf(out,"G1SL bank defined with format: 64I\n");

formatBank("G2SL","64I");
if(verbose) fprintf(out,"G2SL bank defined with format: 64I\n");

formatBank("G3SL","64I");
if(verbose) fprintf(out,"G3SL bank defined with format: 64I\n");

formatBank("G4SL","64I");
if(verbose) fprintf(out,"G4SL bank defined with format: 64I\n");

formatBank("GPAR","F,I,5A");
if(verbose) fprintf(out,"GPAR bank defined with format: F,I,5A\n");

formatBank("HBER","21F,2I");
if(verbose) fprintf(out,"HBER bank defined with format: 21F,2I\n");

formatBank("HBID","2I,2F,2I,4F,2I,4F,2I,4F,2I,4F,2I,4F");
if(verbose) fprintf(out,"HBID bank defined with format: 2I,2F,2I,4F,2I,4F,2I,4F,2I,4F,2I,4F\n");

formatBank("HBLA","I,7F,3I,5F");
if(verbose) fprintf(out,"HBLA bank defined with format: I,7F,3I,5F\n");

formatBank("HBTB","10I");
if(verbose) fprintf(out,"HBTB bank defined with format: 10I\n");

formatBank("HBTR","8F,I");
if(verbose) fprintf(out,"HBTR bank defined with format: 8F,I\n");

formatBank("HCAL","5I");
if(verbose) fprintf(out,"HCAL bank defined with format: 5I\n");

formatBank("HDPL","I,7F");
if(verbose) fprintf(out,"HDPL bank defined with format: I,7F\n");

formatBank("HEAD","8I");
if(verbose) fprintf(out,"HEAD bank defined with format: 8I\n");

formatBank("HEVT","6I,9F,I");
if(verbose) fprintf(out,"HEVT bank defined with format: 6I,9F,I\n");

formatBank("HLS ","B32");
if(verbose) fprintf(out,"HLS  bank defined with format: B32\n");

formatBank("KFIT","46F");
if(verbose) fprintf(out,"KFIT bank defined with format: 46F\n");

formatBank("L1PG","A");
if(verbose) fprintf(out,"L1PG bank defined with format: A\n");

formatBank("L2H ","B16");
if(verbose) fprintf(out,"L2H  bank defined with format: B16\n");

formatBank("L2S ","B16");
if(verbose) fprintf(out,"L2S  bank defined with format: B16\n");

formatBank("LASR","4I");
if(verbose) fprintf(out,"LASR bank defined with format: 4I\n");

formatBank("LCDI","4I");
if(verbose) fprintf(out,"LCDI bank defined with format: 4I\n");

formatBank("LCPB","I,7F,I,F");
if(verbose) fprintf(out,"LCPB bank defined with format: I,7F,I,F\n");

formatBank("MCEV","2I");
if(verbose) fprintf(out,"MCEV bank defined with format: 2I\n");

formatBank("MCHD","5I,11F");
if(verbose) fprintf(out,"MCHD bank defined with format: 5I,11F\n");

formatBank("MCTK","6F,5I");
if(verbose) fprintf(out,"MCTK bank defined with format: 6F,5I\n");

formatBank("MCVX","4F,I");
if(verbose) fprintf(out,"MCVX bank defined with format: 4F,I\n");

formatBank("MTRK","7I");
if(verbose) fprintf(out,"MTRK bank defined with format: 7I\n");

formatBank("MVRT","I,11F,I");
if(verbose) fprintf(out,"MVRT bank defined with format: I,11F,I\n");

formatBank("PART","I,8F,I,2F,I");
if(verbose) fprintf(out,"PART bank defined with format: I,8F,I,2F,I\n");

formatBank("PCO ","7F,I");
if(verbose) fprintf(out,"PCO  bank defined with format: 7F,I\n");

formatBank("PRTM","I");
if(verbose) fprintf(out,"PRTM bank defined with format: I\n");

formatBank("PSO ","I,2F,I");
if(verbose) fprintf(out,"PSO  bank defined with format: I,2F,I\n");

formatBank("PTDB","11I");
if(verbose) fprintf(out,"PTDB bank defined with format: 11I\n");

formatBank("RCST","15I");
if(verbose) fprintf(out,"RCST bank defined with format: 15I\n");

formatBank("RF  ","3F");
if(verbose) fprintf(out,"RF   bank defined with format: 3F\n");

formatBank("RGLK","I,8F,I");
if(verbose) fprintf(out,"RGLK bank defined with format: I,8F,I\n");

formatBank("RNLG","A");
if(verbose) fprintf(out,"RNLG bank defined with format: A\n");

formatBank("RNPE","I,A");
if(verbose) fprintf(out,"RNPE bank defined with format: I,A\n");

formatBank("RTSL","64I");
if(verbose) fprintf(out,"RTSL bank defined with format: 64I\n");

formatBank("RUNC","I,F,I,F,I,F,I,F,I,F,I,F,I");
if(verbose) fprintf(out,"RUNC bank defined with format: I,F,I,F,I,F,I,F,I,F,I,F,I\n");

formatBank("S1ST","17I");
if(verbose) fprintf(out,"S1ST bank defined with format: 17I\n");

formatBank("SC  ","B16");
if(verbose) fprintf(out,"SC   bank defined with format: B16\n");

formatBank("SC1 ","I,8F");
if(verbose) fprintf(out,"SC1  bank defined with format: I,8F\n");

formatBank("SCC ","4I,44F");
if(verbose) fprintf(out,"SCC  bank defined with format: 4I,44F\n");

formatBank("SCDI","5I");
if(verbose) fprintf(out,"SCDI bank defined with format: 5I\n");

formatBank("SCG ","2I,9F");
if(verbose) fprintf(out,"SCG  bank defined with format: 2I,9F\n");

formatBank("SCGD","I,9F");
if(verbose) fprintf(out,"SCGD bank defined with format: I,9F\n");

formatBank("SCH ","7F,2I");
if(verbose) fprintf(out,"SCH  bank defined with format: 7F,2I\n");

formatBank("SCMD","2I,11F");
if(verbose) fprintf(out,"SCMD bank defined with format: 2I,11F\n");

formatBank("SCMT","I");
if(verbose) fprintf(out,"SCMT bank defined with format: I\n");

formatBank("SCMW","I");
if(verbose) fprintf(out,"SCMW bank defined with format: I\n");

formatBank("SCP ","16F");
if(verbose) fprintf(out,"SCP  bank defined with format: 16F\n");

formatBank("SCPB","I,4F,I");
if(verbose) fprintf(out,"SCPB bank defined with format: I,4F,I\n");

formatBank("SCPE","2I,F,I,F");
if(verbose) fprintf(out,"SCPE bank defined with format: 2I,F,I,F\n");

formatBank("SCPS","3I,13F");
if(verbose) fprintf(out,"SCPS bank defined with format: 3I,13F\n");

formatBank("SCR ","I,8F,I,2F");
if(verbose) fprintf(out,"SCR  bank defined with format: I,8F,I,2F\n");

formatBank("SCRC","I,10F,I");
if(verbose) fprintf(out,"SCRC bank defined with format: I,10F,I\n");

formatBank("SCS ","192I");
if(verbose) fprintf(out,"SCS  bank defined with format: 192I\n");

formatBank("SGMP","7I");
if(verbose) fprintf(out,"SGMP bank defined with format: 7I\n");

formatBank("SPAR","4I");
if(verbose) fprintf(out,"SPAR bank defined with format: 4I\n");

formatBank("SPIN","3I");
if(verbose) fprintf(out,"SPIN bank defined with format: 3I\n");

formatBank("ST  ","B16");
if(verbose) fprintf(out,"ST   bank defined with format: B16\n");

formatBank("ST1 ","2I,4F");
if(verbose) fprintf(out,"ST1  bank defined with format: 2I,4F\n");

formatBank("STH ","7F,2I,F");
if(verbose) fprintf(out,"STH  bank defined with format: 7F,2I,F\n");

formatBank("STPB","I,2F,2I");
if(verbose) fprintf(out,"STPB bank defined with format: I,2F,2I\n");

formatBank("STPE","2I,F");
if(verbose) fprintf(out,"STPE bank defined with format: 2I,F\n");

formatBank("STR ","2I,3F,I");
if(verbose) fprintf(out,"STR  bank defined with format: 2I,3F,I\n");

formatBank("STS ","16I");
if(verbose) fprintf(out,"STS  bank defined with format: 16I\n");

formatBank("SYNC","B16");
if(verbose) fprintf(out,"SYNC bank defined with format: B16\n");

formatBank("TACO","I,8F,I");
if(verbose) fprintf(out,"TACO bank defined with format: I,8F,I\n");

formatBank("TAGE","B16");
if(verbose) fprintf(out,"TAGE bank defined with format: B16\n");

formatBank("TAGI","I,2F,I,4F");
if(verbose) fprintf(out,"TAGI bank defined with format: I,2F,I,4F\n");

formatBank("TAGR","3F,3I");
if(verbose) fprintf(out,"TAGR bank defined with format: 3F,3I\n");

formatBank("TAGT","B16");
if(verbose) fprintf(out,"TAGT bank defined with format: B16\n");

formatBank("TBER","21F,2I");
if(verbose) fprintf(out,"TBER bank defined with format: 21F,2I\n");

formatBank("TBID","2I,2F,2I,4F,2I,4F,2I,4F,2I,4F,2I,4F");
if(verbose) fprintf(out,"TBID bank defined with format: 2I,2F,2I,4F,2I,4F,2I,4F,2I,4F,2I,4F\n");

formatBank("TBLA","I,7F,3I,6F");
if(verbose) fprintf(out,"TBLA bank defined with format: I,7F,3I,6F\n");

formatBank("TBTR","8F,2I");
if(verbose) fprintf(out,"TBTR bank defined with format: 8F,2I\n");

formatBank("TCSB","6F");
if(verbose) fprintf(out,"TCSB bank defined with format: 6F\n");

formatBank("TDPL","I,7F");
if(verbose) fprintf(out,"TDPL bank defined with format: I,7F\n");

formatBank("TESC","1F");
if (verbose)  fprintf(out,"TESC bank defined with format: 1F\n");

formatBank("TGBI","B32");
if(verbose) fprintf(out,"TGBI bank defined with format: B32\n");

formatBank("TGPB","I,3F");
if(verbose) fprintf(out,"TGPB bank defined with format: I,3F\n");

formatBank("TGS ","192I");
if(verbose) fprintf(out,"TGS  bank defined with format: 192I\n");

formatBank("TPC ","B16");
if(verbose) fprintf(out,"TPC  bank defined with format: B16\n");

formatBank("TRGS","64I");
if(verbose) fprintf(out,"TRGS bank defined with format: 64I\n");

formatBank("TRKS","I,5F,4I");
if(verbose) fprintf(out,"TRKS bank defined with format: I,5F,4I\n");

formatBank("TRL1","3I,7F");
if(verbose) fprintf(out,"TRL1 bank defined with format: 3I,7F\n");

formatBank("TRPB","I,7F");
if(verbose) fprintf(out,"TRPB bank defined with format: I,7F\n");

formatBank("TSPR","B32");
if(verbose) fprintf(out,"TSPR bank defined with format: B32\n");

formatBank("TSRG","B32");
if(verbose) fprintf(out,"TSRG bank defined with format: B32\n");

formatBank("UNUS","8I");
if(verbose) fprintf(out,"UNUS bank defined with format: 8I\n");

formatBank("VERT","3I,4F");
if(verbose) fprintf(out,"VERT bank defined with format: 3I,4F\n");


 }
