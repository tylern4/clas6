#include <iostream.h>

extern "C" {
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <signal.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <bostypes.h>
#include <bosddl.h>
#include <particleType.h>
#include <map_manager.h>
}

main()
{
  int i, j, isec, sec, aot, lor, value, id, xx, big, ll, ct;
  isec=1;
  aot=lor=ct=0;
  ll=16 ;
  i=0;
  while (cin >> value){
       if (!(i%1)&&i){
	 //    cout << "|" ;
    printf("|");
       }
    if (!(i%192&&i)){
      //      cout << endl;
      printf("\n");
      ct++;
      //      for(j=0;j<5*ll;++j){
      for(j=0;j<4*ll;++j){
	//	cout << "-";
	printf("-");
      }
      if(i!=0){
	//      cout << " " << ct-1;
      printf(" %1i: ",ct-1);
      }
    }
    if (i==0)
      //      cout << endl;
   printf("\n");
    if (!(i%ll)&&i) {     
      //      cout << endl;
      printf("\n");
    }
    i++;
    printf("%3i",value);
    //    cout << value << " ";
  }
    cout << "|" ;
  //  printf("|");
    //printf("  %3i %3i %3i",
    //printf("\n");
  cout << endl;

}
