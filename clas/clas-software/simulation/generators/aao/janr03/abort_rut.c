#include <stdio.h>
#include <signal.h>

void assign_sig_();
void sig_Interrupt__();

void assign_sig_(){

signal(SIGINT, sig_Interrupt__);
fprintf(stderr,"Interrupt handler is set\n");

}

void sig_Interrupt__(int sig){

fprintf(stderr,"Cought interrupt signal [%d]. Trying to exit program safely\n",sig);
safe_exit_();
fprintf(stderr,"Safely exit successfuly complite!\n");
exit(0);

}
