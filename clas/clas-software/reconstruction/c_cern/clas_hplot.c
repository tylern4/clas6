
void hplint(int no)
{
        hplint_(&no);
}

void iuwk(int num1, int num2)
{
        iuwk_(&num1, &num2);
}

void hplego(int no, float theta, float phi)
{
        hplego_(&no, &theta, &phi);
        return;
}

void hplcap(int unit){
  hplcap_(&unit);
}

void hplot(int no, char*chopt, char*chcase, int num)
{
        hplot_(&no,chopt,chcase,&num,strlen(chopt),strlen(chcase));
}

void hplzon(int nx, int ny, int firstplotted, char *option){
  hplzon_(&nx, &ny, &firstplotted, option, strlen(option));
}
