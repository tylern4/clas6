void elast_gen_wrapper(char* f1, char* f2,char* f3,char* f4,float* f,
        float* d,int* g,
	float* tl,float* tr,float* vx,float* vy,float* vz,float* E,
	float* emn,float* smn,float* smx,int* p,int* M,float* c)
	{
	elast_gen_(f1,f2,f3,f4,f,d,g,tl,tr,vx,vy,vz,E,emn,smn,smx,p,M,c);  
	}
	
