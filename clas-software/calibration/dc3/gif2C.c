

#include <stdio.h>
#include <stdlib.h>


#define buff_size 8192

/* This is a small utility for putting a binary image file into   */
/* a C compilable executable. This takes the image files (e.g.    */
/* my_image.gif) and creates a file "my_image_image.c" with a     */
/* routine that will attempt to create the image file when called */

int main(int narg,char *argv[])
{
	int i,j,N,bc=0;
	FILE *f,*outfile;
	unsigned char buff[buff_size];
	char basename[256],fname[256];
	char array_name[256];

	for(N=1;N<narg;N++){

		f=fopen(argv[N],"r");
		if(!f){
			printf("Unable to open \"%s\" for reading\n",argv[N]);
			continue;
		}

		strcpy(basename,argv[N]);
		for(i=strlen(basename);i>0;i--)
			if(basename[i]=='.'){basename[i]=0;break;}

		sprintf(fname,"%s_image.c",basename);
		outfile=fopen(fname,"w");
		if(!outfile){
			printf("Unable to open \"%s\" for writing\n",fname);
			fclose(f);
			continue;
		}

		printf("%s -> %s\n",argv[N],fname);

		/* Write header info */
		fprintf(outfile,"\n\n#include <stdlib.h>\n");
		fprintf(outfile,"#include <stdio.h>\n");
		fprintf(outfile,"#include <unistd.h>\n");
		sprintf(array_name,"%s_image_data",basename);
		fprintf(outfile,"\n\nextern unsigned char %s[];\n",array_name);
		fprintf(outfile,"extern int N_%s;\n\n",basename);
		fprintf(outfile,"FILE *%s_file;\n",basename);
		fprintf(outfile,"char %s_fname[256];\n\n",basename);

		fprintf(outfile,"char* %s_image_init(void)\n{\n",basename);
		fprintf(outfile,"	int  i;\n\n");
		fprintf(outfile,"	sprintf(%s_fname,\"/tmp/dc3_%cd.gif\",getpid());\n\n",basename,'%');
		fprintf(outfile,"	%s_file=fopen(%s_fname,\"w\");\n",basename,basename);
		fprintf(outfile,"	if(!%s_file)return NULL;\n\n",basename);
		fprintf(outfile,"	fwrite(%s,1,N_%s,%s_file);\n\n",array_name,basename,basename);
		fprintf(outfile,"	fclose(%s_file);\n\n",basename);
		fprintf(outfile,"	/* Unlinking and keeping the file open here will ensure it exists */\n");
		fprintf(outfile,"	/* until the program exits. Then the file will be deleted. */\n");
		fprintf(outfile,"	/*unlink(%s_fname);*/\n\n",basename);
		fprintf(outfile,"	return %s_fname;\n",basename);

		fprintf(outfile,"}\n\n");
		
		fprintf(outfile,"\n\nunsigned char %s[]={\n\t",array_name);
		while(j=fread(buff,1,buff_size,f)){
			for(i=0;i<j;i++){
				if((++bc)%6 == 0)fprintf(outfile,"\n\t");
				fprintf(outfile,"%3d",buff[i]);
				if(i==j-1 && j<buff_size)continue;
				fprintf(outfile,", ");
			}

		}
		fprintf(outfile,"};\n\n");
		fprintf(outfile,"int N_%s=%d;\n\n",basename,bc);

		fclose(f);
		fclose(outfile);
	}
	

}


	



