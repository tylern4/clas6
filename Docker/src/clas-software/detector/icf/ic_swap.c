int icswap_(int *id_in){
  /*        short  y;*/
		int x = *id_in;
		
        int y=ICswap(x);
        return y;
}

/*	swap values within function	*/
int ICswap(int id_in){
  int id_out=id_in;
  if(id_in==2577)id_out=(int)3345;   // ADC65 0x0A11 2577 17  10   //missing
  else if(id_in==3345)id_out=(int)2576;	// ADC66 0x0D11 3345 17  13 
  else if(id_in==2576)id_out=(int)3346;	// ADC67 0x0A10 2576 16  10 
  else if(id_in==3346)id_out=(int)2575;	// ADC68 0x0D12 3346 18  13 
  else if(id_in==2575)id_out=(int)3347;	// ADC69 0x0A0F 2575 15  10 
  else if(id_in==3347)id_out=(int)2835;	// ADC70 0x0D13 3347 19  13 
  else if(id_in==2835)id_out=(int)3087;	// ADC71 0x0B13 2835 19  11 
  else if(id_in==3087)id_out=(int)2834;	// ADC72 0x0C0F 3087 15  12 
  else if(id_in==2834)id_out=(int)3088;	// ADC73 0x0B12 2834 18  11 
  else if(id_in==3088)id_out=(int)2833;	// ADC74 0x0C10 3088 16  12 
  else if(id_in==2833)id_out=(int)3089;	// ADC75 0x0B11 2833 17  11 
  else if(id_in==3089)id_out=(int)2832;	// ADC76 0x0C11 3089 17  12 
  else if(id_in==2832)id_out=(int)3090;	// ADC77 0x0B10 2832 16  11 
  else if(id_in==3090)id_out=(int)2831;	// ADC78 0x0C12 3090 18  12 
  else if(id_in==2831)id_out=(int)3091;	// ADC79 0x0B0F 2831 15  11
  else if(id_in==3091)id_out=(int)5643;      // ADC80 0x0C13 3091 19  12 
  else if(id_in==5643)id_out=(int)5644;      // ADC81 0x160B 5643   //missing  
  else if(id_in==5644)id_out=(int)2061; 	// ADC82 0x160C 5644  
  else if(id_in==2061)id_out=(int)2323;	// ADC83 0x080D 2061 13  8	 
  else if(id_in==2323)id_out=(int)2062;	// ADC84 0x0913 2323 19  9	 
  else if(id_in==2062)id_out=(int)2322;	// ADC85 0x080E 2062 14  8	 
  else if(id_in==2322)id_out=(int)2063;	// ADC86 0x0912 2322 18  9	 
  else if(id_in==2063)id_out=(int)2321;	// ADC87 0x080F 2063 15  8	 
  else if(id_in==2321)id_out=(int)2064;	// ADC88 0x0911 2321 17  9	 
  else if(id_in==2064)id_out=(int)2320;	// ADC89 0x0810 2064 16  8	 
  else if(id_in==2320)id_out=(int)2065;	// ADC90 0x0910 2320 16  9	 
  else if(id_in==2065)id_out=(int)2319;	// ADC91 0x0811 2065 17  8	 
  else if(id_in==2319)id_out=(int)2066;	// ADC92 0x090F 2319 15  9	 
  else if(id_in==2066)id_out=(int)2318;	// ADC93 0x0812 2066 18  8	 
  else if(id_in==2318)id_out=(int)2067;	// ADC94 0x090E 2318 14  9	 
  else if(id_in==2067)id_out=(int)2317;	// ADC95 0x0813 2067 19  8	 
  return id_out;
}
