//
// jas example for time graph (to be fixed)
//
                import jas.hist.*;

                class ArrayDataSource implements Rebinnable1DHistogramData
                {
                   ArrayDataSource(double[] data)
                   {
                      this(data,"Array Data Source");
                   }
                   ArrayDataSource(double[] data, String name)
                   {
                      this.data =   data;
                      this.name =   name;
                   }
                   public double[][] rebin(int   rBins, double rMin,   double rMax, 
                                     boolean   wantErrors,   boolean   hurry)
                   {
                      double[][] result =   { data };
                      return result;
                   }
                   public String[]   getAxisLabels()   { return null; }
                   public double getMin() { return   0; }
                   public double getMax() { return   data.length; }
                   public boolean isRebinnable() {   return false; }
                   public int getBins() { return data.length; }
                   public int getAxisType() { return DOUBLE; }
                   public String getTitle() { return name;   }
                             
                   private   double[] data;
                   private   String name;
                }

                class StringDataSource extends ArrayDataSource
                {
                   StringDataSource(double[] data, String[] mylabels )
                   {
                      super(data);
		      labels = new String[data.length];
	              for   (int i=0; i<labels.length; i++) 
		       {
                        labels[i] =   mylabels[i];//.substring(0,5);
                       }
                   }
                   public int getAxisType() { return STRING; }
                   public String[]   getAxisLabels()   { return labels; }   
                   private   String[] labels;
                }


                class DateDataSource extends ArrayDataSource
                {
                   DateDataSource(double[]   data)
                   {
                      super(data);
                      max   = System.currentTimeMillis()/1000; // now
                      min   = max -   90*60; // 90 minutes ago
                   }
                   public int getAxisType() { return DATE;   }
                   public double getMin() { return   min; }
                   public double getMax() { return   max; }
                   private   double min,   max;
                }
