//********************************************************/
//* H.Avagyan
//* Date  : 2001-03-26
//********************************************************/
import java.awt.*;
import jas.hist.*;
import jas.plot.*;

/********************************************************
* CALDB viewer: example of JAVA-API appliaction.
* For more details see http://www.jlab.org/~avakian/caldbJ/
* and the README in CVS caldb/Java
********************************************************/    

public class Caldbjview extends Frame
{
    public static final String DEFAULT_HOST = "clasdb";
    public static final String DEFAULT_DBASE = "calib";

	public Caldbjview()
	{
		setLayout(null);
		setSize(450,600);
		setVisible(false);
		add(chSystems);
		chSystems.setBounds(24,48,168,25);
		add(chSubsystems);
		chSubsystems.setBounds(204,48,168,25);
		add(chItems);
		chItems.setBounds(320,48,168,25);
		textRunIndexTable.setText("RunIndex");
		add(textRunIndexTable);
		textRunIndexTable.setBounds(24,132,169,26);
		textDate.setText("2037-1-1");
		add(textDate);
		textDate.setBounds(204,132,100,26);
		textRunno.setText("14001");
		add(textRunno);
		textRunno.setBounds(320,132,100,26);
		label1.setText("System");
		add(label1);
		label1.setBounds(24,22,80,22);
		label2.setText("Subsystem");
		add(label2);
		label2.setBounds(204,22,80,22);
		label3.setText("Item");
		add(label3);
		label3.setBounds(320,22,80,22);
		label4.setText("RunIndexTable");
		add(label4);
		label4.setBounds(24,96,80,22);
		label5.setText("Date");
		add(label5);
		label5.setBounds(204,96,80,22);
		label6.setText("Runno");
		add(label6);
		label6.setBounds(320,96,168,22);
		add(textAreaOut);
		textAreaOut.setBounds(24,350,400,200);
		buttonConstants.setLabel("Constants");
		add(buttonConstants);
		//		buttonConstants.setBackground(java.awt.Color.lightGray);
		buttonConstants.setBounds(204,180,100,35);
		add(chChannels);
		chChannels.setBounds(24,314,100,26);
		buttonTG.setLabel("Time Graph");
		add(buttonTG);
		//    buttonTG.setBackground(java.awt.Color.lightGray);
		buttonTG.setBounds(204,314,100,27);

		add(buttonDiff);
		buttonDiff.setLabel("Difference");
		//	buttonDiff.setBackground(java.awt.Color.lightGray);
		buttonDiff.setBounds(320,314,100,27);

		add(buttonExit);
		buttonExit.setLabel("Exit");
		//		buttonExit.setBackground(java.awt.Color.lightGray);
		buttonExit.setBounds(24,560,400,27);




		tch.setText("Channels");
		add(tch);
		tch.setBounds(24,288,168,22);

		label7.setText("Date");
		add(label7);
		label7.setBounds(24,220,80,22);

		textTGDate.setText("2037-1-1");
		add(textTGDate);
		textTGDate.setBounds(24,250,100,26);
		label8.setText("Run Min");
		add(label8);
		label8.setBounds(204,220,100,22);
		textTGRunMin.setText("8000");
		add(textTGRunMin);
		textTGRunMin.setBounds(204,250,100,26);
		label9.setText("Run Max");
		add(label9);
		label9.setBounds(320,220,100,22);
		textTGRunMax.setText("29000");
		add(textTGRunMax);
		textTGRunMax.setBounds(320,250,100,26);

		//REGISTER_LISTENERS
		SymWindow aSymWindow = new SymWindow();
		this.addWindowListener(aSymWindow);
		SymItem lSymItem = new SymItem();
		chSystems.addItemListener(lSymItem);
		chSubsystems.addItemListener(lSymItem);
		SymAction lSymAction = new SymAction();
		buttonConstants.addActionListener(lSymAction);
		chItems.addItemListener(lSymItem);
		buttonTG.addActionListener(lSymAction);
		buttonExit.addActionListener(lSymAction);
		buttonDiff.addActionListener(lSymAction);
	}

	public Caldbjview(String title)
	{
		this();
		setTitle(title);
	}

    /**
     * Shows or hides the component depending on the boolean flag b.
     * @param b  if true, show the component; otherwise, hide the component.
     * @see java.awt.Component#isVisible
     */
    public void setVisible(boolean b)
	{
		if(b)
		{
			setLocation(50, 50);
		}
		super.setVisible(b);
	}

	static public void main(String args[])
	{  
                String host;
                String dbase=DEFAULT_DBASE;
                if (args.length > 0 ) {
		    host=args[0]+".jlab.org/";
                }else{
		    host=DEFAULT_HOST+".jlab.org/";
                }
                 if (args.length > 1) {
		     dbase=args[1];
                }

        	Caldbjview f = new Caldbjview();
		f.setTitle("Caldb Viewer      <"+host+dbase+">");
		f.initCaldb(host,dbase);
		f.initGUI();
		f.setVisible(true);
	}
	
      public static void usage() {
        System.out.println("Usage: java Caldbview <hostname> ");
        System.exit(0);
      }    
	
	RSystemsCaldb caldb = new RSystemsCaldb();
	
	public void initCaldb(String host, String dbase) {
	  try {
	    caldb.setDriver("org.gjt.mm.mysql.Driver");
	    caldb.setURL("jdbc:mysql://"+host+dbase);
	    caldb.setUsername("clasuser");
	    caldb.setPassword("");
	    caldb.loadSystems();
	  } catch (Exception e) {
	    this.textAreaOut.appendText("ERROR "+e.getMessage()+e);
	  }
	
	}
	
	public void initGUI() {
	  try {
         initSystems();
         initSubsystems();
         initItems();
	  } catch (Exception e) {
	    this.textAreaOut.appendText("ERROR "+e.getMessage()+e);
	  }
	
	}
	
	
    public void initSystems() {
	  try {
        String[] s = caldb.getSystems();
        setChoicesEnabled(false);
        for(int i=0;i<s.length;i++) this.chSystems.addItem(s[i]);
	  } catch (Exception e) {
	    this.textAreaOut.appendText("ERROR "+e.getMessage()+e);
	  } finally {
	    setChoicesEnabled(true);
	  }
	
	}
	
	public void initSubsystems() {
	  try {
	    String sys = this.chSystems.getSelectedItem();
        String[] s = caldb.getSybsystems(sys);
        setChoicesEnabled(false);
        this.chSubsystems.removeAll();
        for(int i=0;i<s.length;i++) this.chSubsystems.addItem(s[i]);
        initItems();
	  } catch (Exception e) {
	    this.textAreaOut.appendText("ERROR "+e.getMessage()+e);
	  } finally {
	    setChoicesEnabled(true);
	  }
	}
	
    public void initItems() {
	  try {
	    String sys = this.chSystems.getSelectedItem();
	    String sub = this.chSubsystems.getSelectedItem();
        String[] s = caldb.getItems(sys,sub);
        setChoicesEnabled(false);
        this.chItems.removeAll();
        for(int i=0;i<s.length;i++) this.chItems.addItem(s[i]);
        initChannels();
	  } catch (Exception e) {
	    this.textAreaOut.appendText("ERROR "+e.getMessage()+e);
	  } finally {
	    setChoicesEnabled(true);
	  }
	}
	
	 public void initChannels() {
	  try {
	    String systemname = this.chSystems.getSelectedItem();
	    String subsystemname = this.chSubsystems.getSelectedItem();
	    String itemname = this.chItems.getSelectedItem();
	    RItem item = caldb.getItem(systemname,subsystemname,itemname);
        String[] s = item.getAsArray();
        setChoicesEnabled(false);
        this.chChannels.removeAll();
        for(int i=0;i<s.length;i++) this.chChannels.addItem(s[i]);
	  } catch (Exception e) {
	    this.textAreaOut.appendText("ERROR "+e.getMessage()+e);
	  } finally {
	    setChoicesEnabled(true);
	  }
	}
	
	
	public void setChoicesEnabled(boolean b) {
	   this.chSystems.setEnabled(b);
	   this.chSubsystems.setEnabled(b);
	   this.chItems.setEnabled(b);
	   this.chChannels.setEnabled(b);
	}
	
	
	public void constantsGo() {
	    try {
	      String systemname = this.chSystems.getSelectedItem();
	      String  subsystemname = this.chSubsystems.getSelectedItem();
	      String itemname = this.chItems.getSelectedItem();
	      String RunIndexTable = this.textRunIndexTable.getText();
	      String date = this.textDate.getText();
	      int runno = Integer.parseInt(this.textRunno.getText());
	      RConstantSet set = caldb.getConstants(systemname,subsystemname,
	                                       itemname,RunIndexTable,date,runno);
	      this.textAreaOut.appendText(set.toString());
	      // make a histo view
	      HistoView view = new HistoView();
	      JASHist plot = view.getPlot();
	      double[] data1 = set.getAsDoubleArray();
	        plot.addData(new ArrayDataSource(data1));
		    JASHistData d2ata=plot.addData(new ArrayDataSource(data1));
            ((JASHist1DHistogramStyle) d2ata.getStyle()).setShowErrorBars(false);
            ((JASHist1DHistogramStyle) d2ata.getStyle()).setHistogramFill(false);
            ((JASHist1DHistogramStyle) d2ata.getStyle()).setHistogramBarColor(Color.blue);
            d2ata.show(true);
            plot.setTitle("Calib. Const. for "+set.getParent().tableName4Constants());
            plot.setDataAreaBorderType(plot.SHADOW);
            plot.getYAxis().setLabel(itemname);
            plot.getXAxis().setLabel("channel#  (Run "+runno+")");
	      // show it
	      view.setVisible(true);
	    } catch (Exception e) {
	      this.textAreaOut.appendText("ERROR "+e.getMessage()+e);
	    }
	}
	
	public void diffGo() {
	    try {
	      String systemname = this.chSystems.getSelectedItem();
	      String  subsystemname = this.chSubsystems.getSelectedItem();
	      String itemname = this.chItems.getSelectedItem();
	      String RunIndexTable = this.textRunIndexTable.getText();
	      String date = this.textTGDate.getText();
	      int runnoMax = Integer.parseInt(this.textTGRunMax.getText());
	      int runnoMin = Integer.parseInt(this.textTGRunMin.getText());
	      RConstantSet set1 = caldb.getConstants(systemname,subsystemname,
	                                       itemname,RunIndexTable,date,runnoMin);
	      RConstantSet set2 = caldb.getConstants(systemname,subsystemname,
	                                       itemname,RunIndexTable,date,runnoMax);
	      double[] datadiff = getAsDoubleDiffArray(set1,set2);              
 
              String v = ""; 
              for(int j=1; j<=datadiff.length; j++){
              v +="v_"+(j<1000 ? "0":"")+(j<100 ? "0":"")+(j<10 ? "0":"")+j+" "+datadiff[j-1]+"\n";
	      }

	      this.textAreaOut.appendText(v);

	      //this.textAreaOut.appendText(set.toString());
	      // make a histo view
	      HistoView view = new HistoView();
	      JASHist plot = view.getPlot();
	        plot.addData(new ArrayDataSource(datadiff));
		    JASHistData d2ata=plot.addData(new ArrayDataSource(datadiff));
            ((JASHist1DHistogramStyle) d2ata.getStyle()).setShowErrorBars(false);
            ((JASHist1DHistogramStyle) d2ata.getStyle()).setHistogramFill(false);
            ((JASHist1DHistogramStyle) d2ata.getStyle()).setHistogramBarColor(Color.gray);
            d2ata.show(true);
            plot.setTitle("Calib. Const. for "+set1.getParent().tableName4Constants()+" Runs "+runnoMin+" vs "+runnoMax);
            plot.setDataAreaBorderType(plot.SHADOW);
            plot.getYAxis().setLabel("Diff #"+runnoMin +" #"+runnoMax);
            plot.getXAxis().setLabel("channel#");
	      // show it
	      view.setVisible(true);
	    } catch (Exception e) {
	      this.textAreaOut.appendText("ERROR "+e.getMessage()+e);
	    }
	}
	
    public void channelGo() {
	    try {
	      String systemname = this.chSystems.getSelectedItem();
	      String  subsystemname = this.chSubsystems.getSelectedItem();
	      String itemname = this.chItems.getSelectedItem();
	      String RunIndexTable = this.textRunIndexTable.getText();
	      String date = this.textDate.getText();
	      int runnoMax = Integer.parseInt(this.textTGRunMax.getText());
	      int runnoMin = Integer.parseInt(this.textTGRunMin.getText());
	      int j = this.chChannels.getSelectedIndex();
	      String channel = "v_"+(j<1000 ? "0":"")+(j<100 ? "0":"")+(j<10 ? "0":"")+(j+1);
	      RChannelData chdata = caldb.getChannelData(systemname,subsystemname,
	                                       itemname,RunIndexTable,date,runnoMax,runnoMin,channel);
	      this.textAreaOut.appendText(chdata.toString());
	      // make a histo view
	      HistoView view = new HistoView();
	      JASHist plot = view.getPlot();
	      double[] data2 = chdata.getAsDoubleArray();
	      String[] data3 = chdata.getRunNoArrayAsString();
                  plot.setAllowUserInteraction(false);
                  plot.setTitle("Time dependence ");
                  plot.setDataAreaBorderType(plot.SHADOW);
                  plot.getYAxis().setLabel(channel);
                  plot.getXAxis().setLabel("run# (from "+data3[0]+"  to "+data3[data3.length-1]+")");
	          plot.addData(new ArrayDataSource(data2));//.show(true);
		  //	          plot.addData(new StringDataSource(data2,data3));//.show(true);
		  //	       JASHistData data=plot.addData(new StringDataSource(data2,data3));
	       JASHistData data=plot.addData(new ArrayDataSource(data2));
              ((JASHist1DHistogramStyle) data.getStyle()).setShowErrorBars(false);
              ((JASHist1DHistogramStyle) data.getStyle()).setHistogramBarColor(Color.gray);
	      ((JASHist1DHistogramStyle) data.getStyle()).setHistogramFill(false);
               data.show(true);
	      // show it
	      view.setVisible(true);
	    } catch (Exception e) {
	      this.textAreaOut.appendText("ERROR "+e.getMessage()+e+"\n");
	    }
	}
		
	public void addNotify()
	{
	    Dimension d = getSize();
	    
		super.addNotify();

		if (fComponentsAdjusted)
			return;

		Insets insets = getInsets();
		setSize(insets.left + insets.right + d.width, insets.top + insets.bottom + d.height);
		Component components[] = getComponents();
		for (int i = 0; i < components.length; i++)
		{
			Point p = components[i].getLocation();
			p.translate(insets.left, insets.top);
			components[i].setLocation(p);
		}
		fComponentsAdjusted = true;
	}

	boolean fComponentsAdjusted = false;

	//DECLARE_CONTROLS
	java.awt.Choice chSystems = new java.awt.Choice();
	java.awt.Choice chSubsystems = new java.awt.Choice();
	java.awt.Choice chItems = new java.awt.Choice();
	java.awt.Choice chChannels = new java.awt.Choice();

	java.awt.Label label1 = new java.awt.Label();
	java.awt.Label label2 = new java.awt.Label();
	java.awt.Label label3 = new java.awt.Label();
	java.awt.Label label4 = new java.awt.Label();
	java.awt.Label label5 = new java.awt.Label();
	java.awt.Label label6 = new java.awt.Label();
	java.awt.Label tch = new java.awt.Label();
	java.awt.Label label7 = new java.awt.Label();
	java.awt.Label label8 = new java.awt.Label();
	java.awt.Label label9 = new java.awt.Label();

	java.awt.TextArea textAreaOut = new java.awt.TextArea();
	java.awt.TextField textRunIndexTable = new java.awt.TextField();
	java.awt.TextField textDate = new java.awt.TextField();
	java.awt.TextField textRunno = new java.awt.TextField();
	java.awt.TextField textTGRunMin = new java.awt.TextField();
	java.awt.TextField textTGRunMax = new java.awt.TextField();
	java.awt.TextField textTGDate = new java.awt.TextField();

	java.awt.Button buttonConstants = new java.awt.Button();
	java.awt.Button buttonTG = new java.awt.Button();
	java.awt.Button buttonDiff = new java.awt.Button();
	java.awt.Button buttonExit = new java.awt.Button();


	class SymWindow extends java.awt.event.WindowAdapter
	{
		public void windowClosing(java.awt.event.WindowEvent event)
		{
			Object object = event.getSource();
			if (object == Caldbjview.this)
				Caldbjview_WindowClosing(event);
		}
	}
	
	void Caldbjview_WindowClosing(java.awt.event.WindowEvent event)
	{
		setVisible(false);		 // hide the Frame
	}

	class SymItem implements java.awt.event.ItemListener
	{
		public void itemStateChanged(java.awt.event.ItemEvent event)
		{
			Object object = event.getSource();
			if (object == chSystems)
				chSystems_ItemStateChanged(event);
			else if (object == chSubsystems)
				chSubsystems_ItemStateChanged(event);
			else if (object == chItems)
				chItems_ItemStateChanged(event);
		}
	}

        double[] getAsDoubleDiffArray( RConstantSet set1, RConstantSet set2) {
	      double[] data1 = set1.getAsDoubleArray();
	      double[] data2 = set2.getAsDoubleArray();
              int minLength=Math.min(data1.length,data2.length);
              double[] data= new double[minLength];
              for(int i=0;i<minLength;i++)  
              {
                data[i]=data2[i]-data1[i];
	      }
              return data;
	}


	void chSystems_ItemStateChanged(java.awt.event.ItemEvent event)
	{
		try {
			this.initSubsystems();
		} catch (java.lang.Exception e) {
		}
	}

	void chSubsystems_ItemStateChanged(java.awt.event.ItemEvent event)
	{
		try {
			this.initItems();
		} catch (java.lang.Exception e) {
		}
	}

	class SymAction implements java.awt.event.ActionListener
	{
		public void actionPerformed(java.awt.event.ActionEvent event)
		{
			Object object = event.getSource();
			if (object == buttonConstants)
				buttonConstants_ActionPerformed(event);
			else if (object == buttonTG)
				buttonTG_ActionPerformed(event);
			else if (object == buttonDiff)
				buttonDiff_ActionPerformed(event);
			else if (object == buttonExit)
				buttonExit_ActionPerformed(event);
		}
	}


	void buttonConstants_ActionPerformed(java.awt.event.ActionEvent event)
	{
		try {
			this.constantsGo();
		} catch (java.lang.Exception e) {
		}
	}

	void chItems_ItemStateChanged(java.awt.event.ItemEvent event)
	{
		try {
			this.initChannels();
		} catch (java.lang.Exception e) {
		}
	}

	void buttonTG_ActionPerformed(java.awt.event.ActionEvent event)
	{
		try {
			this.channelGo();
		} catch (java.lang.Exception e) {
		}
	}

	void buttonDiff_ActionPerformed(java.awt.event.ActionEvent event)
	{
		try {
			this.diffGo();
		} catch (java.lang.Exception e) {
		}
	}

	void buttonExit_ActionPerformed(java.awt.event.ActionEvent event)
	{
                       System.exit(0); 
	}



}
