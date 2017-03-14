//********************************************************/
//* H.Avagyan
//* Date  : 2001-04-01
//********************************************************/

import javax.swing.*;
import java.awt.*;
import jas.hist.*;
import jas.plot.*;


public class JCaldbview extends JApplet
{
    public static final String DEFAULT_HOST = "clasdb";
    public static final String DEFAULT_DBASE = "calib";
    public static  int unique_on = 1;
    public static  String caldburl;

	public void init()
	{
		getContentPane().setLayout(new BorderLayout(0,0));
		setSize(400,700);
		JPanelMain.setLayout(null);
		getContentPane().add(JPanelMain);
		JPanelMain.add(chSystems);
		chSystems.setBounds(24,48,156,27);
		JLabel1.setText("System");
		JPanelMain.add(JLabel1);
		JLabel1.setBounds(24,24,129,21);
		JLabel2.setText("Subsystem");
		JPanelMain.add(JLabel2);
		JLabel2.setBounds(192,24,129,21);
		JPanelMain.add(chSubsystems);
		chSubsystems.setBounds(192,48,156,27);
		JLabel3.setText("Item");
		JPanelMain.add(JLabel3);
		JLabel3.setBounds(360,24,100,21);
		JPanelMain.add(chItems);
		chItems.setBounds(360,48,100,27);
		JLabel4.setText("Run Index Table");
		JPanelMain.add(JLabel4);
		JLabel4.setBounds(24,96,144,24);
		textRunIndexTable.setText("RunIndex");
		JPanelMain.add(textRunIndexTable);
		textRunIndexTable.setBounds(24,132,156,24);
		JLabel5.setText("Date");
		JPanelMain.add(JLabel5);
		JLabel5.setBounds(192,96,144,24);
		textDate.setText("2037-1-1");
		JPanelMain.add(textDate);
		textDate.setBounds(192,132,156,24);
		JLabel6.setText("Run No");
		JPanelMain.add(JLabel6);
		JLabel6.setBounds(360,96,100,24);
		textRunno.setText("14001");
		JPanelMain.add(textRunno);
		textRunno.setBounds(360,132,100,24);
		buttonConstants.setText("Constants");
		JPanelMain.add(buttonConstants);
		buttonConstants.setBounds(180,180,168,24);
		JLabel7.setText("Channels");
		JPanelMain.add(JLabel7);
		JLabel7.setBounds(24,288,168,22);
		JPanelMain.add(chChannels);
		chChannels.setBounds(24,314,100,26);

		buttonTG.setText("Time Graph");
		JPanelMain.add(buttonTG);
		buttonTG.setBounds(160,314,120,27);
                //checkTG.setText("");
		JPanelMain.add(checkTG);
		checkTG.setBounds(285,314,20,27);
                checkTG.setSelected(true);
                checkTG.setToolTipText("Check this to get a single entry per run");
		buttonDiff.setText("Difference");
		JPanelMain.add(buttonDiff);
		buttonDiff.setBounds(330,314,120,27);

		buttonExit.setText("Exit");
		JPanelMain.add(buttonExit);
		buttonExit.setBounds(24,560,420,27);

		JLabel8.setText("Date");
		JPanelMain.add(JLabel8);
		JLabel8.setBounds(24,220,80,22);
		JLabel9.setText("Run Min");
		JPanelMain.add(JLabel9);
		JLabel9.setBounds(204,220,100,22);
		JLabel10.setText("Run Max");
		JPanelMain.add(JLabel10);
		JLabel10.setBounds(350,220,100,22);
		JPanelMain.add(JScrollPane1);
		JScrollPane1.setBounds(24,350,420,200);
		JScrollPane1.getViewport().add(textAreaOut);
		textAreaOut.setBounds(0,0,420,200);
		textTGDate.setText("2037-1-1");
		JPanelMain.add(textTGDate);
		textTGDate.setBounds(24,250,100,26);
		textTGRunMin.setText("8000");
		JPanelMain.add(textTGRunMin);
		textTGRunMin.setBounds(204,250,100,26);
		textTGRunMax.setText("29000");
		JPanelMain.add(textTGRunMax);
		textTGRunMax.setBounds(350,250,100,26);
		
		chSystems.setModel(new DefaultComboBoxModelHack());
	    chSubsystems.setModel(new DefaultComboBoxModelHack());
	    chItems.setModel(new DefaultComboBoxModelHack());
	    chChannels.setModel(new DefaultComboBoxModelHack());
	    
	    // LISTENERS
		SymItem lSymItem = new SymItem();
		SymAction lSymAction = new SymAction();

		chSystems.addItemListener(lSymItem);
		chSubsystems.addItemListener(lSymItem);
		buttonConstants.addActionListener(lSymAction);
		chItems.addItemListener(lSymItem);
		buttonTG.addActionListener(lSymAction);
		checkTG.addActionListener(lSymAction);
		buttonDiff.addActionListener(lSymAction);
		buttonExit.addActionListener(lSymAction);

  	    this.initCaldb();
            this.initGUI();
		
	}
	
	javax.swing.JPanel JPanelMain = new javax.swing.JPanel();
	javax.swing.JComboBox chSystems = new javax.swing.JComboBox();
	javax.swing.JLabel JLabel1 = new javax.swing.JLabel();
	javax.swing.JLabel JLabel2 = new javax.swing.JLabel();
	javax.swing.JComboBox chSubsystems = new javax.swing.JComboBox();
	javax.swing.JLabel JLabel3 = new javax.swing.JLabel();
	javax.swing.JComboBox chItems = new javax.swing.JComboBox();
	javax.swing.JLabel JLabel4 = new javax.swing.JLabel();
	javax.swing.JTextField textRunIndexTable = new javax.swing.JTextField();
	javax.swing.JLabel JLabel5 = new javax.swing.JLabel();
	javax.swing.JTextField textDate = new javax.swing.JTextField();
	javax.swing.JLabel JLabel6 = new javax.swing.JLabel();
	javax.swing.JTextField textRunno = new javax.swing.JTextField();
	javax.swing.JButton buttonConstants = new javax.swing.JButton();
	javax.swing.JLabel JLabel7 = new javax.swing.JLabel();
	javax.swing.JComboBox chChannels = new javax.swing.JComboBox();
	javax.swing.JButton buttonTG = new javax.swing.JButton();
	javax.swing.JCheckBox checkTG = new javax.swing.JCheckBox();
	javax.swing.JButton buttonDiff = new javax.swing.JButton();
	javax.swing.JButton buttonExit = new javax.swing.JButton();
	javax.swing.JLabel JLabel8 = new javax.swing.JLabel();
	javax.swing.JLabel JLabel9 = new javax.swing.JLabel();
	javax.swing.JLabel JLabel10 = new javax.swing.JLabel();
	javax.swing.JScrollPane JScrollPane1 = new javax.swing.JScrollPane();
	javax.swing.JEditorPane textAreaOut = new javax.swing.JEditorPane();
	javax.swing.JTextField textTGDate = new javax.swing.JTextField();
	javax.swing.JTextField textTGRunMin = new javax.swing.JTextField();
	javax.swing.JTextField textTGRunMax = new javax.swing.JTextField();
	//}}
	
	
	
		
	RSystemsCaldb caldb = new RSystemsCaldb();
	
	public void initCaldb() {
	  try {
	    caldb.setDriver("org.gjt.mm.mysql.Driver");
	    caldb.setURL(caldburl);
	    caldb.setUsername("clasuser");
	    caldb.setPassword("");
	    caldb.loadSystems();
	  } catch (Exception e) {
	    this.textAreaOut.setText("ERROR "+e.getMessage()+e);
	  }
	}
	
	public void initGUI() {
	  try {
         initSystems();
         initSubsystems();
         initItems();
	  } catch (Exception e) {
	    this.textAreaOut.setText("ERROR "+e.getMessage()+e);
	  }
	
	}
	
	
    public void initSystems() {
	  try {
        String[] s = caldb.getSystems();
        setChoicesEnabled(false);
        ((DefaultComboBoxModelHack)this.chSystems.getModel()).setObjects(s);
        this.chSystems.repaint();

        //for(int i=0;i<s.length;i++) this.chSystems.addItem(s[i]);
	  } catch (Exception e) {
	    this.textAreaOut.setText("ERROR "+e.getMessage()+e);
	  } finally {
	    setChoicesEnabled(true);
	  }
	
	}
	
	public void initSubsystems() {
	  try {
	    String sys = (String)this.chSystems.getSelectedItem();
        String[] s = caldb.getSybsystems(sys);
        setChoicesEnabled(false);
        ((DefaultComboBoxModelHack)this.chSubsystems.getModel()).setObjects(s);
        this.chSubsystems.repaint();
        
        //this.chSubsystems.removeAllItems();
        //for(int i=0;i<s.length;i++) this.chSubsystems.addItem(s[i]);
        initItems();
	  } catch (Exception e) {
	    this.textAreaOut.setText("ERROR "+e.getMessage()+e);
	  } finally {
	    setChoicesEnabled(true);
	  }
	}
	
    public void initItems() {
	  try {
	    String sys = (String)this.chSystems.getSelectedItem();
	    String sub = (String)this.chSubsystems.getSelectedItem();
        String[] s = caldb.getItems(sys,sub);
        setChoicesEnabled(false);
        ((DefaultComboBoxModelHack)this.chItems.getModel()).setObjects(s);
        this.chItems.repaint();

        //this.chItems.removeAllItems();
       //for(int i=0;i<s.length;i++) this.chItems.addItem(s[i]);
        initChannels();
	  } catch (Exception e) {
	    this.textAreaOut.setText("ERROR "+e.getMessage()+e);
	  } finally {
	    setChoicesEnabled(true);
	  }
	}
	
	 public void initChannels() {
	  try {
	    String systemname = (String)this.chSystems.getSelectedItem();
	    String subsystemname = (String)this.chSubsystems.getSelectedItem();
	    String itemname = (String)this.chItems.getSelectedItem();
	    RItem item = caldb.getItem(systemname,subsystemname,itemname);
        String[] s = item.getAsArray();
        setChoicesEnabled(false);
       ((DefaultComboBoxModelHack)this.chChannels.getModel()).setObjects(s);
       this.chChannels.repaint();
      //  this.chChannels.removeAllItems();
      //  for(int i=0;i<s.length;i++) this.chChannels.addItem(s[i]);
	  } catch (Exception e) {
	    this.textAreaOut.setText("ERROR "+e.getMessage()+e);
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
	      String systemname = (String)this.chSystems.getSelectedItem();
	      String  subsystemname = (String)this.chSubsystems.getSelectedItem();
	      String itemname = (String)this.chItems.getSelectedItem();
	      String RunIndexTable = this.textRunIndexTable.getText();
	      String date = this.textDate.getText();
	      int runno = Integer.parseInt(this.textRunno.getText());
	      RConstantSet set = caldb.getConstants(systemname,subsystemname,
	                                       itemname,RunIndexTable,date,runno);
	      this.textAreaOut.setText(set.toString());
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
	      this.textAreaOut.setText("ERROR "+e.getMessage()+e);
	    }
	}
	
	public void diffGo() {
	    try {
	      String systemname =  (String)this.chSystems.getSelectedItem();
	      String  subsystemname =  (String)this.chSubsystems.getSelectedItem();
	      String itemname =  (String)this.chItems.getSelectedItem();
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

	      this.textAreaOut.setText(v);

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
	      this.textAreaOut.setText("ERROR "+e.getMessage()+e);
	    }
	}
	
	
    public void channelGo() {
	    try {
	      String systemname = (String)this.chSystems.getSelectedItem();
	      String  subsystemname = (String)this.chSubsystems.getSelectedItem();
	      String itemname = (String)this.chItems.getSelectedItem();
	      String RunIndexTable = this.textRunIndexTable.getText();
	      String date = this.textDate.getText();
	      int runnoMax = Integer.parseInt(this.textTGRunMax.getText());
	      int runnoMin = Integer.parseInt(this.textTGRunMin.getText());
	      int j = this.chChannels.getSelectedIndex();
	      String channel = "v_"+(j<1000 ? "0":"")+(j<100 ? "0":"")+(j<10 ? "0":"")+(j+1);
	      RChannelData chdata = caldb.getChannelData(systemname,subsystemname,
	                                       itemname,RunIndexTable,date,runnoMax,runnoMin,channel);
	      this.textAreaOut.setText(chdata.toString()); //unique_on));
	      // make a histo view
	      HistoView view = new HistoView();
	      JASHist plot = view.getPlot();
	      double[] data2 = chdata.getAsDoubleArray();
	      int[] data3 = chdata.getRunNoArray();       //AsString();
	      double[] data4= getAsDoubleUniqArray(data3,data2);
                  plot.setAllowUserInteraction(false);
                  plot.setTitle("Time dependence ");
                  plot.setDataAreaBorderType(plot.SHADOW);
                  plot.getYAxis().setLabel(channel);
                  plot.getXAxis().setLabel("run# (from "+data3[0]+"  to "+data3[data3.length-1]+")");
		  //          plot.addData(new ArrayDataSource(data4));//.show(true);
      		  //	          plot.addData(new StringDataSource(data2,data3));//.show(true);
		  //	       JASHistData data=plot.addData(new StringDataSource(data2,data3));
	       JASHistData data=plot.addData(new ArrayDataSource(data4));
              ((JASHist1DHistogramStyle) data.getStyle()).setShowErrorBars(false);
              ((JASHist1DHistogramStyle) data.getStyle()).setHistogramBarColor(Color.gray);
	      ((JASHist1DHistogramStyle) data.getStyle()).setHistogramFill(false);
               data.show(true);
	      // show it
	      view.setVisible(true);
	    } catch (Exception e) {
	      this.textAreaOut.setText("ERROR "+e.getMessage()+e+"\n");
	    }
	}
		

	public void destroy () {
	    this.caldb.shutdown();
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
			else if (object == checkTG)
				checkTG_ActionPerformed(event);
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

	void checkTG_ActionPerformed(java.awt.event.ActionEvent event)
	{
                       if( unique_on ==0 ) unique_on=1;
                       else unique_on=0;
	}

	void buttonExit_ActionPerformed(java.awt.event.ActionEvent event)
	{
                       System.exit(0); 
	}

        double[] getAsDoubleUniqArray( int[] list,  double[] values) {
	    int j=0;
            if(unique_on==0) return values;
	    for (int i=0;i< list.length-1 ;i++) if(list[i]==list[i+1]) j++;
            double[] datas =  new double[list.length-j];
            j=0;
            datas[0]=values[0];
            for (int i=1;i< list.length;i++ ){
		if(list[i]!=list[i-1]) {
                    j++;
                 }
		    datas[j]=values[i];
	    }
             return datas;
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
		 caldburl="jdbc:mysql://"+host+dbase;
		JFrame f = new JFrame();
		JCaldbview caldbGUI = new JCaldbview();
		JCaldbview.instance = caldbGUI;
		caldbGUI.init();
		f.setSize(500,600);
		f.setTitle("Caldb Viewer      <"+host+dbase+">");
	    f.getContentPane().setLayout(new BorderLayout(0,0));
		f.getContentPane().add("Center", caldbGUI);
		f.setVisible(true);
	    SymWindowF aSymWindow = new SymWindowF();
		f.addWindowListener(aSymWindow);
	}
	
	public static JCaldbview instance;
	
}

   class SymWindowF extends java.awt.event.WindowAdapter
	{
		public void windowClosing(java.awt.event.WindowEvent event)
		{
			Object object = event.getSource();
	        if(object instanceof JFrame) {
	            JCaldbview.instance.destroy();
	            ((JFrame)object).dispose();
	            System.exit(0);
	        }
		}
	}
