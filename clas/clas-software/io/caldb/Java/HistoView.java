//********************************************************/
//* H.Avagyan
//* Date  : 2001-03-05
//********************************************************/

import java.awt.*;
import javax.swing.*;
import jas.hist.*;
import jas.plot.PrintHelper;
//import jas.hist.test.MemoryDataSource;
import jas.plot.*;

public class HistoView extends javax.swing.JFrame
{
	public HistoView()
	{
		getContentPane().setLayout(new BorderLayout(0,0));
		setSize(640,493);
		setVisible(false);
		panelHisto.setLayout(new BorderLayout(0,0));
		getContentPane().add(BorderLayout.CENTER,panelHisto);
		panelTop.setToolTipText("Right click on the plot for more options");
		panelTop.setLayout(new FlowLayout(FlowLayout.CENTER,5,5));
		getContentPane().add(BorderLayout.NORTH, panelTop);
		labelHeader.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
		labelHeader.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
		labelHeader.setText("  ");
		panelTop.add(labelHeader);
		panelBottom.setToolTipText("Right click on the plot for more options");
		panelBottom.setLayout(new FlowLayout(FlowLayout.CENTER,5,5));
		getContentPane().add(BorderLayout.SOUTH, panelBottom);
		buttonPrint.setText("Print");
		buttonPrint.setActionCommand("jbutton");
		panelBottom.add(buttonPrint);
		buttonClose.setText("Close");
		buttonClose.setActionCommand("jbutton");
		panelBottom.add(buttonClose);

        panelHisto.add(BorderLayout.CENTER,plot);
       
	
		//REGISTER_LISTENERS
		SymAction lSymAction = new SymAction();
		buttonClose.addActionListener(lSymAction);
		buttonPrint.addActionListener(lSymAction);
	}

    private JASHist  plot = new JASHist();
    
    public JASHist getPlot() {
        return plot;
    }
    

	public HistoView(String sTitle)
	{
		this();
		setTitle(sTitle);
	}

	public void setVisible(boolean b)
	{
		if (b)
			setLocation(50, 50);
		super.setVisible(b);
	}

	static public void main(String args[])
	{
		(new HistoView()).setVisible(true);
	}

	public void addNotify()
	{
		// Record the size of the window prior to calling parents addNotify.
		Dimension size = getSize();

		super.addNotify();

		if (frameSizeAdjusted)
			return;
		frameSizeAdjusted = true;

		// Adjust size of frame according to the insets and menu bar
		Insets insets = getInsets();
		javax.swing.JMenuBar menuBar = getRootPane().getJMenuBar();
		int menuBarHeight = 0;
		if (menuBar != null)
			menuBarHeight = menuBar.getPreferredSize().height;
		setSize(insets.left + insets.right + size.width, insets.top + insets.bottom + size.height + menuBarHeight);
	}

	// Used by addNotify
	boolean frameSizeAdjusted = false;

	//CONTROLS
	javax.swing.JPanel panelHisto = new javax.swing.JPanel();
	javax.swing.JPanel panelTop = new javax.swing.JPanel();
	javax.swing.JLabel labelHeader = new javax.swing.JLabel();
	javax.swing.JPanel panelBottom = new javax.swing.JPanel();
	javax.swing.JButton buttonPrint = new javax.swing.JButton();
	javax.swing.JButton buttonClose = new javax.swing.JButton();


    public void setHeaderText(String s) {
        this.labelHeader.setText(s);
    }

	class SymAction implements java.awt.event.ActionListener
	{
		public void actionPerformed(java.awt.event.ActionEvent event)
		{
			Object object = event.getSource();
			if (object == buttonClose)
				buttonClose_actionPerformed(event);
			else if (object == buttonPrint)
				buttonPrint_actionPerformed(event);
		}
	}

	void buttonClose_actionPerformed(java.awt.event.ActionEvent event)
	{
		try {
			this.dispose();
		} catch (java.lang.Exception e) {
		}
	}
	void buttonPrint_actionPerformed(java.awt.event.ActionEvent event)
	{
		try {
                       PrintHelper ph   = PrintHelper.instance();
                       ph.printTarget(this);
		} catch (java.lang.Exception e) {
                       e.printStackTrace();
		}
	}
}
