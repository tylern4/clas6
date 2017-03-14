//********************************************************/
//* H.Avagyan
//* Date  : 2001-03-07
//********************************************************/
import java.util.*;
import java.sql.*;

/********************************************************
* channel info for parent item
* type float replaced by DOUBLE
* dinamic array emulation implemented
********************************************************/
public class RChannelData {
    
    private RItem parent;
    
    private int type;
    private String channel;
    
    public RChannelData(RItem item, String channel) {
        this.parent = item;
        this.channel = channel;
        type = item.getType();
        if(type == Types.FLOAT) type = Types.DOUBLE;
    }
    
    public int getType() {
      return type;
    }
    
    public String getChannel() {
        return channel;
    }
    
    public void init(String RunIndexTable, String date, int runnoMax,int runnoMin,Statement s)  throws Exception {
        
	  this.elementData = new Object[initialCapacity];
	  String query = constructQuery(RunIndexTable,date,runnoMax,runnoMin);
	  ResultSet results = null;
      try {
         results = s.executeQuery(query);     
	     for(int j=0;results.next();j++) {
	       Object[] o = new Object[2];
	           if(type==Types.FLOAT) {
                  o[0] = new Float(results.getFloat(1));
               } else if(type==Types.INTEGER) {
                  o[0] = new Integer(results.getInt(1));
               } else {
                  o[0] = new Double(results.getDouble(1));
               }
	       o[1] = results.getObject(2);
	       elementCount++;
	       ensureCapacity(elementCount+1);
	       elementData[j]=o;
	     }
	    trimToSize();
      } catch (Exception e) {
          throw e;
      } finally {
          if(results!=null) try {results.close();} catch (Exception e) {}
      }
    }
   
    
    private String constructQuery(String RunIndexTable, String date, int runnoMax,int runnoMin) {
         int itemid = parent.getID();
         String tablename = parent.tableName4Constants();
         String tableList=RunIndexTable+","+tablename;
         String fieldList= tablename+"."+getChannel()+",minRun";
         String condList= RunIndexTable+".itemId = "+itemid+
	                      " AND minRun <= "+runnoMax+" AND minRun >="+runnoMin+
	                      " AND "+tablename+".time <= \" "+date+"\" AND "+RunIndexTable+".itemValueId="+tablename+".itemValueid"+
	                      " ORDER BY "+RunIndexTable+".minRun ";
         String query ="SELECT "+ fieldList+" FROM "+tableList+" WHERE "+condList;
         return query;
    }
    
    public int[] getRunNoArray() {
        int[] ret = new int[getCount()];
        for (int i=0;i<ret.length;i++) {
          ret[i] = ((Integer)getElement(i)[1]).intValue();
        }
        return ret;
    } 
    
    public String[] getRunNoArrayAsString() {
        String[] ret = new String[getCount()];
        for (int i=0;i<ret.length;i++) {
          ret[i] = ""+((Integer)getElement(i)[1]).intValue();
        }
        return ret;
    } 
    
    
    public float[] getAsFloatArray() {
        float[] ret = new float[getCount()];
        for (int i=0;i<ret.length;i++) {
          ret[i] = ((Float)getElement(i)[0]).floatValue();
        }
        return ret;
    }
    
    public int[] getAsIntArray() {
        int[] ret = new int[getCount()];
        for (int i=0;i<ret.length;i++) {
          ret[i] = ((Integer)getElement(i)[0]).intValue();
        }
        return ret;
    }
    
    public double[] getAsDoubleArray() {
        double[] ret = new double[getCount()];
        for (int i=0;i<ret.length;i++) {
          ret[i] = ((Double)getElement(i)[0]).doubleValue();
        }
        return ret;
    }
    
    // dynamic array support section
    
     protected int elementCount=0;
     protected int initialCapacity = 100;
     protected int capacityIncrement = 10;
     protected Object elementData[];
    
    
    /**
     * Trims the capacity of the array to be the arrays current
     * size. An application can use this operation to minimize the
     * storage.
     *
     **/

    public final void trimToSize() {
	int oldCapacity = elementData.length;
	if (elementCount < oldCapacity) {
	    Object oldData[] = elementData;
	    elementData = new Object[elementCount];
	    System.arraycopy(oldData, 0, elementData, 0, elementCount);
	}
    }

    /**
     * Increases the capacity of array, if necessary, to ensure
     * that it can hold at least the number of components specified by
     * the minimum capacity argument.
     *
     * @param   minCapacity   the desired minimum capacity.
     */

    public final void ensureCapacity(int minCapacity) {
	int oldCapacity = elementData.length;
	if (minCapacity > oldCapacity) {
	    Object oldData[] = elementData;
	    int newCapacity = (capacityIncrement > 0) ?
		(oldCapacity + capacityIncrement) : (oldCapacity * 2);
    	    if (newCapacity < minCapacity) {
		newCapacity = minCapacity;
	    }
	    elementData = new Object[newCapacity];
	    System.arraycopy(oldData, 0, elementData, 0, elementCount);
	}
	}

    public Object[] getElement(int i) {
        return (Object[])elementData[i];
    }

    public int getCount() { return elementData.length;}

/********************************************************
* return the time graph info as a string.
* used in printout of constants in the viewer.
********************************************************/    
    public String toString() {
      String s = "System "+parent.getParent().getParent().getName()+"\n";
      s+= "Subsystem "+parent.getParent().getName()+"\n";
      s+= "Item "+parent.getName()+"\n";
      s+= "Channel "+this.getChannel()+"\n";
     
      for(int i=0;i<elementData.length;i++){
          s+= i+ "  Run "+getElement(i)[1]+" "+getElement(i)[0]+"\n";
	  }
      return s;
    }
}
