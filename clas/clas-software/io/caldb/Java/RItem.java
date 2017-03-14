//********************************************************/
//* H.Avagyan
//* Date  : 2001-03-03
//********************************************************/
import java.util.*;
import java.sql.*;
/******************************
* Class  with Item related info for CALDB. 
* sets/returns complete list of properties of item for a parent 
*subsystem and system 
*******************************/
public class RItem {
    
  private static String loaditems = "SELECT itemId,itemName,length,type,description FROM Item WHERE subsystemId = ";

  public static String getQuery(int subsystemid) {
      return loaditems+subsystemid;
  }
    
    
  private int ITEMID;
  private String ITEMNAME;
  private int LENGTH;
  private String TYPE;
  private String DESCRIPTION;

  
  public RItem(ResultSet rs) throws SQLException {
        this.ITEMID = rs.getInt("itemId");
        this.ITEMNAME = rs.getString("itemName");
        this.LENGTH = rs.getInt("length");
        this.TYPE = rs.getString("type");
        this.DESCRIPTION = rs.getString("description");
  }
  
    private RSubSystem parent;
    
    public void setParent(RSubSystem p) {
        parent = p;
    }
    
    public RSubSystem getParent() {
        return parent;
    }
  
/****************************** 
* return the ITEMNAME
*******************************/
    public String getName() {
      return ITEMNAME;
    }
  
/****************************** 
* print  ITEMNAME
*******************************/
    public void printout() {
      System.out.println(ITEMNAME);
    }
  
  
    // do some data mapping
/****************************** 
* returns item type (checks  float and integer
* all other types are assigned DOUBLE
*******************************/
    public int getType() {
      if(this.TYPE.equalsIgnoreCase("float")) return Types.FLOAT;
      else if(this.TYPE.equalsIgnoreCase("integer")) return Types.INTEGER;
      else
        return Types.DOUBLE;
    }
    
/****************************** 
* returns length of item 
*******************************/
    public int getLength() {
      return LENGTH;
    }
    
/****************************** 
* returns id of item 
*******************************/
    public int getID() {
      return ITEMID;
    }
    
/****************************** 
* returns the table_name for system_subsystem_item
*******************************/
    public String tableName4Constants() {
      return parent.getParent().getName()+"_"+parent.getName()+"_"+ITEMNAME;
    }
  
/****************************** 
* returns the set of constants from valueID table with 
* runno and date from RunIndex table
*******************************/
    public RConstantSet getConstants(String RunIndexTable, String date, int runno,Statement s) throws Exception {
       RConstantSet set = new RConstantSet(this);
       set.init(RunIndexTable, date, runno, s);
       return set;
    }

/****************************** 
* returns  constant value for a channel  with
* runno and date from RunIndex table
*******************************/
    public RChannelData getChannelData(String RunIndexTable, String date, int runnoMax,int runnoMin, String channel,Statement s) throws Exception {
       RChannelData chdata = new RChannelData(this,channel);
       chdata.init(RunIndexTable,date,runnoMax,runnoMin, s);
       return chdata;
    }
    
    
/****************************** 
* returns  constants for all channels a a string 
* for runno and date from RunIndex table
*******************************/
    public String[] getAsArray() {
        String[] ss = new String[LENGTH];
        for (int i=0;i<ss.length;i++) {
          ss[i] = "Channel "+(i+1);
        }
        return ss;
    }
}
