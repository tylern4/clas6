//********************************************************/
//* H.Avagyan
//* Date  : 2001-03-03
//********************************************************/
import java.util.*;
import java.sql.*;

/********************************************************
* a wrapper for all routines
********************************************************/    

public class RSystemsCaldb {


    private Vector systems = new Vector();
    
    
    private JDBCDataSource datasource = new JDBCDataSource();
    
     public void setDriver(String s) {
       datasource.setDriver(s);
     }
     
     public void setURL(String s) {
       datasource.setURL(s);
     }
    
     public void setUsername(String s) {
       datasource.setUsername(s);
     }  
  
     public void setPassword(String s) {
       datasource.setPassword(s);
     }
    
    public void loadSystems() throws Exception {
        Statement s = null;
       try {
         s = datasource.getStatement();
         loadSystems(s);
       } catch (Exception e) {
         throw e;
       } finally {
         if(s!=null) try {s.close();} catch (Exception e) {}
       }
    }
    
    public void loadSystems(Statement s) throws Exception {
       
       ResultSet rs = null;
       try { 
         s.executeQuery(RSystem.getQuery());
         rs = s.getResultSet();
         while(rs.next()) {
           RSystem system = new RSystem(rs);
           systems.addElement(system);
         }
       } catch (Exception e) {
         throw e;
       } finally {
         if(rs!=null) try {rs.close();} catch (Exception e) {}
       }
       
       for (Enumeration e = systems.elements() ; e.hasMoreElements() ;) {
         RSystem system = (RSystem)e.nextElement(); 
         system.loadSybsystems(s);
       }
       
    }
    
    public RSystem getSystem4Name(String s) throws Exception {
        for (Enumeration e = systems.elements() ; e.hasMoreElements() ;) {
         RSystem system = (RSystem)e.nextElement(); 
         if(system.getName().equals(s)) return system;
       }
      throw new Exception("System "+s+" not found.");
    }
    
    
    public RConstantSet getConstants(String systemname,String  subsystemname,String itemname,
                                     String RunIndexTable, String date, int runno) throws Exception {
       Statement s = null;
       RConstantSet set = null;
       
       try {
         s = datasource.getStatement();
         RItem item = getSystem4Name(systemname).getSubSystem4Name(subsystemname).getItem4Name(itemname);
         set = item.getConstants(RunIndexTable,date,runno,s);
         return set;
       } catch (Exception e) {
         throw e;
       } finally {
         if(s!=null) try {s.close();} catch (Exception e) {}
       }                                 
    }
  
    public RChannelData getChannelData(String systemname,String  subsystemname,String itemname,
                                     String RunIndexTable, String date, int runnoMax,int runnoMin, String channel) throws Exception {
       Statement s = null;
       RChannelData chdata = null;
       try {
         s = datasource.getStatement();
         RItem item = getSystem4Name(systemname).getSubSystem4Name(subsystemname).getItem4Name(itemname);
         chdata = item.getChannelData(RunIndexTable,date,runnoMax,runnoMin,channel,s);
         return chdata;
       } catch (Exception e) {
         throw e;
       } finally {
         if(s!=null) try {s.close();} catch (Exception e) {}
       }                                 
    }
    
    public RItem getItem(String systemname,String  subsystemname,String itemname) throws Exception {
         RItem item = getSystem4Name(systemname).getSubSystem4Name(subsystemname).getItem4Name(itemname);
         return item;
    }
  
    public void printout() {
      for (Enumeration e = systems.elements() ; e.hasMoreElements() ;) {
        RSystem r = (RSystem)e.nextElement(); 
        r.printout();
      }
    }
    
    public String[] getAsArray() {
        String[] ss = new String[systems.size()];
        int i=0;
        for (Enumeration e = systems.elements() ; e.hasMoreElements() ;i++) {
          RSystem system = (RSystem)e.nextElement(); 
          ss[i] = system.getName();
        }
        return ss;
    }
    
    public String[] getSystems()  throws Exception {
        return getAsArray(); 
    }
    
    public String[] getSybsystems(String system)  throws Exception  {
        return getSystem4Name(system).getAsArray();
    }
    
    public String[] getItems(String system,String subsystem)  throws Exception {
        return getSystem4Name(system).getSubSystem4Name(subsystem).getAsArray();
    }
    
    
    public void shutdown() {
      this.datasource.close();
    }
    
    
}
