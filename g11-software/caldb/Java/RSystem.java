//********************************************************/
//* H.Avagyan
//* Date  : 2001-03-03
//********************************************************/
import java.util.*;
import java.sql.*;

/********************************************************
* store/retrieve system info
********************************************************/    
public class RSystem {
    
    
    private static String loadsystems = "SELECT systemId,systemName,description FROM System";
    
    public static String getQuery() {
      return loadsystems;
    }
    
    private int SystemID;
    private String SystemNAME;
    private String DESCRIPTION;
    private Vector subsystems = new Vector();
    
    public RSystem(ResultSet rs) throws SQLException {
        this.SystemID = rs.getInt("systemId");
        this.SystemNAME = rs.getString("systemName");
        this.DESCRIPTION = rs.getString("description");
    }
    
    public String getName() {
      return this.SystemNAME;
    }
    
    public void loadSybsystems(Statement s) throws Exception {

       ResultSet rs = null;
       try { 
         s.executeQuery(RSubSystem.getQuery(this.SystemID));
         rs = s.getResultSet();
         while(rs.next()) {
           RSubSystem subsystem = new RSubSystem(rs);
           subsystem.setParent(this);
           subsystems.addElement(subsystem);
         }
       } catch (Exception e) {
         throw e;
       } finally {
         if(rs!=null) try {rs.close();} catch (Exception e) {}
       }
       
       for (Enumeration e = subsystems.elements() ; e.hasMoreElements() ;) {
         RSubSystem subsystem = (RSubSystem)e.nextElement(); 
         subsystem.loadItems(s);
       }
       
    }
    
    public RSubSystem getSubSystem4Name(String s) throws Exception {
        for (Enumeration e = subsystems.elements() ; e.hasMoreElements() ;) {
         RSubSystem subsystem = (RSubSystem)e.nextElement(); 
         if(subsystem.getName().equals(s)) return subsystem;
       }
      throw new Exception("Subsystem "+s+" not found.");
    }
    
    public String[] getAsArray() {
        String[] ss = new String[subsystems.size()];
        int i=0;
        for (Enumeration e = subsystems.elements() ; e.hasMoreElements() ;i++) {
          RSubSystem subsystem = (RSubSystem)e.nextElement(); 
          ss[i] = subsystem.getName();
        }
        return ss;
    }
    
    
    public void printout() {
       System.out.println(SystemNAME); 
       for (Enumeration e = subsystems.elements() ; e.hasMoreElements() ;) {
         RSubSystem subsystem = (RSubSystem)e.nextElement(); 
         subsystem.printout();
       }
      
 
    }
    
}
