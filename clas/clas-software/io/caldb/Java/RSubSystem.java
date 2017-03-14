//********************************************************/
//* H.Avagyan
//* Date  : 2001-03-03
//********************************************************/
import java.util.*;
import java.sql.*;

public class RSubSystem {
    
    private static String loadsubsystems = "SELECT subsystemId,subsystemName,description FROM Subsystem WHERE systemId = ";

    public static String getQuery(int systemid) {
      return loadsubsystems+systemid;
    }
       
    private int SUBSYSTEMID;
    private String SUBSYSTEMNAME;
    private String DESCRIPTION;
    private Vector items = new Vector();
    
    
    public RSubSystem(ResultSet rs) throws SQLException {
        this.SUBSYSTEMID = rs.getInt("subsystemId");
        this.SUBSYSTEMNAME = rs.getString("subsystemName");
        this.DESCRIPTION = rs.getString("description");
    }
    
    private RSystem parent;
    
    public void setParent(RSystem p) {
        parent = p;
    }
    
    public RSystem getParent() {
        return parent;
    }
    
    public String getName() {
      return this.SUBSYSTEMNAME;
    }
    
    public void loadItems(Statement s) throws Exception {

       ResultSet rs = null;
       try { 
         s.executeQuery(RItem.getQuery(this.SUBSYSTEMID));
         rs = s.getResultSet();
         while(rs.next()) {
           RItem item = new RItem(rs);
           item.setParent(this);
           items.addElement(item);
         }
       } catch (Exception e) {
         throw e;
       } finally {
         if(rs!=null) try {rs.close();} catch (Exception e) {}
       }
    }
    
    
    public RItem getItem4Name(String s) throws Exception {
        for (Enumeration e = items.elements() ; e.hasMoreElements() ;) {
         RItem item = (RItem)e.nextElement(); 
         if(item.getName().equals(s)) return item;
       }
      throw new Exception("Item "+s+" not found.");
    }
    
    
    public String[] getAsArray() {
        String[] ss = new String[items.size()];
        int i=0;
        for (Enumeration e = items.elements() ; e.hasMoreElements() ;i++) {
          RItem item = (RItem)e.nextElement(); 
          ss[i] = item.getName();
        }
        return ss;
    }
    
     public void printout() {
      System.out.println(SUBSYSTEMNAME);
      for (Enumeration e = items.elements() ; e.hasMoreElements() ;) {
        RItem r = (RItem)e.nextElement(); 
        r.printout();
      }
    }
}
