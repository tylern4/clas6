//********************************************************/
//* H.Avagyan
//* Date  : 2001-03-03
//********************************************************/
import java.util.*;
import java.sql.*;

/********************************************************
* return the constant set for system_subsystem_item
* any set not float or integer is treated as double
********************************************************/    
public class RConstantSet {
    
    private RItem parent;
    private int ITEMVALUEID; 
    private String AUTHOR;    
    private Timestamp TIME; 
    private long MINRUNSOURCE; 
    private long MAXRUNSOURCE; 
    private String COMMENT;
    private Object constants; 
    private int type;
    
    public RConstantSet(RItem item) {
        this.parent = item;
        type = item.getType();
        if(type == Types.FLOAT) type = Types.DOUBLE;
        // create the right structure
        if(type==Types.FLOAT) {
            float[] f = new float[item.getLength()];
            constants = f;
        } else if(type==Types.INTEGER) {
            int[] i = new int[item.getLength()];
            constants = i;
        } else {
            double[] d = new double[item.getLength()];
            constants = d;
        }
    }
    
    public int getType() {
      return type;
    }
    
    public void init(String RunIndexTable, String date, int runno,Statement s) throws Exception {
        String query = constructQuery(RunIndexTable,date,runno);
        ResultSet rs = null;
        try {
          rs = s.executeQuery(query);
          if(rs.next()) {
            
            ITEMVALUEID  = rs.getInt("itemValueId");
            AUTHOR       = rs.getString("author");
            TIME         = rs.getTimestamp("time");
            MINRUNSOURCE = rs.getLong("minRunSource");
            MAXRUNSOURCE = rs.getLong("maxRunSource");
            COMMENT       = rs.getString("comment");
            if(type==Types.FLOAT) {
               readFloats(rs);
            } else if(type==Types.INTEGER) {
                readInts(rs);
            } else {
                readDoubles(rs);
            }
          }
        } catch (Exception e) {
          throw e;
        } finally {
          if(rs!=null) try {rs.close();} catch (Exception e) {}
        }
    }
    
    public void readFloats(ResultSet rs) throws SQLException {
        float[] ff = (float[])constants;
        String v = "";
        for(int j=1; j<=ff.length; j++) {
                         v="v_"+(j<1000 ? "0":"")+(j<100 ? "0":"")+(j<10 ? "0":"")+j;
          ff[j-1] = rs.getFloat(v);
        }
    }
    
    public void readInts(ResultSet rs) throws SQLException {
        int[] ii = (int[])constants;
        String v = "";
        for(int j=1; j<=ii.length; j++) {
                         v="v_"+(j<1000 ? "0":"")+(j<100 ? "0":"")+(j<10 ? "0":"")+j;
          ii[j-1] = rs.getInt(v);
        }
    }
    
    public void readDoubles(ResultSet rs) throws SQLException {
        double[] dd = (double[])constants;
        String v = "";
        for(int j=1; j<=dd.length; j++) {
                         v="v_"+(j<1000 ? "0":"")+(j<100 ? "0":"")+(j<10 ? "0":"")+j;
          dd[j-1] = rs.getDouble(v);
        }
    }
    
    private String constructQuery(String RunIndexTable, String date, int runno) {
         int itemid = parent.getID();
         String tablename = parent.tableName4Constants();
         String tableList = RunIndexTable+","+tablename;
         String fieldList = tablename+".*";
         String condList = RunIndexTable+".itemId = "+itemid+
	                       " AND minRun <= "+runno+" AND maxRun >="+runno+
	                       " AND "+tablename+".time <= '"+date+"' AND "+RunIndexTable+".itemValueId="+tablename+".itemValueid"+
	                       " ORDER BY "+RunIndexTable+".time DESC LIMIT 1";
         String query ="SELECT "+ fieldList+" FROM "+tableList+" WHERE "+condList;
	 //	 System.out.println("RConstantSet constructQuery: "+query);          
         return query;
    }
    
    
    public float[] getAsFloatArray() {
        return (float[])constants;
    }
    
    public int[] getAsIntArray() {
        return (int[])constants;
    }
    
    public double[] getAsDoubleArray() {
        return (double[])constants;
    }
    
    public RItem getParent() {
      return this.parent;
    }
    
/********************************************************
* return the constant set as a string.
* used in printout of constants in the viewer.
********************************************************/    
    public String toString() {
      String s = "System "+parent.getParent().getParent().getName()+"\n";
      s+= "Subsystem "+parent.getParent().getName()+"\n";
      s+= "Item "+parent.getName()+"\n";
            if(type==Types.FLOAT) { // only float version ;)
                float[] f = getAsFloatArray();
                for(int i=0;i<f.length;i++) {
                 s+= "v_"+(i<1000 ? "0":"")+(i<100 ? "0":"")+(i<10 ? "0":"")+(i+1)+"="+f[i]+"\n";
		}
            } else if(type==Types.INTEGER) {
                int[] f = getAsIntArray();
                for(int i=0;i<f.length;i++) {
                 s+= "v_"+(i<1000 ? "0":"")+(i<100 ? "0":"")+(i<10 ? "0":"")+(i+1)+"="+f[i]+"\n";
		}
            } else {
                double[] f = getAsDoubleArray();
                for(int i=0;i<f.length;i++) {
                 s+= "v_"+(i<1000 ? "0":"")+(i<100 ? "0":"")+(i<10 ? "0":"")+(i+1)+"="+f[i]+"\n";
		}
            } 
      return s;
    }
    
}
