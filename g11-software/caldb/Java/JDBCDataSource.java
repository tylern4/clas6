//********************************************************/
//* H.Avagyan
//* Date  : 2001-03-03
//********************************************************/
import java.util.*;
import java.sql.*;

/****************************** 
* establish connection with the SQL server 
* set up url,user,password,host... 
*******************************/
public class JDBCDataSource {
  
  String url;
  String username="";
  String password="";
  String drivername;
  Connection connection;
  
  public Connection getConnection() throws Exception {
    try {
     if(connection!=null && !connection.isClosed())
     return connection;
    } catch (Exception e) {}
     connect();
     return connection;
  }
  
  public Statement getStatement() throws Exception {
    return getConnection().createStatement();
  }
  
  public void connect() throws SQLException {
       try {
	   Class.forName(drivername).newInstance();
       } catch (Exception e) {throw new SQLException(e.getMessage());}
       connection = DriverManager.getConnection(getURL(),getUsername(),getPassword());
  }
  
  public void setDriver(String s) {
    drivername=s;
  }

  public String getDriver() {
    return drivername;
  }

  public void setURL(String s) {
    url = s;
  }

  public String getURL() {
    return url;
  }
  
  public void setUsername(String s) {
    username = s;
  }

  public String getUsername() {
    return username;
  }
  
  public void setPassword(String s) {
    password = s;
  }

  public String getPassword() {
    return password;
  }

  public void close() {
   try {
    if(connection!=null) connection.close();
   } catch (Exception e) {}
  }
}
