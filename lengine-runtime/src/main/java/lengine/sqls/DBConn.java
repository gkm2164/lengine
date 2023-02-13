package lengine.sqls;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.LinkedList;
import java.util.List;
import java.util.Properties;

import lengine.functions.LengineLambda2;
import lengine.runtime.CreateIterator;
import lengine.runtime.LengineIterator;
import lengine.util.LengineMap;
import lengine.util.LengineMapKey;
import lengine.util.LengineSequence;

public class DBConn {
  private static boolean initOnce = false;
  private final Connection conn;

  public DBConn(Connection conn) {
    this.conn = conn;
  }

  void setPreparedStatementsParams(PreparedStatement ps, CreateIterator args) throws SQLException {
    LengineIterator it = args.iterator();

    int i = 1;
    while (it.hasNext()) {
      Object arg = it.next();
      if (arg instanceof Character) {
        ps.setString(i, arg.toString());
      } else if (arg instanceof String) {
        ps.setString(i, arg.toString());
      } else if (arg instanceof Long) {
        ps.setLong(i, (Long) arg);
      } else if (arg instanceof Double) {
        ps.setDouble(i, (Double) arg);
      } else if (arg instanceof Boolean) {
        ps.setBoolean(i, (Boolean) arg);
      } else {
        throw new RuntimeException("Unknown type for " + i + "'th element");
      }
      i++;
    }
  }

  public LengineSequence execSelect(String query, CreateIterator args) throws SQLException {
    List<Object> results = new LinkedList<>();
    try (PreparedStatement ps = conn.prepareStatement(query)) {
      setPreparedStatementsParams(ps, args);
      ResultSet rs = ps.executeQuery();

      ResultSetMetaData rsMetadata = rs.getMetaData();
      while (rs.next()) {
        LengineMap.Builder resultBuilder = LengineMap.builder();

        for (int i = 1; i <= rsMetadata.getColumnCount(); i++) {
          String keyName = rsMetadata.getColumnName(i);
          Object value = rs.getObject(i);

          resultBuilder.put(key(keyName), value);
        }

        results.add(resultBuilder.build());
      }
    }

    return LengineSequence.create(results);
  }

  public Long execUpdate(String query, CreateIterator args) throws SQLException {
    try (PreparedStatement ps = conn.prepareStatement(query)) {
      setPreparedStatementsParams(ps, args);
      return (long) ps.executeUpdate();
    }
  }

  public static LengineMap connect(
      String connString,
      String username,
      String password
  ) {
    if (!initOnce) {
      // Load driver
      try {
        Class.forName("com.mysql.cj.jdbc.Driver");
      } catch (ClassNotFoundException e) {
        throw new RuntimeException(e);
      }
    }

    Properties props = new Properties();
    props.put("user", username);
    props.put("password", password);
    try {
      Connection conn = DriverManager.getConnection(
          connString,
          props
      );
      DBConn thisConn = new DBConn(
          conn
      );

      LengineLambda2<LengineSequence, String, CreateIterator> EXEC_SELECT = (query, args) -> {
        try {
          return thisConn.execSelect(query, args);
        } catch (SQLException e) {
          throw new RuntimeException(e);
        }
      };

      LengineLambda2<Long, String, CreateIterator> EXEC_UPDATE = (query, args) -> {
        try {
          return thisConn.execUpdate(query, args);
        } catch (SQLException e) {
          throw new RuntimeException(e);
        }
      };

      return LengineMap.builder()
          .put(key("exec-select"), EXEC_SELECT)
          .put(key("exec-update"), EXEC_UPDATE)
          .build();
    } catch (SQLException e) {
      throw new RuntimeException(e);
    }
  }

  private static LengineMapKey key(String keyName) {
    return LengineMapKey.create(keyName);
  }
}
