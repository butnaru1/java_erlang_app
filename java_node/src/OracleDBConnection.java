import java.sql.*;
public class OracleDBConnection {
    Connection connection;

    public OracleDBConnection() {
        try {
            Class.forName("oracle.jdbc.OracleDriver");
            String dbURL = "jdbc:oracle:thin:@localhost:1521:orcl";
            String username = "erlang_user";
            String password = "admin";
            connection = DriverManager.getConnection(dbURL, username, password);
        }catch (SQLException | ClassNotFoundException throwables) {
            throwables.printStackTrace();
        }
    }

    public byte[] get(byte[] key) throws SQLException {
        String sql = "SELECT VALUE FROM CACHE WHERE KEY = ?";
        PreparedStatement statement = connection.prepareStatement(sql);
        statement.setBytes(1, key);
        ResultSet result = statement.executeQuery();
        return result.getBytes("value");
    }

    public void put(byte[] key, byte[] value) throws SQLException {
        String sql = "INSERT INTO CACHE (KEY, VALUE ) VALUES (?, ?)";
        PreparedStatement statement = connection.prepareStatement(sql);
        statement.setBytes(1, key);
        statement.setBytes(2, value);
        statement.executeQuery();
    }

    public void delete(byte[] key) throws SQLException {
        String sql = "DELETE FROM CACHE WHERE KEY=?";
        PreparedStatement statement = connection.prepareStatement(sql);
        statement.setBytes(1, key);
        statement.executeQuery();
    }
}
