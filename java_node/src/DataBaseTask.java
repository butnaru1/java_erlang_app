import com.ericsson.otp.erlang.*;

import java.sql.Connection;
import java.sql.SQLException;

public class DataBaseTask implements Runnable {

    private OtpMbox mbox;
    private OracleDBConnection connection;
    private OtpErlangPid from;
    private OtpErlangRef ref;
    private String action;
    private byte[] key;
    private byte[] value;

    public DataBaseTask(OtpMbox mbox, OracleDBConnection connection, OtpErlangPid from, OtpErlangRef ref, String action, byte[] key, byte[] value) {
        this.mbox = mbox;
        this.connection = connection;
        this.from = from;
        this.ref = ref;
        this.action = action;
        this.key = key;
        this.value = value;
    }

    @Override
    public void run() {
        try {
            if (action.equals("get")) {
                doGet();
            } else if (action.equals("put")) {
                doPut();
            } else if (action.equals("delete")) {
                doDelete();
            } else {
                System.out.println("invalid action: " + action);
            }
        } catch (Exception e) {
            System.out.println("Exception" + e);
        }
    }

    private void doDelete() throws SQLException {
        connection.delete(key);
        OtpErlangTuple reply = new OtpErlangTuple(new OtpErlangObject[] {
                new OtpErlangAtom("reply"), ref,
                new OtpErlangAtom("ok")
        });
        mbox.send(from, reply);
    }

    private void doPut() throws SQLException {
        connection.put(key, value);
        OtpErlangTuple reply = new OtpErlangTuple(new OtpErlangObject[] {
                new OtpErlangAtom("reply"), ref,
                new OtpErlangAtom("ok")
        });
        mbox.send(from, reply);
    }

    private void doGet() {
        OtpErlangObject result;
        try {
            result = new OtpErlangBinary(connection.get(key));
        } catch (SQLException throwables) {
            result = new OtpErlangAtom("not_found");
        }
        OtpErlangTuple reply = new OtpErlangTuple(new OtpErlangObject[]{new OtpErlangAtom("reply"), ref, result});
        mbox.send(from, reply);
    }
}
