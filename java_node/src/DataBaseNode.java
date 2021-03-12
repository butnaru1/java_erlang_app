import com.ericsson.otp.erlang.*;

import java.io.IOException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class DataBaseNode {
    private OracleDBConnection connection;
    private ExecutorService executorService;
    private OtpNode node;
    private OtpMbox otpMbox;

    public DataBaseNode(String nodeName, String cookie) {
        super();
        try {
            connection = new OracleDBConnection();
            executorService = Executors.newFixedThreadPool(4);
            node = new OtpNode(nodeName, cookie);
            otpMbox = node.createMbox("db_server");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private void process() {
        while (true) {
            try {
                OtpErlangObject msg = otpMbox.receive();
                OtpErlangTuple t = (OtpErlangTuple) msg;
                String action = ((OtpErlangAtom) t.elementAt(0)).atomValue();
                OtpErlangPid from = (OtpErlangPid) t.elementAt(1);
                OtpErlangRef ref = (OtpErlangRef) t.elementAt(2);
                byte[] key = ((OtpErlangBinary) t.elementAt(3)).binaryValue();
                byte[] value;
                DataBaseTask task = null;
                if (t.arity() == 5 && action.equals("put")) {
                    value = ((OtpErlangBinary) t.elementAt(4)).binaryValue();
                    task = new DataBaseTask(otpMbox, connection, from, ref, action, key, value);
                } else if (t.arity() == 4 && action.equals("get")) {
                    task = new DataBaseTask(otpMbox, connection, from, ref, action, key, null);
                } else if (t.arity() == 4 && action.equals("delete")) {
                    task = new DataBaseTask(otpMbox, connection, from, ref, action, key, null);
                } else {
                    System.out.println("invalid request: " + t);
                    continue;
                }
                executorService.submit(task);
            } catch (Exception e) {
                System.out.println("caught error: " + e);
            }
        }
    }

    public static void main(String[] args) {
        if (args.length != 2) {
            System.out.println("wrong number of arguments");
            System.out.println("expected: nodeName, cookie");
        }
        DataBaseNode dataBaseNode = new DataBaseNode(args[0], args[1]);
        dataBaseNode.process();
    }
}
