package org.server;

import com.sun.net.httpserver.HttpServer;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;
import com.google.gson.Gson;

import java.io.IOException;
import java.io.OutputStream;
import java.io.InputStreamReader;
import java.io.BufferedReader;
import java.net.InetSocketAddress;
import java.sql.*;
import java.util.stream.Collectors;
import java.security.MessageDigest;

public class MusicAppServer {
    static class User { String username; String password; }
    private static final Gson gson = new Gson();

    public static void main(String[] args) throws IOException {
        HttpServer server = HttpServer.create(new InetSocketAddress(4567), 0);
        server.createContext("/register", new RegisterHandler());
        server.createContext("/login", new LoginHandler());
        server.setExecutor(null);
        server.start();
        System.out.println("Server started at http://localhost:4567");
    }

    // 加密函数
    public static String sha256(String base) {
        try {
            MessageDigest digest = MessageDigest.getInstance("SHA-256");
            byte[] hash = digest.digest(base.getBytes("UTF-8"));
            StringBuilder hexString = new StringBuilder();
            for (byte b : hash) {
                String hex = Integer.toHexString(0xff & b);
                if (hex.length() == 1) hexString.append('0');
                hexString.append(hex);
            }
            return hexString.toString();
        } catch (Exception ex) {
            throw new RuntimeException(ex);
        }
    }

    //创建数据库连接。
    public static Connection GetConnection(String username, String passwd) {
        String driver = "org.opengauss.Driver";
        String sourceURL = "jdbc:opengauss://192.168.129.135:26000/music_sys";
        Connection conn = null;
        try {
            //加载数据库驱动。
            Class.forName(driver).newInstance();
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }

        try {
            //创建数据库连接。
            conn = DriverManager.getConnection(sourceURL, username, passwd);
            System.out.println("Connection succeed!");
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }

        return conn;
    };

    static class RegisterHandler implements HttpHandler {
        @Override
        public void handle(HttpExchange exchange) throws IOException {
            if (!"POST".equals(exchange.getRequestMethod())) {
                exchange.sendResponseHeaders(405, -1);
                return;
            }
            String body = new BufferedReader(new InputStreamReader(exchange.getRequestBody()))
                    .lines().collect(Collectors.joining("\n"));
            User u = gson.fromJson(body, User.class);
            try (Connection conn = MusicAppServer.GetConnection("myroot", "Lhx050918")) {
                String sql = "INSERT INTO users(username, password, created_at) " +
                        "VALUES(?, ?, now())";
                try (PreparedStatement ps = conn.prepareStatement(sql)) {
                    ps.setString(1, u.username);
                    ps.setString(2, sha256(u.password));
                    ps.executeUpdate();
                }
            } catch (SQLException e) {
                e.printStackTrace();
                sendResponse(exchange, 500, "{\"status\":\"error\"}");
                return;
            }
            sendResponse(exchange, 200, gson.toJson("Registered"));
        }
    }

    static class LoginHandler implements HttpHandler {
        @Override
        public void handle(HttpExchange exchange) throws IOException {
            if (!"POST".equals(exchange.getRequestMethod())) {
                exchange.sendResponseHeaders(405, -1);
                return;
            }
            String body = new BufferedReader(new InputStreamReader(exchange.getRequestBody()))
                    .lines().collect(Collectors.joining("\n"));
            User u = gson.fromJson(body, User.class);
            boolean success = false;
            try (Connection conn = MusicAppServer.GetConnection("myroot", "Lhx050918")) {
                String key = "Abc123!@";
                String sql = "SELECT count(*) FROM users WHERE username = ? AND password = ?";
                try (PreparedStatement ps = conn.prepareStatement(sql)) {
                    ps.setString(1, u.username);
                    ps.setString(2, sha256(u.password));
                    try (ResultSet rs = ps.executeQuery()) {
                        if (rs.next() && rs.getInt(1) == 1) success = true;
                    }
                }
            } catch (SQLException e) {
                e.printStackTrace();
            }
            sendResponse(exchange, 200, gson.toJson(success ? "Login Successful" : "Invalid credentials"));
        }
    }

    private static void sendResponse(HttpExchange exchange, int statusCode, String response) throws IOException {
        exchange.getResponseHeaders().add("Content-Type", "application/json");
        byte[] bytes = response.getBytes("UTF-8");
        exchange.sendResponseHeaders(statusCode, bytes.length);
        try (OutputStream os = exchange.getResponseBody()) {
            os.write(bytes);
        }
    }
}