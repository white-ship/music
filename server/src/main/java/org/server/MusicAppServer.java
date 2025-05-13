package org.server;

import com.google.gson.JsonArray;
import org.apache.commons.fileupload.FileItem;
import org.apache.commons.fileupload.disk.DiskFileItemFactory;
import org.apache.commons.fileupload.servlet.ServletFileUpload;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.sun.net.httpserver.HttpServer;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;
import com.google.gson.Gson;

import java.io.*;
import java.net.InetSocketAddress;
import java.net.URI;
import java.sql.*;
import java.util.List;
import java.util.stream.Collectors;
import java.security.MessageDigest;

public class MusicAppServer {
    static class User { String username; String password; }
    private static final Gson gson = new Gson();

    public static void main(String[] args) throws IOException {
        HttpServer server = HttpServer.create(new InetSocketAddress(4567), 0);
        server.createContext("/register", new RegisterHandler());
        server.createContext("/login", new LoginHandler());
        server.createContext("/upload", new UploadHandler());
        server.createContext("/music/list", new ListHandler());
        server.createContext("/music/download", new DownloadHandler());
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
        String sourceURL = "jdbc:opengauss://192.168.129.135:26000/music_sys?characterEncoding=UTF-8&useUnicode=true\"";
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
                sendResponse(exchange, 500, "error");
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
            JsonObject resp = new JsonObject();
            try (Connection conn = MusicAppServer.GetConnection("myroot", "Lhx050918")) {
                String key = "Abc123!@";
                String sql = "SELECT id, username FROM users WHERE username = ? AND password = ?";
                try (PreparedStatement ps = conn.prepareStatement(sql)) {
                    ps.setString(1, u.username);
                    ps.setString(2, sha256(u.password));
                    try (ResultSet rs = ps.executeQuery()) {
                        if (rs.next()) {
                            // 登录成功
                            resp.addProperty("status", "success");
                            resp.addProperty("userID", rs.getInt("id"));
                            resp.addProperty("username", rs.getString("username"));
                        } else {
                            // 登录失败
                            resp.addProperty("status", "fail");
                            resp.addProperty("message", "Invalid username or password");
                        }
                    }
                }
            } catch (SQLException e) {
                e.printStackTrace();
                resp.addProperty("status", "error");
                resp.addProperty("message", "Server error");
            }
            sendResponse(exchange, 200, gson.toJson(resp));
        }
    }

    static class UploadHandler implements HttpHandler {
        @Override
        public void handle(HttpExchange exchange) throws IOException {
            if (!"POST".equals(exchange.getRequestMethod())) {
                exchange.sendResponseHeaders(405, -1); // Method Not Allowed
                return;
            }

            try {
                // 检查请求是否包含多部分内容
                if (!ServletFileUpload.isMultipartContent(new HttpExchangeRequestContext(exchange))) {
                    sendResponse(exchange, 400, "{\"status\":\"error\",\"message\":\"Invalid request\"}");
                    return;
                }

                // 配置文件上传处理
                DiskFileItemFactory factory = new DiskFileItemFactory();
                ServletFileUpload upload = new ServletFileUpload(factory);

                // 解析请求
                List<FileItem> items = upload.parseRequest(new HttpExchangeRequestContext(exchange));
                String title = null, album = null, duration = null, uploaderId = null, uploaderName = null;
                byte[] fileData = null;
                String fileName = null;

                for (FileItem item : items) {
                    if (item.isFormField()) {
                        // 处理表单字段
                        switch (item.getFieldName()) {
                            case "title": title = item.getString("UTF-8"); break;
                            case "album": album = item.getString("UTF-8"); break;
                            case "duration": duration = item.getString("UTF-8"); break;
                            case "uploader_id": uploaderId = item.getString("UTF-8"); break;
                            case "uploader_name": uploaderName = item.getString("UTF-8"); break;
                        }
                    } else {
                        // 处理文件字段
                        fileName = item.getName();
                        fileData = item.get();
                    }
                }

                // 保存文件
                String savePath = "uploads/" + fileName;
                try (FileOutputStream fos = new FileOutputStream(savePath)) {
                    fos.write(fileData);
                }

                // 保存信息到数据库
                int artistId = -1;
                try (Connection conn = GetConnection("myroot", "Lhx050918")) {
                    String queryArtistIdSql = "SELECT id FROM artists WHERE user_id = ?";
                    try (PreparedStatement ps = conn.prepareStatement(queryArtistIdSql)) {
                        ps.setInt(1, Integer.parseInt(uploaderId));
                        try (ResultSet rs = ps.executeQuery()) {
                            if (rs.next()) {
                                artistId = rs.getInt("id");
                            } else {
                                sendResponse(exchange, 400, "{\"status\":\"error\",\"message\":\"Artist not found\"}");
                                return;
                            }
                        }
                    }
                }

                try (Connection conn = GetConnection("myroot", "Lhx050918")) {
                    String sql = "INSERT INTO songs (title, album, duration, uploaded_by,  released_at, file_key) " +
                            "VALUES (?, ?, ?, ?, now(), ?)";
                    try (PreparedStatement ps = conn.prepareStatement(sql)) {
                        ps.setString(1, title);
                        ps.setString(2, album);
                        ps.setString(3, duration);
                        ps.setInt(4, artistId);
                        ps.setString(5, savePath);
                        ps.executeUpdate();
                    }
                }

                sendResponse(exchange, 200, "{\"status\":\"success\"}");
            } catch (Exception e) {
                e.printStackTrace();
                sendResponse(exchange, 500, "{\"status\":\"error\",\"message\":\"" + e.getMessage() + "\"}");
            }
        }
    }

    static class ListHandler implements HttpHandler {
        @Override
        public void handle(HttpExchange exchange) throws IOException {
            if (!"GET".equals(exchange.getRequestMethod())) {
                exchange.sendResponseHeaders(405, -1);
                return;
            }

            JsonArray arr = new JsonArray();
            try (Connection conn = GetConnection("myroot", "Lhx050918")) {
                String sql = "SELECT id, title, album, duration, uploaded_by FROM songs ORDER BY released_at";
                try (Statement st = conn.createStatement();
                     ResultSet rs = st.executeQuery(sql)) {
                    while (rs.next()) {
                        JsonObject obj = new JsonObject();
                        obj.addProperty("id", rs.getInt("id"));
                        obj.addProperty("title", rs.getString("title"));
                        obj.addProperty("album", rs.getString("album"));
                        obj.addProperty("duration", rs.getString("duration"));
                        obj.addProperty("uploaded by", rs.getString("uploaded_by"));
                        arr.add(obj);
                    }
                }
            } catch (SQLException e) {
                e.printStackTrace();
            }

            sendResponse(exchange, 200, gson.toJson(arr));
        }
    }

    static class DownloadHandler implements HttpHandler {
        @Override
        public void handle(HttpExchange exchange) throws IOException {
            URI req = exchange.getRequestURI();
            String query = req.getQuery(); // e.g. "id=3"
            int id = Integer.parseInt(query.substring(query.indexOf('=') + 1));

            String path = null, filename = null;
            try (Connection conn = GetConnection("myroot", "Lhx050918")) {
                String sql = "SELECT file_key, title FROM songs WHERE id = ?";
                try (PreparedStatement ps = conn.prepareStatement(sql)) {
                    ps.setInt(1, id);
                    try (ResultSet rs = ps.executeQuery()) {
                        if (rs.next()) {
                            path = rs.getString("file_key");
                            filename = rs.getString("title") + ExtractFileExt(path);
                        }
                    }
                }
            } catch (SQLException e) {
                e.printStackTrace();
            }

            if (path == null) {
                exchange.sendResponseHeaders(404, -1);
                return;
            }

            File file = new File(path);
            exchange.getResponseHeaders().add("Content-Type", "application/octet-stream");
            exchange.getResponseHeaders().add("Content-Disposition", "attachment; filename=\"" + filename + "\"");
            exchange.sendResponseHeaders(200, file.length());
            try (OutputStream os = exchange.getResponseBody();
                 FileInputStream fis = new FileInputStream(file)) {
                byte[] buf = new byte[8192];
                int len;
                while ((len = fis.read(buf)) > 0) {
                    os.write(buf, 0, len);
                }
            }
        }

        private String ExtractFileExt(String fullpath) {
            int i = fullpath.lastIndexOf('.');
            return i >= 0 ? fullpath.substring(i) : "";
        }
    }
    private static void sendResponse(HttpExchange exchange, int statusCode, String response) throws IOException {
        exchange.getResponseHeaders().add("Content-Type", "application/json; charset=UTF-8");
        byte[] bytes = response.getBytes("UTF-8");
        exchange.sendResponseHeaders(statusCode, bytes.length);
        try (OutputStream os = exchange.getResponseBody()) {
            os.write(bytes);
        }
    }
}