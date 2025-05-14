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
import java.nio.charset.StandardCharsets;
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
        server.createContext("/music/delete", new DeleteHandler());
        server.createContext("/user/isArtist", new IsArtistHandler());
        server.createContext("/users/list", new UserListHandler());
        server.createContext("/users/delete", new UserDeleteHandler());
        server.createContext("/users/set_admin", new SetAdminHandler());
        server.createContext("/users/set_artist", new SetArtistHandler());
        server.createContext("/users/remove_admin", new RemoveAdminHandler());
        server.createContext("/users/remove_artist", new RemoveArtistHandler());
        server.createContext("/music/search", new SearchHandler());
        server.createContext("/history", new HistoryHandler());
        server.createContext("/favorites/list", new FavoritesListHandler());
        server.createContext("/favorites/add", new FavoritesAddHandler());
        server.createContext("/favorites/remove", new FavoritesRemoveHandler());
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
        String sourceURL = "jdbc:opengauss://192.168.129.135:26000/music_sys?characterEncoding=UTF-8&useUnicode=true";
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

            JsonObject resp = new JsonObject();

            try (Connection conn = MusicAppServer.GetConnection("myroot", "Lhx050918")) {
                String sql = """
                SELECT u.id, u.username,
                       EXISTS (
                         SELECT 1 FROM user_roles ur
                         JOIN roles r ON ur.role_id = r.id
                         WHERE ur.user_id = u.id AND r.name = 'admin'
                       ) AS is_admin
                FROM users u
                WHERE u.username = ? AND u.password = ?""";

                try (PreparedStatement ps = conn.prepareStatement(sql)) {
                    ps.setString(1, u.username);
                    ps.setString(2, sha256(u.password));
                    try (ResultSet rs = ps.executeQuery()) {
                        if (rs.next()) {
                            resp.addProperty("status", "success");
                            resp.addProperty("userID", rs.getInt("id"));
                            resp.addProperty("username", rs.getString("username"));
                            resp.addProperty("isAdmin", rs.getBoolean("is_admin")); // 添加 isAdmin 字段
                        } else {
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
                upload.setHeaderEncoding("UTF-8");
                List<FileItem> items = upload.parseRequest(new HttpExchangeRequestContext(exchange));
                String title = null, album = null, duration = null, uploaderId = null;
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
                String sql = "SELECT id, title, album, duration, uploaded_by,downloads FROM songs ORDER BY downloads";
                try (Statement st = conn.createStatement();
                     ResultSet rs = st.executeQuery(sql)) {
                    while (rs.next()) {
                        JsonObject obj = new JsonObject();
                        obj.addProperty("id", rs.getInt("id"));
                        obj.addProperty("title", rs.getString("title"));
                        obj.addProperty("album", rs.getString("album"));
                        obj.addProperty("duration", rs.getString("duration"));
                        obj.addProperty("uploaded by", rs.getString("uploaded_by"));
                        obj.addProperty("downloads", rs.getInt("downloads"));
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
            if (!"GET".equals(exchange.getRequestMethod())) {
                exchange.sendResponseHeaders(405, -1);
                return;
            }

            // 解析 songId
            URI req = exchange.getRequestURI();
            String query = req.getQuery(); // e.g. "id=3"
            int songId;
            try {
                songId = Integer.parseInt(query.substring(query.indexOf('=') + 1));
            } catch (Exception e) {
                sendResponse(exchange, 400, "{\"status\":\"error\",\"message\":\"Invalid id\"}");
                return;
            }

            // 从请求头获取 userId
            String userIdHeader = exchange.getRequestHeaders().getFirst("userId");
            int userId;
            try {
                userId = Integer.parseInt(userIdHeader);
            } catch (Exception e) {
                sendResponse(exchange, 400, "{\"status\":\"error\",\"message\":\"Missing or invalid userId\"}");
                return;
            }

            String filePath = null, filename = null;
            // 查询文件路径和名称
            try (Connection conn = GetConnection("myroot", "Lhx050918")) {
                String sql = "SELECT file_key, title FROM songs WHERE id = ?";
                try (PreparedStatement ps = conn.prepareStatement(sql)) {
                    ps.setInt(1, songId);
                    try (ResultSet rs = ps.executeQuery()) {
                        if (rs.next()) {
                            filePath = rs.getString("file_key");
                            filename = rs.getString("title") + extractFileExt(filePath);
                        }
                    }
                }
            } catch (SQLException e) {
                e.printStackTrace();
            }

            if (filePath == null) {
                exchange.sendResponseHeaders(404, -1);
                return;
            }

            // 插入播放历史并更新下载次数
            try (Connection conn = GetConnection("myroot", "Lhx050918")) {
                conn.setAutoCommit(false); // 开启事务

                // 插入播放历史
                String insertHistorySql = "INSERT INTO history(user_id, song_id, played_at) VALUES(?, ?, now())";
                try (PreparedStatement ps = conn.prepareStatement(insertHistorySql)) {
                    ps.setInt(1, userId);
                    ps.setInt(2, songId);
                    ps.executeUpdate();
                }

                // 更新下载次数
                String updateDownloadsSql = "UPDATE songs SET downloads = downloads + 1 WHERE id = ?";
                try (PreparedStatement ps = conn.prepareStatement(updateDownloadsSql)) {
                    ps.setInt(1, songId);
                    ps.executeUpdate();
                }

                conn.commit(); // 提交事务
            } catch (SQLException e) {
                e.printStackTrace();
                sendResponse(exchange, 500, "{\"status\":\"error\",\"message\":\"Database error\"}");
                return;
            }

            // 发送文件
            File file = new File(filePath);
            exchange.getResponseHeaders().add("Content-Type", "application/octet-stream");
            exchange.getResponseHeaders().add("Content-Disposition",
                    "attachment; filename=\"" + filename + "\"");
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

        private String extractFileExt(String fullpath) {
            int i = fullpath.lastIndexOf('.');
            return i >= 0 ? fullpath.substring(i) : "";
        }
    }


    static class IsArtistHandler implements HttpHandler {
        @Override
        public void handle(HttpExchange exchange) throws IOException {
            if (!"GET".equals(exchange.getRequestMethod())) {
                exchange.sendResponseHeaders(405, -1);
                return;
            }
            // 解析 userId 参数，例如 ?userId=123
            String query = exchange.getRequestURI().getQuery();
            int userId = Integer.parseInt(query.split("=")[1]);

            boolean isArtist = false;
            try (Connection conn = GetConnection("myroot", "Lhx050918")) {
                String sql = """
                    SELECT count(*) FROM user_roles
                    JOIN roles ON user_roles.role_id = roles.id
                    WHERE user_id = ? AND roles.name='artist'
                    """;
                try (PreparedStatement ps = conn.prepareStatement(sql)) {
                    ps.setInt(1, userId);
                    try (ResultSet rs = ps.executeQuery()) {
                        if (rs.next() && rs.getInt(1) > 0) {
                            isArtist = true;
                        }
                    }
                }
            } catch (SQLException e) {
                e.printStackTrace();
            }

            // 返回 JSON：{"isArtist":true} 或 {"isArtist":false}
            JsonObject resp = new JsonObject();
            resp.addProperty("isArtist", isArtist);
            exchange.getResponseHeaders().set("Content-Type", "application/json; charset=UTF-8");
            byte[] bytes = resp.toString().getBytes(StandardCharsets.UTF_8);
            exchange.sendResponseHeaders(200, bytes.length);
            try (OutputStream os = exchange.getResponseBody()) {
                os.write(bytes);
            }
        }
    }

    static class DeleteHandler implements HttpHandler {
        @Override
        public void handle(HttpExchange exchange) throws IOException {
            if (!"DELETE".equals(exchange.getRequestMethod())) {
                exchange.sendResponseHeaders(405, -1);
                return;
            }

            // 从查询参数中获取 id
            String query = exchange.getRequestURI().getQuery();
            if (query == null || !query.startsWith("id=")) {
                sendResponse(exchange, 400, "{\"status\":\"error\",\"message\":\"Missing or invalid id parameter\"}");
                return;
            }

            int musicId;
            try {
                musicId = Integer.parseInt(query.split("=")[1]);
            } catch (NumberFormatException e) {
                sendResponse(exchange, 400, "{\"status\":\"error\",\"message\":\"Invalid id format\"}");
                return;
            }

            // 验证权限（管理员）
            boolean isAdmin = false;
            String userId = exchange.getRequestHeaders().getFirst("userId");
            try (Connection conn = MusicAppServer.GetConnection("myroot", "Lhx050918")) {
                String sql = """
            SELECT EXISTS (
                SELECT 1 FROM user_roles ur
                JOIN roles r ON ur.role_id = r.id
                WHERE ur.user_id = ? AND r.name = 'admin'
            ) AS is_admin
            """;
                try (PreparedStatement ps = conn.prepareStatement(sql)) {
                    ps.setInt(1, Integer.parseInt(userId));
                    try (ResultSet rs = ps.executeQuery()) {
                        if (rs.next()) {
                            isAdmin = rs.getBoolean("is_admin");
                        }
                    }
                }
            } catch (SQLException e) {
                e.printStackTrace();
                sendResponse(exchange, 500, "{\"status\":\"error\",\"message\":\"Database error\"}");
                return;
            }

            if (!isAdmin) {
                sendResponse(exchange, 403, "{\"status\":\"error\",\"message\":\"Permission denied\"}");
                return;
            }

            // 执行数据库删除
            String filePath = null;
            try (Connection conn = MusicAppServer.GetConnection("myroot", "Lhx050918")) {
                String querySql = "SELECT file_key FROM songs WHERE id = ?";
                try (PreparedStatement ps = conn.prepareStatement(querySql)) {
                    ps.setInt(1, musicId);
                    try (ResultSet rs = ps.executeQuery()) {
                        if (rs.next()) {
                            filePath = rs.getString("file_key");
                        } else {
                            sendResponse(exchange, 404, "{\"status\":\"error\",\"message\":\"Music not found\"}");
                            return;
                        }
                    }
                }

                String deleteSql = "DELETE FROM songs WHERE id = ?";
                try (PreparedStatement ps = conn.prepareStatement(deleteSql)) {
                    ps.setInt(1, musicId);
                    ps.executeUpdate();
                }
            } catch (SQLException e) {
                e.printStackTrace();
                sendResponse(exchange, 500, "{\"status\":\"error\",\"message\":\"Database error\"}");
                return;
            }

            // 删除文件
            if (filePath != null) {
                File file = new File(filePath);
                if (file.exists() && !file.delete()) {
                    sendResponse(exchange, 500, "{\"status\":\"error\",\"message\":\"Failed to delete file\"}");
                    return;
                }
            }

            sendResponse(exchange, 200, "{\"status\":\"success\",\"message\":\"Deleted\"}");
        }
    }

    static class UserListHandler implements HttpHandler {
        @Override
        public void handle(HttpExchange exchange) throws IOException {
            if (!"GET".equals(exchange.getRequestMethod())) {
                exchange.sendResponseHeaders(405, -1);
                return;
            }

            JsonArray users = new JsonArray();
            try (Connection conn = MusicAppServer.GetConnection("myroot", "Lhx050918")) {
                String sql = """
                SELECT u.id, u.username,
                       EXISTS (
                         SELECT 1 FROM user_roles ur
                         JOIN roles r ON ur.role_id = r.id
                         WHERE ur.user_id = u.id AND r.name = 'admin'
                       ) AS is_admin,
                        EXISTS(
                         SELECT 1 FROM user_roles ur
                         JOIN roles r ON ur.role_id = r.id
                         WHERE ur.user_id = u.id AND r.name = 'artist'
                        ) AS is_artist
                FROM users u
            """;
                try (Statement st = conn.createStatement();
                     ResultSet rs = st.executeQuery(sql)) {
                    while (rs.next()) {
                        JsonObject user = new JsonObject();
                        user.addProperty("id", rs.getInt("id"));
                        user.addProperty("username", rs.getString("username"));
                        user.addProperty("is_admin", rs.getBoolean("is_admin"));
                        user.addProperty("is_artist", rs.getBoolean("is_artist"));
                        users.add(user);
                    }
                }
            } catch (SQLException e) {
                e.printStackTrace();
                sendResponse(exchange, 500, "{\"status\":\"error\",\"message\":\"Database error\"}");
                return;
            }

            sendResponse(exchange, 200, users.toString());
        }
    }

    static class UserDeleteHandler implements HttpHandler {
        @Override
        public void handle(HttpExchange exchange) throws IOException {
            if (!"DELETE".equals(exchange.getRequestMethod())) {
                exchange.sendResponseHeaders(405, -1);
                return;
            }

            String query = exchange.getRequestURI().getQuery();
            if (query == null || !query.startsWith("id=")) {
                sendResponse(exchange, 400, "{\"status\":\"error\",\"message\":\"Missing or invalid id parameter\"}");
                return;
            }

            int userId;
            try {
                userId = Integer.parseInt(query.split("=")[1]);
            } catch (NumberFormatException e) {
                sendResponse(exchange, 400, "{\"status\":\"error\",\"message\":\"Invalid id format\"}");
                return;
            }

            try (Connection conn = MusicAppServer.GetConnection("myroot", "Lhx050918")) {
                String sql = "DELETE FROM users WHERE id = ?";
                try (PreparedStatement ps = conn.prepareStatement(sql)) {
                    ps.setInt(1, userId);
                    int rowsAffected = ps.executeUpdate();
                    if (rowsAffected == 0) {
                        sendResponse(exchange, 404, "{\"status\":\"error\",\"message\":\"User not found\"}");
                        return;
                    }
                }
            } catch (SQLException e) {
                e.printStackTrace();
                sendResponse(exchange, 500, "{\"status\":\"error\",\"message\":\"Database error\"}");
                return;
            }

            sendResponse(exchange, 200, "{\"status\":\"success\",\"message\":\"User deleted\"}");
        }
    }

    static class SetAdminHandler implements HttpHandler {
        @Override
        public void handle(HttpExchange exchange) throws IOException {
            if (!"POST".equals(exchange.getRequestMethod())) {
                exchange.sendResponseHeaders(405, -1);
                return;
            }

            String query = exchange.getRequestURI().getQuery();
            if (query == null || !query.startsWith("id=")) {
                sendResponse(exchange, 400, "{\"status\":\"error\",\"message\":\"Missing or invalid id parameter\"}");
                return;
            }

            int userId;
            try {
                userId = Integer.parseInt(query.split("=")[1]);
            } catch (NumberFormatException e) {
                sendResponse(exchange, 400, "{\"status\":\"error\",\"message\":\"Invalid id format\"}");
                return;
            }

            try (Connection conn = MusicAppServer.GetConnection("myroot", "Lhx050918")) {
                String sql = """
                INSERT INTO user_roles (user_id, role_id)
                SELECT ?, r.id FROM roles r WHERE r.name = 'admin'
            """;
                try (PreparedStatement ps = conn.prepareStatement(sql)) {
                    ps.setInt(1, userId);
                    ps.executeUpdate();
                }
            } catch (SQLException e) {
                e.printStackTrace();
                sendResponse(exchange, 500, "{\"status\":\"error\",\"message\":\"Database error\"}");
                return;
            }

            sendResponse(exchange, 200, "{\"status\":\"success\",\"message\":\"Admin role granted\"}");
        }
    }

    static class SetArtistHandler implements HttpHandler {
        @Override
        public void handle(HttpExchange exchange) throws IOException {
            if (!"POST".equals(exchange.getRequestMethod())) {
                exchange.sendResponseHeaders(405, -1);
                return;
            }

            String query = exchange.getRequestURI().getQuery();
            if (query == null || !query.startsWith("userId=")) {
                sendResponse(exchange, 400, "{\"status\":\"error\",\"message\":\"Missing or invalid id parameter\"}");
                return;
            }

            int userId;
            try {
                userId = Integer.parseInt(query.split("=")[1]);
            } catch (NumberFormatException e) {
                sendResponse(exchange, 400, "{\"status\":\"error\",\"message\":\"Invalid id format\"}");
                return;
            }

            try (Connection conn = MusicAppServer.GetConnection("myroot", "Lhx050918")) {
                String sql = """
                INSERT INTO artists (user_id)
                SELECT id FROM users u WHERE u.id = ?
            """;
                try (PreparedStatement ps = conn.prepareStatement(sql)) {
                    ps.setInt(1, userId);
                    ps.executeUpdate();
                }
            } catch (SQLException e) {
                e.printStackTrace();
                sendResponse(exchange, 500, "{\"status\":\"error\",\"message\":\"Database error\"}");
                return;
            }

            sendResponse(exchange, 200, "{\"status\":\"success\",\"message\":\"Admin role granted\"}");
        }
    }

    static class RemoveAdminHandler implements HttpHandler {
        @Override
        public void handle(HttpExchange exchange) throws IOException {
            if (!"POST".equals(exchange.getRequestMethod())) {
                exchange.sendResponseHeaders(405, -1);
                return;
            }

            String query = exchange.getRequestURI().getQuery();
            if (query == null || !query.startsWith("id=")) {
                sendResponse(exchange, 400, "{\"status\":\"error\",\"message\":\"Missing or invalid id parameter\"}");
                return;
            }

            int userId;
            try {
                userId = Integer.parseInt(query.split("=")[1]);
            } catch (NumberFormatException e) {
                sendResponse(exchange, 400, "{\"status\":\"error\",\"message\":\"Invalid id format\"}");
                return;
            }

            try (Connection conn = MusicAppServer.GetConnection("myroot", "Lhx050918")) {
                String sql = """
                DELETE FROM user_roles
                WHERE user_id = ? AND role_id = (SELECT id FROM roles WHERE name = 'admin')
            """;
                try (PreparedStatement ps = conn.prepareStatement(sql)) {
                    ps.setInt(1, userId);
                    ps.executeUpdate();
                }
            } catch (SQLException e) {
                e.printStackTrace();
                sendResponse(exchange, 500, "{\"status\":\"error\",\"message\":\"Database error\"}");
                return;
            }

            sendResponse(exchange, 200, "{\"status\":\"success\",\"message\":\"Admin role removed\"}");
        }
    }

    static class RemoveArtistHandler implements HttpHandler {
        @Override
        public void handle(HttpExchange exchange) throws IOException {
            if (!"POST".equals(exchange.getRequestMethod())) {
                exchange.sendResponseHeaders(405, -1);
                return;
            }

            String query = exchange.getRequestURI().getQuery();
            if (query == null || !query.startsWith("userId=")) {
                sendResponse(exchange, 400, "{\"status\":\"error\",\"message\":\"Missing or invalid id parameter\"}");
                return;
            }

            int userId;
            try {
                userId = Integer.parseInt(query.split("=")[1]);
            } catch (NumberFormatException e) {
                sendResponse(exchange, 400, "{\"status\":\"error\",\"message\":\"Invalid id format\"}");
                return;
            }

            try (Connection conn = MusicAppServer.GetConnection("myroot", "Lhx050918")) {
                String sql = """
                DELETE FROM artists
                WHERE user_id = ?
            """;
                try (PreparedStatement ps = conn.prepareStatement(sql)) {
                    ps.setInt(1, userId);
                    ps.executeUpdate();
                }
            } catch (SQLException e) {
                e.printStackTrace();
                sendResponse(exchange, 500, "{\"status\":\"error\",\"message\":\"Database error\"}");
                return;
            }

            sendResponse(exchange, 200, "{\"status\":\"success\",\"message\":\"Admin role removed\"}");
        }
    }

    static class SearchHandler implements HttpHandler {
        @Override
        public void handle(HttpExchange exchange) throws IOException {
            if (!"GET".equals(exchange.getRequestMethod())) {
                exchange.sendResponseHeaders(405, -1);
                return;
            }

            String query = exchange.getRequestURI().getQuery();
            String keyword = null;
            if (query != null && query.startsWith("keyword=")) {
                keyword = query.split("=")[1];
            }

            if (keyword == null || keyword.isEmpty()) {
                sendResponse(exchange, 400, "{\"status\":\"error\",\"message\":\"Missing or invalid keyword parameter\"}");
                return;
            }

            JsonArray result = new JsonArray();

            try (Connection conn = MusicAppServer.GetConnection("myroot", "Lhx050918")) {
                String sql = "SELECT * FROM songs WHERE title LIKE ?";
                try (PreparedStatement stmt = conn.prepareStatement(sql)) {
                    stmt.setString(1, "%" + keyword + "%");
                    try (ResultSet rs = stmt.executeQuery()) {
                        while (rs.next()) {
                            JsonObject song = new JsonObject();
                            song.addProperty("id", rs.getInt("id"));
                            song.addProperty("title", rs.getString("title"));
                            song.addProperty("album", rs.getString("album"));
                            song.addProperty("duration", rs.getString("duration"));
                            song.addProperty("uploaded by", rs.getString("uploaded_by")); // 替换为实际字段名
                            result.add(song);
                        }
                    }
                }
            } catch (SQLException e) {
                e.printStackTrace();
                sendResponse(exchange, 500, "{\"status\":\"error\",\"message\":\"Database error\"}");
                return;
            }

            sendResponse(exchange, 200, result.toString());
        }
    }

    static class HistoryHandler implements HttpHandler {
        @Override
        public void handle(HttpExchange exchange) throws IOException {
            // 只允许 GET
            if (!"GET".equals(exchange.getRequestMethod())) {
                exchange.sendResponseHeaders(405, -1);
                return;
            }

            // 从请求参数中获取 userId
            String query = exchange.getRequestURI().getQuery(); // userId=123
            int userId = -1;
            if (query != null && query.startsWith("userId=")) {
                try {
                    userId = Integer.parseInt(query.substring("userId=".length()));
                } catch (NumberFormatException e) { }
            }
            if (userId < 0) {
                sendResponse(exchange, 400, "{\"status\":\"error\",\"message\":\"Missing or invalid userId\"}");
                return;
            }

            JsonArray arr = new JsonArray();
            try (Connection conn = GetConnection("myroot", "Lhx050918")) {
                // 联表查询 song 信息
                String sql = """
                SELECT h.id, h.played_at, s.id AS song_id, s.title, s.album, s.duration
                  FROM history h
                  JOIN songs s ON h.song_id = s.id
                 WHERE h.user_id = ?
                 ORDER BY h.played_at DESC
            """;
                try (PreparedStatement ps = conn.prepareStatement(sql)) {
                    ps.setInt(1, userId);
                    try (ResultSet rs = ps.executeQuery()) {
                        while (rs.next()) {
                            JsonObject obj = new JsonObject();
                            obj.addProperty("historyId", rs.getInt("id"));
                            obj.addProperty("songId", rs.getInt("song_id"));
                            obj.addProperty("title", rs.getString("title"));
                            obj.addProperty("album", rs.getString("album"));
                            obj.addProperty("duration", rs.getString("duration"));
                            obj.addProperty("playedAt", rs.getTimestamp("played_at").toString());
                            arr.add(obj);
                        }
                    }
                }
            } catch (SQLException e) {
                e.printStackTrace();
                sendResponse(exchange, 500, "{\"status\":\"error\",\"message\":\"Database error\"}");
                return;
            }

            sendResponse(exchange, 200, arr.toString());
        }
    }

    static class FavoritesListHandler implements HttpHandler {
        @Override
        public void handle(HttpExchange exchange) throws IOException {
            if (!"GET".equals(exchange.getRequestMethod())) {
                exchange.sendResponseHeaders(405, -1);
                return;
            }

            String query = exchange.getRequestURI().getQuery();
            int userId;
            try {
                userId = Integer.parseInt(query.split("=")[1]);
            } catch (Exception e) {
                sendResponse(exchange, 400, "{\"status\":\"error\",\"message\":\"Invalid userId\"}");
                return;
            }

            JsonArray favorites = new JsonArray();
            try (Connection conn = GetConnection("myroot", "Lhx050918")) {
                String sql = """
                SELECT s.id, s.title, s.album, s.duration, u.username AS uploaded_by
                  FROM favorites f
                  JOIN songs s ON f.song_id = s.id
                  JOIN users u ON s.uploaded_by = u.id
                 WHERE f.user_id = ?
                 ORDER BY f.favorited_at DESC
            """;
                try (PreparedStatement ps = conn.prepareStatement(sql)) {
                    ps.setInt(1, userId);
                    try (ResultSet rs = ps.executeQuery()) {
                        while (rs.next()) {
                            JsonObject song = new JsonObject();
                            song.addProperty("id", rs.getInt("id"));
                            song.addProperty("title", rs.getString("title"));
                            song.addProperty("album", rs.getString("album"));
                            song.addProperty("duration", rs.getString("duration"));
                            song.addProperty("uploaded by", rs.getString("uploaded_by"));
                            favorites.add(song);
                        }
                    }
                }
            } catch (SQLException e) {
                e.printStackTrace();
                sendResponse(exchange, 500, "{\"status\":\"error\",\"message\":\"Database error\"}");
                return;
            }

            sendResponse(exchange, 200, favorites.toString());
        }
    }

    static class FavoritesAddHandler implements HttpHandler {
        @Override
        public void handle(HttpExchange exchange) throws IOException {
            if (!"POST".equals(exchange.getRequestMethod())) {
                exchange.sendResponseHeaders(405, -1);
                return;
            }

            // 解析请求体
            String body = new BufferedReader(new InputStreamReader(exchange.getRequestBody(), StandardCharsets.UTF_8))
                    .lines().collect(Collectors.joining("\n"));
            JsonObject requestJson;
            int userId, songId;

            try {
                requestJson = JsonParser.parseString(body).getAsJsonObject();
                userId = requestJson.get("userId").getAsInt();
                songId = requestJson.get("songId").getAsInt();
            } catch (Exception e) {
                sendResponse(exchange, 400, "{\"status\":\"error\",\"message\":\"Invalid JSON format\"}");
                return;
            }

            // 插入收藏记录
            try (Connection conn = GetConnection("myroot", "Lhx050918")) {
                String sql = "INSERT INTO favorites(user_id, song_id, favorited_at) VALUES(?, ?, now())";
                try (PreparedStatement ps = conn.prepareStatement(sql)) {
                    ps.setInt(1, userId);
                    ps.setInt(2, songId);
                    ps.executeUpdate();
                }
            } catch (SQLException e) {
                e.printStackTrace();
                sendResponse(exchange, 500, "{\"status\":\"error\",\"message\":\"Database error\"}");
                return;
            }

            sendResponse(exchange, 200, "{\"status\":\"success\",\"message\":\"Favorite added\"}");
        }
    }

    static class FavoritesRemoveHandler implements HttpHandler {
        @Override
        public void handle(HttpExchange exchange) throws IOException {
            if (!"POST".equals(exchange.getRequestMethod())) {
                exchange.sendResponseHeaders(405, -1);
                return;
            }

            // 解析请求体
            String body = new BufferedReader(new InputStreamReader(exchange.getRequestBody(), StandardCharsets.UTF_8))
                    .lines().collect(Collectors.joining("\n"));
            JsonObject requestJson;
            int userId, songId;

            try {
                requestJson = JsonParser.parseString(body).getAsJsonObject();
                userId = requestJson.get("userId").getAsInt();
                songId = requestJson.get("songId").getAsInt();
            } catch (Exception e) {
                sendResponse(exchange, 400, "{\"status\":\"error\",\"message\":\"Invalid JSON format\"}");
                return;
            }

            // 删除收藏记录
            try (Connection conn = GetConnection("myroot", "Lhx050918")) {
                String sql = "DELETE FROM favorites WHERE user_id = ? AND song_id = ?";
                try (PreparedStatement ps = conn.prepareStatement(sql)) {
                    ps.setInt(1, userId);
                    ps.setInt(2, songId);
                    int rowsAffected = ps.executeUpdate();
                    if (rowsAffected == 0) {
                        sendResponse(exchange, 404, "{\"status\":\"error\",\"message\":\"Favorite not found\"}");
                        return;
                    }
                }
            } catch (SQLException e) {
                e.printStackTrace();
                sendResponse(exchange, 500, "{\"status\":\"error\",\"message\":\"Database error\"}");
                return;
            }

            sendResponse(exchange, 200, "{\"status\":\"success\",\"message\":\"Favorite removed\"}");
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