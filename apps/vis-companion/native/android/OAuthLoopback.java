package com.blockether.viscompanion;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.URI;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.SynchronousQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;

/** One bounded native loopback return. No provider names, token exchange, storage or logging. */
public final class OAuthLoopback {
    public final URI authorization;
    private final URI redirect;
    private final String state;
    private final long expiresAt;
    private final List<ServerSocket> listeners = new ArrayList<>();
    private final Set<Socket> clients = new HashSet<>();
    private final ExecutorService acceptors = Executors.newFixedThreadPool(2);
    private final ThreadPoolExecutor workers =
            new ThreadPoolExecutor(0, 8, 5, TimeUnit.SECONDS, new SynchronousQueue<>());
    private final ScheduledExecutorService expiry = Executors.newSingleThreadScheduledExecutor();
    private Consumer<String> completion;
    private boolean claimed;
    private boolean done;

    public OAuthLoopback(
            String authorizationUrl, String redirectUri, String state, long expiresAt) {
        try {
            this.authorization = URI.create(authorizationUrl);
            this.redirect = URI.create(redirectUri);
            this.state = state;
            this.expiresAt = expiresAt;
            Map<String, String> query = query(authorization.getRawQuery());
            if (authorizationUrl.length() > 16384
                    || redirectUri.length() > 2048
                    || state.isEmpty()
                    || state.length() > 1024
                    || !"https".equals(authorization.getScheme())
                    || authorization.getHost() == null
                    || authorization.getRawUserInfo() != null
                    || authorization.getRawFragment() != null
                    || !state.equals(query.get("state"))
                    || !redirectUri.equals(query.get("redirect_uri"))
                    || !"http".equals(redirect.getScheme())
                    || !Arrays.asList("localhost", "127.0.0.1", "[::1]")
                            .contains(redirect.getHost())
                    || redirect.getPort() < 1024
                    || redirect.getPort() > 65535
                    || redirect.getRawUserInfo() != null
                    || redirect.getRawQuery() != null
                    || redirect.getRawFragment() != null
                    || redirect.getRawPath().isEmpty()
                    || redirect.getRawPath().contains("%")
                    || expiresAt <= System.currentTimeMillis()
                    || expiresAt - System.currentTimeMillis() > 900000)
                throw new IllegalArgumentException();
        } catch (RuntimeException error) {
            throw new IllegalArgumentException("Invalid sign-in request.");
        }
    }

    private static String form(String value, boolean encode) {
        try {
            return encode ? URLEncoder.encode(value, "UTF-8") : URLDecoder.decode(value, "UTF-8");
        } catch (java.io.UnsupportedEncodingException impossible) {
            throw new IllegalStateException("UTF-8 unavailable.");
        }
    }

    private static Map<String, String> query(String raw) {
        Map<String, String> result = new HashMap<>();
        if (raw == null) return result;
        for (String field : raw.split("&", -1)) {
            String[] parts = field.split("=", 2);
            String key = form(parts[0], false);
            String value = parts.length == 2 ? form(parts[1], false) : "";
            if (result.putIfAbsent(key, value) != null) throw new IllegalArgumentException();
        }
        return result;
    }

    public String callback(String header) {
        try {
            if (header.length() > 8192 || !header.endsWith("\r\n\r\n") || header.indexOf(0) >= 0)
                return null;
            String[] lines = header.split("\r\n");
            String[] request = lines[0].split(" ", -1);
            if (request.length != 3
                    || !"GET".equals(request[0])
                    || !Arrays.asList("HTTP/1.0", "HTTP/1.1").contains(request[2])
                    || !request[1].startsWith("/")
                    || request[1].startsWith("//")) return null;
            Map<String, String> headers = new HashMap<>();
            for (int i = 1; i < lines.length; i++) {
                int colon = lines[i].indexOf(':');
                if (colon < 1 || Character.isWhitespace(lines[i].charAt(0))) return null;
                String key = lines[i].substring(0, colon).toLowerCase(java.util.Locale.ROOT);
                if (headers.putIfAbsent(key, lines[i].substring(colon + 1).trim()) != null)
                    return null;
            }
            String host = redirect.getHost() + ":" + redirect.getPort();
            if (!host.equalsIgnoreCase(headers.get("host"))
                    || headers.containsKey("transfer-encoding")
                    || !"0".equals(headers.getOrDefault("content-length", "0"))) return null;
            URI returned = URI.create("http://" + host + request[1]);
            if (returned.getRawFragment() != null
                    || !redirect.getRawPath().equals(returned.getRawPath())) return null;
            Map<String, String> query = query(returned.getRawQuery());
            if (!state.equals(query.get("state"))
                    || query.containsKey("code") == query.containsKey("error")) return null;
            String key = query.containsKey("code") ? "code" : "error";
            String value = query.get(key);
            if (value.trim().isEmpty()) return null;
            return redirect + "?state=" + form(state, true) + "&" + key + "=" + form(value, true);
        } catch (RuntimeException error) {
            return null;
        }
    }

    /** Binding finishes before the caller opens the browser. A failed bind never launches it. */
    public synchronized void start(Consumer<String> complete) throws IOException {
        if (completion != null || done) throw new IOException("Sign-in unavailable.");
        completion = complete;
        String host = redirect.getHost();
        String[] addresses =
                "localhost".equals(host)
                        ? new String[] {"127.0.0.1", "::1"}
                        : new String[] {"[::1]".equals(host) ? "::1" : host};
        try {
            for (String address : addresses) {
                ServerSocket server = new ServerSocket();
                listeners.add(server);
                server.setReuseAddress(true);
                server.bind(
                        new InetSocketAddress(InetAddress.getByName(address), redirect.getPort()),
                        8);
            }
            expiry.schedule(
                    () -> finish(null),
                    Math.max(1, expiresAt - System.currentTimeMillis()),
                    TimeUnit.MILLISECONDS);
            for (ServerSocket server : listeners) acceptors.execute(() -> accept(server));
        } catch (IOException | RuntimeException error) {
            completion = null;
            finish(null);
            throw new IOException("Cannot bind sign-in callback.");
        }
    }

    private void accept(ServerSocket server) {
        while (!server.isClosed()) {
            try {
                Socket socket = server.accept();
                synchronized (this) {
                    if (done || claimed || clients.size() >= 8) {
                        socket.close();
                        continue;
                    }
                    clients.add(socket);
                    try {
                        workers.execute(() -> receive(socket));
                    } catch (RuntimeException rejected) {
                        clients.remove(socket);
                        socket.close();
                    }
                }
            } catch (IOException error) {
                if (!server.isClosed()) finish(null);
                return;
            }
        }
    }

    private void receive(Socket socket) {
        try (Socket client = socket) {
            client.setSoTimeout(5000);
            ByteArrayOutputStream header = new ByteArrayOutputStream();
            int end = 0;
            long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(5);
            while (header.size() <= 8192 && end != 4 && System.nanoTime() < deadline) {
                int next = client.getInputStream().read();
                if (next < 0) return;
                header.write(next);
                end = next == (end % 2 == 0 ? '\r' : '\n') ? end + 1 : 0;
            }
            String input;
            synchronized (this) {
                if (done) return;
                input =
                        claimed || System.currentTimeMillis() >= expiresAt
                                ? null
                                : callback(
                                        new String(header.toByteArray(), StandardCharsets.UTF_8));
                if (input != null) claimed = true;
            }
            String status = input == null ? "400 Bad Request" : "200 OK";
            String body =
                    input == null ? "Invalid sign-in return." : "Sign-in received. Return to Vis.";
            String reply =
                    "HTTP/1.1 "
                            + status
                            + "\r\nContent-Type: text/plain; charset=utf-8\r\nContent-Length: "
                            + body.length()
                            + "\r\n"
                            + "Cache-Control: no-store\r\n"
                            + "Referrer-Policy: no-referrer\r\n"
                            + "Content-Security-Policy: default-src 'none'\r\n"
                            + "Connection: close\r\n\r\n"
                            + body;
            try {
                client.getOutputStream().write(reply.getBytes(StandardCharsets.UTF_8));
            } finally {
                if (input != null) finish(input);
            }
        } catch (IOException error) {
            /* A malformed or abandoned local request does not spend the flow. */
        } finally {
            synchronized (this) {
                clients.remove(socket);
            }
        }
    }

    public void cancel() {
        finish(null);
    }

    private synchronized void finish(String input) {
        if (done) return;
        done = true;
        for (ServerSocket server : listeners)
            try {
                server.close();
            } catch (IOException ignored) {
            }
        for (Socket socket : clients)
            try {
                socket.close();
            } catch (IOException ignored) {
            }
        listeners.clear();
        clients.clear();
        acceptors.shutdownNow();
        workers.shutdownNow();
        expiry.shutdownNow();
        Consumer<String> callback = completion;
        completion = null;
        if (callback != null) callback.accept(input);
    }
}
