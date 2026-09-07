package com.blockether.viscompanion;

import org.junit.Test;
import static org.junit.Assert.*;
import java.net.Socket;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;

public class OAuthLoopbackTest {
    private OAuthLoopback receiver(String redirect, long ttl) {
        String auth = "https://gateway.example.com/authorize?state=test-state&redirect_uri=" + URLEncoder.encode(redirect, StandardCharsets.UTF_8);
        return new OAuthLoopback(auth, redirect, "test-state", System.currentTimeMillis() + ttl);
    }
    private String request(String query) { return "GET /callback?" + query + " HTTP/1.1\r\nHost: localhost:53692\r\n\r\n"; }
    private String exchange(String address, String query) throws Exception {
        try (Socket socket = new Socket(address, 53692)) {
            socket.setSoTimeout(3000); socket.getOutputStream().write(request(query).getBytes(StandardCharsets.UTF_8));
            return new String(socket.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
        }
    }
    @Test public void stateDestinationAndAmbiguity() {
        OAuthLoopback server = receiver("http://localhost:53692/callback", 60000);
        try {
            assertEquals("http://localhost:53692/callback?state=test-state&code=test-code",
                server.callback(request("state=test-state&code=test-code&ignored=field")));
            for (String query : new String[]{"state=wrong&code=test-code", "state=test-state&code=", "state=test-state&code=test-code&error=",
                "state=test-state&code=test-code&code=other", "state=test-state&state=test-state&code=test-code", "state=test-state&code=test-code#fragment"})
                assertNull(server.callback(request(query)));
            assertNull(server.callback(request("state=test-state&code=test-code").replace("localhost:53692", "10.0.0.5:53692")));
            assertNull(server.callback(request("state=test-state&code=test-code").replace("GET ", "POST ")));
            assertEquals("http://localhost:53692/callback?state=test-state&code=a%2Bb+c",
                server.callback(request("state=test-state&code=a%2Bb+c")));
        } finally { server.cancel(); }
    }
    @Test public void refusesNonLoopbackAndMismatchedAuthorization() {
        for (String redirect : new String[]{"http://10.0.0.5:53692/callback", "http://localhost:80/callback", "https://localhost:53692/callback",
            "http://user@localhost:53692/callback", "http://localhost:53692/callback?next=other"})
            assertThrows(IllegalArgumentException.class, () -> receiver(redirect, 60000));
        assertThrows(IllegalArgumentException.class, () -> receiver("http://localhost:53692/callback", -1));
        assertThrows(IllegalArgumentException.class, () -> receiver("http://localhost:53692/callback", 901000));
        assertThrows(IllegalArgumentException.class, () -> new OAuthLoopback("https://gateway.example.com/authorize?state=other",
            "http://localhost:53692/callback", "test-state", System.currentTimeMillis() + 60000));
    }
    @Test public void freshReturnOverRealIPv4AndIPv6Listener() throws Exception {
        OAuthLoopback server = receiver("http://localhost:53692/callback", 60000);
        CompletableFuture<String> returned = new CompletableFuture<>();
        try {
            server.start(returned::complete);
            assertTrue(exchange("::1", "state=wrong&code=test-code").startsWith("HTTP/1.1 400"));
            assertFalse(returned.isDone());
            String response = exchange("127.0.0.1", "state=test-state&code=test-code");
            assertTrue(response.startsWith("HTTP/1.1 200")); assertTrue(response.contains("Cache-Control: no-store"));
            assertFalse(response.contains("test-code"));
            assertEquals("http://localhost:53692/callback?state=test-state&code=test-code", returned.get(3, TimeUnit.SECONDS));
        } finally { server.cancel(); }
    }
    @Test public void cancellationExpiryAndExclusivePortOwnership() throws Exception {
        for (long ttl : new long[]{60000, 100}) {
            OAuthLoopback server = receiver("http://localhost:53692/callback", ttl);
            CompletableFuture<String> returned = new CompletableFuture<>();
            try {
                server.start(returned::complete);
                if (ttl == 60000) {
                    OAuthLoopback other = receiver("http://localhost:53692/callback", 60000);
                    try { assertThrows(java.io.IOException.class, () -> other.start(value -> fail("A failed bind cannot claim a flow"))); }
                    finally { other.cancel(); }
                    server.cancel(); server.cancel();
                }
                assertNull(returned.get(3, TimeUnit.SECONDS));
            } finally { server.cancel(); }
        }
    }
}
