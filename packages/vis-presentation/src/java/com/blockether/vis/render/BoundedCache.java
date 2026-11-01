package com.blockether.vis.render;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * A thread-safe cache that keeps at most {@code capacity} entries and drops the least
 * recently used entry first. Each method synchronizes on this cache, so a caller can
 * hold its monitor to make a read and a write one step.
 *
 * <p>Use it for pure-function results: a dropped or raced entry is computed again to
 * an equal value.
 */
public final class BoundedCache<K, V> {
    private static final class Lru<K, V> extends LinkedHashMap<K, V> {
        private static final long serialVersionUID = 1L;
        private final int capacity;

        Lru(int capacity) {
            super(Math.min(capacity, 256), 0.75f, true);
            this.capacity = capacity;
        }

        @Override
        protected boolean removeEldestEntry(Map.Entry<K, V> eldest) {
            return size() > capacity;
        }
    }

    private final Lru<K, V> entries;

    public BoundedCache(int capacity) {
        if (capacity < 1) {
            throw new IllegalArgumentException("capacity must be positive: " + capacity);
        }
        this.entries = new Lru<>(capacity);
    }

    /** Return the value for {@code key} and mark it as recently used, or null. */
    public synchronized V get(Object key) {
        return entries.get(key);
    }

    /** Store {@code value} for {@code key} and return {@code value}. */
    public synchronized V put(K key, V value) {
        entries.put(key, value);
        return value;
    }

    public synchronized int size() {
        return entries.size();
    }

    public synchronized void clear() {
        entries.clear();
    }
}
