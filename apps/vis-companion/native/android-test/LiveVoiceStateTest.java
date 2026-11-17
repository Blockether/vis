package com.blockether.viscompanion;

import static org.junit.Assert.*;
import org.junit.Test;

public class LiveVoiceStateTest {
    @Test public void liveVoiceRequiresTheCommunicationRouteCallback() {
        assertFalse(LiveVoiceState.supportsPlatform(26));
        assertFalse(LiveVoiceState.supportsPlatform(30));
        assertTrue(LiveVoiceState.supportsPlatform(31));
        assertTrue(LiveVoiceState.supportsPlatform(36));
    }

    @Test public void communicationRouteWatchDoesNotDependOnCaptureOrPlayback() {
        LiveVoiceState state = new LiveVoiceState();
        var watch = new LiveVoiceState.RouteWatch(2);
        assertFalse(watch.changed(2));
        state.capture(1);
        state.stopCapture(2);
        state.resetOutput(1);
        assertTrue(watch.changed(3));
        assertTrue(watch.changed(0));
        watch.stop();
        assertFalse(watch.changed(3));
    }

    @Test public void speakerSelectionPreservesExternalAndConcurrentRoutes() {
        assertNull(LiveVoiceState.SpeakerRoute.select(3, true, 2));
        assertNull(LiveVoiceState.SpeakerRoute.select(2, false, 2));
        var route = LiveVoiceState.SpeakerRoute.select(1, false, 2);
        assertEquals(1, route.previous);
        assertTrue(route.owns(2));
        assertFalse(route.owns(3));
        assertFalse(route.owns(0));
        var noPrevious = LiveVoiceState.SpeakerRoute.select(0, false, 2);
        assertEquals(0, noPrevious.previous);
        assertTrue(noPrevious.owns(2));
    }

    @Test public void captureCreditIsBoundedAndReceiptsAreValidated() {
        LiveVoiceState state = new LiveVoiceState();
        state.capture(1);
        for (int i = 0; i < 100; i++) assertEquals(i, state.captured());
        assertThrows(IllegalStateException.class, state::captured);
        assertThrows(IllegalArgumentException.class, () -> state.acknowledge(0, 0));
        assertThrows(IllegalArgumentException.class, () -> state.acknowledge(1, 100));
        state.acknowledge(1, 49);
        assertEquals(100, state.captured());
        state.capture(2);
        assertEquals(0, state.captured());
        assertThrows(IllegalArgumentException.class, () -> state.capture(1));
    }

    @Test public void duplicateStopsDoNotDisruptPlaybackOnlyAudio() {
        LiveVoiceState state = new LiveVoiceState();
        state.capture(1);
        assertTrue(state.stopCapture(2));
        state.enqueue(0, 0, 480);
        assertFalse(state.stopCapture(2));
        assertFalse(state.resetOutput(0));
        assertEquals(0, state.played(480).get(0).id);
        assertThrows(IllegalArgumentException.class, () -> state.stopCapture(1));
        assertThrows(IllegalArgumentException.class, () -> state.capture(1));
        assertTrue(state.resetOutput(1));
        assertFalse(state.resetOutput(1));
    }

    @Test public void onlyConsumedFramesGetReceipts() {
        LiveVoiceState state = new LiveVoiceState();
        state.enqueue(0, 0, 480);
        state.enqueue(0, 1, 240);
        assertTrue(state.played(479).isEmpty());
        var first = state.played(480);
        assertEquals(1, first.size());
        assertEquals(0, first.get(0).id);
        assertEquals(480, first.get(0).samples);
        assertTrue(state.played(480).isEmpty());
        assertEquals(1, state.played(720).get(0).id);
        assertThrows(IllegalStateException.class, () -> state.played(721));
    }

    @Test public void outputIsBoundedAndResetDropsOldReceipts() {
        LiveVoiceState state = new LiveVoiceState();
        for (int i = 0; i < 50; i++) state.enqueue(0, i, 480);
        assertThrows(IllegalStateException.class, () -> state.enqueue(0, 50, 1));
        state.resetOutput(1);
        assertTrue(state.played(0).isEmpty());
        assertThrows(IllegalArgumentException.class, () -> state.enqueue(0, 51, 480));
        assertThrows(IllegalArgumentException.class, () -> state.enqueue(1, 51, 481));
        assertThrows(IllegalArgumentException.class, () -> state.enqueue(1, 51, 0));
        state.enqueue(1, 51, 480);
        assertThrows(IllegalArgumentException.class, () -> state.enqueue(1, 51, 480));
        assertThrows(IllegalArgumentException.class, () -> state.resetOutput(0));
        assertEquals(51, state.played(480).get(0).id);
    }
}
