import pytest
from music21 import midi

from studio import songs


def read(item):
    """Return the note-on events and the tempo events of the MIDI file of a song."""
    file = midi.MidiFile()
    file.readstr(songs.midi_file(item))
    events = [event for track in file.tracks for event in track.events]
    notes = [event for event in events if event.type == midi.ChannelVoiceMessages.NOTE_ON and event.velocity]
    return notes, [event for event in events if event.type == midi.MetaEvents.SET_TEMPO]


@pytest.mark.parametrize("item", songs.CATALOG, ids=lambda item: item.id)
def test_the_midi_file_has_each_note_and_the_tempo(item):
    notes, tempos = read(item)
    assert len(notes) == sum(len(track["notes"]) for track in songs.tracks(item)) > 0
    assert [int.from_bytes(event.data, "big") for event in tempos] == [round(60_000_000 / item.bpm)]


@pytest.mark.parametrize("item", songs.CATALOG, ids=lambda item: item.id)
def test_only_the_drums_use_channel_10(item):
    notes, _ = read(item)
    drum_notes = sum(len(track["notes"]) for track in songs.tracks(item) if track["drums"])
    assert sum(event.channel == 10 for event in notes) == drum_notes


def test_the_round_starts_the_second_voice_one_bar_later():
    flute, clarinet, _ = songs.tracks(songs.song("row-row"))
    assert [note["pitch"] for note in flute["notes"]] == [note["pitch"] for note in clarinet["notes"]]
    assert clarinet["notes"][0]["time"] - flute["notes"][0]["time"] == 4


def test_a_scale_step_above_the_octave_keeps_the_key():
    from music21 import key

    assert [songs.step(key.Key("C"), number).nameWithOctave for number in (0, 7, -3)] == ["C4", "C5", "G3"]
