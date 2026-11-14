"""Songs as code. music21 turns keys, scale degrees and chord symbols into notes.

A song is a function that returns the parts of a score. Time is in quarter notes.
Change a line here and the studio plays the new music.
"""

from __future__ import annotations

import functools
from collections.abc import Callable, Iterable, Iterator
from dataclasses import dataclass
from fractions import Fraction
from typing import Any

from music21 import chord, instrument, key, midi, note, pitch, roman, scale, stream, tempo

# General MIDI drum notes. A drum part uses MIDI channel 10.
KICK, SNARE, HAT = 36, 38, 42
THIRD, TWO_THIRDS = Fraction(1, 3), Fraction(2, 3)

Event = tuple[Any, Any, Any]


def line(lengths: Iterable[Any], pitches: Iterable[Any], start: Any = 0) -> Iterator[Event]:
    """Yield (start, length, pitch) for notes that follow each other. A pitch of None is a rest."""
    for length, sound in zip(lengths, pitches, strict=False):
        yield start, length, sound
        start += length


def step(tonality: key.Key, number: int, octaves: int = 0) -> pitch.Pitch:
    """Return the pitch of a scale step. Step 0 is the tonic, step 7 is the tonic one octave up."""
    return tonality.pitchFromDegree(number % 7 + 1).transpose(12 * (number // 7 + octaves))


def part(name: str, program: int, velocity: int, events: Iterable[Event]) -> stream.Part:
    """Return a part with a General MIDI program. The pitches of an event are one pitch or a list of pitches."""
    made = stream.Part()
    made.partName = name
    made.insert(0, instrument.instrumentFromMidiProgram(program))
    for start, length, sound in events:
        if sound is None:
            continue
        item = chord.Chord(sound) if isinstance(sound, (list, tuple)) else note.Note(sound)
        item.quarterLength = length
        item.volume.velocity = velocity
        made.insert(start, item)
    return made


def drums(velocity: int, events: Iterable[Event]) -> stream.Part:
    """Return a drum part. Its pitches are General MIDI drum notes."""
    made = part("Drums", 0, velocity, events)
    kit = instrument.Percussion()
    kit.midiChannel = 9
    made.replace(made.getInstrument(), kit)
    return made


def row_row() -> list[stream.Part]:
    """Row, row, row your boat as a round: the clarinet starts one bar after the flute."""
    tonality = key.Key("C")
    lengths = [1, 1, TWO_THIRDS, THIRD, 1, TWO_THIRDS, THIRD, TWO_THIRDS, THIRD, 2]
    lengths += [THIRD] * 12 + [TWO_THIRDS, THIRD, TWO_THIRDS, THIRD, 2]
    steps = [0, 0, 0, 1, 2, 2, 1, 2, 3, 4, 7, 7, 7, 4, 4, 4, 2, 2, 2, 0, 0, 0, 4, 3, 2, 1, 0]
    melody = [step(tonality, number) for number in steps]
    bass = [step(tonality, number, -2) for number in [0, -3, 0] * 5]
    return [
        part("Flute", 73, 100, line(lengths, melody)),
        part("Clarinet", 71, 84, line(lengths, melody, start=4)),
        part("Bass", 32, 92, line([1, 1, 2] * 5, bass)),
    ]


def night_drive() -> list[stream.Part]:
    """Four chords in A minor: a pad, an arpeggio, a bass and a drum machine."""
    pad, arpeggio, bass = [], [], []
    for bar, figure in enumerate(["i", "VI", "III", "VII"] * 2):
        start = 4 * bar
        root, third, fifth = roman.RomanNumeral(figure, "a").pitches
        pad.append((start, 4, [tone.transpose(-12) for tone in (root, third, fifth)]))
        arpeggio += line(
            [0.5] * 8, [root, third, fifth, root.transpose(12), fifth, third, fifth, root.transpose(12)], start
        )
        bass += line([1] * 4, [root.transpose(-24)] * 4, start)
    beat = list(line([1] * 32, [KICK, SNARE] * 16)) + list(line([0.5] * 64, [HAT] * 64))
    return [
        part("Pad", 89, 60, pad),
        part("Arpeggio", 81, 88, arpeggio),
        part("Bass", 38, 100, bass),
        drums(96, beat),
    ]


def twelve_bar() -> list[stream.Part]:
    """A twelve-bar blues in A: a walking bass under one riff of the blues scale."""
    blues = scale.ConcreteScale(pitches=["A4", "C5", "D5", "E-5", "E5", "G5"])
    riff = [blues.pitchFromDegree(degree) if degree else None for degree in [1, 3, 4, None, 5, 4]]
    walk = []
    for bar, figure in enumerate(["I", "I", "I", "I", "IV", "IV", "I", "I", "V", "IV", "I", "V"]):
        root, third, fifth = (tone.transpose(-24) for tone in roman.RomanNumeral(figure, "A").pitches)
        walk += line([1] * 4, [root, third, fifth, root.transpose(9)], 4 * bar)
    beat = list(line([1] * 48, [KICK, SNARE] * 24)) + list(line([TWO_THIRDS, THIRD] * 48, [HAT] * 96))
    return [
        part("Organ", 16, 90, line([0.5, 0.5, 1, 0.5, 0.5, 1] * 12, riff * 12)),
        part("Bass", 32, 104, walk),
        drums(88, beat),
    ]


@dataclass(frozen=True)
class Song:
    """One song of the studio."""

    id: str
    title: str
    key: str
    bpm: int
    compose: Callable[[], list[stream.Part]]


CATALOG = [
    Song("row-row", "Row, row, row your boat", "C major", 100, row_row),
    Song("night-drive", "Night drive", "A minor", 112, night_drive),
    Song("twelve-bar", "Twelve-bar blues", "A blues", 120, twelve_bar),
]


def song(name: str) -> Song | None:
    """Return the song with this id, or None."""
    return next((item for item in CATALOG if item.id == name), None)


@functools.cache
def score(item: Song) -> stream.Score:
    """Return the score of a song, with its tempo."""
    made = stream.Score()
    for index, voice in enumerate(item.compose()):
        if index == 0:
            voice.insert(0, tempo.MetronomeMark(number=item.bpm))
        made.insert(0, voice)
    return made


def summary(item: Song) -> dict[str, Any]:
    """Return the facts of a song for a list."""
    return {
        "id": item.id,
        "title": item.title,
        "key": item.key,
        "bpm": item.bpm,
        "beats": float(score(item).highestTime),
    }


def tracks(item: Song) -> list[dict[str, Any]]:
    """Return the parts of a song with plain numbers, for the page."""
    found = []
    for voice in score(item).parts:
        sound = voice.getInstrument()
        notes = [
            {
                "time": float(event.offset),
                "duration": float(event.quarterLength),
                "pitch": tone.midi,
                "velocity": event.volume.velocity,
            }
            for event in voice.flatten().notes
            for tone in event.pitches
        ]
        found.append(
            {"name": voice.partName, "program": sound.midiProgram or 0, "drums": sound.midiChannel == 9, "notes": notes}
        )
    return found


def midi_file(item: Song) -> bytes:
    """Return the bytes of the Standard MIDI File of a song."""
    return midi.translate.streamToMidiFile(score(item)).writestr()
