// The page of the studio: a piano roll and a WebAudio player. There is no build step.
"use strict";

const COLORS = ["#f2a33a", "#5ac8c8", "#c78bf2", "#8bd450", "#f26d6d", "#6d9cf2"];
const state = { song: null, audio: null, started: 0, frame: null };
const el = (id) => document.getElementById(id);
const say = (text) => { el("status").textContent = text; };

async function read(url) {
  try {
    const answer = await fetch(url);
    if (!answer.ok) throw new Error(`status ${answer.status}`);
    return await answer.json();
  } catch (error) {
    say(`The studio cannot read ${url}: ${error.message}`);
    return null;
  }
}

// Piano roll

function draw(song, beat) {
  const canvas = el("roll");
  const g = canvas.getContext("2d");
  const { width, height } = canvas;
  const pitches = song.tracks.flatMap((track) => track.notes.map((note) => note.pitch));
  const low = Math.min(...pitches) - 1;
  const high = Math.max(...pitches) + 2;
  const x = (beats) => (width * beats) / song.beats;
  const row = height / (high - low);
  g.fillStyle = "#0e0d0c";
  g.fillRect(0, 0, width, height);
  g.fillStyle = "#1d1b19";
  for (let bar = 0; bar < song.beats; bar += 4) g.fillRect(x(bar), 0, 1, height);
  song.tracks.forEach((track, index) => {
    g.fillStyle = COLORS[index % COLORS.length];
    for (const note of track.notes) {
      g.fillRect(x(note.time), row * (high - note.pitch - 1), Math.max(2, x(note.duration) - 1), Math.max(2, row - 1));
    }
  });
  if (beat !== null) {
    g.fillStyle = "#e8e6e3";
    g.fillRect(x(beat), 0, 2, height);
  }
}

// Sound

const hertz = (pitch) => 440 * 2 ** ((pitch - 69) / 12);

// Connect a node through a gain that fades from a level to silence.
function envelope(audio, node, at, length, level) {
  const gain = audio.createGain();
  gain.gain.setValueAtTime(0.0001, at);
  gain.gain.exponentialRampToValueAtTime(level, at + 0.01);
  gain.gain.exponentialRampToValueAtTime(0.0001, at + length);
  node.connect(gain);
  gain.connect(audio.destination);
}

function tone(audio, wave, at, length, level, frequency) {
  const oscillator = audio.createOscillator();
  oscillator.type = wave;
  oscillator.frequency.setValueAtTime(frequency, at);
  envelope(audio, oscillator, at, length, level);
  oscillator.start(at);
  oscillator.stop(at + length + 0.05);
  return oscillator;
}

function noiseBuffer(audio) {
  const size = Math.floor(0.2 * audio.sampleRate);
  const buffer = audio.createBuffer(1, size, audio.sampleRate);
  const samples = buffer.getChannelData(0);
  for (let i = 0; i < size; i += 1) samples[i] = 2 * Math.random() - 1;
  return buffer;
}

function noise(audio, buffer, at, length, level, cutoff) {
  const source = audio.createBufferSource();
  const filter = audio.createBiquadFilter();
  source.buffer = buffer;
  filter.type = "highpass";
  filter.frequency.setValueAtTime(cutoff, at);
  source.connect(filter);
  envelope(audio, filter, at, length, level);
  source.start(at);
}

// General MIDI drum notes: 36 is the kick, 38 is the snare, each other note is a hi-hat.
function drum(audio, buffer, at, level, pitch) {
  if (pitch === 36) {
    tone(audio, "sine", at, 0.25, 1.6 * level, 140).frequency.exponentialRampToValueAtTime(45, at + 0.2);
  } else if (pitch === 38) {
    noise(audio, buffer, at, 0.16, level, 1500);
  } else {
    noise(audio, buffer, at, 0.05, 0.5 * level, 7000);
  }
}

// The oscillator of a General MIDI program: basses are round, leads are bright.
function wave(program) {
  if (program >= 32 && program <= 39) return "triangle";
  if (program >= 80 && program <= 95) return "sawtooth";
  if (program >= 16 && program <= 23) return "square";
  return "sine";
}

function stop() {
  if (state.audio) state.audio.close();
  if (state.frame) cancelAnimationFrame(state.frame);
  state.audio = null;
  state.frame = null;
  if (state.song) draw(state.song, null);
}

// Move the play position until the song ends.
function follow() {
  const { song, audio, started } = state;
  if (!audio) return;
  const beat = (audio.currentTime - started) / (60 / song.bpm);
  if (beat >= song.beats) {
    stop();
    return;
  }
  draw(song, Math.max(0, beat));
  state.frame = requestAnimationFrame(follow);
}

function play() {
  stop();
  const song = state.song;
  if (!song) return;
  const audio = new (window.AudioContext || window.webkitAudioContext)();
  const buffer = noiseBuffer(audio);
  const start = audio.currentTime + 0.1;
  const seconds = (beats) => (beats * 60) / song.bpm;
  for (const track of song.tracks) {
    for (const note of track.notes) {
      const at = start + seconds(note.time);
      const level = 0.22 * ((note.velocity || 90) / 127);
      if (track.drums) drum(audio, buffer, at, level, note.pitch);
      else tone(audio, wave(track.program), at, Math.max(0.08, seconds(note.duration)), level, hertz(note.pitch));
    }
  }
  state.audio = audio;
  state.started = start;
  follow();
}

// Page

function show(song) {
  stop();
  state.song = song;
  el("song").hidden = false;
  el("title").textContent = song.title;
  const notes = song.tracks.reduce((sum, track) => sum + track.notes.length, 0);
  el("facts").textContent = `${song.key}, ${song.bpm} beats for each minute, ${song.beats} beats, ${notes} notes`;
  el("download").href = `songs/${song.id}.mid`;
  el("legend").replaceChildren(...song.tracks.map((track, index) => {
    const item = document.createElement("li");
    item.textContent = track.name;
    item.style.setProperty("--part", COLORS[index % COLORS.length]);
    return item;
  }));
  for (const button of document.querySelectorAll("#songs button")) {
    button.setAttribute("aria-pressed", String(button.value === song.id));
  }
  draw(song, null);
  say("");
}

async function open(id) {
  const song = await read(`api/songs/${id}`);
  if (song) show(song);
}

async function start() {
  const songs = await read("api/songs");
  if (!songs) return;
  el("songs").replaceChildren(...songs.map((song) => {
    const button = document.createElement("button");
    button.type = "button";
    button.value = song.id;
    button.textContent = song.title;
    button.addEventListener("click", () => open(song.id));
    return button;
  }));
  if (songs.length) open(songs[0].id);
}

el("play").addEventListener("click", play);
el("stop").addEventListener("click", stop);
start();
