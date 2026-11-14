# vis-playground

A MIDI studio in Python. Each song is code: [music21](https://www.music21.org/) turns keys,
scale steps and chord symbols into notes. The studio shows the notes as a piano roll, plays
them in the browser and gives each song as a Standard MIDI File.

This is the project that the [Vis playground](https://github.com/Blockether/vis/tree/main/examples/aws-playground)
works on. A Vis session on AWS Lambda gets a clone of the `main` branch. It can read, change
and test the code, and publish its commits back to this repository. The Studio page of the
playground runs the app in its own Lambda function and shows each change of a session at once.

## Files

- `src/studio/songs.py` has the songs and the catalog. Change a line there and the music changes.
- `src/studio/app.py` is the web app (WSGI, standard library only): the page, the songs as JSON and the MIDI files.
- `src/studio/public/` has the page: a piano roll and a player on the Web Audio API. It has no build step.
- `tests/` has the tests. They read the MIDI file of each song of the catalog.

## Run it

```sh
python3 -m pip install music21 pytest
PYTHONPATH=src python3 -m studio          # the page, on http://127.0.0.1:8090/
PYTHONPATH=src python3 -m studio render   # the MIDI file of each song, in out/
python3 -m pytest
```

## Add a song

1. Write a function in `src/studio/songs.py` that returns the parts of the song. `part` makes a
   part with a General MIDI program, `drums` makes the drum part and `line` puts notes one after
   the other. Time is in quarter notes.
2. Add one `Song` with a new id to `CATALOG`.
3. Run the tests: they check each song of the catalog.

The project holds no setting of its deployment. The playground stack decides how the app runs.
