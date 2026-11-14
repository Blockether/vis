import json

from studio.app import application


def call(path, method="GET"):
    """Call the WSGI application. Return the status code, the headers and the body."""
    seen = {}

    def start_response(status, headers):
        seen.update(status=int(status.split()[0]), headers=dict(headers))

    body = b"".join(application({"REQUEST_METHOD": method, "PATH_INFO": path}, start_response))
    return seen["status"], seen["headers"], body


def test_the_page_loads_its_own_files():
    status, headers, body = call("/")
    assert status == 200 and headers["Content-Type"].startswith("text/html")
    assert b'src="public/app.js"' in body
    assert call("/public/app.js")[0] == 200 and call("/public/app.css")[0] == 200


def test_the_api_lists_the_songs_and_gives_the_notes_of_one():
    listed = json.loads(call("/api/songs")[2])
    song = json.loads(call(f"/api/songs/{listed[0]['id']}")[2])
    assert song["beats"] > 0
    assert all(track["notes"] for track in song["tracks"])


def test_a_song_is_a_midi_file():
    status, headers, body = call("/songs/night-drive.mid")
    assert (status, headers["Content-Type"], body[:4]) == (200, "audio/midi", b"MThd")


def test_the_application_refuses_what_it_does_not_serve():
    assert call("/api/songs/none")[0] == 404
    assert call("/public/../app.py")[0] == 404
    assert call("/", "POST")[0] == 405
