// @vitest-environment jsdom
import { act, fireEvent, screen, waitFor, within } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

import type { Session } from '../lib/types';
import { listSession, renderSessionsScreen } from './sessions-screen-harness';

let restore = () => {};
afterEach(() => restore());

const first = listSession({ id: 'a1', title: 'First' });

/** `First` as the search answers it: a list row carrying where the query hit it. */
const matched = { ...first, match: { rank: 1, is_in_request: true, request_snippet: 'the needle' } };

const machines = (found: Session[]) => [
  {
    label: 'alpha',
    sessions: [first],
    routes: { '/v1/sessions/actions/search': { sessions: found, total: found.length } },
  },
];

/** One query parameter of every search request, in the order they were sent. */
const searchParams = (requests: { path: string }[], name: string) =>
  requests
    .filter((request) => request.path.startsWith('/v1/sessions/actions/search'))
    .map((request) => new URLSearchParams(request.path.split('?')[1]).get(name));

const searches = (requests: { path: string }[]) => searchParams(requests, 'q');

// Search is one ranked FTS query on the active machine and the gateway spends real
// time in SQLite before it answers, so the ONE thing this screen owes the network is
// restraint: ask once when typing rests, and never let a query the user has already
// replaced land on top of the one they are looking at.
describe('search asks the active gateway once per pause', () => {
  it('asks once for the query the typing rested on, not once per keystroke', async () => {
    const view = renderSessionsScreen({ machines: machines([matched]) });
    restore = view.restore;
    await screen.findByText('First');
    view.requests.length = 0;

    view.setQuery('n');
    view.setQuery('ne');
    view.setQuery('needle');
    await waitFor(() => expect(searches(view.requests)).toEqual(['needle']));
    // Nothing else lands after the pause either.
    await new Promise((resolve) => setTimeout(resolve, 250));
    expect(searches(view.requests)).toEqual(['needle']);
  });

  it('asks for the recents on a blank field, and never searches its spaces', async () => {
    const view = renderSessionsScreen({ machines: machines([matched]) });
    restore = view.restore;
    await screen.findByText('First');
    view.requests.length = 0;

    view.setQuery('   ');
    await new Promise((resolve) => setTimeout(resolve, 300));
    expect(searches(view.requests)).toEqual(['']);
  });

  it('cancels a superseded query: the flight the user replaced is aborted', async () => {
    const view = renderSessionsScreen({ machines: machines([matched]) });
    restore = view.restore;
    await screen.findByText('First');
    view.requests.length = 0;

    view.setQuery('first');
    await waitFor(() => expect(searches(view.requests)).toEqual(['first']));
    const superseded = view.requests.at(-1)!;
    view.setQuery('second');
    await waitFor(() => expect(searches(view.requests)).toEqual(['first', 'second']));
    // A response that outran its own cancellation must not be written on top of the
    // query the user is now looking at.
    expect(superseded.signal?.aborted).toBe(true);
  });

  // While a query is live the header reports the SEARCH instead of the scope's totals —
  // that tally is the only proof the query left this gateway.
  it('reports the search in the header, and says so when a machine has no hit', async () => {
    const view = renderSessionsScreen({ machines: machines([matched]) });
    restore = view.restore;
    await screen.findByText('First');
    expect(screen.getByText('1 session')).toBeVisible();

    view.setQuery('needle');
    expect(await screen.findByText('1 match')).toBeVisible();
  });

  it('says a machine has no hit, never "No sessions yet"', async () => {
    const view = renderSessionsScreen({ machines: machines([]) });
    restore = view.restore;
    await screen.findByText('First');

    view.setQuery('needle');
    await waitFor(() => expect(searches(view.requests)).toEqual(['needle']));
    await waitFor(() => expect(screen.getByText('No matching sessions')).toBeVisible());
    expect(screen.getByText('0 matches')).toBeVisible();
    const results = screen.getByRole('region', { name: 'Matching sessions' });
    expect(within(results).queryByText('First')).toBeNull();
    expect(document.body.textContent).not.toContain('No sessions yet');
  });

  // Regression, issue: a fleet search sat completely silent for as long as it took —
  // the row above the list said "0 matches" and the empty list said "No matching
  // sessions", both of them answers this screen did not have yet, and the real rows
  // appeared much later with no word in between about where the search was.
  it('says a search is IN FLIGHT before it can say what it found', async () => {
    const view = renderSessionsScreen({ machines: machines([matched]) });
    restore = view.restore;
    await screen.findByText('First');

    view.setQuery('needle');
    expect(await screen.findByText('searching...')).toBeVisible();
    // Not a result, so not a dead end either.
    expect(document.body.textContent).not.toContain('No matching sessions');
    await waitFor(() => expect(screen.getByText('1 match')).toBeVisible());
    expect(screen.queryByText('searching...')).toBeNull();
  });

  // Search follows the selected machine. An inactive destination must not add a request
  // or make the active machine's complete answer look unfinished.
  it('does not wait for an inactive machine', async () => {
    const view = renderSessionsScreen({
      machines: [
        ...machines([matched]),
        {
          label: 'beta',
          sessions: [listSession({ id: 'b1', title: 'Second' })],
          searchHangs: true,
        },
      ],
    });
    restore = view.restore;
    await screen.findByText('First');
    expect(screen.queryByText('Second')).toBeNull();
    view.requests.length = 0;

    view.setQuery('needle');
    expect(await screen.findByText('1 match')).toBeVisible();
    expect(searches(view.requests)).toEqual(['needle']);
    expect(document.body.textContent).not.toContain('machines...');
  });
});

// A machine outside the selected scope is not part of the question. In particular, a
// paired machine already known to be unreachable must not turn a complete local answer
// into a partial-fleet warning.
describe('an inactive dead machine is outside the search', () => {
  it('asks only the active machine', async () => {
    const view = renderSessionsScreen({
      machines: [
        ...machines([matched]),
        { label: 'beta', down: true, sessions: [listSession({ id: 'b1', title: 'Second' })] },
      ],
    });
    restore = view.restore;
    await screen.findByText('First');
    view.requests.length = 0;

    view.setQuery('needle');
    expect(await screen.findByText('1 match')).toBeVisible();
    expect(document.body.textContent).not.toContain('did not answer');
    expect(searches(view.requests)).toEqual(['needle']);
  });
});

describe('a search gives up on a silent machine on its own clock', () => {
  beforeEach(() => {
    vi.useFakeTimers();
  });
  afterEach(() => {
    vi.useRealTimers();
  });

  /** Let every request, timer and repaint that fits inside `ms` happen. */
  const settle = async (ms = 0) => {
    await act(async () => {
      await vi.advanceTimersByTimeAsync(ms);
    });
  };

  it('stops waiting on the selected machine long before the transport would', async () => {
    const view = renderSessionsScreen({
      machines: [
        {
          label: 'alpha',
          sessions: [listSession({ id: 'a1', title: 'First' })],
          searchHangs: true,
        },
      ],
    });
    restore = view.restore;
    await settle(50);

    view.setQuery('needle');
    await settle(300);
    expect(screen.getByText('searching...')).toBeVisible();

    // Seven seconds of silence is still a machine reading its transcripts.
    await settle(7_000);
    expect(screen.getByText('searching...')).toBeVisible();

    // Past the search's own deadline it is absence, not a transport-length wait.
    await settle(2_000);
    expect(screen.queryByText('searching...')).toBeNull();
    expect(screen.getByText('This machine did not answer.')).toBeVisible();
  });

  // A machine that stopped answering is not a machine that read its transcripts and
  // found nothing — the dead end the empty list offers has to say which one happened.
  it('says the machine never answered rather than that nothing matched', async () => {
    const view = renderSessionsScreen({
      machines: [
        {
          label: 'alpha',
          sessions: [listSession({ id: 'a1', title: 'First' })],
          hangs: true,
        },
      ],
    });
    restore = view.restore;
    await settle(50);

    view.setQuery('needle');
    // The query lands one PAUSE after the last keystroke (`SEARCH_DEBOUNCE_MS`), and the
    // search's own deadline starts from there.
    await settle(300);
    await settle(8_700);
    expect(screen.getByText('This machine did not answer.')).toBeVisible();
    expect(document.body.textContent).not.toContain('Nothing on any paired machine');
  });
});

// Regression, user report: an active machine that had just spent a whole search
// deadline in silence was asked again by the very next query, so every further pause
// bought another wait on a gateway already known to be dark.
describe('a machine already known to be dark is not asked again', () => {
  beforeEach(() => {
    vi.useFakeTimers();
  });
  afterEach(() => {
    vi.useRealTimers();
  });

  /** Let every request, timer and repaint that fits inside `ms` happen. */
  const settle = async (ms = 0) => {
    await act(async () => {
      await vi.advanceTimersByTimeAsync(ms);
    });
  };

  /** Alive to the list, silent to every search — dark in the only way a search can tell. */
  const machine = () => [
    {
      label: 'alpha',
      sessions: [listSession({ id: 'a1', title: 'First' })],
      searchHangs: true,
    },
  ];

  it('answers for a silent machine without spending a second request on it', async () => {
    const view = renderSessionsScreen({ machines: machine() });
    restore = view.restore;
    await settle(50);
    view.requests.length = 0;

    view.setQuery('needle');
    await settle(300);
    await settle(8_700);
    expect(searches(view.requests)).toEqual(['needle']);
    expect(screen.getByText('This machine did not answer.')).toBeVisible();

    view.requests.length = 0;
    view.setQuery('other');
    await settle(300);
    expect(searches(view.requests)).toEqual([]);
    expect(screen.getByText('This machine did not answer.')).toBeVisible();
  });

  it('asks it again as soon as a list read proves the machine alive', async () => {
    const view = renderSessionsScreen({ machines: machine() });
    restore = view.restore;
    await settle(50);

    view.setQuery('needle');
    await settle(300);
    await settle(8_700);
    expect(screen.getByText('This machine did not answer.')).toBeVisible();

    // The blackout is a memory of one failure, not a verdict on the machine: the 10s
    // poll's list read lands and the next search asks this machine again.
    view.requests.length = 0;
    await settle(2_000);
    view.setQuery('third');
    await settle(300);
    expect(searches(view.requests)).toEqual(['third']);
  });
});

// Regression, user report (paraphrased: "now it makes everything jump on every character —
// it is not natural and most likely not debounced"): the pause held back only the NETWORK.
// Every keystroke still re-filed the answers under the half-typed needle, so the transcript
// hits on screen were discarded and the rows they had put in the list vanished and came
// back a pause later — the list rearranging itself under the thumb one letter at a time.
describe('typing does not redraw the list under the thumb', () => {
  beforeEach(() => {
    vi.useFakeTimers();
  });
  afterEach(() => {
    vi.useRealTimers();
  });

  /** Let every request, timer and repaint that fits inside `ms` happen. */
  const settle = async (ms = 0) => {
    await act(async () => {
      await vi.advanceTimersByTimeAsync(ms);
    });
  };

  /** One machine, two sessions, and a transcript hit in exactly one of them. */
  const machine = () => [
    {
      label: 'alpha',
      sessions: [
        listSession({ id: 'a1', title: 'First' }),
        listSession({ id: 'a2', title: 'Later' }),
      ],
      routes: { '/v1/sessions/actions/search': { sessions: [matched] } },
    },
  ];

  /** The row in the results: the matching-messages pane repeats the picked row's title. */
  const listed = (title: string) =>
    within(screen.getByRole('region', { name: 'Matching sessions' })).getByText(title);

  it('holds the rows and the tally of the settled needle through the whole pause', async () => {
    const view = renderSessionsScreen({ machines: machine() });
    restore = view.restore;
    await settle(50);
    view.requests.length = 0;

    view.setQuery('need');
    await settle(300);
    // A transcript hit, not a title match: this row is on screen only because the answer
    // to "need" put it there, which is exactly what a keystroke used to throw away.
    expect(listed('First')).toBeVisible();
    expect(screen.getByText('1 match')).toBeVisible();
    expect(searches(view.requests)).toEqual(['need']);

    view.setQuery('needl');
    await settle(50);
    expect(listed('First')).toBeVisible();
    expect(screen.getByText('1 match')).toBeVisible();

    view.setQuery('needle');
    await settle(150);
    expect(listed('First')).toBeVisible();
    expect(screen.getByText('1 match')).toBeVisible();

    // One question for the word the typing rested on, not one per letter.
    await settle(300);
    expect(searches(view.requests)).toEqual(['need', 'needle']);
  });

  // A count is an ANSWER. Before a needle has been asked there is nothing filtered to
  // count, and printing the unfiltered list's size would be the screen answering a
  // question nobody has finished typing.
  it('publishes no tally for a needle nobody has rested on', async () => {
    const view = renderSessionsScreen({ machines: machine() });
    restore = view.restore;
    await settle(50);

    view.setQuery('n');
    await settle(50);
    expect(screen.getByText('searching...')).toBeVisible();
    expect(screen.queryByText('2 matches')).toBeNull();
    expect(screen.queryByText('1 match')).toBeNull();

    await settle(300);
    expect(screen.getByText('1 match')).toBeVisible();
  });

  // Regression, user report (paraphrased: "this looks awful on iPhone", with a screenshot
  // of the switch strip cut mid-address and "271 matches / 1 machine did not answer"
  // running to the very edge of the glass): the report stood in the trailing cluster of a
  // row that could not shrink, so on a 390px screen it ate the switch and then overran the
  // row's own 12px inset.
  it("gives the search report a line of its own instead of the switch's row", async () => {
    const view = renderSessionsScreen({ machines: machine() });
    restore = view.restore;
    await settle(50);

    view.setQuery('needle');
    await settle(300);
    const report = screen.getByText('1 match').closest('div');
    expect(report).toBeVisible();
    const line = report!.parentElement!;
    // The report stands on the line under the search choices, and that line wraps.
    expect(line.className).toContain('flex-wrap');
    expect(line.previousElementSibling).toContainElement(screen.getByRole('combobox', { name: 'Project' }));
    // And it shares no box with a machine switch or the machine's own verb.
    expect(line.querySelector('[role="group"][aria-label="Machines"]')).toBeNull();
    expect(report!.querySelector('[aria-label^="Projects on"]')).toBeNull();
  });
});

/** The rows the search dialog lists, in the order it paints them. */
const rowOrder = (region = 'Matching sessions') =>
  [...screen.getByRole('region', { name: region }).querySelectorAll('[data-session-id]')].map((row) =>
    row.getAttribute('data-session-id'),
  );

// Regression, user report (paraphrased: "the search results are not sorted by
// freshness — I care far more about freshness than about which band the hit
// landed in"). The screen sorted what a query matched by `SessionMatch.rank`,
// the gateway's relevance band, so every year-old session whose TITLE held the
// word sat above the one touched this morning and the dates jumped up and down
// the list. The gateway now answers freshest-first and the screen paints THAT
// order.
describe('search results are ordered by freshness', () => {
  const ancient = listSession({
    id: 'ancient',
    title: 'star charts',
    modified_at: '2024-01-02T10:00:00Z',
  });
  const today = listSession({ id: 'today', title: 'Deploy', modified_at: '2024-06-01T10:00:00Z' });
  const machines = [
    {
      label: 'alpha',
      sessions: [ancient, today],
      routes: {
        '/v1/sessions/actions/search': {
          // The gateway's own order: today's session first, matched in a REPLY
          // (band 2); the ancient one second, matched in its very TITLE (band 0).
          sessions: [
            { ...today, match: { rank: 2, is_in_reply: true, reply_snippet: 'a star to steer by' } },
            { ...ancient, match: { rank: 0, is_in_title: true } },
          ],
        },
      },
    },
  ];

  it('paints the freshest match first, whatever band it matched in', async () => {
    const view = renderSessionsScreen({ machines });
    restore = view.restore;
    await screen.findByText('Deploy');

    view.setQuery('star');
    await waitFor(() => expect(rowOrder()).toEqual(['today', 'ancient']));
  });

  it('keeps that order while the answer is the one being painted', async () => {
    const view = renderSessionsScreen({ machines });
    restore = view.restore;
    await screen.findByText('Deploy');

    view.setQuery('star');
    await waitFor(() => expect(rowOrder()).toEqual(['today', 'ancient']));
    await new Promise((resolve) => setTimeout(resolve, 250));
    expect(rowOrder()).toEqual(['today', 'ancient']);
  });
});

// The results answer "which session"; the pane beside them answers "where in it", the
// same split the terminal's session switcher draws. A phone stacks the pane under them.
describe('search shows the matching messages of one session beside the results', () => {
  const a1 = listSession({ id: 'a1', title: 'First' });
  const a2 = listSession({ id: 'a2', title: 'Later' });
  const found = [
    {
      label: 'alpha',
      sessions: [a1, a2],
      routes: {
        '/v1/sessions/actions/search': {
          sessions: [
            {
              ...a1,
              match: {
                rank: 1,
                is_in_request: true,
                hits: [{ side: 'request', snippet: 'Where is the **needle** now?', at: null }],
              },
            },
            {
              ...a2,
              match: {
                rank: 2,
                is_in_reply: true,
                hits: [{ side: 'reply', snippet: 'The needle is in the drawer.', at: null }],
              },
            },
          ],
        },
      },
    },
  ];
  const pane = () => screen.getByRole('region', { name: 'Matching messages' });
  const noPane = () => screen.queryByRole('region', { name: 'Matching messages' });
  const results = () => screen.getByRole('region', { name: 'Matching sessions' });
  const row = (sid: string) => results().querySelector<HTMLElement>(`[data-session-id="${sid}"]`)!;
  const opened = (onOpen: ReturnType<typeof vi.fn>) => onOpen.mock.calls.map((call) => call[1]);

  /** Mount, search for "needle" and wait until the first result's messages are painted. */
  const searched = async (onOpen = vi.fn()) => {
    const view = renderSessionsScreen({ machines: found, onOpen });
    restore = view.restore;
    await screen.findByText('First');
    expect(noPane()).toBeNull();
    view.setQuery('needle');
    await waitFor(() => expect(within(pane()).getByText('You')).toBeVisible());
    return view;
  };

  it('shows the messages of the first result, with the query words marked', async () => {
    const view = await searched();

    expect(within(pane()).getByRole('heading')).toHaveTextContent('First');
    expect(within(pane()).getByText('needle', { selector: 'mark' })).toBeVisible();
    expect(row('a1')).toHaveAttribute('aria-current', 'true');
    // The rows keep to "which session": no message text under them any more.
    expect(within(results()).queryByText(/now\?|drawer/)).toBeNull();
    // Side by side in a wide dialog, stacked under the results on a phone.
    const side = pane().parentElement!;
    expect(side.parentElement!.className).toContain('@min-[40rem]/search:flex-row');
    expect(side.className).toContain('border-t');
    expect(side.className).toContain('@min-[40rem]/search:border-l');

    view.setQuery('');
    await waitFor(() => expect(noPane()).toBeNull());
  });

  it('previews another session on the first press and opens it on the second', async () => {
    const onOpen = vi.fn();
    await searched(onOpen);

    fireEvent.click(row('a2'));
    await waitFor(() => expect(within(pane()).getByRole('heading')).toHaveTextContent('Later'));
    expect(within(pane()).getByText('Vis')).toBeVisible();
    expect(row('a2')).toHaveAttribute('aria-current', 'true');
    expect(row('a1')).not.toHaveAttribute('aria-current');
    expect(onOpen).not.toHaveBeenCalled();

    fireEvent.click(row('a2'));
    await waitFor(() => expect(opened(onOpen)).toEqual(['a2']));
  });

  it('opens the previewed session from its Open button or from one of its messages', async () => {
    const onOpen = vi.fn();
    await searched(onOpen);

    fireEvent.click(within(pane()).getByRole('button', { name: 'Open' }));
    fireEvent.click(within(pane()).getByRole('button', { name: /needle/ }));
    await waitFor(() => expect(opened(onOpen)).toEqual(['a1', 'a1']));
  });
});

// Regression, user report (paraphrased: "the session search should list the recent
// sessions, as the terminal's list does"): the dialog opened on an empty list that only
// said what to type. A blank field now asks the gateway's one search endpoint for the
// recents, and the dialog lists them in the order that answer gave.
describe('the search dialog opens on the recent sessions', () => {
  const older = listSession({ id: 'older', title: 'Older work', modified_at: '2024-01-02T10:00:00Z' });
  const newer = listSession({ id: 'newer', title: 'Newer work', modified_at: '2024-06-01T10:00:00Z' });
  /** One machine whose search answers a blank question with these recents. */
  const recents = (sessions: Session[]) => [
    {
      label: 'alpha',
      sessions: [older, newer],
      routes: { '/v1/sessions/actions/search': { query: '', sessions, total: sessions.length } },
    },
  ];
  const recent = (sid: string) =>
    screen
      .getByRole('region', { name: 'Recent sessions' })
      .querySelector<HTMLElement>(`[data-session-id="${sid}"]`)!;

  it('lists the recents in the order the gateway answered, with no messages pane', async () => {
    const view = renderSessionsScreen({ machines: recents([newer, older]), isSearchOpen: true });
    restore = view.restore;

    await waitFor(() => expect(rowOrder('Recent sessions')).toEqual(['newer', 'older']));
    expect(searches(view.requests)).toEqual(['']);
    expect(screen.queryByRole('region', { name: 'Matching messages' })).toBeNull();
  });

  it('opens a recent session on the first press', async () => {
    const onOpen = vi.fn();
    const view = renderSessionsScreen({
      machines: recents([newer, older]),
      isSearchOpen: true,
      onOpen,
    });
    restore = view.restore;
    await waitFor(() => expect(rowOrder('Recent sessions')).toEqual(['newer', 'older']));

    fireEvent.click(recent('older'));
    await waitFor(() => expect(onOpen.mock.calls.map((call) => call[1])).toEqual(['older']));
  });

  it('says there is no recent session once the machine has answered', async () => {
    const view = renderSessionsScreen({ machines: recents([]), isSearchOpen: true });
    restore = view.restore;

    expect(await screen.findByText('No recent sessions')).toBeVisible();
    expect(screen.getByText('Type a word from its title or messages.')).toBeVisible();
  });

  it('asks for the query instead once typing rests, and for the recents again when cleared', async () => {
    const view = renderSessionsScreen({ machines: recents([newer, older]), isSearchOpen: true });
    restore = view.restore;
    await waitFor(() => expect(searches(view.requests)).toEqual(['']));

    view.setQuery('work');
    await waitFor(() => expect(searches(view.requests)).toEqual(['', 'work']));
    view.setQuery('');
    await waitFor(() => expect(searches(view.requests)).toEqual(['', 'work', '']));
  });

  // The dialog used to fetch every hit by id, archived sessions included. A typed query
  // still finds a session the human put away; the recents stay the active work.
  it('reads the archive for a typed query, never for the recents', async () => {
    const view = renderSessionsScreen({ machines: recents([newer, older]), isSearchOpen: true });
    restore = view.restore;
    await waitFor(() => expect(searches(view.requests)).toEqual(['']));

    view.setQuery('work');
    await waitFor(() => expect(searches(view.requests)).toEqual(['', 'work']));
    expect(searchParams(view.requests, 'archived')).toEqual([null, 'include']);
  });
});

// Regression, user report (paraphrased: "in the search, the session highlighted by default
// should be the one I am in"). The dialog started on the freshest row, whichever session
// stood open beside it; the terminal's switcher starts on the session in use.
describe('the search starts on the session the reader is in', () => {
  const fresh = listSession({
    id: 'fresh',
    title: 'Fresh work',
    modified_at: '2024-06-01T10:00:00Z',
    workspace: { root: '/Users/dev/alpha' },
  });
  const inUse = listSession({
    id: 'in-use',
    title: 'Work in use',
    modified_at: '2024-01-02T10:00:00Z',
    workspace: { root: '/Users/dev/zulu' },
  });
  /** `session` as the search answers it: a list row carrying one message that matched. */
  const hit = (session: Session, snippet: string) => ({
    ...session,
    match: { rank: 1, is_in_request: true, hits: [{ side: 'request', snippet, at: null }] },
  });
  /** One machine holding both sessions, whose search answers with `sessions`. */
  const holding = (sessions: object[]) => [
    {
      label: 'desk',
      sessions: [fresh, inUse],
      routes: { '/v1/sessions/actions/search': { sessions, total: sessions.length } },
    },
  ];
  const pane = () => screen.getByRole('region', { name: 'Matching messages' });
  const row = (region: string, sid: string) =>
    screen.getByRole('region', { name: region }).querySelector<HTMLElement>(`[data-session-id="${sid}"]`)!;
  it('shows the recents as explicit rows, including the session in use outside the window', async () => {
    const view = renderSessionsScreen({ machines: holding([fresh]), isSearchOpen: true });
    restore = view.restore;
    view.setOpenSession({ conn: view.conns[0], sid: 'in-use' });

    await waitFor(() => expect(rowOrder('Recent sessions')).toEqual(['in-use', 'fresh']));
    const results = within(screen.getByRole('region', { name: 'Recent sessions' }));
    expect(results.getByText('Project: zulu')).toBeVisible();
    expect(results.getByText('Project: alpha')).toBeVisible();
    expect(row('Recent sessions', 'in-use')).toHaveAttribute('aria-current', 'page');
  });

  it('previews it when the query matches it, ahead of a fresher match', async () => {
    const view = renderSessionsScreen({
      machines: holding([hit(fresh, 'Fresh **work** today'), hit(inUse, 'The **work** I am in')]),
    });
    restore = view.restore;
    view.setOpenSession({ conn: view.conns[0], sid: 'in-use' });
    await screen.findByText('Fresh work');

    view.setQuery('work');
    await waitFor(() => expect(rowOrder()).toEqual(['in-use', 'fresh']));
    expect(within(pane()).getByRole('heading')).toHaveTextContent('Work in use');
    expect(row('Matching sessions', 'in-use')).toHaveAttribute('aria-current', 'page');
    expect(row('Matching sessions', 'fresh')).not.toHaveAttribute('aria-current');
  });

  it('starts on the first result when the query does not match it', async () => {
    const view = renderSessionsScreen({ machines: holding([hit(fresh, 'Fresh **work** today')]) });
    restore = view.restore;
    view.setOpenSession({ conn: view.conns[0], sid: 'in-use' });
    await screen.findByText('Fresh work');

    view.setQuery('fresh');
    await waitFor(() => expect(rowOrder()).toEqual(['fresh']));
    expect(within(pane()).getByRole('heading')).toHaveTextContent('Fresh work');
    expect(row('Matching sessions', 'fresh')).toHaveAttribute('aria-current', 'true');
  });
 });

/** Picks one choice from the app's own picker. */
async function choose(picker: HTMLElement, option: string) {
  await userEvent.click(picker);
  await userEvent.click(await screen.findByRole('option', { name: option }));
}

// The search scope belongs on the gateway request, not on the held list window.
describe('explicit search locations and scopes', () => {
  it('names each project and group, offers groups only inside a project and sends OR-group scopes', async () => {
    const rows = [
      listSession({ id: 'one', title: 'Needle one', project_id: 'p1', project_name: 'Workbench', group_id: 'g1' }),
      listSession({ id: 'two', title: 'Needle two', project_id: 'p1', project_name: 'Workbench', group_id: 'g2' }),
      listSession({ id: 'other', title: 'Needle other', project_id: 'p2', project_name: 'Archive', group_id: 'g3' }),
    ];
    const view = renderSessionsScreen({ machines: [{
      label: 'alpha', sessions: rows,
      routes: {
        '/v1/projects': { projects: [
          { id: 'p1', name: 'Workbench', workspace_root: '/work' },
          { id: 'p2', name: 'Archive', workspace_root: '/archive' },
        ] },
        '/v1/session-groups': { groups: [
          { id: 'g1', name: 'Planning', project_id: 'p1', color: 'red' },
          { id: 'g2', name: 'Review', project_id: 'p1', color: 'blue' },
          { id: 'g3', name: 'History', project_id: 'p2', color: 'green' },
        ] },
      },
    }] });
    restore = view.restore;
    view.setQuery('needle');
    const results = await screen.findByRole('region', { name: 'Matching sessions' });
    await waitFor(() => expect(within(results).getByText('Needle one')).toBeVisible());
    expect(within(results).getAllByText('Project: Workbench')[0]).toBeVisible();
    expect(within(results).getByText('Group: Planning')).toBeVisible();
    const project = await screen.findByRole('combobox', { name: 'Project' });
    // With one machine there is no machine to choose.
    expect(screen.queryByRole('combobox', { name: 'Machine' })).not.toBeInTheDocument();
    // All projects already means all groups, so groups appear only inside a project.
    expect(screen.queryByRole('combobox', { name: 'Groups' })).not.toBeInTheDocument();
    await choose(project, 'Workbench');
    await waitFor(() => expect(searchParams(view.requests, 'project_id').at(-1)).toBe('p1'));
    const groups = screen.getByRole('combobox', { name: 'Groups' });
    await userEvent.click(groups);
    expect(screen.getByRole('listbox', { name: 'Groups' })).toHaveAttribute('aria-multiselectable', 'true');
    await userEvent.click(screen.getByRole('option', { name: 'Planning' }));
    await userEvent.click(screen.getByRole('option', { name: 'Review' }));
    await waitFor(() => expect(searchParams(view.requests, 'group_ids').at(-1)?.split(',').sort()).toEqual(['g1', 'g2']));
    await userEvent.keyboard('{Escape}');
    expect(groups).toHaveTextContent('Planning, Review');
    expect(screen.getByRole('dialog', { name: 'Search sessions' })).toBeInTheDocument();
    const oldRequest = view.requests.filter((request) => request.path.startsWith('/v1/sessions/actions/search')).at(-1)!;
    // All projects widens the search again: it drops the groups with the project, so no
    // separate control repeats it.
    await choose(project, 'All projects');
    await waitFor(() => expect(searchParams(view.requests, 'group_ids').at(-1)).toBeNull());
    expect(searchParams(view.requests, 'project_id').at(-1)).toBeNull();
    expect(oldRequest.signal?.aborted).toBe(true);
    expect(project).toHaveTextContent('All projects');
    expect(screen.queryByRole('combobox', { name: 'Groups' })).not.toBeInTheDocument();
    expect(screen.queryByRole('button', { name: /clear filters|search everything/i })).not.toBeInTheDocument();
  });
  it('pages the selected project and groups and preserves unfiled and empty-project scopes', async () => {
    const rows = Array.from({ length: 60 }, (_, index) => listSession({
      id: `hit-${index}`, title: `Needle ${index}`, project_id: 'p1', project_name: 'Workbench',
      group_id: index % 2 === 0 ? 'g1' : 'g2', workspace: { root: '/work' },
    }));
    rows.push(listSession({ id: 'other', title: 'Needle other', project_id: 'p2', workspace: { root: '/other' } }));
    rows.push(listSession({ id: 'unfiled', title: 'Needle unfiled', workspace: { root: '' } }));
    const view = renderSessionsScreen({ machines: [{
      sessions: rows,
      routes: {
        '/v1/projects': { projects: [
          { id: 'p1', name: 'Workbench', workspace_root: '/work' },
          { id: 'p2', name: 'Other', workspace_root: '/other' },
          { id: 'empty', name: 'Empty project', workspace_root: '/empty', archived_at: 1 },
        ] },
        '/v1/session-groups': { groups: [
          { id: 'g1', name: 'Planning', project_id: 'p1' },
          { id: 'g2', name: 'Review', project_id: 'p1' },
        ] },
      },
    }] });
    restore = view.restore;
    view.setQuery('needle');
    const results = await screen.findByRole('region', { name: 'Matching sessions' });
    const project = await screen.findByRole('combobox', { name: 'Project' });
    await userEvent.click(project);
    await screen.findByRole('option', { name: 'Empty project' });
    await userEvent.click(screen.getByRole('option', { name: 'Workbench' }));
    await userEvent.click(screen.getByRole('combobox', { name: 'Groups' }));
    await userEvent.click(await screen.findByRole('option', { name: 'Planning' }));
    await userEvent.click(screen.getByRole('option', { name: 'Review' }));
    await userEvent.keyboard('{Escape}');
    await waitFor(() => expect(screen.getByText('50 of 60 matches')).toBeVisible());
    expect(within(results).queryByText('Needle 59')).not.toBeInTheDocument();
    expect(within(results).queryByText('Needle other')).not.toBeInTheDocument();
    await userEvent.click(within(results).getByRole('button', { name: 'Load more results' }));
    await waitFor(() => expect(within(results).getByText('Needle 59')).toBeVisible());
    expect(screen.getByText('60 matches')).toBeVisible();
    expect(within(results).queryByRole('button', { name: 'Load more results' })).not.toBeInTheDocument();
    expect(within(results).getAllByText('Project: Workbench')).toHaveLength(60);
    const last = view.requests.filter((request) => request.path.startsWith('/v1/sessions/actions/search')).at(-1)!;
    const params = new URLSearchParams(last.path.split('?')[1]);
    expect(params.get('project_id')).toBe('p1');
    expect(params.get('group_ids')).toBe('g1,g2');
    expect(params.get('after')).toBe('50');
    await choose(project, 'No project');
    await waitFor(() => expect(within(results).getByText('Needle unfiled')).toBeVisible());
    expect(within(results).getByText('Project: No project')).toBeVisible();
    expect(within(results).getByText('Group: No group')).toBeVisible();
    expect(within(results).queryByText('Needle 0')).not.toBeInTheDocument();
    expect(searchParams(view.requests, 'root').at(-1)).toBe('');
    expect(searchParams(view.requests, 'group_ids').at(-1)).toBeNull();
    await choose(project, 'Empty project');
    await waitFor(() => expect(within(results).queryByText('Needle unfiled')).not.toBeInTheDocument());
    await screen.findByText('No matching sessions');
  });

  it('discards a late continuation when the project changes without changing the query', async () => {
    const rows = Array.from({ length: 51 }, (_, index) => listSession({
      id: `old-${index}`, title: `Needle old ${index}`, project_id: 'p1', workspace: { root: '/work' },
    }));
    rows.push(listSession({ id: 'unfiled', title: 'Needle unfiled', workspace: { root: '' } }));
    const view = renderSessionsScreen({ machines: [{ sessions: rows,
      routes: { '/v1/projects': { projects: [{ id: 'p1', name: 'Workbench', workspace_root: '/work' }] } },
    }] });
    restore = view.restore;
    view.setQuery('needle');
    const project = await screen.findByRole('combobox', { name: 'Project' });
    await choose(project, 'Workbench');
    await screen.findByText('50 of 51 matches');
    const fetch = globalThis.fetch;
    let release: ((answer: Response) => void) | undefined;
    let lateAnswer: Response | undefined;
    globalThis.fetch = async (input, init) => {
      const answer = await fetch(input, init);
      if (String(input).includes('after=')) {
        lateAnswer = answer;
        return new Promise<Response>((resolve) => { release = resolve; });
      }
      return answer;
    };
    fireEvent.click(screen.getByRole('button', { name: 'Load more results' }));
    await waitFor(() => expect(typeof release).toBe('function'));
    const oldRequest = view.requests.filter((request) => request.path.includes('after=')).at(-1)!;
    await choose(project, 'No project');
    const results = screen.getByRole('region', { name: 'Matching sessions' });
    await waitFor(() => expect(within(results).getByText('Needle unfiled')).toBeVisible());
    expect(oldRequest.signal?.aborted).toBe(true);
    await act(async () => { release!(lateAnswer!); });
    expect(within(results).queryByText('Needle old 50')).not.toBeInTheDocument();
    expect(within(results).getByText('Needle unfiled')).toBeVisible();
  });

  // Regression, user report (paraphrased: the search looked bad and hard to use on an
  // iPhone, and its pickers were the system's, not the app's): the machine strip ran off
  // the glass inside the dialog. The search now names its machine in the app's own picker.
  it('chooses the machine from a picker that lists a silent machine without letting it be chosen', async () => {
    const view = renderSessionsScreen({ machines: [
      { label: 'alpha', sessions: [listSession({ id: 'a1', title: 'Needle alpha' })] },
      { label: 'gamma', sessions: [listSession({ id: 'g1', title: 'Needle gamma' })] },
      { label: 'beta', down: true, sessions: [listSession({ id: 'b1', title: 'Needle beta' })] },
    ] });
    restore = view.restore;
    view.setQuery('needle');
    const results = await screen.findByRole('region', { name: 'Matching sessions' });
    await waitFor(() => expect(within(results).getByText('Needle alpha')).toBeVisible());
    const dialog = screen.getByRole('dialog', { name: 'Search sessions' });
    expect(within(dialog).queryByRole('group', { name: 'Machines' })).not.toBeInTheDocument();
    const machine = within(dialog).getByRole('combobox', { name: 'Machine' });
    expect(machine).toHaveTextContent('alpha');
    await userEvent.click(machine);
    await waitFor(() => expect(screen.getByRole('option', { name: 'beta · Not answering' }))
      .toHaveAttribute('aria-disabled', 'true'));
    await userEvent.click(screen.getByRole('option', { name: 'gamma' }));
    await waitFor(() => expect(within(results).getByText('Needle gamma')).toBeVisible());
    expect(within(results).queryByText('Needle alpha')).not.toBeInTheDocument();
    expect(machine).toHaveTextContent('gamma');
  });
});
