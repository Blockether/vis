// @vitest-environment jsdom
// Regression, user report: in a pause on a phone, a double tap on the one-time
// code could not paste, the keyboard jumped at every digit, and a short trip out
// of the app erased every answer typed so far.
import { act, cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { useState } from 'react';
import { afterEach, describe, expect, it } from 'vitest';
import { HumanInputPrompt, HumanInputSheet } from './HumanInputPrompt';
import { HUMAN_INPUT_REQUESTS } from '../dev/humanInputVariants';
import {
  initialHumanInputValues,
  type HumanInputRequest,
  type HumanInputValues,
} from '../lib/human-input';
import type { GatewayClient } from '../lib/gateway';
import type { SessionSubscriptionHub } from '../lib/subscriptions';

afterEach(cleanup);

const noop = () => {};

/** The sheet with real state behind it, so typed digits come back as values. */
function OtpSheet() {
  const request = HUMAN_INPUT_REQUESTS.otp;
  const [values, setValues] = useState<HumanInputValues>(() => initialHumanInputValues(request));
  return (
    <HumanInputSheet
      request={request}
      values={values}
      onChange={(id, value) => setValues((current) => ({ ...current, [id]: value }))}
      onSubmit={noop}
      onCancel={noop}
    />
  );
}

const codeInput = () => screen.getByLabelText(/One-time code/) as HTMLInputElement;

const shownDigits = () =>
  Array.from(document.querySelectorAll('[data-otp-box]')).map((box) => box.textContent);

describe('a one-time code on a phone', () => {
  it('pastes a whole code into the boxes, wherever the paste lands', async () => {
    const user = userEvent.setup();
    render(<OtpSheet />);
    await user.click(codeInput());
    await user.paste('123 456');
    expect(codeInput().value).toBe('123456');
    expect(shownDigits()).toEqual(['1', '2', '3', '4', '5', '6']);
  });

  it('replaces the code on a second paste instead of keeping the first', async () => {
    const user = userEvent.setup();
    render(<OtpSheet />);
    await user.click(codeInput());
    await user.paste('111111');
    await user.paste('222-333');
    expect(codeInput().value).toBe('222333');
  });

  it('keeps focus on one input while digits are typed, so the keyboard stays still', async () => {
    const user = userEvent.setup();
    render(<OtpSheet />);
    const input = codeInput();
    await user.click(input);
    const focused: Array<Element | null> = [];
    for (const key of ['4', 'x', '0', '8']) {
      await user.keyboard(key);
      focused.push(document.activeElement);
    }
    expect(focused.every((element) => element === input)).toBe(true);
    expect(document.querySelectorAll('[data-otp-row] input')).toHaveLength(1);
    expect(shownDigits().slice(0, 4)).toEqual(['4', '0', '8', '']);
    await user.keyboard('{Backspace}');
    expect(input.value).toBe('40');
    expect(document.activeElement).toBe(input);
  });

  // Regression, user report: a double tap on the code boxes gave no paste menu. WebKit
  // shows it only for a tap on the caret, which stood at the row's left edge.
  it('puts the caret under the finger that touches the boxes', () => {
    render(<OtpSheet />);
    const input = codeInput();
    input.getBoundingClientRect = () => new DOMRect(16, 400, 370, 44);
    fireEvent.pointerDown(input, { pointerType: 'mouse', clientX: 200, clientY: 420 });
    expect(input.style.paddingRight).toBe('');
    fireEvent.pointerDown(input, { pointerType: 'touch', clientX: 200, clientY: 420 });
    expect(input.style.textAlign).toBe('right');
    expect(input.style.paddingRight).toBe('186px');
    fireEvent.pointerDown(input, { pointerType: 'touch', clientX: 500, clientY: 420 });
    expect(input.style.paddingRight).toBe('0px');
  });
});

/** A gateway that serves one open request and a stream that can reconnect. */
function harness(request: HumanInputRequest) {
  const connection = new Set<(connected: boolean) => void>();
  let served = request;
  const client = {
    inputViews: () => Promise.resolve([served]),
    viewAction: () => Promise.resolve({ is_accepted: true }),
  } as unknown as GatewayClient;
  const subscriptions = {
    subscribeConnection: (listener: (connected: boolean) => void) => {
      connection.add(listener);
      return () => connection.delete(listener);
    },
    subscribeSession: () => () => undefined,
  } as unknown as SessionSubscriptionHub;
  return {
    client,
    subscriptions,
    /** Read the request again as the gateway serves it after a reconnect: a new object. */
    reconnect: (next: HumanInputRequest = { ...served }) => {
      served = next;
      for (const listener of connection) listener(true);
    },
  };
}

describe('an answer survives a trip out of the app', () => {
  const request = HUMAN_INPUT_REQUESTS.otp;
  const notifyInput = () => screen.getByLabelText('Notify on failure') as HTMLInputElement;

  it('keeps typed answers when the stream reconnects with the same request', async () => {
    const user = userEvent.setup();
    const gateway = harness(request);
    render(<HumanInputPrompt client={gateway.client} subscriptions={gateway.subscriptions} sid="s-keep" />);
    await user.type(await screen.findByLabelText('Notify on failure'), 'ops@example.com');
    await act(async () => gateway.reconnect());
    expect(notifyInput().value).toBe('ops@example.com');
  });

  it('keeps typed answers when the session screen mounts again', async () => {
    const user = userEvent.setup();
    const gateway = harness(request);
    const view = render(
      <HumanInputPrompt client={gateway.client} subscriptions={gateway.subscriptions} sid="s-mount" />,
    );
    await user.type(await screen.findByLabelText('Notify on failure'), 'ops@example.com');
    view.unmount();
    render(<HumanInputPrompt client={gateway.client} subscriptions={gateway.subscriptions} sid="s-mount" />);
    await waitFor(() => expect(notifyInput().value).toBe('ops@example.com'));
  });

  it('opens a different request on its own defaults', async () => {
    const user = userEvent.setup();
    const gateway = harness(request);
    render(<HumanInputPrompt client={gateway.client} subscriptions={gateway.subscriptions} sid="s-next" />);
    await user.type(await screen.findByLabelText('Notify on failure'), 'ops@example.com');
    await act(async () => gateway.reconnect({ ...request, id: `${request.id}-next` }));
    await waitFor(() => expect(notifyInput().value).toBe(''));
  });
});
