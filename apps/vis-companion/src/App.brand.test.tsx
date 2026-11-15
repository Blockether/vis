// @vitest-environment jsdom
import { act, cleanup, render, screen } from '@testing-library/react';
import { afterEach, describe, expect, it } from 'vitest';

import { Header } from './App';
import { setGatewayBrand } from './lib/brand';

afterEach(() => {
  cleanup();
  setGatewayBrand({});
});

describe('the header of a white label', () => {
  it('shows the brand, its address and its icon links', () => {
    render(<Header onSearch={null} onAppSettings={() => {}} />);
    expect(screen.getByLabelText('Vis').getAttribute('href')).toBeNull();

    act(() => {
      setGatewayBrand({
        name: 'Acme',
        logo: '/acme.svg',
        href: 'https://acme.example',
        links: [{ icon: 'book', label: 'Handbook', href: 'https://acme.example/handbook' }],
      });
    });

    const mark = screen.getByRole('link', { name: 'Acme' });
    expect(mark.getAttribute('href')).toBe('https://acme.example');
    expect(mark.querySelector('img')?.getAttribute('src')).toBe('/acme.svg');
    const handbook = screen.getByRole('link', { name: 'Handbook', hidden: true });
    expect(handbook.getAttribute('href')).toBe('https://acme.example/handbook');
    expect(handbook.getAttribute('target')).toBe('_blank');
  });
});
