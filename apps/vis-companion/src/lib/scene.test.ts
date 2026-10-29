/** Easing between two ticks of a live `scene`. */
import { describe, expect, it } from 'vitest';
import { easePose, type ScenePose } from './scene';

const pose = (x: number, rz = 0): ScenePose => ({ x, y: 4, z: 0, rx: 0, ry: 0, rz, scale: 1 });

describe('a scene object between two ticks', () => {
  // A ship that leaves the right edge of a wrapping field must slide out over
  // that edge, not fly back through the whole field.
  it('crosses the near edge of a wrapping field', () => {
    const half = easePose(pose(15.5), pose(0.5), 0.5, { width: 16, height: 9 });
    expect(half.x).toBeCloseTo(0);
    expect(easePose(pose(15.5), pose(0.5), 0.5).x).toBeCloseTo(8);
  });

  it('turns the short way round', () => {
    expect(easePose(pose(0, 350), pose(0, 10), 0.5).rz).toBeCloseTo(360);
    expect(easePose(pose(0, 10), pose(0, 350), 0.5).rz).toBeCloseTo(0);
  });
});
