/** Easing between two ticks of a live `scene`. */
import { describe, expect, it } from 'vitest';
import { easeAt, easeDuration, easePose, retarget, sceneGap, type ScenePose } from './scene';

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

describe('a scene object over several patches', () => {
  // Patches of an SDK owner come a little more than one tick apart. A move
  // that ended after exactly one tick made every object stop and start again.
  it('keeps moving until a late patch arrives', () => {
    let gap = 100;
    for (let i = 0; i < 12; i++) gap = sceneGap(gap, 120, 100);
    expect(easeDuration(100, gap)).toBeGreaterThan(120);
    expect(sceneGap(gap, 5000, 100)).toBe(gap);
  });

  it('keeps its speed when a patch does not move it', () => {
    let ease = retarget(undefined, pose(0), 0, 100);
    ease = retarget(ease, pose(10), 0, 100);
    ease = retarget(ease, pose(10), 50, 100);
    expect(easeAt(ease, 75).x).toBeCloseTo(7.5);
  });
});
