export function compute(): number {
  let s = 0;
  for (let i = 1; i <= 100; i++) s += i * i;
  return s;
}
