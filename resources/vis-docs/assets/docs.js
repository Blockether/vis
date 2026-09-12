/* The static docs load the bundled highlighter before this deferred script. */
Prism.highlightAll();

/* Scrolling and full-size image links still work without JavaScript. */
for (const gallery of document.querySelectorAll('[data-screenshot-gallery]')) {
  const track = gallery.querySelector('.screenshot-gallery__track');
  const slides = Array.from(track.children);
  const controls = gallery.querySelector('.screenshot-gallery__controls');
  const previous = controls.querySelector('[data-previous]');
  const next = controls.querySelector('[data-next]');
  const status = controls.querySelector('[role="status"]');
  let current = 0;

  function update(index) {
    current = index;
    previous.disabled = current === 0;
    next.disabled = current === slides.length - 1;
    status.textContent = `${current + 1} / ${slides.length}`;
  }

  function goTo(index) {
    const target = Math.max(0, Math.min(index, slides.length - 1));
    track.scrollTo({
      left: slides[target].offsetLeft - slides[0].offsetLeft,
      behavior: window.matchMedia('(prefers-reduced-motion: reduce)').matches
        ? 'instant'
        : 'smooth',
    });
    update(target);
  }

  previous.addEventListener('click', () => goTo(current - 1));
  next.addEventListener('click', () => goTo(current + 1));
  track.addEventListener('keydown', (event) => {
    if (event.target !== track || event.altKey || event.ctrlKey || event.metaKey) return;
    const targets = {
      ArrowLeft: current - 1,
      ArrowRight: current + 1,
      Home: 0,
      End: slides.length - 1,
    };
    if (Object.hasOwn(targets, event.key)) {
      event.preventDefault();
      goTo(targets[event.key]);
    }
  });
  track.addEventListener(
    'scroll',
    () => {
      const origin = slides[0].offsetLeft;
      const nearest = slides.reduce(
        (best, slide, index) =>
          Math.abs(slide.offsetLeft - origin - track.scrollLeft) <
          Math.abs(slides[best].offsetLeft - origin - track.scrollLeft)
            ? index
            : best,
        0,
      );
      update(nearest);
    },
    { passive: true },
  );
  update(0);
  controls.hidden = false;
}
