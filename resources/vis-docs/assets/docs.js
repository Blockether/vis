/* Both docs modes load the bundled highlighter before this script. */
Prism.highlightAll();

/* A page with paired examples shows its Python or its HTTP variant. The page
   renders both with labels, so it stays readable without JavaScript. This
   script shows the switch, hides the labels and shows one variant. */
const VARIANTS = ['python', 'http'];
const VARIANT_KEY = 'vis-docs-variant';
const variantSwitch = document.querySelector('.variant-switch');
if (variantSwitch) {
  const known = (value) => (VARIANTS.includes(value) ? value : null);
  const stored = () => {
    try {
      return localStorage.getItem(VARIANT_KEY);
    } catch {
      return null;
    }
  };
  const store = (variant) => {
    try {
      localStorage.setItem(VARIANT_KEY, variant);
    } catch {
      /* Without storage, the choice holds only for this page. */
    }
  };
  const show = (variant) => {
    document.documentElement.dataset.docsVariant = variant;
    for (const button of variantSwitch.querySelectorAll('[data-variant-choice]')) {
      button.setAttribute('aria-pressed', String(button.dataset.variantChoice === variant));
    }
  };
  /* A link to an old `python-X` or `http-X` page names its variant. */
  const linked = known(new URLSearchParams(window.location.search).get('variant'));
  if (linked) store(linked);
  /* An unknown stored value must never hide both variants, so it falls back to Python. */
  show(linked || known(stored()) || VARIANTS[0]);
  variantSwitch.addEventListener('click', (event) => {
    const button = event.target.closest('[data-variant-choice]');
    const variant = button && known(button.dataset.variantChoice);
    if (!variant) return;
    store(variant);
    show(variant);
  });
  variantSwitch.hidden = false;
}

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

/* Safari ignores `user-scalable=no` and `touch-action`, so the page refuses its
   pinch gestures directly; the layout is already sized for the screen. */
for (const gesture of ['gesturestart', 'gesturechange', 'gestureend']) {
  document.addEventListener(gesture, (event) => event.preventDefault(), { passive: false });
}
