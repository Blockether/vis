import { mount } from './app.js';
mount(
  document.querySelector('#app'),
  fetch,
  JSON.parse(document.querySelector('#catalog-data').textContent),
);
