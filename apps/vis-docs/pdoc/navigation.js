const navigation = document.querySelector('.api-navigation');
const compact = window.matchMedia?.('(max-width: 800px)');

if (compact) {
  const resize = () => {
    navigation.open = !compact.matches;
  };
  resize();
  compact.addEventListener('change', resize);
  navigation.addEventListener('click', (event) => {
    if (compact.matches && event.target.closest('a')) navigation.open = false;
  });
}

function revealTarget() {
  let id;
  try {
    id = decodeURIComponent(window.location.hash.slice(1));
  } catch {
    return;
  }
  let ancestor = document.getElementById(id)?.closest('details');
  while (ancestor) {
    ancestor.open = true;
    ancestor = ancestor.parentElement.closest('details');
  }
}

revealTarget();
window.addEventListener('hashchange', revealTarget);
