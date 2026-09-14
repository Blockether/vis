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
