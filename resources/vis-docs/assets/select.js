let sequence = 0;

/**
 * App-owned single-choice menus; native fields remain the no-script/form source of truth.
 * Refresh after replacing fields or setting native values; dispose restores the original controls.
 */
export function mountSelects(container) {
  const controls = new Map();
  const document = container.ownerDocument || container;
  const window = document.defaultView;

  function enhance(select) {
    const wrapper = document.createElement('span');
    wrapper.className = 'vis-select';
    const trigger = document.createElement('button');
    trigger.type = 'button';
    trigger.className = 'vis-select__trigger';
    trigger.setAttribute('role', 'combobox');
    trigger.setAttribute('aria-haspopup', 'listbox');
    trigger.setAttribute('aria-expanded', 'false');
    const text = document.createElement('span');
    text.className = 'vis-select__value';
    // Lucide ChevronDown and Check, with the shared icon stroke and viewBox.
    const icon = (path) => {
      const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
      svg.setAttribute('viewBox', '0 0 24 24');
      svg.setAttribute('fill', 'none');
      svg.setAttribute('stroke', 'currentColor');
      svg.setAttribute('stroke-width', '2');
      svg.setAttribute('stroke-linecap', 'round');
      svg.setAttribute('stroke-linejoin', 'round');
      svg.setAttribute('aria-hidden', 'true');
      const line = document.createElementNS(svg.namespaceURI, 'path');
      line.setAttribute('d', path);
      svg.append(line);
      return svg;
    };
    trigger.append(text, icon('m6 9 6 6 6-6'));
    const list = document.createElement('div');
    list.className = 'vis-select__list';
    do {
      list.id = `vis-select-${++sequence}`;
    } while (document.getElementById(list.id));
    list.setAttribute('role', 'listbox');
    const labels = [...select.labels];
    const wasHidden = select.hidden;
    const form = select.form;
    let open = false,
      active = -1,
      options = [],
      search = '',
      searchedAt = 0,
      disposed = false;

    function enabled(index) {
      const option = options[index];
      return option && !option.disabled && !option.parentElement?.disabled;
    }
    function activate(index) {
      active = index;
      [...list.children].forEach((node, i) => node.toggleAttribute('data-active', i === active));
      const node = list.children[active];
      if (node) {
        trigger.setAttribute('aria-activedescendant', node.id);
        node.scrollIntoView?.({ block: 'nearest' });
      } else trigger.removeAttribute('aria-activedescendant');
    }
    function position() {
      if (!open) return;
      const box = trigger.getBoundingClientRect();
      const width = window.innerWidth,
        height = window.innerHeight,
        gap = 8,
        edge = 12;
      list.style.maxWidth = `min(max(24rem, ${box.width}px), ${Math.max(0, width - edge * 2)}px)`;
      list.style.width = 'max-content';
      list.style.width = `${Math.min(Math.max(box.width, list.getBoundingClientRect().width), width - edge * 2)}px`;
      const below = height - box.bottom - gap - edge,
        above = box.top - gap - edge;
      const upwards = below < Math.min(list.scrollHeight, 320) && above > below;
      list.style.maxHeight = `${Math.max(0, Math.min(320, upwards ? above : below))}px`;
      const menu = list.getBoundingClientRect();
      list.style.left = `${Math.max(edge, Math.min(box.left, width - menu.width - edge))}px`;
      list.style.top = `${Math.max(edge, upwards ? box.top - gap - menu.height : box.bottom + gap)}px`;
    }
    function outside(event) {
      if (!wrapper.contains(event.target) && !list.contains(event.target)) close(false);
    }
    function close(focus = true) {
      if (!open) return;
      open = false;
      list.remove();
      trigger.setAttribute('aria-expanded', 'false');
      trigger.removeAttribute('aria-activedescendant');
      trigger.removeAttribute('aria-controls');
      document.removeEventListener('pointerdown', outside, true);
      document.removeEventListener('scroll', position, true);
      window.removeEventListener('resize', position);
      if (focus && !trigger.disabled && trigger.isConnected) trigger.focus({ preventScroll: true });
    }
    function commit(index, focus = true) {
      if (!enabled(index)) return;
      const changed = select.selectedIndex !== index;
      select.selectedIndex = index;
      close(focus);
      refresh();
      if (changed) {
        select.dispatchEvent(new window.Event('input', { bubbles: true }));
        select.dispatchEvent(new window.Event('change', { bubbles: true }));
      }
    }
    function refresh() {
      options = [...select.options];
      trigger.disabled = select.matches(':disabled') || options.length === 0;
      text.textContent = options.length
        ? options[select.selectedIndex]?.label || ''
        : 'No options available';
      trigger.title = text.textContent;
      for (const name of ['aria-label', 'aria-labelledby', 'aria-describedby', 'aria-invalid']) {
        const value = select.getAttribute(name);
        if (value) trigger.setAttribute(name, value);
        else trigger.removeAttribute(name);
      }
      if (!trigger.hasAttribute('aria-label') && !trigger.hasAttribute('aria-labelledby')) {
        trigger.setAttribute(
          'aria-label',
          labels
            .map((label) => {
              const copy = label.cloneNode(true);
              copy.querySelectorAll('select, .vis-select').forEach((node) => node.remove());
              return copy.textContent.trim();
            })
            .join(' '),
        );
      }
      for (const name of ['aria-label', 'aria-labelledby']) {
        const value = trigger.getAttribute(name);
        if (value) list.setAttribute(name, value);
        else list.removeAttribute(name);
      }
      if (trigger.disabled || wrapper.closest('[hidden]')) close(false);
      if (open) {
        list.replaceChildren(
          ...options.map((option, index) => {
            const node = document.createElement('div');
            node.className = 'vis-select__option';
            node.id = `${list.id}-${index}`;
            node.setAttribute('role', 'option');
            node.setAttribute('aria-selected', String(index === select.selectedIndex));
            if (!enabled(index)) node.setAttribute('aria-disabled', 'true');
            const label = document.createElement('span');
            label.textContent = option.label;
            node.append(label, icon('M20 6 9 17l-5-5'));
            node.addEventListener('click', () => commit(index));
            return node;
          }),
        );
        activate(
          enabled(select.selectedIndex)
            ? select.selectedIndex
            : options.findIndex((_, i) => enabled(i)),
        );
        position();
      }
    }
    function show() {
      if (open) return;
      refresh();
      if (trigger.disabled) return;
      trigger.focus({ preventScroll: true });
      open = true;
      search = '';
      (select.closest('dialog') || document.body).append(list);
      trigger.setAttribute('aria-expanded', 'true');
      trigger.setAttribute('aria-controls', list.id);
      refresh();
      document.addEventListener('pointerdown', outside, true);
      document.addEventListener('scroll', position, true);
      window.addEventListener('resize', position);
    }
    function key(event) {
      if (event.altKey || event.ctrlKey || event.metaKey || event.isComposing) return;
      const { key } = event;
      if (key === 'Tab') {
        if (open) commit(active, false);
        close(false);
        return;
      }
      if (key === 'Escape') {
        if (!open) return;
        event.preventDefault();
        event.stopPropagation();
        close();
        return;
      }
      if (!['ArrowDown', 'ArrowUp', 'Home', 'End', 'Enter', ' '].includes(key) && key.length !== 1)
        return;
      event.preventDefault();
      event.stopPropagation();
      if (key === 'Enter' || (key === ' ' && (!search || Date.now() - searchedAt > 700))) {
        if (open) commit(active);
        else show();
        return;
      }
      const wasOpen = open;
      show();
      if (!open) return;
      const indices = options.map((_, i) => i).filter(enabled);
      if (key === 'Home') activate(indices[0] ?? -1);
      else if (key === 'End') activate(indices.at(-1) ?? -1);
      else if (key === 'ArrowDown' || key === 'ArrowUp') {
        if (wasOpen)
          activate(
            indices[
              Math.max(
                0,
                Math.min(
                  indices.length - 1,
                  indices.indexOf(active) + (key === 'ArrowDown' ? 1 : -1),
                ),
              )
            ] ?? -1,
          );
      } else {
        const now = Date.now();
        search = now - searchedAt > 700 ? key : search + key;
        searchedAt = now;
        const query = [...search].every((letter) => letter === search[0]) ? search[0] : search;
        const start = active + (query.length === 1 ? 1 : 0);
        for (let step = 0; step < options.length; step++) {
          const index = (Math.max(0, start) + step) % options.length;
          if (
            enabled(index) &&
            options[index].label.toLocaleLowerCase().startsWith(query.toLocaleLowerCase())
          ) {
            activate(index);
            break;
          }
        }
      }
    }
    const click = () => (open ? close() : show());
    const labelClick = (event) => {
      if (trigger.contains(event.target)) return;
      event.preventDefault();
      trigger.focus();
    };
    const reset = () =>
      Promise.resolve().then(() => {
        if (!disposed) refresh();
      });
    const mouseDown = (event) => event.preventDefault();
    select.before(wrapper);
    wrapper.append(select, trigger);
    select.hidden = true;
    trigger.addEventListener('click', click);
    trigger.addEventListener('keydown', key);
    list.addEventListener('mousedown', mouseDown);
    select.addEventListener('change', refresh);
    labels.forEach((label) => label.addEventListener('click', labelClick));
    form?.addEventListener('reset', reset);
    refresh();
    return {
      refresh,
      dispose() {
        disposed = true;
        close(false);
        trigger.removeEventListener('click', click);
        trigger.removeEventListener('keydown', key);
        select.removeEventListener('change', refresh);
        labels.forEach((label) => label.removeEventListener('click', labelClick));
        form?.removeEventListener('reset', reset);
        select.hidden = wasHidden;
        wrapper.replaceWith(select);
      },
    };
  }

  function refresh() {
    for (const [select, control] of controls) {
      if (!container.contains(select) || select.multiple || select.size > 1) {
        control.dispose();
        controls.delete(select);
      }
    }
    for (const select of container.querySelectorAll('select')) {
      if (!select.multiple && select.size <= 1 && (!select.hidden || controls.has(select))) {
        if (!controls.has(select)) controls.set(select, enhance(select));
        else controls.get(select).refresh();
      }
    }
  }
  refresh();
  return {
    refresh,
    dispose() {
      controls.forEach((control) => control.dispose());
      controls.clear();
    },
  };
}
