/** Public-site metadata shared by generated docs, Worker SSR and client navigation. */
export const origin = 'https://vis.blockether.com';
/** Catalog identity is the GitHub namespace, not the Python distribution name. */
export const extensionName = (item) => item.repository.toLowerCase();
const escape = (value) =>
  String(value ?? '').replace(
    /[&<>"']/g,
    (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' })[c],
  );
const json = (value) => JSON.stringify(value).replace(/</g, '\\u003c');
// Lucide grid-2x2; the sidebar retains the full navigation label.
export const extensionIcon =
  '<svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><rect width="18" height="18" x="3" y="3" rx="2"/><path d="M3 12h18M12 3v18"/></svg>';
export function metadataHead({
  title,
  description,
  path,
  type = 'WebPage',
  noindex = false,
  markdown,
  mainEntity,
}) {
  const url = origin + path,
    image = origin + '/assets/social-preview.png';
  const meta = (name, value, property = false) =>
    `<meta data-discovery ${property ? 'property' : 'name'}="${name}" content="${escape(value)}">`;
  return (
    `<title data-discovery>${escape(title)}</title>` +
    meta('description', description) +
    meta('robots', noindex ? 'noindex, follow' : 'index, follow, max-image-preview:large') +
    `<link data-discovery rel="canonical" href="${escape(url)}">` +
    '<link data-discovery rel="icon" type="image/x-icon" sizes="32x32" href="/favicon.ico">' +
    '<link data-discovery rel="icon" type="image/png" sizes="48x48" href="/favicon-48.png">' +
    '<link data-discovery rel="icon" type="image/png" sizes="32x32" href="/favicon-32.png">' +
    '<link data-discovery rel="icon" type="image/png" sizes="16x16" href="/favicon-16.png">' +
    '<link data-discovery rel="apple-touch-icon" sizes="180x180" href="/apple-touch-icon.png">' +
    '<link data-discovery rel="manifest" href="/site.webmanifest">' +
    '<link data-discovery rel="sitemap" type="application/xml" href="/sitemap.xml">' +
    '<link data-discovery rel="alternate" type="text/plain" title="LLM documentation index" href="/llms.txt">' +
    (markdown
      ? `<link data-discovery rel="alternate" type="text/markdown" title="Markdown" href="${escape(markdown)}">`
      : '') +
    meta('og:type', type === 'TechArticle' ? 'article' : 'website', true) +
    meta('og:site_name', 'Vis by Blockether', true) +
    meta('og:title', title, true) +
    meta('og:description', description, true) +
    meta('og:url', url, true) +
    meta('og:image', image, true) +
    meta('og:image:alt', 'Vis logo', true) +
    meta('og:image:type', 'image/png', true) +
    meta('og:image:width', '1200', true) +
    meta('og:image:height', '630', true) +
    meta('twitter:card', 'summary_large_image') +
    meta('twitter:title', title) +
    meta('twitter:description', description) +
    meta('twitter:image', image) +
    meta('twitter:image:alt', 'Vis logo') +
    `<script data-discovery type="application/ld+json">${json({ '@context': 'https://schema.org', '@type': type, name: title, description, url, inLanguage: 'en', publisher: { '@type': 'Organization', name: 'Blockether', url: 'https://blockether.com/' }, isPartOf: { '@type': 'WebSite', name: 'Vis', url: origin + '/' }, ...(mainEntity ? { mainEntity } : {}) })}</script>`
  );
}
export function catalogMetadata({ item, error, detailError } = {}) {
  return metadataHead({
    title: (item ? extensionName(item) : 'Extension Center') + ' · Vis · Blockether',
    description:
      item?.description ||
      'Browse public GitHub extensions for Vis by Blockether. Find tools, model providers and workflows, review the source and install a specific commit.',
    path: item ? '/extensions/' + item.id : '/extensions/',
    type: item ? 'WebPage' : 'CollectionPage',
    mainEntity: item
      ? {
          '@type': 'SoftwareSourceCode',
          name: extensionName(item),
          description: item.description,
          codeRepository: item.repository_url,
          version: item.version,
          programmingLanguage: 'Python',
        }
      : undefined,
    noindex: !!error || !!detailError,
  });
}
export function sitemap(paths, index = false) {
  const tag = index ? 'sitemap' : 'url';
  return (
    `<?xml version="1.0" encoding="UTF-8"?><${index ? 'sitemapindex' : 'urlset'} xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">` +
    paths.map((path) => `<${tag}><loc>${escape(origin + path)}</loc></${tag}>`).join('') +
    `</${index ? 'sitemapindex' : 'urlset'}>\n`
  );
}
export function catalogText(items) {
  return (
    '# Vis Extension Center\n\n> Public, moderated GitHub listings for Vis tools, providers and workflows.\n\nListing is not an endorsement or a code audit. Review source and dependencies before trusting an extension.\n\n' +
    `- [Documentation](${origin}/llms.txt)\n- [Authoring guide](${origin}/extending.md)\n- [Catalog JSON API](${origin}/api/extensions)\n\n## Extensions\n\n` +
    items
      .map(
        (item) =>
          `- [${extensionName(item).replace(/[\r\n[\]\\]/g, ' ')}](${origin}/extensions/${item.id})\n`,
      )
      .join('')
  );
}
