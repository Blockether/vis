import { useBrand, VIS_LOGO } from '../lib/brand';

/** The picture of the brand: the Vis mark, or the logo of a white label. */
export function BrandLogo({ className, alt = '' }: { className?: string; alt?: string }) {
  const { logo } = useBrand();
  return <img src={logo ?? VIS_LOGO} alt={alt} className={className} aria-hidden={alt === '' ? true : undefined} />;
}
