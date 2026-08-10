import {
  createContext,
  useContext,
  useEffect,
  useMemo,
  useState,
  type ReactNode,
} from "react";

/** One step of a gallery: the bytes to show and the name to title them with. */
export type GalleryPicture = { src: string; name: string };

/**
 * Registration and CONTENT are two contexts on purpose.
 *
 * A picture registers itself from an effect, which writes the registry's state;
 * if the registrar changed identity with that state, every registered picture
 * would unregister and register again on each write and the gallery would spin
 * forever. The registrar is therefore stable for the life of the gallery and
 * only the ordered content re-renders the readers.
 */
type GalleryRegistrar = {
  register: (at: number, picture: GalleryPicture) => void;
  forget: (at: number) => void;
};

const RegistrarContext = createContext<GalleryRegistrar | null>(null);
const PicturesContext = createContext<ReadonlyMap<number, GalleryPicture> | null>(
  null,
);

/**
 * The pictures under it are ONE gallery: opening any of them can walk to the
 * others without going back to the transcript.
 *
 * The group is discovered rather than declared because the bytes are not: a
 * produced artifact is a descriptor until its tile fetches an object URL, so
 * the list a viewer can step through only exists once the tiles are mounted.
 * Each picture carries the ORDINAL its call site laid it out with, so the order
 * is the reading order of the grid and never the order in which slow bytes
 * happened to land.
 */
export function ImageGallery({ children }: { children: ReactNode }) {
  const [pictures, setPictures] = useState<ReadonlyMap<number, GalleryPicture>>(
    () => new Map(),
  );
  const registrar = useMemo<GalleryRegistrar>(
    () => ({
      register(at, picture) {
        setPictures((current) => {
          const seen = current.get(at);
          if (seen && seen.src === picture.src && seen.name === picture.name) {
            return current;
          }
          const next = new Map(current);
          next.set(at, picture);
          return next;
        });
      },
      forget(at) {
        setPictures((current) => {
          if (!current.has(at)) return current;
          const next = new Map(current);
          next.delete(at);
          return next;
        });
      },
    }),
    [],
  );

  return (
    <RegistrarContext.Provider value={registrar}>
      <PicturesContext.Provider value={pictures}>
        {children}
      </PicturesContext.Provider>
    </RegistrarContext.Provider>
  );
}

/**
 * Join the gallery this picture is laid out in, and report where it stands in
 * it — `null` outside a gallery, and `null` for the only picture in one, since
 * a viewer with nowhere to step must not offer to step.
 */
export function useGalleryStep(
  at: number | undefined,
  picture: GalleryPicture,
): { pictures: GalleryPicture[]; at: number } | null {
  const registrar = useContext(RegistrarContext);
  const registered = useContext(PicturesContext);
  const { src, name } = picture;

  useEffect(() => {
    if (!registrar || at === undefined || !src) return;
    registrar.register(at, { src, name });
    return () => registrar.forget(at);
  }, [registrar, at, src, name]);

  return useMemo(() => {
    if (!registered || at === undefined || registered.size < 2) return null;
    const ordered = [...registered.entries()].sort(
      ([left], [right]) => left - right,
    );
    const step = ordered.findIndex(([ordinal]) => ordinal === at);
    if (step < 0) return null;
    return { pictures: ordered.map(([, entry]) => entry), at: step };
  }, [registered, at]);
}
