// Keep this import first: Justice needs a segmenter while its module evaluates.
import { restoreSegmenter, segmenterAvailable } from './justice-segmenter';
import * as justice from '@kitlangton/justice';

restoreSegmenter();

/**
 * Justice, bundled with the app like any other package. Null where the runtime
 * cannot segment text, so prose keeps its native line breaks there.
 */
export const engine = segmenterAvailable ? justice : null;
