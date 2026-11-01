package com.blockether.vis.scene;

/**
 * Pixel passes of the TUI scene raster that run for each pixel of a face.
 *
 * <p>The arithmetic keeps the order of the Clojure code that it replaces, so each
 * pixel and each depth come out the same.
 */
public final class SceneRaster {
  private SceneRaster() {}

  /**
   * Fills one projected triangle into {@code pixels} where it is nearer than {@code zbuf}.
   *
   * <p>{@code pa}, {@code pb} and {@code pc} hold screen x, screen y and closeness. {@code area}
   * is the signed doubled area of the triangle and must not be zero. A pixel is drawn when its
   * centre is inside the triangle and its closeness is larger than the buffer value.
   *
   * @return whether the triangle drew at least one pixel
   */
  public static boolean fillTriangle(
      int[] pixels,
      double[] zbuf,
      int pw,
      int ph,
      double[] pa,
      double[] pb,
      double[] pc,
      double area,
      int ink) {
    double x0 = pa[0];
    double y0 = pa[1];
    double z0 = pa[2];
    double x1 = pb[0];
    double y1 = pb[1];
    double z1 = pb[2];
    double x2 = pc[0];
    double y2 = pc[1];
    double z2 = pc[2];
    long left = Math.max(0L, (long) Math.floor(Math.min(Math.min(x0, x1), x2)));
    long right = Math.min(pw - 1L, (long) Math.ceil(Math.max(Math.max(x0, x1), x2)));
    long top = Math.max(0L, (long) Math.floor(Math.min(Math.min(y0, y1), y2)));
    long bottom = Math.min(ph - 1L, (long) Math.ceil(Math.max(Math.max(y0, y1), y2)));
    boolean drawn = false;
    for (long py = top; py <= bottom; py++) {
      double cy = py + 0.5;
      for (long px = left; px <= right; px++) {
        double cx = px + 0.5;
        double l0 = ((x2 - x1) * (cy - y1) - (y2 - y1) * (cx - x1)) / area;
        double l1 = ((x0 - x2) * (cy - y2) - (y0 - y2) * (cx - x2)) / area;
        double l2 = 1.0 - l0 - l1;
        if (l0 >= 0.0 && l1 >= 0.0 && l2 >= 0.0) {
          int i = (int) (px + py * pw);
          double z = l0 * z0 + l1 * z1 + l2 * z2;
          if (z > zbuf[i]) {
            zbuf[i] = z;
            pixels[i] = ink;
            drawn = true;
          }
        }
      }
    }
    return drawn;
  }
}
