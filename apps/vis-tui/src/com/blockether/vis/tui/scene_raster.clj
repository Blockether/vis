(ns com.blockether.vis.tui.scene-raster
  "A scene node drawn into a pixel grid, without a terminal.

   The TUI paints a scene as half blocks: each cell is two pixels, the top one in
   the ink and the bottom one in the background. This namespace makes those
   pixels from the same world data that Companion draws with WebGL: the same
   camera, the same built-in meshes, the same light and the same flat shading,
   all read from the contract. So both screens show one state.

   [[raster]] is pure. It returns `{:width :height :pixels}`; `:pixels` holds
   one `0xRRGGBB` int for each pixel, row by row from the top."
  (:require [com.blockether.vis.contract.view :as spec]))

(set! *warn-on-reflection* true)

(set! *unchecked-math* :warn-on-boxed)

(defn- rgb
  "`#rrggbb` as an int; a value that is not one falls back to `fallback`."
  ^long [color ^long fallback]
  (let [hex (when (string? color) (second (re-matches #"#([0-9a-fA-F]{6})" color)))]
    (if hex (Long/parseLong hex 16) fallback)))

(defn- shade
  "`color` times `k` on each channel, rounded, in plain sRGB arithmetic."
  ^long [^long color ^double k]
  (let [k
        (min 1.0 (max 0.0 k))

        r
        (Math/round (* k (double (bit-and 0xff (bit-shift-right color 16)))))

        g
        (Math/round (* k (double (bit-and 0xff (bit-shift-right color 8)))))

        b
        (Math/round (* k (double (bit-and 0xff color))))]

    (bit-or (bit-shift-left r 16) (bit-shift-left g 8) b)))

(defn- turn
  "The 3x3 matrix that scales, then turns about x, then y, then z (three.js Euler `ZYX`)."
  ^doubles [^double rx ^double ry ^double rz ^double scale]
  (let [rad
        (/ Math/PI 180.0)

        a
        (* rx rad)

        b
        (* ry rad)

        c
        (* rz rad)

        ca
        (Math/cos a)

        sa
        (Math/sin a)

        cb
        (Math/cos b)

        sb
        (Math/sin b)

        cc
        (Math/cos c)

        sc
        (Math/sin c)]

    ;; Rz · Ry · Rx, row by row.
    (double-array (map #(* scale (double %))
                       [(* cc cb) (- (* cc sb sa) (* sc ca)) (+ (* cc sb ca) (* sc sa)) (* sc cb)
                        (+ (* sc sb sa) (* cc ca)) (- (* sc sb ca) (* cc sa)) (- sb) (* cb sa)
                        (* cb ca)]))))

(defn- placed
  "The `[x y]` of an object, wrapped into the field when the scene wraps."
  [{:keys [x y]} {:keys [width height is-wrapping]}]
  (let [x
        (double (or x 0))

        y
        (double (or y 0))]

    (if is-wrapping [(mod x (double width)) (mod y (double height))] [x y])))

(defn- mesh-table
  "Mesh id to `{:vertices :faces}`: the built-ins and the scene's own meshes."
  [meshes]
  (into spec/scene-meshes
        (map (fn [{:keys [id vertices faces]}]
               [id {:vertices vertices :faces faces}]))
        meshes))

(defn size
  "The `[width height]` in pixels of a scene drawn `cols` wide and at most
   `max-rows` cells high. The height is even, so the rows split into cells, and
   the grid keeps the scene's aspect: a half-block pixel is about square."
  [{:keys [width height]} cols max-rows]
  (let [aspect
        (/ (double height) (double width))

        cols
        (max 1 (long cols))

        rows
        (max 1 (min (long max-rows) (Math/round (/ (* (double cols) aspect) 2.0))))

        pw
        (if (< (long rows) (Math/round (/ (* (double cols) aspect) 2.0)))
          (max 1 (min cols (Math/round (/ (* 2.0 (double rows)) aspect))))
          cols)]

    [pw (* 2 (long rows))]))

(defn raster
  "The scene `node` drawn into `pw` x `ph` pixels.

   A face that turns away from the camera is not drawn, and the nearest face
   wins each pixel. An object that faces the camera but is too small to cover a
   pixel centre still gets the pixel under its centre, so a shot never goes missing."
  [node pw ph]
  (let [{:keys [width height background fov objects meshes]}
        (merge (dissoc spec/scene-defaults :object) node)

        pw
        (long pw)

        ph
        (long ph)

        w
        (double width)

        h
        (double height)

        fov
        (double fov)

        n
        (* pw ph)

        pixels
        (int-array n (rgb background 0x0b0e14))

        zbuf
        (double-array n Double/NEGATIVE_INFINITY)

        sx
        (/ (double pw) w)

        sy
        (/ (double ph) h)

        ortho?
        (<= fov 0.0)

        dist
        (if ortho? 0.0 (/ (/ h 2.0) (Math/tan (/ (* fov (/ Math/PI 180.0)) 2.0))))

        depth
        (* 100.0 (max w h))

        near
        (/ dist 100.0)

        far
        (* dist 100.0)

        {:keys [ambient diffuse direction]}
        spec/scene-light

        ambient
        (double ambient)

        diffuse
        (double diffuse)

        lx
        (double (nth direction 0))

        ly
        (double (nth direction 1))

        lz
        (double (nth direction 2))

        table
        (mesh-table meshes)

        fallback
        (:object spec/scene-defaults)

        ;; Screen x, screen y (down) and closeness: larger is nearer, linear on screen.
        project
        (fn [^double x ^double y ^double z ^doubles out]
          (if ortho?
            (do (aset out 0 (* x sx))
                (aset out 1 (* (- h y) sy))
                (aset out 2 z)
                (< (Math/abs z) depth))
            (let [d
                  (- dist z)

                  k
                  (/ dist d)]

              (aset out 0 (* (+ (/ w 2.0) (* (- x (/ w 2.0)) k)) sx))
              (aset out 1 (* (- (/ h 2.0) (* (- y (/ h 2.0)) k)) sy))
              (aset out 2 (/ 1.0 d))
              (and (> d near) (< d far)))))

        plot!
        (fn [^long i ^double z ^long color]
          (when (> z (aget zbuf i)) (aset zbuf i z) (aset pixels i (unchecked-int color)) true))

        pa
        (double-array 3)

        pb
        (double-array 3)

        pc
        (double-array 3)]

    (doseq [object
            objects

            :let [{:keys [mesh rx ry rz scale z color is-emissive]}
                  (merge fallback object)

                  {:keys [vertices faces]}
                  (get table mesh)]
            :when vertices]

      (let [^doubles m
            (turn (double rx) (double ry) (double rz) (double scale))

            [ox oy]
            (placed object node)

            ox
            (double ox)

            oy
            (double oy)

            oz
            (double z)

            base
            (rgb color 0xcccccc)

            emissive?
            (boolean is-emissive)

            world
            (mapv (fn [[vx vy vz]]
                    (let [vx
                          (double vx)

                          vy
                          (double vy)

                          vz
                          (double vz)]

                      (double-array
                        [(+ ox (* (aget m 0) vx) (* (aget m 1) vy) (* (aget m 2) vz))
                         (+ oy (* (aget m 3) vx) (* (aget m 4) vy) (* (aget m 5) vz))
                         (+ oz (* (aget m 6) vx) (* (aget m 7) vy) (* (aget m 8) vz))])))
                  vertices)

            facing
            (volatile! false)

            drawn
            (volatile! false)]

        (doseq [[ia ib ic]
                faces

                :let [^doubles a
                      (nth world ia)

                      ^doubles b
                      (nth world ib)

                      ^doubles c
                      (nth world ic)]
                :when (and (project (aget a 0) (aget a 1) (aget a 2) pa)
                           (project (aget b 0) (aget b 1) (aget b 2) pb)
                           (project (aget c 0) (aget c 1) (aget c 2) pc))]

          (let [x0
                (aget pa 0)

                y0
                (aget pa 1)

                x1
                (aget pb 0)

                y1
                (aget pb 1)

                x2
                (aget pc 0)

                y2
                (aget pc 1)

                area
                (- (* (- x1 x0) (- y2 y0)) (* (- y1 y0) (- x2 x0)))]

            ;; Counter-clockwise in the world is clockwise on a screen whose y runs down.
            (when (< area 0.0)
              (vreset! facing true)
              (let [ux
                    (- (aget b 0) (aget a 0))

                    uy
                    (- (aget b 1) (aget a 1))

                    uz
                    (- (aget b 2) (aget a 2))

                    vx
                    (- (aget c 0) (aget a 0))

                    vy
                    (- (aget c 1) (aget a 1))

                    vz
                    (- (aget c 2) (aget a 2))

                    nx
                    (- (* uy vz) (* uz vy))

                    ny
                    (- (* uz vx) (* ux vz))

                    nz
                    (- (* ux vy) (* uy vx))

                    len
                    (Math/sqrt (+ (* nx nx) (* ny ny) (* nz nz)))

                    lit
                    (if (pos? len) (max 0.0 (/ (+ (* nx lx) (* ny ly) (* nz lz)) len)) 0.0)

                    ink
                    (if emissive? base (shade base (+ ambient (* diffuse lit))))

                    z0
                    (aget pa 2)

                    z1
                    (aget pb 2)

                    z2
                    (aget pc 2)

                    left
                    (max 0 (long (Math/floor (min x0 x1 x2))))

                    right
                    (min (dec pw) (long (Math/ceil (max x0 x1 x2))))

                    top
                    (max 0 (long (Math/floor (min y0 y1 y2))))

                    bottom
                    (min (dec ph) (long (Math/ceil (max y0 y1 y2))))]

                (loop [py top]
                  (when (<= py bottom)
                    (let [cy (+ (double py) 0.5)]
                      (loop [px left]
                        (when (<= px right)
                          (let [cx (+ (double px) 0.5)
                                l0 (/ (- (* (- x2 x1) (- cy y1)) (* (- y2 y1) (- cx x1))) area)
                                l1 (/ (- (* (- x0 x2) (- cy y2)) (* (- y0 y2) (- cx x2))) area)
                                l2 (- 1.0 l0 l1)]

                            (when (and (>= l0 0.0) (>= l1 0.0) (>= l2 0.0))
                              (when (plot! (+ px (* py pw)) (+ (* l0 z0) (* l1 z1) (* l2 z2)) ink)
                                (vreset! drawn true))))
                          (recur (inc px)))))
                    (recur (inc py))))))))
        (when (and @facing (not @drawn) (project ox oy oz pa))
          (let [px
                (long (Math/floor (aget pa 0)))

                py
                (long (Math/floor (aget pa 1)))]

            (when (and (< -1 px pw) (< -1 py ph))
              (plot! (+ px (* py pw))
                     (aget pa 2)
                     (if emissive? base (shade base (+ ambient (* diffuse (max 0.0 lz)))))))))))
    {:width pw :height ph :pixels pixels}))
