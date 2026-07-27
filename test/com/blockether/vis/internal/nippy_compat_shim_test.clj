(ns com.blockether.vis.internal.nippy-compat-shim-test
  "The Nippy shim exposes Vis persistence BLOB decoding/encoding to real sandbox
   Python while preserving the strings-only boundary and disabling Java
   Serializable fallback."
  (:require [com.blockether.vis.internal.env-python :as ep]
            [com.blockether.vis.internal.foundation.shim-nippy :as shim-nippy]
            [lazytest.core :refer [defdescribe expect it]]
            [taoensso.nippy :as nippy])
  (:import [java.util Base64 Date]
           [mikera.vectorz Vector Vector1 Vector2 Vector3 Vector4]
           [org.graalvm.polyglot Context]))

(defn- ev [^Context context code] (ep/->clj (.eval context "python" code)))

(defmacro with-python-context
  [& body]
  `(let [~(with-meta 'python-context {:tag `Context}) (:python-context (ep/create-python-context
                                                                         {}))]
     (try ~@body (finally (.close ~'python-context)))))

(defn- encoded-fixture [value] (.encodeToString (Base64/getEncoder) (nippy/freeze value)))

(defn- encoded-vectorz-fixture
  []
  (shim-nippy/ensure-vectorz-installed!)
  (encoded-fixture {:vectors [(Vector/wrap (double-array [1.25 2.5 5.75 11.5 23.25]))
                              (Vector1. 1.25) (Vector2. 1.25 2.5) (Vector3. 1.25 2.5 5.75)
                              (Vector4. 1.25 2.5 5.75 11.5)]}))

(defdescribe
  nippy-module-test
  (it "stays lazy, imports as a module, and publishes no-import helpers"
      (with-python-context
        (expect (true?
                  (ev python-context
                      (str "import sys\n" "before = 'nippy' not in sys.modules\n"
                           "import nippy\n" "before and nippy is sys.modules['nippy'] "
                           "and nippy.decode is nippy_decode and nippy.encode is nippy_encode "
                           "and nippy.loads is nippy.decode and nippy.dumps is nippy.encode")))))))

(defdescribe
  nippy-codec-test
  (it "decodes a real Vis-shaped Nippy BLOB into native canonical Python data"
      (let [fixture (encoded-fixture {:tool-calls [{:svar/tool-call-id "toolu_DECODE_ME"
                                                    :status :ok}]
                                      :created-at (Date. 0)})]
        (with-python-context
          (expect (= [true true "toolu_DECODE_ME" "ok" 0]
                     (ev python-context
                         (str "import base64\n"
                              "value = nippy_decode(base64.b64decode('" fixture
                              "'))\n"
                              "[isinstance(value, dict), isinstance(value['tool_calls'], list), "
                              " value['tool_calls'][0]['tool_call_id'], "
                              " value['tool_calls'][0]['status'], value['created_at']]")))))))
  (it "loads Vectorz compatibility on codec use and exposes vectors as Python lists"
      (let [fixture (encoded-vectorz-fixture)]
        (with-python-context
          (expect
            (= [true
                [[1.25 2.5 5.75 11.5 23.25] [1.25] [1.25 2.5] [1.25 2.5 5.75] [1.25 2.5 5.75 11.5]]]
               (ev python-context
                   (str
                     "import base64, nippy\n"
                     "value=nippy.decode(base64.b64decode('" fixture
                     "'))\n"
                     "[all(isinstance(x, list) for x in value['vectors']), value['vectors']]")))))))
  (it
    "round-trips Python plain data through real Nippy bytes"
    (with-python-context
      (expect
        (= [true "toolu_ROUND_TRIP" [1 2 3] nil]
           (ev
             python-context
             (str
               "value={'tool_id':'toolu_ROUND_TRIP','xs':[1,2,3],'none':None}\n"
               "blob=nippy_encode(value)\n"
               "decoded=nippy_decode(blob)\n"
               "[isinstance(blob, bytes), decoded['tool_id'], decoded['xs'], decoded['none']]"))))))
  (it "maps malformed bytes to catchable NippyError and rejects text input"
      (with-python-context
        (expect (= [true true]
                   (ev python-context
                       (str "import nippy\n" "bad_blob=False\n"
                            "bad_type=False\n" "try:\n nippy.decode(b'not-nippy')\n"
                            "except nippy.NippyError:\n bad_blob=True\n"
                            "try:\n nippy_decode('not-bytes')\n"
                            "except TypeError:\n bad_type=True\n" "[bad_blob,bad_type]")))))))
