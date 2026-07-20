(ns com.blockether.vis.internal.numpy-compat-shim-test
  "The numpy-compat shim installed into every sandbox context via the generic
   sandbox-shim mechanism (`extension/sandbox-shims`): a `numpy` module published
   into `sys.modules` (so `import numpy` works) and implemented in PURE Python on
   the stdlib (math + random) — an ndarray with broadcasting, reductions, ufuncs,
   indexing, dot/matmul, a linalg submodule and a random submodule. No host bridge."
  (:require [com.blockether.vis.internal.env-python :as ep]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [org.graalvm.polyglot Context]))

(defn- ev [^Context c code] (ep/->clj (.eval c "python" code)))

(defmacro with-python-context
  [& body]
  `(let
     [~(with-meta 'python-context {:tag `Context}) (:python-context (ep/create-python-context {}))]
     (try ~@body (finally (.close ~'python-context)))))

(defdescribe
  numpy-module-test
  (it "publishes numpy under sys.modules"
      (with-python-context
        (expect (true? (ev python-context "__import__('sys').modules.get('numpy') is not None")))))
  (it "autoloads numpy onto builtins (no import needed)"
      (with-python-context (expect (true? (ev python-context "numpy.array([1,2,3]).sum() == 6")))))
  (it "supports `import numpy as np`"
      (with-python-context
        (expect (true? (ev python-context "import numpy as np\nnp.array([1,2]).shape == (2,)")))))
  (it "exposes a version string + constants"
      (with-python-context
        (expect (true? (ev python-context
                           (str
                             "import numpy as np\n"
                             "isinstance(np.__version__, str) and abs(np.pi - 3.14159265) < 1e-6 "
                             "and np.newaxis is None")))))))

(defdescribe
  numpy-ndarray-test
  (it "array infers shape + dtype and round-trips via tolist"
      (with-python-context
        (expect (true? (ev python-context
                           (str "import numpy as np\n" "a = np.array([[1,2,3],[4,5,6]])\n"
                                "a.shape == (2,3) and a.ndim == 2 and a.size == 6 "
                                "and a.tolist() == [[1,2,3],[4,5,6]]"))))))
  (it "reshape / ravel / transpose"
      (with-python-context (expect (true? (ev python-context
                                              (str "import numpy as np\n"
                                                   "a = np.arange(6).reshape(2,3)\n"
                                                   "a.reshape(3,2).tolist() == [[0,1],[2,3],[4,5]] "
                                                   "and a.T.tolist() == [[0,3],[1,4],[2,5]] "
                                                   "and a.ravel().tolist() == [0,1,2,3,4,5]"))))))
  (it "broadcasting: matrix + row vector, and scalar ops"
      (with-python-context
        (expect (true? (ev python-context
                           (str "import numpy as np\n"
                                "a = np.array([[1,2,3],[4,5,6]])\n"
                                "(a + np.array([10,20,30])).tolist() == [[11,22,33],[14,25,36]] "
                                "and (a * 2).tolist() == [[2,4,6],[8,10,12]] "
                                "and (2 ** np.array([1,2,3])).tolist() == [2,4,8]"))))))
  (it "indexing: int / tuple / slice / boolean mask"
      (with-python-context (expect (true? (ev python-context
                                              (str "import numpy as np\n"
                                                   "a = np.array([[1,2,3],[4,5,6]])\n"
                                                   "a[1,2] == 6 and a[0].tolist() == [1,2,3] "
                                                   "and a[:,1].tolist() == [2,5] "
                                                   "and a[a > 3].tolist() == [4,5,6]"))))))
  (it "setitem: scalar into a slice and via a boolean mask"
      (with-python-context
        (expect (true? (ev python-context
                           (str "import numpy as np\n"
                                "a = np.array([[1,2,3],[4,5,6]])\n" "a[0] = 0\n"
                                "b = np.array([1,2,3,4]); b[b > 2] = 9\n"
                                "a.tolist() == [[0,0,0],[4,5,6]] and b.tolist() == [1,2,9,9]")))))))

(defdescribe
  numpy-reductions-test
  (it "sum / mean along axes"
      (with-python-context
        (expect
          (true? (ev python-context
                     (str "import numpy as np\n"
                          "a = np.array([[1,2,3],[4,5,6]])\n"
                          "np.sum(a) == 21 and np.sum(a, axis=0).tolist() == [5,7,9] "
                          "and np.sum(a, axis=1).tolist() == [6,15] "
                          "and np.mean(a) == 3.5 and np.mean(a, axis=1).tolist() == [2.0,5.0]"))))))
  (it "min / max / argmax / std / var"
      (with-python-context
        (expect (true? (ev python-context
                           (str
                             "import numpy as np\n" "a = np.array([1,2,3,4])\n"
                             "np.amin(a) == 1 and np.amax(a) == 4 and int(np.argmax(a)) == 3 "
                             "and abs(np.std(a) - 1.1180339887) < 1e-6 and np.var(a) == 1.25"))))))
  (it "cumsum / clip / sort / unique / where"
      (with-python-context
        (expect (true?
                  (ev python-context
                      (str
                        "import numpy as np\n" "np.cumsum(np.array([1,2,3])).tolist() == [1,3,6] "
                        "and np.clip(np.array([-1,5,20]), 0, 10).tolist() == [0,5,10] "
                        "and np.sort(np.array([3,1,2])).tolist() == [1,2,3] "
                        "and np.unique(np.array([3,1,2,1,3])).tolist() == [1,2,3] "
                        "and np.where(np.array([1,0,1]) > 0, 10, 20).tolist() == [10,20,10]")))))))

(defdescribe
  numpy-linalg-test
  (it "dot / matmul for 1d and 2d"
      (with-python-context
        (expect (true? (ev python-context
                           (str "import numpy as np\n"
                                "int(np.dot(np.array([1,2,3]), np.array([4,5,6]))) == 32 "
                                "and (np.array([[1,2],[3,4]]) @ np.array([[5,6],[7,8]])).tolist() "
                                "== [[19,22],[43,50]]"))))))
  (it "linalg det / inv round-trips to the identity"
      (with-python-context
        (expect (true? (ev python-context
                           (str "import numpy as np\n" "A = np.array([[1.,2.],[3.,4.]])\n"
                                "abs(np.linalg.det(A) - (-2.0)) < 1e-9 "
                                "and np.allclose(np.linalg.inv(A) @ A, np.eye(2))"))))))
  (it "linalg solve recovers the known solution"
      (with-python-context
        (expect (true? (ev python-context
                           (str
                             "import numpy as np\n"
                             "x = np.linalg.solve(np.array([[3.,1.],[1.,2.]]), np.array([9.,8.]))\n"
                             "np.allclose(x, np.array([2.,3.]))"))))))
  (it "linalg norm / trace / matrix_power"
      (with-python-context
        (expect (true? (ev python-context
                           (str "import numpy as np\n"
                                "abs(np.linalg.norm(np.array([3.,4.])) - 5.0) < 1e-9 "
                                "and int(np.trace(np.array([[1,2],[3,4]]))) == 5 "
                                "and np.linalg.matrix_power(np.array([[1,1],[0,1]]), 3).tolist() "
                                "== [[1,3],[0,1]]")))))))

(defdescribe
  numpy-ufunc-and-random-test
  (it "ufuncs: sqrt / exp / abs / floor"
      (with-python-context
        (expect (true? (ev python-context
                           (str "import numpy as np\n"
                                "np.sqrt(np.array([4,9,16])).tolist() == [2.0,3.0,4.0] "
                                "and np.abs(np.array([-1,-2,3])).tolist() == [1,2,3] "
                                "and np.floor(np.array([1.7,2.2])).tolist() == [1.0,2.0]"))))))
  (it "creation helpers: zeros / ones / eye / full / linspace"
      (with-python-context
        (expect (true? (ev python-context
                           (str "import numpy as np\n"
                                "np.zeros((2,2)).tolist() == [[0.0,0.0],[0.0,0.0]] "
                                "and np.ones(3).tolist() == [1.0,1.0,1.0] "
                                "and np.eye(2).tolist() == [[1.0,0.0],[0.0,1.0]] "
                                "and np.full((2,), 7).tolist() == [7,7] "
                                "and np.linspace(0,1,5).tolist() == [0.0,0.25,0.5,0.75,1.0]"))))))
  (it "random is seedable + bounded"
      (with-python-context (expect
                             (true? (ev python-context
                                        (str "import numpy as np\n"
                                             "np.random.seed(0)\n" "r = np.random.rand()\n"
                                             "0.0 <= r <= 1.0 and 0 <= np.random.randint(0, 5) < 5 "
                                             "and np.random.rand(2,3).shape == (2,3)"))))))
  (it "RandomState / default_rng are reproducible independent generators"
      (with-python-context
        (expect (true? (ev python-context
                           (str "import numpy as np\n"
                                "a = np.random.RandomState(42).rand(3).tolist()\n"
                                "b = np.random.RandomState(42).rand(3).tolist()\n"
                                "g = np.random.default_rng(7)\n" "a == b "
                                "and np.random.default_rng(7).integers(0,10,size=4).shape == (4,) "
                                "and g.standard_normal(3).size == 3")))))))
