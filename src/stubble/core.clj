(ns stubble.core)

(def ^:dynamic *spy-data* nil)


(defn ensure-context! []
  (assert *spy-data*))


(defn called?
  ([f count]
   (ensure-context!)
   (let [data @*spy-data*
         stats (get data {:func f :args :any} 0)]
     (= count stats)))
  ([f count args]
   (ensure-context!)
   (let [data @*spy-data*
         stats (get data {:func f :args (vec args)} 0)]
     (= count stats))))


(defn called-once?
  ([f]
   (called? f 1))
  ([f args]
   (called? f 1 args)))


(defn not-called?
  ([f]
   (called? f 0))
  ([f args]
   (called? f 0 args)))


(defn spy [f]
  (ensure-context!)
  (letfn [(wrap-f [& args]
                  (swap! *spy-data*
                         (fn [data]
                           (let [key {:func wrap-f :args (vec args)}
                                 key-any {:func wrap-f :args :any}
                                 count (get data key 0)
                                 count-any (get data key-any 0)]
                             (assoc data key (inc count)
                                         key-any (inc count-any)))))
                  (apply f args))]
    wrap-f))


(defn noop [& _args]
  nil)


(defn stub [ret]
  (constantly ret))


(defn spy-stub [ret]
  (spy (stub ret)))


(defmacro stub-context [fn-redefs & body]
  `(with-redefs-fn ~fn-redefs
     #(do ~@body)))


(defmacro spy-context [fn-redefs & body]
  `(binding [*spy-data* (atom {})]
     (stub-context ~fn-redefs ~@body)))