(ns clidget.widget
  #+clj (:require [clojure.core :as clj])
  #+cljs (:require-macros [clidget.widget :refer [with-widget-cache]]))

(def ^:dynamic ^:private *context* nil)

(defn- get-widget-key [system keys-binding]
  (select-keys system (map :val-key keys-binding)))

(defn- resolve-state [system keys-binding]
  (->> (for [{:keys [val-key atom-key]} keys-binding]
         [val-key (or (get system val-key)
                      (some->> atom-key
                               (get system)
                               deref))])
       (into {})))

(defn get-cached-widget [{:keys [from-cache !to-cache]} widget-key]
  (when from-cache
    (let [cached-widget (get from-cache widget-key)]
      (when (and cached-widget !to-cache)
        (swap! !to-cache assoc widget-key cached-widget)
        cached-widget))))

(defn cache-widget! [widget !widget-cache widget-key]
  (when !widget-cache
    (swap! !widget-cache assoc widget-key widget)))

(defn- init-locals [system locals-binding]
  (reduce (fn [system [atom-key init-fn]]
            (assoc system
              atom-key (init-fn)))
          system
          locals-binding))

(defn add-watches [system keys-binding render-widget-fn]
  (doseq [{:keys [val-key atom-key]} keys-binding]
    (when-let [watched-atom (some->> atom-key
                                     (get system))]
      (add-watch watched-atom (gensym "clidget")
                 (fn [_ _ _ new-value]
                   (render-widget-fn (assoc system val-key new-value)))))))

(defmacro with-widget-cache [!cache & body]
  `(let [from-cache# @~!cache]
     (binding [*context* {:from-cache from-cache#
                          :!to-cache (doto ~!cache
                                       (reset! {}))}]
       ~@body)))

(defn re-render-widget [{!parent-widget-cache :!to-cache} widget-key system keys-binding render-widget-fn]
  (let [!widget (atom nil)
        !widget-cache (atom {})
        render-widget (fn [system]
                        (doto (with-widget-cache !widget-cache
                                (render-widget-fn (-> system
                                                      (merge (resolve-state system keys-binding))
                                                      (dissoc :clidget/widget-key
                                                              :clidget/widget-type))))
                          
                          (cache-widget! !parent-widget-cache widget-key)

                          (#(when-let [current-widget @!widget]
                              ;; This is called when an atom that
                              ;; we're watching changes - our parent
                              ;; may not have updated.
                              (.. current-widget -parentNode (replaceChild % current-widget))))
                          
                          (->> (reset! !widget))))]

    (add-watches system keys-binding render-widget)
    (reset! !widget (render-widget system))))

(defn updated-widget [system keys-binding locals-binding render-widget-fn]
  ;; this fn is called whenever a parent-widget asks us to reload

  (let [widget-key (get-widget-key system keys-binding)]
    (or (get-cached-widget *context* widget-key)
        (re-render-widget *context*
                          widget-key
                          (-> system
                              (init-locals locals-binding))
                          keys-binding
                          render-widget-fn))))

#+clj
(defn parse-keys-binding [binding]
  (->> (for [sym binding]
         {:val-key (keyword sym)
          :atom-key (keyword (format "!%s" sym))})
       vec))

#+clj
(defn wrap-local-inits [locals]
  (->> (for [[local-key init] locals]
         [local-key `(fn [] ~init)])
       (into {})))

#+clj
(defmacro defwidget [name [system-binding & params] & body]
  (let [widget-type (gensym "widget")]
    `(defn ~name [system# & params#]
       (updated-widget (assoc system#
                         :clidget/widget-type '~widget-type)
                       ~(-> system-binding :keys parse-keys-binding)
                       ~(-> system-binding :locals wrap-local-inits)
                       (fn [resolved-state#]
                         (let [~(dissoc system-binding :locals) resolved-state#
                               ~(vec params) params#]
                           ~@body))))))

#+clj
(defmacro for [bindings & [body]]
  (let [parsed-bindings (clojure.core/for [[variable binding] (partition 2 bindings)]
                          (if-not (#{:when :let} variable)
                            (let [surrogate-var (gensym "surrogate")]
                              {:binding-pairs [surrogate-var binding
                                               :let [variable surrogate-var]]
                               :surrogate-vars [surrogate-var]})
                            {:binding-pairs [variable binding]}))
        widget-type (gensym "clidget-for")]
    
    `(clojure.core/for ~(vec (mapcat :binding-pairs parsed-bindings))
       (let [widget-key# (clojure.core/for [surrogate-var# ~(->> parsed-bindings
                                                                 (mapcat :surrogate-vars)
                                                                 vec)]
                           (or (:clidget/id surrogate-var#)
                               (:id surrogate-var#)
                               (key surrogate-var#)))]
         (prn widget-key#)
         ~body))))

(defn split-bindings [bindings]
  (->> bindings
       (partition-all 2)
       (partition-by (complement (comp keyword? first)))
       (partition-all 2)
       (mapcat (fn [[sym-bindings pred-bindings]]
                 (concat (clj/for [sym (butlast sym-bindings)]
                           {:sym-binding sym})
                         [{:sym-binding (last sym-bindings)
                           :pred-bindings pred-bindings}])))))

#_(let [bindings '[test (range 4)
                   test-2 (range 10)
                   :when (even? test)
                   :let [test-1 (dec test)]
                   :let [test-3 (inc test-2)]]]
    (split-bindings bindings))

(defn to-loops [[{:keys [sym-binding pred-bindings]} & more] body]
  (let [[sym form] sym-binding
        rest-sym (gensym "rest")
        result-sym (gensym "result")]
    `(loop [[~sym & ~rest-sym] ~form
            ~result-sym []]
       (if ~sym
         ~(reduce (fn [form [pred pred-form]]
                    (case pred
                      :let `(let ~pred-form
                              ~form)
                      :when `(if ~pred-form
                               ~form
                               (recur ~rest-sym ~result-sym))
                      :while `(if ~pred-form
                                ~form
                                ~result-sym)))
                  (recur ~rest-sym
                         (if (seq more)
                           `(into ~result-sym ~(to-loops more body))
                           `(conj ~result-sym ~body)))
                  (reverse pred-bindings))
         ~result-sym))))

#_(let [bindings '[x [2 2 4 6 3 1 2 4]
                   y (range 3)
                   :let [z (+ x y)]
                   :when (even? z)]
        body '[x y]]
    (eval (to-loops (split-bindings bindings) body)))



(defmacro test-for [bindings & [body]]
  (to-loops (split-bindings bindings) body))
