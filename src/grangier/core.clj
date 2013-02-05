(ns grangier.core
  (:refer-clojure :exclude [char])
  (:use [the.parsatron])
  (:import [the.parsatron Continue]))

;; Polymorphic Tokens

(defprotocol PToken
  (get-field [this key] "Gets a field value from a token"))

(extend-protocol PToken
  clojure.lang.IPersistentMap
  (get-field [this key] (get this key)))

(defmacro extend-stanford-token
  []
  (try
    (Class/forName "edu.stanford.nlp.util.TypesafeMap")
    `((require 'stanford-nlp-tools.core :as stf)
      (extend-protocol PToken
        edu.stanford.nlp.util.TypesafeMap
        (get-field [this key] (stf/get-ts this key))))
    (catch ClassNotFoundException _
      `())))

(extend-stanford-token)

;; Spec Compiler

(declare compile-spec)
(declare compile-field)

(defrecord OrPattern [specs])
(defrecord AndPattern [specs])

(defn compile-or
  [{:keys [specs]}]
  (let [compiled (map compile-spec specs)]
    (fn [token]
      (loop [todo compiled]
        (if-let [spec (first todo)]
          (let [res (spec token)]
            (if res
              true
              (recur (rest todo))))
          false)))))

(defn compile-and
  [{:keys [specs]}]
  (let [compiled (map compile-spec specs)]
    (fn [token]
      (loop [todo compiled]
        (if-let [spec (first todo)]
          (let [res (spec token)]
            (if res
              (recur (rest todo))
              false))
          true)))))

(defn compile-map-spec
  [spec]
  (let [preds (for [[k v] spec]
                [k (compile-field v)])]
    (fn [token]
      (loop [todo preds]
        (if-let [[k pred] (first todo)]
          (let [res (pred (get-field token k))]
            (if res
              (recur (rest preds))
              false))
          true)))))

(defprotocol PPred
  (compile-field [spec] "Compiles field spec to predicate"))

(extend-protocol PPred
  String
  (compile-field [spec] #(= % spec))
  clojure.lang.Keyword
  (compile-field [spec] #(= % spec))
  clojure.lang.PersistentHashSet
  (compile-field [spec] #(spec %))
  java.util.regex.Pattern
  (compile-field [spec] #(re-matches spec %))
  clojure.lang.IFn
  (compile-field [spec] #(spec %))
  nil
  (compile-field [spec] nil?)
  Object
  (compile-field [spec] #(= % spec)))

(defprotocol PSpecPred
  (compile-spec [spec] "Compiles full spec to predicate"))

(extend-protocol PSpecPred
  clojure.lang.PersistentArrayMap
  (compile-spec [spec] (compile-map-spec spec))
  OrPattern
  (compile-spec [spec] (compile-or spec))
  AndPattern
  (compile-spec [spec] (compile-and spec)))

;; Parser for patterns

(declare parse-specs)

(defparser spec []
  (let->> [spec (token map?)]
          (always spec)))

(defparser annotation []
  (let->> [annotation (token (fn [t]
                               (and (list? t)
                                    (keyword? (first t)))))]
          (always
           (map (fn [spec]
                  (vary-meta spec update-in [:names]
                             (fn [old new] (if old (conj old new ) [new]))
                             (first annotation)))
                (parse-specs (rest annotation))))))

(defparser bool []
  (let->> [boolean-c (token (fn [t]
                              (and (list? t)
                                   (#{'or 'and} (first t)))))]
          (let [b (keyword (first boolean-c))
                all-clauses (parse-specs (rest boolean-c))]
            (always
             (case b
               :or (OrPattern. all-clauses)
               :and (AndPattern. all-clauses))))))

(def spec-parser (many (choice (spec) (bool) (annotation))))

(defn parse-specs
  [specs]
  (flatten
   (run spec-parser specs)))

;; Rule Engine

(defn conj-vec
  [old new]
  (if old (conj old new) [new]))

(defn group-indexed
  [s]
  (reduce
   (fn [acc [idx names]]
     (let [acc* (update-in acc [:all]
                           conj-vec idx)]
       (if-not (empty? names)
         (reduce
          (fn [acc n]
            (update-in acc [n]
                       conj-vec idx))
          acc* names)
         acc*)))
   {} (map-indexed (fn [idx names] [idx names]) s)))

(defn annotate
  [names f]
  (fn [tokens names-idx]
    (reduce
     (fn [toks n]
       (let [toks-idx (get names-idx n)]
         (reduce
          (fn [toks idx]
            (update-in toks [idx] f))
          toks toks-idx)))
     tokens names)))

(defn transform
  [names f]
  (fn [tokens names-idx]
    (reduce
     (fn [toks n]
       (let [toks-idx (get names-idx n)
             res (f (subvec toks (first toks-idx) (inc (last toks-idx))))]
         (if (vector? res) res (vector res))))
     tokens names)))

(defmacro pattern
  [raw-specs]
  (let [specs (parse-specs raw-specs)
        nb-toks (count specs)
        toks-names (into [] (map #(gensym (str "tok-" % "-")) (range nb-toks)))]
    `(fn [rules#]
       (fn [state# cok# cerr# eok# eerr#]
         (let [names-idx# ~(group-indexed (map #(-> % (meta) (:names)) specs))
               p# (let->> [~@(apply concat
                                    (for [[idx spec] (map-indexed
                                                      (fn [idx spec]
                                                        [idx spec]) specs)]
                                      [(nth toks-names idx) `(token (compile-spec ~spec))]))]
                          (let [all-tokens# ~toks-names]
                            (always (rules# all-tokens# names-idx#))))]
           (Continue. #(p# state# cok# cerr# eok# eerr#)))))))

(defparser any-factory [f]
  (let->> [tok (token (fn [t] t))]
          (always (f tok))))

(def any (any-factory vector))

(defn make-parser
  [parsers]
  (many (apply either (concat parsers [any]))))

(defn parse
  [parser tokens]
  (apply concat (run parser tokens)))
