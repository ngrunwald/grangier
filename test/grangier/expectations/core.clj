(ns grangier.expectations.core
  (:use expectations)
  (:use grangier.core))

(def sample [{:lemma "oh"} {:lemma "le" :pos :det} {:lemma "joli" :pos :adj}
             {:pos :nom :lemma "chat"} {:pos :vrb :lemma "dort"}])

(let [[l m o] (parse-specs
               (quote [{:lemma "le"} (:entity {:pos :adj} (or {:pos :nom} {:pos :nam}))]))]
  (expect {:lemma "le"} l)
  (expect {:pos :adj} m)
  (expect {:names [:entity]} (in (meta m)))
  (expect {:names [:entity]} (in (meta o)))
  (expect grangier.core.OrPattern o)
  (expect {:pos :nom} (in (:specs o)))
  (expect {:pos :nam} (in (:specs o))))

(def pat (pattern [{:lemma "le"} (:entity {:pos :adj} (or {:pos :nom} {:pos :nam}))]))
(expect clojure.lang.IFn pat)

(def rule (pat (annotate [:entity] (fn [tok] (assoc tok :ner true)))))
(def parser (make-parser [rule]))

(let [[t1 t2 t3 t4 t5] (parse parser sample)]
  (expect {:lemma "le"} (in t2))
  (expect nil? (:ner t2))
  (expect {:lemma "joli" :ner true} (in t3))
  (expect {:lemma "chat" :ner true} (in t4))
  (expect {:lemma "dort"} (in t5))
  (expect nil? (:ner t5)))
