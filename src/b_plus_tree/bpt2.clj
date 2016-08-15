(ns b-plus-tree.bpt2
  (:require [taoensso.timbre :as log]
            [clojure.string :as s]
            [b-plus-tree.util :refer [charset]]))
(use 'clojure.repl)
(try
    (require '[clojure.tools.namespace.repl :refer [refresh]])
    (catch Exception e nil))

(defn explain-error [e]
  (.printStackTrace e))

(def branching-factor 4)
(def b2! (/ branching-factor 2))
(def history (atom []))
(def gt? (comp pos? compare))
(def lt? (comp neg? compare))

{:key  nil
 :val nil}

(defn new-leaf [nodes & markers]
{ :markers (or markers [])
  :t :leaf
  :nodes nodes })

(defn new-inter [nodes markers ]
  { :markers (or markers [])
    :t :inter
    :nodes nodes }
  )

(defn new-n [k v] {:k k :v v :t :l :d? true})


(defn find-marker-loc
  "Returns the elements whose key
  value is greater than this key"
  [markers key]
  (let [marker-count (count markers)]
    (loop [i 0]
      (when-let [current (when-not (empty? markers )(nth markers i))]
        (log/infof "Compare#%d %s vs " i key current)
        (if (lt? key current )
          i
          (if (< (inc i) marker-count)
            (recur (inc i))
            (inc i)))))))

(defn slice [nodes loc]
  (log/info "Slice nodes" loc)
  [ (subvec (vec nodes) 0 loc)
    (subvec (vec nodes) loc)]
  )

(defn overflow? [leaf]
    ; (log/info leaf)
   (> (count leaf) branching-factor)
  )
(defn marker-overflow? [node]
  (let [overflow? (> (count (:markers node)) (dec branching-factor))]
  (when-not (= :leaf (:t node ))
    (log/info "Node has marker overflow? " overflow? (:markers node)))
    overflow?
  ))

(defn split-leaf [leaf]
  ; leaf
  (let [[left right] (slice leaf b2!)]
    [left  {:marker (-> (first right) :k) :nodes right }]
  ))

(defn split-inter [node]
  ; (log/info node)
  (let [[lnodes rnodes] (slice (:nodes node) b2!)
        lnodes (conj lnodes (first rnodes))
        rnodes (vec (rest rnodes))
        [lmarkers rmarkers] (-> node :markers (slice b2!) )
        marker (-> rmarkers first)
        rmarkers (vec (rest rmarkers))
        _ (log/info "RMARKERS!!!!" rmarkers rnodes)
        split-result {:elected-marker marker
                      :left (new-inter lnodes lmarkers)
                      :right (new-inter rnodes rmarkers)}]
    (log/info "Split result " split-result)
    split-result))

(defn update-path
  [leaf path stack]
  (swap! history conj stack)
  (log/info "Updating path" leaf path )
  (loop [i 0 n leaf
         path' path
         stack' stack]
    (log/info (count path') (count stack'))
    (let [idx (or (peek path') 0)
          node (peek stack')
          rest-of-stack (vec (pop stack'))
          node' (assoc node :nodes (assoc (:nodes node) idx n ))
          overflow? (marker-overflow? node')]
        (if overflow?
          (let [parent (peek rest-of-stack)
                {:keys [elected-marker left right]} (split-inter node')
                parent' (-> parent
                          (assoc :markers
                            (vec (sort (conj (:markers parent) elected-marker ))))
                          (assoc :nodes
                            (vec (sort-by #(-> % :markers first)
                              ;; place it to the correct location
                              ;; rather than adding to the end!!
                              (conj (:nodes parent) right)
                              ))))
                rest-of-stack' (-> rest-of-stack pop (conj parent'))
                _ (swap! history conj parent')
                node' left]
            (log/info "Overflow markers" left)
            (if (empty? rest-of-stack)
              node'
              (recur (inc i) node' (vec (pop path')) rest-of-stack')))
          (if (empty? rest-of-stack)
            node'
            (recur (inc i) node' (pop path') rest-of-stack))))))

(defn insert
  "Inserts an element into B+ tree"
  ; ([tree k v]
  ;   (insert tree k v []))

  ([tree k v]
    ; (log/info "Inserting" k (count stack) tree)
    (let [new-data (new-n k v)
          [path node stack]
          (loop [i 0
                node tree
                stack [] path []]
              (log/infof "Insert path #%d %s %s" i (s/join "/" path) node)
            (if (= (:t node) :leaf)
              [path node stack]
              (let [{:keys [t markers nodes]} node
                  loc (find-marker-loc markers k)
                  _ (when (and (not (= t :leaf)) (nil? loc))
                      (throw (ex-info "Cannot be nil" {:node node})))
                  _ (log/info "Searching " markers k loc)
                  node' (get nodes loc)
                  _ (when (nil? node')
                      (throw (ex-info "Cannot be nil" {:nodes nodes})))
                  ]
                (when (<  i 100 )
                  (recur (inc i) node' (conj stack node) (conj path loc))))))]

       (let [leaves (sort-by :k (conj (:nodes node ) new-data))
             node' (assoc node :nodes leaves)]
          (if (overflow? leaves)
            (do
              (let [[left inter] (split-leaf leaves)
                    parent (peek stack)
                    parent' (-> parent
                              (assoc  :markers
                                (vec (sort (conj (:markers parent) (:marker inter)))))
                              (assoc :nodes
                                (vec (sort-by #(-> % :nodes first :k)
                                (conj (:nodes parent)
                                  (new-leaf (:nodes inter)))
                                  ))))
                    stack' (-> stack pop (conj parent'))]
                (update-path
                  (assoc node' :nodes  left )
                  path stack')))
             (update-path node' path stack))))))

; (insert (new-leaf) 1 1)
(def dep2 {:markers [ 60 ] :t :root
      :nodes [
        {:markers [20 40 51] :t :inter
          :nodes [
            {:markers [] :t :leaf :nodes [(new-n 0 1)  (new-n 10 1)]}
            {:markers [] :t :leaf :nodes [(new-n 20 1) (new-n 30 1)]}
            {:markers [] :t :leaf :nodes [(new-n 40 1) (new-n 50 1)]}
            {:markers [] :t :leaf :nodes [(new-n 51 1) (new-n 52 1)
                                          (new-n 53 1) (new-n 53 1)]}]}
        {:markers [80] :t :inter
          :nodes [
            {:markers [] :t :leaf :nodes [(new-n 60 1)(new-n 70 1)]}
            {:markers [] :t :leaf :nodes [(new-n 80 1)(new-n 81 1)]}
            ]}]})

(def dep1
   {:markers [20 40 60] :t :root
          :nodes [
            {:markers [] :t :leaf :nodes [(new-n 1 1)(new-n 10 1)]}
            {:markers [] :t :leaf :nodes [(new-n 20 1)]}
             {:markers [] :t :leaf :nodes [(new-n 40 1)]}
              {:markers [] :t :leaf :nodes [(new-n 60 1) (new-n 61 1)
                                            (new-n 62 1) (new-n 63 1)]}
          ]} )

; (def r2 (test-data))
(defn popsticle [max-el]
  (let [starting (insert dep2 82 1)]
    (loop [i 0 tree starting]

      (if (> i max-el)
        tree
        (recur (inc i)
        (-> tree
          (insert (+ i 83) 1)
           )
      )))))

(defn node-iter
  [root writer]
  (loop [i 0
         nodes [root]]
     (when-not (empty? nodes)
       (let [subnode-writer
               (fn [p]
                 (when-not (= (:t p) :leaf)
                  (.append writer (format "\"%s\" [label = \"%s\"];\n"
                    (.hashCode p)
                    (clojure.string/join "-"
                      (if (empty? (:markers p))
                        [(.hashCode p)]
                        (:markers p))) )))
                   (mapv (fn [n]
                     (.append writer (format "\"%s\"->\"%s\"; \n" (.hashCode p)
                        (if-not (= (:t n) :l)
                          (.hashCode n)
                          (:k n))))
                     n)
                     (:nodes p)))]

            (recur (inc i)
                    (mapcat subnode-writer nodes))
         ))))

(defn snap [tree f]
  (let [dot (StringBuilder. "digraph rendering { ")]
    (node-iter tree dot)
    (.append dot "}")
    (spit f dot)
    (clojure.java.shell/sh "dot" "-Tsvg" f "-O")
  ))
