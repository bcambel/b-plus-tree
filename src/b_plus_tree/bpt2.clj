(ns b-plus-tree.bpt2
  (:require [taoensso.timbre :as log]
            [b-plus-tree.util :refer [charset]]))
(use 'clojure.repl)
(try
    (require '[clojure.tools.namespace.repl :refer [refresh]])
    (catch Exception e nil))

(defn explain-error [e]
  (.printStackTrace e))

(def branching-factor 4)
(def history (atom []))
(def gt? (comp pos? compare))
(def lt? (comp neg? compare))

{:key  nil
 :val nil}

(defn new-leaf [])
{ :markers []
  :t :leaf
  :nodes [] }

(defn new-n [k v] {:k k :v v :t :l})


(defn find-marker-loc
  "Returns the elements whose key
  value is greater than this key"
  [markers key]
  (let [marker-count (count markers)]
    (loop [i 0]
      (let [current (nth markers i)]
        (log/infof "Compare#%d %s vs " i key current)
        (if (lt? key current )
          i
          (if (< (inc i) marker-count)
            (recur (inc i))
            (inc i)))))))

(defn insert
  "Inserts an element into B+ tree"
  ([tree k v]
    (insert tree k v []))

  ([tree k v stack]
    (log/info "Inserting" k (count stack) tree)
    (let [{:keys [t markers nodes]} tree
      visit-subs (fn [current-stack]
                  (let [loc (find-marker-loc markers k)]
                          (log/info "Position " loc)
                          (let [node (get nodes loc)
                                node' (insert node k v current-stack )
                                parent (peek current-stack)
                                ex-stack (pop current-stack)]
                            (conj ex-stack
                              (assoc parent
                                :nodes (assoc (:nodes parent) loc node') )))

                          ))]
      (condp = t
        :leaf  (assoc tree :nodes (conj nodes (new-n  k  v)))
        :inter  (visit-subs (conj stack tree))
        :root (visit-subs (conj stack tree))
        (log/info "Weird thigs" t)
        )
      )))

(defn insert-into
  [tree k v]
  (let [stack (insert tree k v)]
    (first stack)
  )
  )
; (insert (new-leaf) 1 1)

(defn test-data []
  (insert-into {:markers [20 40 60] :t :root
          :nodes [
            {:markers [] :t :leaf
              :nodes [(new-n 1 1)]}
            {:markers [] :t :leaf
             :nodes [(new-n 20 1)]}
             {:markers [] :t :leaf
              :nodes [(new-n 40 1)]}
              {:markers [] :t :leaf
               :nodes [(new-n 60 1) (new-n 61 1) (new-n 62 1) (new-n 63 1)]}
          ]} 51 50))

(def r2 (test-data))
