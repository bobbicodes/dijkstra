(ns porkostomus.dijkstra
  (:require [rhizome.viz :refer [view-graph]]
            [clojure.data.priority-map :refer [priority-map]]))

(defn show! [graph]
  (view-graph
   (keys graph) (fn [n] (map first (get graph n)))
   :node->descriptor (fn [n] {:label (name n)})
   :edge->descriptor (fn [src dst] {:label (dst (src graph))})))

(def g {:1 {:2 1 :3 2}
        :2 {:4 4}
        :3 {:4 2}
        :4 {}})

(def demo-graph
  {:red    {:green 10, :blue   5, :orange 8}
   :green  {:red 10,   :blue   3}
   :blue   {:green 3,  :red    5, :purple 7}
   :purple {:blue 7,   :orange 2}
   :orange {:purple 2, :red    2}})

(comment
  (show! g)
  (show! demo-graph)
  )

(defn neighbors [node g costs]
  (->> (g node)
       (map (fn [[neighbor edge-cost]]
              [neighbor [(first (costs neighbor)) edge-cost]]))
       (into {})))

(defn process-neighbor
  [parent parent-cost costs [neighbor [old-cost edge-cost]]]
  (let [new-cost (+ parent-cost edge-cost)]
    (if (< new-cost old-cost)
      (assoc costs neighbor [new-cost parent])
      costs)))

(defn next-node [costs nodes]
  (->> costs
       (filter #(nodes (first %)))
       (sort-by #(first (second %)))
       ffirst))

(defn unwind-path [a b costs]
  (letfn [(f [a b costs]
            (when-not (= a b)
              (cons b (f a (second (costs b)) costs))))]
    (cons a (reverse (f a b costs)))))

(defn shortest-paths [s costs]
  (let [paths (->> (keys costs)
                   (remove #{s})
                   (map (fn [n] [n (unwind-path s n costs)])))]
    (into {}
          (map (fn [[n p]]
                 [n [(first (costs n)) p]])
               paths))))

(defn dijkstra [a b g]
   (loop [costs (assoc (zipmap (keys g)
                               (repeat [Integer/MAX_VALUE nil]))
                       a [0 a])
          nodes (set (keys g))]
     (let [cur (next-node costs nodes)
           cost (first (costs cur))]
       (cond (nil? cur) (shortest-paths a costs)
             (= cur b) [cost (unwind-path a b costs)]
             :else
             (recur (reduce #(process-neighbor cur cost %1 %2)
                            costs
                            (filter #(nodes (first %))
                                    (neighbors cur g costs)))
                    (disj nodes cur))))))

(defn dijkstra-pm [a b g]
  (loop [q (priority-map a 0) r {}]
    (if-let [[node cost] (peek q)]
      (let [nodes (select-keys (g node) (remove r (keys (g node))))
            costs (into {} (for [[k v] nodes] [k (+ cost v)]))]
        (recur (merge-with min (pop q) costs) (assoc r node cost)))
      {b (b r)})))

(comment
  (dijkstra-pm :red :green demo-graph)
  )