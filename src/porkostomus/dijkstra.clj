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
  (show! demo-graph))

(defn neighbors [node g costs]
  (into {} (map (fn [[neighbor cost]]
                  [neighbor [(first (costs neighbor)) cost]])
                (g node))))

(defn update-neighbor [parent parent-cost costs [neighbor [old-cost cost]]]
  (let [new-cost (+ parent-cost cost)]
    (if (< new-cost old-cost)
      (assoc costs neighbor [new-cost parent])
      costs)))

(defn next-node [costs nodes]
  (->> costs
       (filter #(nodes (first %)))
       (sort-by #(first (second %)))
       ffirst))

(defn update-path [a b costs]
  (letfn [(f [a b costs]
            (when-not (= a b)
              (cons b (f a (second (costs b)) costs))))]
    (cons a (reverse (f a b costs)))))

(defn shortest-paths [s costs]
  (let [paths (->> (keys costs)
                   (remove #{s})
                   (map (fn [n] [n (update-path s n costs)])))]
    (into {}
          (map (fn [[n p]]
                 [n [(first (costs n)) p]])
               paths))))

(defn dijkstra [a b g]
  (loop [costs (assoc (zipmap (keys g) (repeat [Integer/MAX_VALUE nil])) a [0 a])
         nodes (set (keys g))]
    (let [cur (next-node costs nodes)
          cost (first (costs cur))]
      (cond (nil? cur) (shortest-paths a costs)
            (= cur b) [cost (update-path a b costs)]
            :else
            (recur (reduce #(update-neighbor cur cost %1 %2)
                           costs
                           (filter #(nodes (first %))
                                   (neighbors cur g costs)))
                   (disj nodes cur))))))

(defn dijkstra-pm [a b g]
  (loop [frontier (priority-map a 0) explored {}]
    (if-let [[node cost] (peek frontier)]
      (let [nodes (select-keys
                   (g node)
                   (remove explored (keys (g node))))
            costs (into {} (for [[k v] nodes] [k (+ cost v)]))]
        (recur (merge-with min (pop frontier) costs)
               (assoc explored node cost)))
      {b (b explored)})))

(def costs (assoc (zipmap (keys demo-graph)
                          (repeat [Integer/MAX_VALUE nil])) :red [0 :red]))

costs

(def nodes (set (keys demo-graph)))

nodes

(defn next-node [costs nodes]
  (->> costs
       (filter #(nodes (first %)))
       (sort-by #(first (second %)))
       ffirst))

(next-node costs nodes)

(def cur
  (->> costs
       (filter #(nodes (first %)))
       (sort-by #(first (second %)))
       ffirst))

cur

(def cost
  (first (costs cur)))

cost

(cur demo-graph)

(defn neighbor-cost [[neighbor cost]]
   [neighbor [(first (neighbor costs)) cost]])

(filter #(nodes (first %))
        (into {} (map neighbor-cost (cur demo-graph))))

(defn update-neighbor [parent parent-cost costs [neighbor [old-cost cost]]]
  (let [new-cost (+ parent-cost cost)]
    (if (< new-cost old-cost)
      (assoc costs neighbor [new-cost parent])
      costs)))

(reduce #(update-neighbor cur cost %1 %2)
        costs
        (filter #(nodes (first %))
                (into {} (map neighbor-cost (cur demo-graph)))))

(disj nodes cur)

(comment

  demo-graph

  {:red {:green 10, :blue 5, :orange 8}
   :green {:red 10, :blue 3}
   :blue {:green 3, :red 5, :purple 7}
   :purple {:blue 7, :orange 2}
   :orange {:purple 2, :red 2}}

  #{:orange :green :red :blue :purple}

  (dijkstra :red :green demo-graph)
  (dijkstra-pm :red :green demo-graph))

(def graph-db
  (atom {:nodes (zipmap (keys demo-graph)
                        (repeat {:visited? false
                                 :distance Integer/MAX_VALUE}))
         :current-node nil}))

(defn initial-distances [initial-node]
  (swap! graph-db assoc-in
         [:nodes initial-node] {:distance 0
                                :current? true
                                :visited? false})
  (swap! graph-db assoc :current-node initial-node))

(initial-distances :red)

@graph-db