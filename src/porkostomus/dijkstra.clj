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

(def computerphile
  {:s {:a 7 :b 2 :c 3}
   :a {:s 7 :b 3 :d 4}
   :b {:s 2 :a 3 :d 4 :h 1}
   :c {:s 3 :l 2}
   :d {:a 4 :b 4 :f 5}
   :e {:g 2 :k 5}
   :f {:d 5 :h 3}
   :g {:h 2 :e 2}
   :h {:b 1 :f 3 :g 2}
   :i {:l 4 :j 6 :k 4}
   :j {:i 6 :l 4 :k 4}
   :k {:i 4 :j 4 :e 5}})

(dijkstra :s :e computerphile)

; 1. Mark all nodes unvisited. Create a set of all the unvisited nodes called the unvisited set.
; 2. Assign to every node a tentative distance value: 
;    set it to zero for our initial node and to infinity for all other nodes.
;    Set the initial node as current.

(def graph-db (atom {}))

(defn init-graph! [graph initial-node]
  (swap! graph-db assoc :nodes (zipmap (keys graph) (repeat {:distance Integer/MAX_VALUE
                                                             :parent nil})))
  (swap! graph-db assoc :unvisited (set (keys graph)))
  (swap! graph-db assoc-in [:nodes initial-node :distance] 0)
  (swap! graph-db assoc :current-node initial-node))

(init-graph! computerphile :s)

(defn update-node! [node]
  (swap! graph-db assoc-in [:nodes node :distance] (node ((:current-node @graph-db) computerphile)))
  (swap! graph-db assoc-in [:nodes node :parent] :s))

(map update-node! (keys (:s computerphile)))

@graph-db

; 3. For the current node, consider all of its unvisited neighbours
;    and calculate their tentative distances through the current node.
;    Compare the newly calculated tentative distance to the current assigned value and assign the smaller one.

;    For example, if the current node A is marked with a distance of 6,
;    and the edge connecting it with a neighbour B has length 2,
;    then the distance to B through A will be 6 + 2 = 8.
;    If B was previously marked with a distance greater than 8 then change it to 8.
;    Otherwise, the current value will be kept.



; 4. When we are done considering all of the unvisited neighbours of the current node, mark the current node as visited and remove it from the unvisited set. A visited node will never be checked again.
; 5. If the destination node has been marked visited (when planning a route between two specific nodes) or if the smallest tentative distance among the nodes in the unvisited set is infinity (when planning a complete traversal; occurs when there is no connection between the initial node and remaining unvisited nodes), then stop. The algorithm has finished.
; 6. Otherwise, select the unvisited node that is marked with the smallest tentative distance, set it as the new "current node", and go back to step 3.
