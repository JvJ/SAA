(ns rdl.rule
  (:use [clojure.set]
        [rdl.jpl]))

(def ^:dynamic *rule-tags*
  "Maps of relations to sets of rules.
If the modified relation is in the *modified-relations* set, 
then those rules should be fired."
  (atom {}))

(def ^:dynamic *rules*
  "Map of rule names to rules."
  (atom {}))

(def ^:dynamic *update-fns*
  ""
  (atom ()))

(defn clear
  "Clear the modified relations."
  []
  (reset! *modified-relations* #{}))

(defn make-filter
  "Creates a filter from a given vector.
For instance, [:X + 2] means that the value reprsented by :X
in a hash map should be incremented by 2 and re-associated in the
map."
  [[[kw1 kw2] f & args]]
  (fn [hm]
    (assoc hm kw2 (apply f (hm kw1) args))))

(defn defrule
  "Create a set of rules from a set of clojure terms."
  [nme & terms]
  (let [splits (partition-by #(= % :==>) terms)
        _ (when-not (or (= 5 (count splits)) (= 3 (count splits)))
            (throw (Exception. (str "Invalid rule def: \n" terms))))
        
        ;; Are the filters present?
        filters? (= 5 (count splits))
        
        precon (apply compose (first splits))
        precon-meta (meta precon)
        
        ;; Filters may or may not be present
        filters (if filters? (map make-filter (nth splits 2)) ())
        
        ;; Effect has a different index depending on whether
        ;; or not filters are present.
        effect (apply compose
                      (if filters?
                        (nth splits 4)
                        (nth splits 2)))
        effect-meta (meta effect)
        
        _ (when-not (empty? (:mod-rels precon-meta))
            (throw (Exception. "Preconditions cannot have side effects!")))
        ;; Enter the tags and the rules in the global maps
        rels (:rels precon-meta)
        
        ;; Change the map values to sets instead of symbols
        _ (swap! *rule-tags* (partial merge-with union) (into {}
                                                              (for [r rels]
                                                                [r #{nme}])))
        ;; Relations modified by the effects.
        mod-rels (:mod-rels effect-meta)
        
        ;; This is the function itself
        retfn
        (fn []
          (let [res (exec-query (clj-term precon))
                _ (println "res: " res)
                
                ;; Execute the filters!
                newres (map (fn [r]
                              (println "received r!" r)
                              (reduce #(%2 %1) r filters)) res)
                
                _ (println "filters: " filters)
                _ (println "newres: " newres)
                ;; We need to bind variables from the previous results
                eq-clauses (map (fn [m]
                                  (for [[k v] m]
                                    `(~'= ~k ~v))) newres)]
            (fn []
              (swap! *modified-relations* union mod-rels)
              (doseq [bind-list eq-clauses]
                (apply query
                       (concat bind-list [effect]))))))
        
        _ (swap! *rules* conj [nme retfn])
        ]
    retfn))

(defn update-head
  "Returns a set of update functions."
  []
  
  (->>
    (for [rel @*modified-relations*]
      (for [sym (@*rule-tags* rel)]
        ((@*rules* sym))))
    (apply concat)
    (reset! *update-fns*))
  
  (reset! *modified-relations* #{}))
  

(defn update-tail
  "Takes a set of update functions and executes them."
  []
  (doseq [f @*update-fns*]
    (f)))