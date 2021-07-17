(ns aoc-2020.day8
  (:require
   [clojure.string :as str]))

(defn- input->commands
  [input]
  (->> input
       (str/split-lines)
       (mapv (fn [line]
               (let [[_ op arg] (re-find #"(\w+) ([+|-]\d+)" line)]
                 {:op  op
                  :arg (Long/parseLong arg)})))))

(defn incr-pc
  ([ctx]
   (incr-pc ctx 1))
  ([ctx amount]
   (update ctx :pc + amount)))

(defmulti execute (fn [{:keys [op]} ctx] op))

(defmethod execute "nop"
  [_cmd ctx]
  (incr-pc ctx))

(defmethod execute "jmp"
  [{:keys [arg]} ctx]
  (incr-pc ctx arg))

(defmethod execute "acc"
  [{:keys [arg]} ctx]
  (-> ctx
      (update :accum + arg)
      (incr-pc)))

(defn step
  [{:keys [pc] :as ctx}]
  (if-let [cmd (get-in ctx [:cmds pc])]
    (execute cmd ctx)
    (incr-pc ctx)))

(defn build-trace
  [cmds]
  (let [initial-ctx {:accum 0
                     :pc    0
                     :cmds  cmds}]
    (iterate step initial-ctx)))

(defn run-trace
  [trace]
  (reduce
    (fn [{:keys [visited] :as m} {:keys [^long pc cmds] :as ctx}]
      (if (>= pc (count cmds))
        (reduced (assoc ctx :result :finished))
        (if (contains? visited pc)
          (reduced (assoc ctx :result :loop))
          (update m :visited conj pc))))
    {:visited #{}}
    trace))

(defn run-until-loop
  [input]
  (->> (input->commands input)
       (build-trace)
       (run-trace)))

(def substitutions {"nop" "jmp"
                    "jmp" "nop"})

(defn can-alter?
  [{:keys [op]}]
  (some? ((set (keys substitutions)) op)))

(defn find-next-alterable
  [cmds ^long start-idx]
  (let [cmds-to-search (subvec cmds start-idx)]
    (if-let [^long idx (->> cmds-to-search
                            (map-indexed vector)
                            (filter (fn [[_idx cmd]] (can-alter? cmd)))
                            (take 1)
                            (ffirst))]
      (+ start-idx idx)
      nil)))

(defn alter-cmd
  [{:keys [op] :as cmd}]
  (assoc cmd :op (substitutions op)))

(defn alter-cmds
  [{:keys [original idx result] :as trace-ctx}]
  (if (some? result)
    nil
    (if-let [next-idx ^long (find-next-alterable original idx)]
      (assoc trace-ctx :cmds (update original next-idx alter-cmd)
                       :idx (inc next-idx))
      (assoc trace-ctx :result :finished))))

(defn build-alt-traces
  [cmds]
  (let [trace-ctx {:original cmds
                   :cmds     cmds
                   :idx      0}]
    (->> (iterate alter-cmds trace-ctx)
         (take-while some?)
         (map :cmds)
         (map build-trace))))

(defn run-traces
  [input]
  (->> (input->commands input)
       (build-alt-traces)
       (map run-trace)
       (filter #(= :finished (:result %)))
       (take 1)
       (first)))
