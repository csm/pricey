(ns pricey.util
  (require [clansi :refer [style]]))

(defn print-table
  "Like clojure.pprint/print-table, but uses awesome ansi colors."
  [rows & {:keys [colorize? header-color cell-color ks]
           :or {header-color [:yellow :bright] cell-color [:white :bright] ks (keys (first rows))}}]
   (when (seq rows)
     (let [widths (map
                   (fn [k]
                     (apply max (count (str k)) (map #(count (str (get % k))) rows)))
                   ks)
           spacers (map #(apply str (repeat % "─")) widths)
           fmts (map #(str "%" % "s") widths)
           maybe-style (fn [col is-header? color?]
                         (if color?
                           (apply style col (if is-header? header-color cell-color))
                           col))
           fmt-row (fn [leader divider trailer row is-header? color?]
                     (str leader
                          (apply str (interpose divider
                                                (for [[col fmt] (map vector (map #(get row %) ks) fmts)]
                                                  (maybe-style (format fmt (str col)) is-header? color?))))
                          trailer))]
         (println)
         (println (fmt-row "┌─" "─┬─" "─┐" (zipmap ks spacers) true false))
         (println (fmt-row "│ " " │ " " │" (zipmap ks ks) true colorize?))
         (println (fmt-row "├─" "─┼─" "─┤" (zipmap ks spacers) false false))
         (doseq [row rows]
           (println (fmt-row "│ " " │ " " │ " row false colorize?)))
         (println (fmt-row "└─" "─┴─" "─┘" (zipmap ks spacers) true false)))))

(defn try-with-backoff*
  [n ms thunk]
  (loop [n n
         pause ms]
    (if-let [result (try
                      [(thunk)]
                      (catch Exception e
                        (if (zero? n)
                          (throw e)
                          (Thread/sleep pause))))]
      (result 0)
      (recur (dec n) (+ pause ms)))))

(defmacro try-with-backoff
  [n ms & body]
  `(try-with-backoff* ~n ~ms (fn [] ~@body)))
