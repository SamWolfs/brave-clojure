(ns fwpd.core)
(def filename "suspects.csv")

(def vamp-keys [:name :glitter-index])

(defn str->int
  [str]
  (Integer. str))

(def conversions {:name identity
                  :glitter-index str->int})

(def validators {:name true
                  :glitter-index true})

(defn convert
  [vamp-key value]
  ((get conversions vamp-key) value))

(defn parse
  "Convert a CSV into rows of columns"
  [string]
  (map #(clojure.string/split % #",")
       (clojure.string/split string #"\n")))

(defn mapify
  "Returns a seq of maps like {:name \"Edward Cullen\" :glitter-index 10}"
  [keys rows]
  (map (fn [unmapped-row]
         (reduce (fn [row-map [key value]]
                   (assoc row-map key (convert key value)))
                 {}
                 (map vector keys unmapped-row)))
       rows))

(def mapify->vampires
  (partial mapify vamp-keys))

(defn mapify-csv
  [mapper filename]
  (mapper (parse (slurp filename))))

(defn glitter-filter
  [minimum-glitter records]
  (map #(:name %)
       (filter #(>= (:glitter-index %) minimum-glitter) records)))

(defn validate
  [validators record]
  (reduce (fn [valid? [key value]]
            (and valid? (get validators key)))
          true
          record))

(defn append
  [record records]
  (if (validate validators record)
    (cons record records)
    records))

(defn demapify-csv
  [records]
  (clojure.string/join "\n" (map #(clojure.string/join "," (vals %)) records)))

;; EXAMPLES
;; (glitter-filter 3 (mapify-csv mapify->vampires filename))
