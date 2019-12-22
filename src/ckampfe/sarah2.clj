(ns ckampfe.sarah2
  (:gen-class))

(def keyspace "_abcdefghijklmnopqrstuvwxyz")

(defn pprint-sbox [sbox]
  (let [keyspace-as-strs (->> keyspace
                              (partition 1)
                              (map (comp str first)))
        sbox-as-rows (partition 27 sbox)
        header-row (cons " "
                         (map (fn [letter] (str " " letter))
                              keyspace-as-strs))
        rows-with-prefix-column (map (fn [prefix-letter row]
                                       (cons prefix-letter row))
                                     keyspace-as-strs
                                     sbox-as-rows)
        rows-as-strings (map (fn [row]
                               (clojure.string/join " " row))
                             (cons header-row rows-with-prefix-column))]

    (clojure.string/join "\n" rows-as-strings)))

(defn generate-sbox []
  (let [letters (seq (char-array keyspace))]
    (shuffle (for [letter1 letters
                   letter2 letters]
               (str letter1 letter2)))))

(defn sbox->map [sbox]
  (let [keyspace-as-strs (->> keyspace
                              (partition 1)
                              (map (comp str first)))
        rowcols (for [row keyspace-as-strs
                      col keyspace-as-strs]
                  (str row col))]

    (zipmap rowcols sbox)))

(defn permute [s]
  (let [s-vec (mapv str s)
        len (count s)
        idxs (range len)
        even-odd (into {} (map (fn [[k v]] [k (sort v)]) (group-by even? idxs)))
        odd (get even-odd true)
        even (get even-odd false)
        odd-even-indexes (concat odd even)]
    (clojure.string/join (map (fn [i] (nth s-vec i)) odd-even-indexes))))

(defn unpermute [s]
  (assert (even? (count s)) "input must be even")
  (let [[left right] (split-at (/ (count s) 2) s)]
    (clojure.string/join (mapcat (fn [a b] [a b]) left right))))

(= "attack_at_dawn" (unpermute (permute "attack_at_dawn")))

(defn next-highest-power-of-2 [n]
  (last (take-while (fn [i] (>= n (/ i 2)))
                    (map (fn [i] (Math/pow 2 i))
                         (range)))))

(defn log2 [^long n]
  (/ (Math/log n) (Math/log 2)))

(defn encrypt-round
  [padded-plaintext sbox]
  (let [pairs (map clojure.string/join (partition 2 padded-plaintext))
        sbox-as-map (sbox->map sbox)
        pairs-from-sbox-map (map (fn [pair]
                                   (get sbox-as-map pair))
                                 pairs)
        substituted (clojure.string/join pairs-from-sbox-map)]

    (permute substituted)))

(defn encrypt
  ([plaintext sbox]
   (encrypt plaintext sbox :log2n*2))
  ([plaintext sbox number-of-rounds-strategy]
   (let [padded-plaintext (if (even? (count plaintext))
                            plaintext
                            (clojure.string/join (conj (mapv identity plaintext) "_")))
         log2-plaintext-length (log2 (next-highest-power-of-2 (count padded-plaintext)))
         log2n*2 (* log2-plaintext-length 2)
         number-of-rounds (case number-of-rounds-strategy
                            :log2n log2-plaintext-length
                            :log2n+2 (+ log2-plaintext-length 2)
                            :log2n*2 (* log2-plaintext-length 2))]

     (last (take number-of-rounds (iterate (fn [s] (encrypt-round s sbox)) plaintext))))))

(defn decrypt-round [ciphertext sbox]
  (let [sbox-as-map (sbox->map sbox)
        inverted-sbox (clojure.set/map-invert sbox-as-map)
        unpermuted (unpermute ciphertext)
        pairs (map clojure.string/join (partition 2 unpermuted))
        rows-cols (map (fn [pair]
                         (get inverted-sbox pair))
                       pairs)
        unsubstituted (clojure.string/join rows-cols)]

    unsubstituted))

(defn decrypt
  ([ciphertext sbox] (decrypt ciphertext sbox :log2n*2))
  ([ciphertext sbox number-of-rounds-strategy]
   (let [log2-ciphertext-length (log2 (next-highest-power-of-2 (count ciphertext)))
         log2n*2 (* log2-ciphertext-length 2)
         number-of-rounds (case number-of-rounds-strategy
                            :log2n log2-ciphertext-length
                            :log2n+2 (+ log2-ciphertext-length 2)
                            :log2n*2 (* log2-ciphertext-length 2))]

     (last (take number-of-rounds (iterate (fn [s] (decrypt-round s sbox)) ciphertext))))))

(comment
  (def sb (generate-sbox))

  (time (encrypt "attack_at_dawn" sb))

  (-> "attack_at_dawn"
      (encrypt sb)
      (decrypt sb)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
