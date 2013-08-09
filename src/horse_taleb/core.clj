(ns horse-taleb.core
  [:require [clojure.data.json :as json]
            [clojure.string :as string]
   ]
  [:use [clojure.contrib.seq :only [indexed]]
        [opennlp.nlp :only [make-sentence-detector make-tokenizer]]
   ]
  )

(def fb-credentials {
                     :id (System/getenv "FB_ID")
                     :secret (System/getenv "FB_SECRET")
                     :token (System/getenv "FB_TOKEN")
                    })


(def posts (atom {}))

(defn update-post [current new-value] new-value)

(defn fb-graph-url [path]
  (str "https://graph.facebook.com/" path "?access_token=" (:token fb-credentials))
  )

(defn read-fb-data [data]
    (-> data
      (json/read-str :key-fn clojure.core/keyword)
      :data
      ((partial map :message))
    ))

(defn get-page-posts [id]
  (let [
        url (fb-graph-url (str id "/posts"))
        ]
    (read-fb-data (slurp url)))
  )

(defn get-page-file [id]
  (read-fb-data (slurp "/tmp/taleb.json")))


(def get-sentences (make-sentence-detector "en-sent.bin"))
(def tokenize (make-tokenizer "en-token.bin"))


(defn strings-to-sentences [strings]
  (->> strings
    (mapcat get-sentences)
    (map tokenize)
    (map #(concat ["START-SENTENCE"] %))
    ))

(defn prefixes-to-observed-ngrams [n sentences]
  (->> 
   sentences
   (mapcat #(partition n 1 %))
   (group-by #(take (- n 1) %))
  ))

(defn frequences-to-cdf [freqs]
  (let [sum (reduce + (vals freqs))]
    (letfn [
        (make-cdf [m k v]
          (let [current (+ (:cume m) (/ v sum))]
          {
            :cume current
            :all  (into (:all m) {k current})
          }
          ))
      ]
      (:all (reduce-kv make-cdf {:cume 0 :all {}} freqs))
    )
  )
)

(defn unigrams [words] {nil (frequences-to-cdf (frequencies words))})

(defn n-gram-model [n sentences]
   (if (= n 1)
     (unigrams (apply concat sentences))
     (let [prefixes-to-ngrams (prefixes-to-observed-ngrams n sentences)
           suffix-freqs (map (comp frequences-to-cdf frequencies) (map #(last %) (vals prefixes-to-ngrams)))]
       (zipmap (keys prefixes-to-ngrams) suffix-freqs)
       ))
  )

(defn strings-to-ngrams [strings max-n]
  (reduce (fn [all n]
    (into all {n (n-gram-model n (strings-to-sentences strings))})
    ) {} strings)
  )


(defn cdf-item-from-point [point cdf]
   (first (first (drop-while (fn [[k v]] (> point v)) cdf)))
  )

(defn random-cdf-item [cdf]
  (cdf-item-from-point (rand) cdf))

(defn n-gram-suffix [prefix n-grams]
  (let [
    suffixes (get n-grams prefix)
    ]
    (if suffixes
      (random-cdf-item suffixes))
    ))

(defn next-word [chain max-n n-grams-models-by-n]
  (let [n (min max-n (apply max (keys n-grams-models-by-n)))
        prefix (take-last (- n 1) chain)
        word (n-gram-suffix prefix (or (get n-grams-models-by-n n) {}))
        ]
      (or word 
          (if (= n 0)
            nil
            (next-word chain (- n 1) n-grams-models-by-n)))
  ))



; take all n-grams and create suffix | prefix frequencies

(defn format-sentence [s]
  (let [to-remove '#{"START-SENTENCE"}]
    (string/join " " (filter #(not (to-remove %)) s))
  ))

(defn generate-sentence [max-length n-grams]
  (loop [
         words ["START-SENTENCE"]
        ]
    (let [word-candidate (next-word words max-length n-grams)]
      (if (> (count (format-sentence (concat words word-candidate))) max-length)
        (format-sentence words)
        (recur (concat words word-candidate))
        )
    ))
  )


#_(defn horsey-run []
  (swap! posts update-post (get-page-file 13012333374))
  (let [
    chunks (half-sentences (sentences @posts))
  ]
    (while true
      (prn (string/join " " [ (rand-nth chunks) (rand-nth chunks) ]))
      (Thread/sleep 500)
      )
    )
  )

#_(defn -main [& args]
  (horsey-run))
