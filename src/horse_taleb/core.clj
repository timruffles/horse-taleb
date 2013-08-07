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

(def unigrams frequencies)

(defn strings-to-sentences [posts]
  (->> posts
    get-sentences
    tokenize
    (map #(concat ['START-SENTENCE] %))
    ))

(defn prefixes-to-observed-ngrams [n sentences]
  (->> 
   sentences
   (mapcat #(partition n 1 %))
   (group-by #(take (- n 1) %))
  ))


(defn n-gram-model [n sentences]
   (let [prefixes-to-ngrams (prefixes-to-observed-ngrams n sentences)
         suffix-freqs (map frequencies (map #(last %) (vals prefixes-to-ngrams)))]
     (zipmap (keys prefixes-to-ngrams) suffix-freqs)
     )
  )

(defn frequences-to-cdf [freqs]
  (let [sum (reduce + (vals freqs))]
    (into {} (for [[k v] freqs] 
               [k (/ v sum)]
               ))
    )
  )

#_(defn random-cdf-item [cdf]
  (let [point rand]
  ))

(defn cdf-item-from-point [point cdf]
   (first (first (take-while (fn [[k v]] (<= point v)) cdf)))
  )

#_(defn next-word [chain max-n n-grams-models-by-n]
  (let [n (min max-n (count n-grams-models-by-n))
    ; choose a suffix of max-n if possible
        ; return
        ; backoff to (- max-n 1)
        word (random-cdf-item (n-grams-models-by-n n))
        ]
  ))


; take all n-grams and create suffix | prefix frequencies


#_(defn generate-sentence [max-length n-grams]
  (loop [
         grams []
         gram-step (start-gram n-grams)
        ]
    (if (> (gram-chain-count (concat grams grams-step) max-length))
      (gram-chain-to-sentence grams)
      (recur (concat grams grams-step) (next-gram gram-step n-grams))
      )
    )
  )

#_(defn format-sentence
  (let fn [to-remove '#{START-SENTENCE END-SENTENCE}]
  ))

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
