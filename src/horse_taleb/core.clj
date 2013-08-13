(ns horse-taleb.core
  [:require [clojure.data.json :as json]
            [clojure.string :as string]
   ]
  [:use [clojure.contrib.seq :only [indexed]]
        [opennlp.nlp :only [make-sentence-detector make-tokenizer]]
        [clojure.pprint :only [pprint]]
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
        data (slurp url)
        ]
    (spit "/tmp/taleb.json" data)
    (spit "corpa/facebook.json" (json/write-str (read-fb-data data)))
    (read-fb-data data))
  )

(defn get-page-file [id]
  (read-fb-data (slurp "/tmp/taleb.json")))


(def get-sentences (make-sentence-detector "en-sent.bin"))
(def tokenize (make-tokenizer "en-token.bin"))


(defn strings-to-sentences [strings]
  (->> strings
    (map string/lower-case)
    (mapcat get-sentences)
    (map tokenize)
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
           suffix-freqs (map (comp frequences-to-cdf frequencies) (map #(map last %) (vals prefixes-to-ngrams)))]
       (zipmap (keys prefixes-to-ngrams) suffix-freqs)
       ))
  )

(defn strings-to-ngrams [strings max-n]
  (let [sentences (strings-to-sentences strings)]
    (reduce (fn [all n]
      (into all {n (n-gram-model n sentences)})
      ) {} (range 1 (+ max-n 1)))
    )
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
  (let [no-space #{"" "?" "." ")" "(" "[" "]" "," ";" ":"}]
    (string/join "" (mapcat (fn [[w n]]
      (if (no-space n)
          w
          [w " "]
            )) (partition 2 1 (concat s [""])))
      )
  ))

(defn generate-sentence [max-length n-grams]
  (loop [
         words []
        ]
    (let [word-candidate (next-word words max-length n-grams)
          sentence-candidate (concat words [word-candidate])
          ]
      (if (> (count (format-sentence sentence-candidate)) max-length)
        (format-sentence words)
        (recur sentence-candidate)
        )
    ))
  )

(def json-slurp (comp json/read-str slurp))


(defn horsey-run []
  (swap! posts update-post (json-slurp "corpa/facebook.json"))
  (swap! posts update-post (concat @posts (json-slurp "corpa/notebook.json")))
  (let [
    n-grams (strings-to-ngrams @posts 3)
  ]
    (while true
      (prn (generate-sentence (+ 40 (rand-int 100)) n-grams))
      (Thread/sleep 500)
      )
    )
  )

(defn -main [& args]
  (horsey-run))
