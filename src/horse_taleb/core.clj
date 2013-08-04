(ns horse-taleb.core
  [:require [clojure.data.json :as json]
            [clojure.string :as string]
   ]
  [:use [clojure.contrib.seq :only [indexed]]]
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

(defn get-page-posts [id]
  (let [
        url (fb-graph-url (str id "/posts"))]
    (-> (slurp url)
      (json/read-str :key-fn clojure.core/keyword)
      :data
      ((partial map :message))
    )))

(defn sentences [posts]
  (-> 
    (string/join " " posts)
    (string/split #"\b\. +|\n[\n\s]+")
    ((partial filter #(not= % "")))
    ))

(defn split-to-length [n xs]
  (map string/join (partition n xs))
  )

(defn half-sentences [sentences]
  (->> sentences
    (map #(string/split % #" "))
    (map #(split-at (/ (count %) 2) %))
    (apply concat)
    (map #(string/join " " %))
    (map #(split-to-length 70 %))
    (apply concat)
  ))

(defn horsey-run []
  (swap! posts update-post (get-page-posts 13012333374))
  (let [
    chunks (half-sentences (sentences @posts))
  ]
    (while true
      (prn (string/join " " [ (rand-nth chunks) (rand-nth chunks) ]))
      (Thread/sleep 500)
      )
    )
  )

(defn -main [& args]
  (horsey-run))
