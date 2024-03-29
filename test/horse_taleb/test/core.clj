(ns horse-taleb.test.core
  (:use [horse-taleb.core])
  (:use [clojure.pprint])
  (:use [clojure.string :only [split]])
  (:use [clojure.test]))

(deftest n-grams-test
  (let [expected {'("a" "b") {"a" 1/3 "b" 1}}
        corpus [
          ["a" "b" "a"]
          ["a" "b" "b"]
          ["a" "b" "b"]
        ]         
       ]
    
  (is (= expected (n-gram-model 3 corpus)))
         ))

(deftest n-grams-unigrams
  (let [expected {nil {"a" 4/9 "b" 1}}
        corpus [
          ["a" "b" "a"]
          ["a" "b" "b"]
          ["a" "b" "b"]
        ]         
       ]
    
  (is (= expected (n-gram-model 1 corpus)))
         ))

(deftest freqs-to-cdf
  (is (= {
          "a" 1/2
          "b" 3/4
          "c" 1
          } (frequences-to-cdf (frequencies ["a" "a" "b" "c"]))
      ))
         )

(deftest cdf-item-from-point-test
  (let [test-map {"a" 1/2 "b" 1}]
  (is (= "a" (cdf-item-from-point 0.5 test-map)))
  (is (= "b" (cdf-item-from-point 0.51 test-map)))
         ))

(deftest next-word-test
  (let [
        n-gram-models {3 {'("a" "b") {"a" 1/3 "b" 1}}}
      ]
    (is (not= nil (next-word ["a" "b"] 3 n-gram-models))))
         )

(deftest next-word-backoff-test
  (let [
        n-gram-models {1 {nil {"a" 1/3 "b" 1}}}
      ]
    (is (not= nil (next-word ["a" "b"] 3 n-gram-models))))
         )

#_(deftest prefixes-to-observed-ngrams-test
  (pprint (n-gram-model 3 [(split "of a middle-aged man boasting of his successes in college In addition, school produces fear" #" ")])))



(def test-sentences [
 "of a middle-aged man boasting of his successes in college In addition, school produces fear"
  "As you age, or get richer, you have more duties than. Nothing new for the members here but we placed it withing"
  "Best of all, your body weight drops up working for a C-minus entrepreneur"
                    ])

(deftest generate-sentence-test
  (let [
    a-sentence (generate-sentence 140 (strings-to-ngrams test-sentences 3))
   ]
  (is (< 0 (count a-sentence)))))


         


