(ns horse-taleb.test.core
  (:use [horse-taleb.core])
  (:use [clojure.test]))

(deftest n-grams-test
  (let [expected {'(\a \b) {\a 1/3 \b 1}}
        corpus [
          [\a \b \a]
          [\a \b \b]
          [\a \b \b]
        ]         
       ]
    
  (is (= expected (n-gram-model 3 corpus)))
         ))

(deftest n-grams-unigrams
  (let [expected {nil {\a 4/9 \b 1}}
        corpus [
          [\a \b \a]
          [\a \b \b]
          [\a \b \b]
        ]         
       ]
    
  (is (= expected (n-gram-model 1 corpus)))
         ))

(deftest freqs-to-cdf
  (is (= {
          \a 1/2
          \b 3/4
          \c 1
          } (frequences-to-cdf (frequencies [\a \a \b \c]))
      ))
         )

(deftest cdf-item-from-point-test
  (let [test-map {\a 1/2 \b 1}]
  (is (= \a (cdf-item-from-point 0.5 test-map)))
  (is (= \b (cdf-item-from-point 0.51 test-map)))
         ))

(deftest next-word-test
  (let [
        n-gram-models {3 {'(\a \b) {\a 1/3 \b 1}}}
      ]
    (is (not= nil (next-word [\a \b] 3 n-gram-models))))
         )


