(ns clozen.helpers-test
  (:use clojure.test
        clozen.helpers))

(deftest extract-test 
  (let [data [{:a nil :b 'data-to-extract} {:a nil :b 'more-data}]]
    (is (= (extract data :b) '(data-to-extract more-data)))))

(deftest coll-to-keys-test
  (let [data '(= (+ y 2) (+ (sin (/ x 2)) 3))
        expected-result {[2 1] '(sin (/ x 2)), [1] '(+ y 2), [1 2] 2, [1 1] 'y,
                         [2 1 1 1] 'x, [2 1 1 2] 2, [2 1 1] '(/ x 2),
                         [2 2] 3, [2] '(+ (sin (/ x 2)) 3)}]
    (is (= (coll-to-keys data (fn [elem pos] (zero? pos))) expected-result))))