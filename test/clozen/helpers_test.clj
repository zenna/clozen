(ns clozen.helpers-test
  (:use clojure.test
        clozen.helpers))

(deftest consistent?-test
  (let [mixed-data [3 2 1]
        even-data '(6 2 4)]
    (is (= (consistent? even? mixed-data) false))
    (is (= (consistent? odd? even-data) true))))

(deftest extract-test 
  (let [data [{:a nil :b 'data-to-extract} {:a nil :b 'more-data}]]
    (is (= (extract data :b) '(data-to-extract more-data)))))

(deftest coll-to-keys-test
  (let [data '(= (+ y 2) (+ (sin (/ x 2)) 3))
        expected-result {[2 1] '(sin (/ x 2)), [1] '(+ y 2), [1 2] 2, [1 1] 'y,
                         [2 1 1 1] 'x, [2 1 1 2] 2, [2 1 1] '(/ x 2),
                         [2 2] 3, [2] '(+ (sin (/ x 2)) 3)}]
    (is (= (coll-to-keys data (fn [elem pos] (zero? pos))) expected-result))))

(deftest count=-test
  (is (true? (count= [1 2 3] [2 2 3] [2 1 2])))
  (is (false? (count= [1 2 3] [1 2 2 3] [2 1 2])))
  (is (true? (count= [2 1 2]))))

(deftest counts-test
  (is (= {'a 2 'b 1 'c 1 'd 3}
         (counts '[a b c d d d a]))))

(deftest normalise-test
  (is (= (normalise [1 2 3]) [1/6 2/6 3/6])
  (is (tolerant= (sum (normalise (repeatedly 10 rand)))
                  1.0))))

(deftest pass-test
  (pass + [] [4 5 9 8]))

(deftest categorical-test
  (let [objects '[a b c]
        weights [1 2 3]
        samples (repeatedly 1000 #(categorical objects weights))
        sample-counts (counts samples)]
    (every? 
      #(<= (Math/abs
              (double (- (/ (val %) 1000)
                         ((zipmap objects (normalise weights)) (key %)))))
           0.1)
      (seq sample-counts))))

(deftest vec-remove-test
  (is (= (vec-remove [1 2 3 4 5] 2) [1 2 4 5])))