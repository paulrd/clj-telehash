(ns clj-telehash.hashname-test
  (:require [clojure.test :refer :all]
            [clj-telehash.hashname :refer :all]))

(deftest keys-test
  (testing "should generate from two keys"
    (let [ks {"3a" "eg3fxjnjkz763cjfnhyabeftyf75m2s4gll3gvmuacegax5h6nia"
              "1a" "an7lbl5e6vk4ql6nblznjicn5rmf3lmzlm"}]
      (is (= (from-keys ks) 
             "27ywx5e5ylzxfzxrhptowvwntqrd3jhksyxrfkzi6jfn64d3lwxa")))))

(deftest one-key
  (testing "should generate from one key"
    (let [k {"1a" "vgjz3yjb6cevxjomdleilmzasbj6lcc7"}]
      (is (= (from-keys k)
             "echmb6eke2f6z2mqdwifrt6i6hkkfua7hiisgrms6pwttd6jubiq")))))

(deftest from-to-buffer
  (let [keys1 {"3a" "hp6yglmmqwcbw5hno37uauh6fn6dx5oj7s5vtapaifrur2jv6zha"
               "1a" "vgjz3yjb6cevxjomdleilmzasbj6lcc7"}
        buf (hex-to-buffer
             "3bfd832d8c85841b74ed76ff4050fe2b7c3bf5c9fcbb5981e0416348e935f64e")
        keys2 {"3a" buf}]
    (testing "make hashname from a buffer (byte array"
      (is (= (from-keys keys2) 
             "nzf4f6j7ylv53z3m4egrwltv2t2yks4rtpaimeg3avwqsoshqxba")))))

(deftest no-keys
  (testing "should fail with no keys"
    (is (thrown-with-msg? Throwable #"should not be empty" (from-keys {})))))

(deftest bad-key
  (testing "should fail with bad key"
    (let [k {"bad" "8jze4merv08q6med3u21y460fjdcphkyuc858538mh48zu8az39t1vxdg9tadzun"}]
      (is (thrown-with-msg? Throwable #"must be a valid hex string" 
                            (from-keys k))))))

(deftest packet
  (testing "making hashname from a packet"
    (let [json {"1a" "ym7p66flpzyncnwkzxv2qk5dtosgnnstgfhw6xj2wvbvm7oz5oaq" 
                "3a" true}
          json2 {"1a" "ym7p66flpzyncnwkzxv2qk5dtosgnnstgfhw6xj2wvbvm7oz5oaq"}
          key (hex-to-buffer 
               "3bfd832d8c85841b74ed76ff4050fe2b7c3bf5c9fcbb5981e0416348e935f64e")
          res "jvdoio6kjvf3yqnxfvck43twaibbg4pmb7y3mqnvxafb26rqllwa"]
      (is (= (from-packet {:json json :body key}) res))
      (is (= (from-packet {:json json2 :body key} "3a") res))))
  (testing "making a packet"
    (let [keys-map {"3a" "hp6yglmmqwcbw5hno37uauh6fn6dx5oj7s5vtapaifrur2jv6zha"
                    "1a" "vgjz3yjb6cevxjomdleilmzasbj6lcc7"}
          packet1 (to-packet keys-map "3a")
          packet2 (to-packet keys-map "1a")]
      (is (buffer? (:body packet1)))
      (is (= ((:json packet1) "1a") 
             "ym7p66flpzyncnwkzxv2qk5dtosgnnstgfhw6xj2wvbvm7oz5oaq"))
      (is (= ((:json packet2) "3a") 
             "bmxelsxgecormqjlnati6chxqua7wzipxliw5le35ifwxlge2zva")))))

(deftest valid-id
  (testing "check if id of cypher is valid"
    (is (id? "1a"))
    (is (= (id? "at") false))
    (is (= (id? "1 ") false))))

(deftest match-id
  (testing "return highest common cypher set id"
    (is (= (match ["1a" "2a" "44"] ["1a" "2a" "55"])
           "2a"))))

(deftest encode-decode
  (testing "base32 encode / decode"
    (is (= (base-32-encode (byte-array (map byte "foo")))
           "mzxw6"))
    (is (= (apply str (map char (base-32-decode "mzxw6"))) "foo"))))

(deftest valid-hashname
  (testing "return false for bad hashnames"
    (is (hashname? "anptpctxorixfzzj6dwwncwz3vzeessbhuokkfsdlx2upxw4qocq"))
    (is (= (hashname? {}) false))
    (is (= (hashname? "anptpctxorixfzzj6dwwncwz3vzeessbhuokkfsdlx2upxw4qoc") 
           false))))



