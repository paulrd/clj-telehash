(ns clj-telehash.hashname
  (:import org.apache.commons.codec.binary.Base32
           org.apache.commons.codec.binary.Hex
           org.apache.commons.codec.digest.DigestUtils))

(defn hex-to-buffer 
  "Takes a hex number as a string and converts it to a byte-array."
  [hex-str]
  (-> hex-str char-array Hex/decodeHex))

(defn base-32-encode
  "simple wrapper to handle buffer (byte-array) -> base32"
  [ba]
  (->> ba (.encodeToString (Base32.)) .toLowerCase (#(.split % "=")) 
       (String/join "")))

(defn base-32-decode
  "simple base32 -> buffer (byte-array) decoder"
  [b32]
  (->> b32 .toUpperCase (.decode (Base32.))))

(defn- rf [rollup [k v]]
  (let [ru (try (-> k (Byte/parseByte 16) vector byte-array (#(concat rollup %))
                    byte-array DigestUtils/sha256)
                (catch NumberFormatException e 
                  (throw (Throwable. "cipher key must be a valid hex string"))))]
    (->> v (concat ru) byte-array DigestUtils/sha256)))

(defn buffer? [id]
  (= (.getClass id) (Class/forName "[B")))

(defn- decode-if-not-buffer [id]
  (if (buffer? id) id (base-32-decode id)))

(defn from-keys
  "generate hashname from keys json, vals are either base32 keys or key binary
  Buffer's"
  [keys-map]
  (when (empty? keys-map) (throw (Throwable. "keys-map should not be empty")))
  (->> keys-map 
       (reduce-kv 
        #(->> %3 decode-if-not-buffer DigestUtils/sha256 (assoc %1 %2))
        (sorted-map))
       (reduce rf (byte-array [])) base-32-encode))

(defn- rpf 
  "ruduce fn for packet to create intermediate buffers"
  [body hint m k v]
  (let [res (assoc m k (if (= v true) (DigestUtils/sha256 body)
                           (base-32-decode v)))]
    (if hint (assoc res hint (DigestUtils/sha256 body))
        res)))

(defn from-packet 
  "create hashname from a packet"
  [packet & [hint]]
  (->> packet :json (reduce-kv (partial rpf (:body packet) hint) (sorted-map))
       (reduce rf (byte-array [])) base-32-encode))

(defn to-packet
  "generate the more compact packet format"
  [key-map id]
  (let [json (reduce-kv 
              #(assoc %1 %2 (if (= %2 id) true 
                                (-> %3 base-32-decode DigestUtils/sha256
                                    base-32-encode)))
              (sorted-map) key-map)]
    {:json json :body (base-32-decode (key-map id))}))

(defn hashname? [hn]
  (and (string? hn) (= (count hn) 52) (= (count (base-32-decode hn)) 32)))

(defn id? [id]
  (or (and (buffer? id)
           (= (count id) 1))
      (and (string? id) (= (count id) 2)
           (try 
             (= (format "%x" (Byte/parseByte id 16)) id)
             (catch NumberFormatException _ false)))))

(defn match [keys1 keys2]
  (-> keys1 set (clojure.set/intersection (set keys2)) sort last))

