
(import scheme (chicken base) (aux base) (aux simdjson))

#;(display (->string/json (simdjson-load "twitter.json")))
(display (identity (simdjson-load/ondemand "twitter.json")))
