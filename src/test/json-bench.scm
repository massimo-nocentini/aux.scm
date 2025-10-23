
(import scheme (chicken base) (chicken pretty-print) (chicken port) (chicken io) (aux base) (aux simdjson) (chicken pretty-print) json json-abnf)

(display "Json egg, twitter.json benchmark:\n")
(time (call-with-input-file "./test/twitter.json" json-read))

(display "Json abnf egg, twitter.json benchmark:\n")
(time (call-with-input-file "./test/twitter.json" (λ (port) (parser (read-string #f port)))))

(display "Simdjson egg, twitter.json benchmark:\n")
(time (simdjson-load/ondemand "./test/twitter.json"))

(display "Json egg, users_100k.json benchmark:\n")
(time (call-with-input-file "./test/users_100k.json" json-read))

(display "Json abnf egg, users_100k.json benchmark:\n")
(time (call-with-input-file "./test/users_100k.json" (λ (port) (parser (read-string #f port)))))

(display "Simdjson egg, users_100k.json benchmark:\n")
(time (simdjson-load/ondemand "./test/users_100k.json"))

(display "Json abnf egg, users_1.7m.json benchmark:\n")
(time (call-with-input-file "./test/users_1.7m.json" (λ (port) (parser (read-string #f port)))))

(display "Simdjson egg, users_1.7m.json benchmark:\n")
(time (simdjson-load/ondemand "./test/users_1.7m.json"))
