(reset-handler abort)
(import (swish mat))
(if (for-all values
      (map run-mats-from-file
        (if (null? (command-line-arguments))
            '(
              "swish/cli.ms"
              "swish/erlang.ms"
              "swish/heap.ms"
              "swish/html.ms"
              "swish/io.ms"
              "swish/json.ms"
              "swish/meta.ms"
              "swish/pregexp.ms"
              "swish/stream.ms"
              "swish/string-utils.ms"
              )
            (command-line-arguments))))
    (exit 0)
    (begin
      (printf "Tests FAILED~n")
      (exit 1)))
