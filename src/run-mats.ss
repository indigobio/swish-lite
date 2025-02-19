(reset-handler abort)
(define profile? (equal? (getenv "PROFILE_MATS") "yes"))
(when profile?
  (compile-profile #t)
  (compile-interpret-simple #f)
  (cp0-effort-limit 0)
  (run-cp0 (lambda (f x) x)))
(import (swish mat))
(define passed?
  (for-all values
    (map run-mats-from-file
      (if (null? (command-line-arguments))
          '(
            "swish/cli.ms"
            "swish/dsm.ms"
            "swish/erlang.ms"
            "swish/heap.ms"
            "swish/html.ms"
            "swish/io.ms"
            "swish/json.ms"
            "swish/meta.ms"
            "swish/oop.ms"
            "swish/pregexp.ms"
            "swish/stream.ms"
            "swish/string-utils.ms"
            )
          (command-line-arguments)))))
(when profile?
  (unless (file-directory? "profile/")
    (mkdir "profile"))
  (profile-dump-html "profile/"))
(cond
 [passed? (exit 0)]
 [else
  (printf "Tests FAILED~n")
  (exit 1)])
