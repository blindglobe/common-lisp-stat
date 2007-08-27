
(require 'asdf)

(asdf:operate 'asdf:load-op 'ch-util)
(asdf:operate 'asdf:load-op 'ch-util-test)

(ch-util-test:run-tests)