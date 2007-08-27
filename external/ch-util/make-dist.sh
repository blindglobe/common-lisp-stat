PACKAGE=ch-util
SYSTEMS=":${PACKAGE} :${PACKAGE}-test"

sbcl --noinform --noprint \
    --eval '(require :asdf)' \
    --eval "(pushnew (make-pathname :directory \""`pwd`"\") asdf:*central-registry*)" \
    --eval "(asdf:operate 'asdf:load-op 'ch-util)" \
    --eval "(asdf:operate 'asdf:load-op 'asdf-package)" \
    --eval "(asdf-package:package-system ${SYSTEMS})" \
    --eval '(quit)'
