PACKAGE=clem
SYSTEMS=":${PACKAGE} :${PACKAGE}-test :${PACKAGE}-doc :${PACKAGE}-benchmark"

sbcl --noinform --noprint \
    --eval '(require :asdf)' \
    --eval "(pushnew (make-pathname :directory \""`pwd`"\") asdf:*central-registry*)" \
    --eval "(asdf:operate 'asdf:load-op 'ch-util)" \
    --eval "(asdf:operate 'asdf:load-op 'asdf-package)" \
    --eval "(load \"make-tinaa-docs.lisp\")" \
    --eval "(asdf-package:package-system ${SYSTEMS})" \
    --eval '(quit)'
