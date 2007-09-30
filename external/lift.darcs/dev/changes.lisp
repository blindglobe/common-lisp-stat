#|
ok - in run-test, we should catch errors / failures in test-setup and test-teardown too
ok~ - better warning and errors and restarts
ok - make interaction an option (and don't print stuff if compiling or loading)
ok - add *features* check to not make tests (NO-LIFT-TESTS)
ok - add counts to list-tests
ok~ - When working interactively, bad tests get inserted and then it's hard to
  fix things. Maybe compile immediately and check for warnings / errors
  with handler-case. A remove-test command (but when is it invoked)?
not - allow addtest to add multiple tests or add addtests
ok - downcase create test message (per Westy)
ok - need with-test or something like it
ok - need list-tests (both names and classes)
ok - remove initforms, this lets us pretend that we're in something like a let*
ok I'd like run-tests to not require any arguments
ok - allow for :setup ... and (:setup ...)
ok - addtest can specify a test-suite / class
ok - allow (:documentation for each test), but where to put it
ok get rid of abstract-test
ok - interactive-test-switch: when you add or define, run the tests right then 
ok - add code to optionally test all subclasses of a test class
deferred - better error message when there is no test name
ok - don't require superclasses or slots (if only one, assume it's slots unless
  one of the putative slots is already a test-mixin subclass)
ok - put initforms of test class into the setup automatically (even if no
  other setup is defined. (maybe a :before)
ok - currently, each addtest will add a new test-case with the same test but a 
  different name, we need to save the text and compare. It would be somewhat
  gross, but we could sxhash the contents of the test for a mostly good fix.
ok - don't allow two tests for the same class to have the same name
ok - addtest should redefine existing tests--maybe with a warning
ok - allow automatic naming but save code too so that it's easy to see 
  what went wrong
ok - need to empty hash-tables in deftest before parsing the tests
ok - test-names must be keywords? Why is that again? Can we check and
  warn for errors
ok - allow for export-p option
ok - cannot do (addtest (ensure-warning (let ((x 0)) (print (/ 4 x)))))
ok - how about (remove-test [(class)] name)
ok - tests that a form generates an error or a warning
ok - add :test parameter to ensure-same (and redo to get better messages)
ok - addtest should handle verbose? too and print the full details of the
  single problem, if any
~ - Add interactivity to make-sure-slots-are-not-superclasses
ok - If there are superclasses, add all of their slots to the slot list too
ok - should redefining the superclass remove all methods of its subclasses?
  (probably, could have slot dependencies)
ok - need runtest
ok - refactor print-test-report
obsolete - handle errors and use restarts in make-sure-slots-are-not-superclasses
ok? - deftest's defclass repeats initargs and accessors
ok - deftest's defclass has slots in wrong order
ok - undeftest ==> remove-test 
ok - use *test-output* as stream
ok - use dynamic scope for *TEST-IS-BEING-DEFINED?* rather than unwind-protect
obsolete - add :evaluate option to ensure-same
obsolete - maybe ensure= instead of ensure-same
ok - refactor ensure, etc.
ok - get rid of #FEATURE-CASE

|#