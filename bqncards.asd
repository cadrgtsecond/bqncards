(in-package :asdf-user)

(defsystem "bqncards"
  :author "Abhinav Krishna <abhinavkrishnacr2020@gmail.com>"
  :description "BQN Documentation, but better"
  :depends-on ("cmark" "ten" "iterate" "str")
  :components
  ((:module "src"
    :components
    ((:file "main")))))
