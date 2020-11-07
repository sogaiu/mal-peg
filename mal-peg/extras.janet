(import ./grammar :prefix "")

# XXX: with-meta
(def cap-grammar
  (let [cg (table ;(kvs grammar))]
    (each kwd [# :comment # XX: don't capture comments
               :boolean :keyword :number
               :string :symbol
               # :ws # XXX: dont' capture whitespace
              ]
          (put cg kwd
               ~(cmt (capture ,(in cg kwd))
                     ,|[kwd $])))
    (each kwd [:deref :quasiquote :quote :splice-unquote :unquote]
          (put cg kwd
               ~(cmt (capture ,(in cg kwd))
                     ,|[kwd ;(slice $& 0 -2)])))
    (each kwd [:list :hash-map :vector]
          (put cg kwd
               (tuple # array needs to be converted
                 ;(put (array ;(in cg kwd))
                       2 ~(cmt (capture ,(get-in cg [kwd 2]))
                               ,|[kwd ;(slice $& 0 -2)])))))
    # tried using a table with a peg but had a problem, so use a struct
    (table/to-struct cg)))

(comment

  (peg/match cap-grammar "false")
  # => '@[(:boolean "false") "false"]

  (peg/match cap-grammar "[1 2 3]")
  ``
  '@[(:vector
      (:number "1")
      (:number "2")
      (:number "3")) "[1 2 3]"]
  ``

  (peg/match cap-grammar ";; hi there")
  # => @[";; hi there"]

  )

# XXX: this grammar seems to lead to things being difficult
#      e.g. supporting both lists and vectors
#      e.g. supporting metadata
(def read-grammar
  (let [cg (table ;(kvs grammar))]
    (put cg :boolean
            ~(replace (capture ,(in cg :boolean))
                      ,|(cond
                          (= $ "true")
                          true
                          #
                          (= $ "false")
                          false)))
    (put cg :nil
            ~(replace (capture ,(in cg :nil))
                      ,(fn [_] nil)))
    (put cg :symbol
            ~(cmt (capture ,(in cg :symbol))
                  ,(fn [arg]
                     (symbol arg))))
    (put cg :keyword
            ~(cmt (capture ,(in cg :keyword))
                  ,|(keyword (string/slice $ 1))))
    (put cg :number
            ~(replace (capture ,(in cg :number))
                      ,scan-number))
    (put cg :string
            ~(cmt (capture ,(in cg :string))
                  ,|(string $)))
    (put cg :list
            (tuple ;(put (array ;(in cg :list))
                         2 ~(cmt (capture ,(get-in cg [:list 2]))
                                 ,|(tuple ;(slice $& 0 -2))))))
    (put cg :vector
            (tuple ;(put (array ;(in cg :vector))
                         2 ~(cmt (capture ,(get-in cg [:vector 2]))
                                 ,|(array ;(slice $& 0 -2))))))
    (put cg :hash-map
            (tuple ;(put (array ;(in cg :hash-map))
                         2 ~(cmt (capture ,(get-in cg [:hash-map 2]))
                                 ,|(table ;(slice $& 0 -2))))))
    (put cg :deref
            (tuple ;(put (array ;(in cg :deref))
                         2 ~(cmt (capture ,(get-in cg [:deref 2]))
                                 ,|(tuple (symbol "deref")
                                          (get $& 0))))))
    (put cg :quasiquote
            (tuple ;(put (array ;(in cg :quasiquote))
                         2 ~(cmt (capture ,(get-in cg [:quasiquote 2]))
                                 ,|(tuple (symbol "quasiquote")
                                          (get $& 0))))))
    (put cg :quote
            (tuple ;(put (array ;(in cg :quote))
                         2 ~(cmt (capture ,(get-in cg [:quote 2]))
                                 ,|(tuple (symbol "quote")
                                          (get $& 0))))))
    (put cg :splice-unquote
            (tuple ;(put (array ;(in cg :splice-unquote))
                         2 ~(cmt (capture ,(get-in cg [:splice-unquote 2]))
                                 ,|(tuple (symbol "splice-unquote")
                                          (get $& 0))))))
    (put cg :unquote
            (tuple ;(put (array ;(in cg :unquote))
                         2 ~(cmt (capture ,(get-in cg [:unquote 2]))
                                 ,|(tuple (symbol "unquote")
                                          (get $& 0))))))
    # tried using a table with a peg but had a problem, so use a struct
    (table/to-struct cg)))

(comment

  (type (first
          (peg/match read-grammar "true")))

  (type (first
          (peg/match read-grammar "false")))

  (type (first
          (peg/match read-grammar "nil")))

  (type (first
          (peg/match read-grammar "sym")))

  (type (first
          (peg/match read-grammar "1")))

  (type (first
          (peg/match read-grammar "\"hello\"")))

  (type (first
          (peg/match read-grammar "(+ 1 1)")))

  (type (first
          (peg/match read-grammar "[:a :b :c]")))

  (type (first
          (peg/match read-grammar "{:a 1 :b 2}")))

  (type (first
          (peg/match read-grammar "'(a b c)")))

  (type (first
          (peg/match read-grammar "'a")))

  (type (first
          (peg/match read-grammar ";; hello")))

  )
