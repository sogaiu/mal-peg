(defn code*
  [ast buf]
  (case (ast :tag)
    :boolean
    (buffer/push-string buf (ast :content))
    :nil
    (buffer/push-string buf (ast :content))
    :keyword
    (buffer/push-string buf (ast :content))
    :number
    (buffer/push-string buf (ast :content))
    :string
    (buffer/push-string buf (ast :content))
    :symbol
    (buffer/push-string buf (ast :content))
    #
    :list
    (do
      (buffer/push-string buf "(")
      (var remove false)
      (each elt (ast :content)
            (code* elt buf)
            (buffer/push-string buf " ")
            (set remove true))
      (when remove
        (buffer/popn buf 1))
      (buffer/push-string buf ")"))
    :hash-map
    (do
      (buffer/push-string buf "{")
      (var remove false)
      (each elt (ast :content)
            (code* elt buf)
            (buffer/push-string buf " ")
            (set remove true))
      (when remove
        (buffer/popn buf 1))
      (buffer/push-string buf "}"))
    :vector
    (do
      (buffer/push-string buf "[")
      (var remove false)
      (each elt (ast :content)
            (code* elt buf)
            (buffer/push-string buf " ")
            (set remove true))
      (when remove
        (buffer/popn buf 1))
      (buffer/push-string buf "]"))
    #
    :deref
    (do
      (buffer/push-string buf "(deref ")
      (var remove false)
      (each elt (ast :content)
            (code* elt buf)
            (buffer/push-string buf " ")
            (set remove true))
      (when remove
        (buffer/popn buf 1))
      (buffer/push-string buf ")"))
    :quasiquote
    (do
      (buffer/push-string buf "(quasiquote ")
      (var remove false)
      (each elt (ast :content)
            (code* elt buf)
            (buffer/push-string buf " ")
            (set remove true))
      (when remove
        (buffer/popn buf 1))
      (buffer/push-string buf ")"))
    :quote
    (do
      (buffer/push-string buf "(quote ")
      (var remove false)
      (each elt (ast :content)
            (code* elt buf)
            (buffer/push-string buf " ")
            (set remove true))
      (when remove
        (buffer/popn buf 1))
      (buffer/push-string buf ")"))
    :splice-unquote
    (do
      (buffer/push-string buf "(splice-unquote ")
      (var remove false)
      (each elt (ast :content)
            (code* elt buf)
            (buffer/push-string buf " ")
            (set remove true))
      (when remove
        (buffer/popn buf 1))
      (buffer/push-string buf ")"))
    :unquote
    (do
      (buffer/push-string buf "(unquote ")
      (var remove false)
      (each elt (ast :content)
            (code* elt buf)
            (buffer/push-string buf " ")
            (set remove true))
      (when remove
        (buffer/popn buf 1))
      (buffer/push-string buf ")"))
    #
    :with-meta
    (do
      (buffer/push-string buf "(with-meta ")
      (var remove false)
      (each elt (ast :content)
            (code* elt buf)
            (buffer/push-string buf " ")
            (set remove true))
      (when remove
        (buffer/popn buf 1))
      (buffer/push-string buf ")"))
    ))

(comment

  (let [buf @""]
    (code* {:tag :number
            :content "1"}
      buf))
  # => @"1"

  (let [buf @""]
    (code* {:tag :number
            :content "(+ 1 1)"}
      buf))
  # => @"(+ 1 1)"

  (let [buf @""]
    (code* {:tag :number
            :content "{:a 1 :b [:x :y]}"}
      buf))
  # => @"{:a 1 :b [:x :y]}"

  )

(defn code
  [ast]
  (let [buf @""]
    (code* ast buf)
    (string buf)))

(comment

  (code {:tag :number
         :content "{:a 1 :b [:x :y]}"})
  # => "{:a 1 :b [:x :y]}"

  )
