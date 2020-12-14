(require '[clojure.string :as str])

;; Read EAP, MWS, HPL into global variables
(def eap (read-file "eap.txt"))
(def mws (read-file "mws.txt"))
(def hpl (read-file "hpl.txt"))

;; Create a variable containing a vector of all 3 authors
(def authors [eap mws hpl])

;; Compute the percentage of sentences that contain the world “the” at least once
(defn pct-with-word [word sentences]
   (float (/ (num-with-word word sentences) (count sentences)))
)
 
;; Now test out on the sentences of all of the authors
(println "#1 : Percent of words in each of the authors sentences that are 'the'" (map #(pct-with-word "the" %) (map2 towords authors)))


;; Compute the percentage of words that are punctuation
(defn punctuation-punc [sentences]
    (float (/
        (count (filter ispunc ( flatten sentences)))
        (count (flatten sentences))
    ))
)

(println "#2 : Percent of words in each of the authors sentences that are punctuation" (map #(punctuation-punc %) (map2 towords authors)))


;; Compute the percentage of words that are stopwords

(defn stopwords-pct [sentences]
    (float (/
        (count (filter isstop (flatten sentences)))
        (count (flatten sentences))
    ))
)

(println "#3 : Percent of words in each of the authors sentences that are stopwords" (map #(stopwords-pct %) (map2 towords authors)))

;; Compute the percentage of words that are neither stopwords, nor punctuation
(defn words-pct [sentences]
    (float (/
        (count (remove ispunc (remove isstop (flatten sentences))))
        (count (flatten sentences))
    ))
)


(println "#4 : Percent of words in each of the authors sentences that are neither stopwords, nor punctuation" (map #(words-pct %) (map2 towords authors)))

;; Compute the 10 most common words that are not stopwords and are not punctuation
(defn common-words [sentences]
    (take 10
        (sort-by second >
            (frequencies
                (remove ispunc (remove isstop (flatten sentences)))
            )
        )
    )
)


(println "#5 : Percent of words in each of the authors sentences that are 10 most common words that are not punctuation" (map #(common-words %) (map2 towords authors)))

;; Compute the N most common words that are neither stopwords nor punctuation
(defn common-words-n [sentences n]
    (take n
        (sort-by second >
            (frequencies
                (remove ispunc (remove isstop (flatten sentences)))
            )
        )
    )
)

(println "#6 : Nth most common words that are neither stopwords nor punctuation" (map #(common-words-n % 5) (map2 towords authors)))

;; Compute the 10 most common pairs of words that occur next to each other, for each author
(defn most-common-pairs [sentences]
    (take 10
        (sort-by second >
            (frequencies
                (map #(str/join " " %) (mapcat #(partition 3 1 %) sentences))
            )
        )
    )
)

;; Compute the K most common ngrams. 
;; An ngram is a sequence of N words (where N is a parameter to the function) that occur consecutively for each author
(defn k-most-common-ngrams [sentences k n]
    (take k
        (sort-by second >
            (frequencies
                (map #(str/join " " %) (mapcat #(partition n 1 %) sentences))
            )
        )
    )
)


(println "#7 : Kth most common ngrams for all authors:" (map #(k-most-common-ngrams % 5 4) (map2 towords authors)))


;;  Look at the most popular punctuation for each author
(defn common-punc-n [sentences n]
    (take n
        (sort-by second >
            (frequencies
                (filter ispunc ( flatten sentences))
            )
        )
    )
)


(println "#8 : Most common punctuation for all authors:" (map #(common-punc-n % 5) (map2 towords authors)))