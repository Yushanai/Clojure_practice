(ns db)

(defn loadData 
    [file_name]
    (let [file_content (slurp file_name)
    lines (clojure.string/split-lines file_content)
    split-lines (map #(clojure.string/split % #"\|") lines)]
    (mapv (partial into []) split-lines))
    )
 
(def studDB (loadData "studs.txt"))
(def courDB (loadData "courses.txt"))
(def gradsDB (loadData "grades.txt"))
