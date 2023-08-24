(ns app
  (:require [db :refer [studDB courDB gradsDB]])
  (:require [menu :refer [menulist]]))

(def grade_map {"A+" 4.3, "A" 4, "A-" 3.7, "B+" 3.3, "B" 3, "B-" 2.7, "C+" 2.3, "C" 2, "C-" 1.7, "D+" 1.3, "D" 1, "D-" 0.7, "F" 0})

;;function used in option 1, 2, 3
(defn display
  [db]
   (doseq [element db]
   (println element))
  )

;;subfunctions used in opeion 4,5  
(defn get_id 
  []
  (print "Enter your choice of id: ")
  (flush)
  (let [input (read-line)]
   input))

;;subfunctions used in opeion 4  
(defn get_student_name [student_id]
  (let [matching_student (first (filter #(= student_id (first %)) studDB))]
    (if matching_student
      (println [(nth matching_student 0) (nth matching_student 1)])
      "Student not found")))      

;;subfunctions used in opeion 4  
(defn combine [grades cours]
  (for [v1 grades
        v2 cours
        :when (= (second v1) (first v2))]
    [(first v1) (second v2) (nth v2 3) (nth v1 2) (nth v1 3)]))      

;;subfunctions used in opeion 4  
(defn get_student_record [student_id]
  (let [matching_students (filter #(= student_id (first %)) (combine gradsDB courDB))]
    (if (empty? matching_students)
      (println "Student not found")
      (doseq [student matching_students]
        (println [(nth student 1) (nth student 2) (nth student 3) (nth student 4)])))))

;;function used for option 4
(defn records
  []
  (def id (get_id))
  (get_student_name id)
  (get_student_record id)
  )

;;function for option 6
(defn gpa
  []
  (def student_id (get_id))
  (let [matching_records (filter #(= student_id (first %)) gradsDB)]
    (if (empty? matching_records)
      (println "Student not found")
      (let [grades (map #(get grade_map (nth % 3)) matching_records)
            average (if (seq grades) (/ (apply + grades) (count grades)) 0)]
      (println "Average grade: " average)))) )
 
;;convert the grades to numerical value
(defn get_grade_value
  [grade]
  (grade_map grade))
  
;;subfunction for option 6
(defn calculate_average_grades 
  [data]
  (let [grouped_data (group-by #(nth % 1) data)]
    (map (fn [[course grades]]
           (let [semester_grades (group-by #(nth % 3) grades)]
             [course
              (map (fn [[semester grades]]
                     (let [numerical_grades (map #(get_grade_value (nth % 4)) grades)
                           average_grade (/ (apply + numerical_grades) (count numerical_grades))]
                       [semester (format "%.2f" average_grade)]))
                   semester_grades)]))
         grouped_data)))

;;function for option 6       
(defn c_gpa
  []
  (display (calculate_average_grades (combine gradsDB courDB)))
)
;;Main menu functionality
(defn menu_Choice
  [choice]
  (case choice
   "1" (display courDB)
   "2" (display studDB)
   "3" (display gradsDB)
   "4" (records)
   "5" (gpa)
   "6" (c_gpa)
   "7" (do
   (println "Your program will terminate")
   (System/exit 0))
    :else (println "Invalid choice.")))
    
;;keep display the menu until Exit 
(loop []
  (let [choice (menulist)]
    (menu_Choice choice)
    (println)
      (recur)))
      
