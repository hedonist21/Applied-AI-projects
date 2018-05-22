(import nrc.fuzzy.*)

(import nrc.fuzz.jess.*)

(load-package nrc.fuzzy.jess.FuzzyFunctions)

(deftemplate applicant
    (slot name)
    ; Applying for course (Right now limited to CS, ECE, Bioengineering, Civil, Mech, Chemical and Electrical)
    (slot course)
    ; Intended term of application 
    (slot term (allowed-values fall spring))
    ; GPA in High school out of 4.0
    (slot gpa (type FLOAT))
    ; Any pending backlogs from high school?
    (slot backlog (allowed-values Yes No))
    ; Out-of-school experiences, including summer activities, work, and hobbies in form of years.
    (slot ex (type INTEGER))
	; Availibility of school report.
	(slot schlrep (allowed-values Yes No))
    ; International applicant or domestic
    (slot intrnatnl (allowed-values Yes No)) 
    ;Score in SAT
    (slot sat (type INTEGER))
    ; TOEFL score (enter 120 if domestic student)
    (slot toefl (type INTEGER))
    ; Recommendation Letters from experience or academic.
    (slot letter (type INTEGER))
    ; Common Application Personal Essay
    (slot cape (allowed-values Yes No))  
    )

(deftemplate gpa_data
    "Auto-generated"
    (declare (ordered TRUE)))

(deftemplate ex_data
    "Auto-generated"
    (declare (ordered TRUE)))

(deftemplate sat_data
    "Auto-generated"
    (declare (ordered TRUE)))

(deftemplate toefl_data
    "Auto-generated"
   (declare (ordered TRUE)))
   
(defglobal ?*gpaVar* = (new FuzzyVariable "gpa" 0.0 4.0))

(defglobal ?*exVar* = (new FuzzyVariable "ex" 0 20))

(defglobal ?*satVar* = (new FuzzyVariable "sat" 400 1600))

(defglobal ?*toeflVar* = (new FuzzyVariable "toefl" 0 120))

(call nrc.fuzzy.FuzzyValue setMatchThreshold 0.1)

; Rule 0
; Initializing global variables
(defrule MAIN::init-FuzzyVariables
    (declare (salience 100))
    ?applicant <- (applicant (name ?name))
    =>
    (call ?*gpaVar* addTerm "low" (new ZFuzzySet 2.0 3.0))
    (?*gpaVar* addTerm "medium" (new TrapezoidFuzzySet 2.7 3.0 3.2 3.4))
    (?*gpaVar* addTerm "High" (new SFuzzySet 3.2 3.6))
    (?*exVar* addTerm "low" (new ZFuzzySet 0 2))
    (?*exVar* addTerm "moderate" (new TrapezoidFuzzySet 1 3 4 6))
    (?*exVar* addTerm "experienced" (new SFuzzySet 5 8))
    (?*satVar* addTerm "low" (new ZFuzzySet 400 1000))
    (?*satVar* addTerm "medium" (new TrapezoidFuzzySet 995 1100 1200 1320))
    (?*satVar* addTerm "High" (new SFuzzySet 1315 1425))
    (?*toeflVar* addTerm "low" (new ZFuzzySet 30 75))
    (?*toeflVar* addTerm "medium" (new TrapezoidFuzzySet 70 90 100 110))
    (?*toeflVar* addTerm "High" (new SFuzzySet 105 115))
    (assert (gpa_data (new FuzzyValue ?*gpaVar* (new SingletonFuzzySet ?applicant.gpa))))
    (assert (ex_data (new FuzzyValue ?*exVar* (new SingletonFuzzySet ?applicant.ex))))
    (assert (sat_data (new FuzzyValue ?*satVar* (new SingletonFuzzySet ?applicant.sat))))
    (assert (toefl_data (new FuzzyValue ?*toeflVar* (new SingletonFuzzySet ?applicant.toefl))))        
    )

; Rule 1
; Print initial information
(defrule credentials
    (declare (salience 100))
    ?applicant <- (applicant (name ?name))
    =>
    (printout t "Welcome to Undergraduate University Application Consultancy Services" crlf)
    (printout t "Hey "?applicant.name ". Thank you for reaching out to us" crlf)
    (printout t "The information you provided to us" crlf)
    (printout t "Intended course: " ?applicant.course crlf)
    (printout t "Intended term: " ?applicant.term crlf)
    (printout t "GPA in high school: " ?applicant.gpa crlf)
    (printout t "Backlogs in high school: " ?applicant.backlog crlf)
    (printout t "Work Experience: " ?applicant.ex " years" crlf)
	(printout t "School Report availability:  " ?applicant.schlrep crlf)
    (printout t "International or Domestic Student?: " ?applicant.intrnatnl crlf)
    (printout t "SAT Score: " ?applicant.sat crlf)
    (printout t "TOEFL Score (Auto-filled to be 120 if you are a domestic applicant): " ?applicant.toefl crlf)
    (printout t "Letter of Recommendation: " ?applicant.letter crlf)
    (printout t "Statement of Purpose: " ?applicant.cape crlf)
	)
        
; Rule 2-i
; To check whether the applicant's GPA satisfies the University's criteria  
(defrule GPA_low
    (declare (salience 98))
    (gpa_data ?applicant&:(fuzzy-match ?applicant "low"))
     =>
    (printout t "We regret to inform you that you do not satisfy the academic performance criteria required." crlf)
    )

; Rule 2-ii
(defrule GPA_medium
    (declare (salience 97))
    (gpa_data ?applicant&:(fuzzy-match ?applicant "medium"))
     =>
    (printout t "The admit rate for people with better gpa is higher. We would like you to keep that in mind while applying." crlf)
    )

; Rule 2-iii
(defrule GPA_high
    (declare (salience 96))
    (gpa_data ?applicant&:(fuzzy-match ?applicant "high"))
     =>
    (printout t "Your GPA exceeds the average requirement of the university. While this is not the only criteria your profile is judged on, it is a strong steering factor." crlf)
    )

; Rule 3
; To check whether the the applicant's intended course is offered by the university or not
(defrule checkSpecialization
    (declare (salience 97))
    ?applicant <- (applicant (name ?name))
    =>
    (if (and (<> ?applicant.course CS) (<> ?applicant.course ECE) (<> ?applicant.course Bioengineering) (<> ?applicant.course Civil) (<> ?applicant.course Mech) (<> ?applicant.course Chem) (<> ?applicant.course Elec)) then
        (printout t "We are sorry but the university does not offer the specialization you intend to apply for." crlf)
        )
    ) 

; Rule 4
; If a student has any pending backlogs, he will not be accepted by the university
(defrule checkBacklog
    (declare (salience 96))
    ?applicant <- (applicant (name ?name))
    =>
    (if (= ?applicant.backlog Yes) then
        (printout t "Sorry but the university finds your academic performance unsatisfactory and cannot offer you admission at this time. (Pending Backlogs)" crlf)
        )
    )
	
; Rule 5-i
If the applicant has no Out-of-school experiences, including summer activities, work, and hobbies then he is notified that getting in might be difficult but he can still apply
(defrule ex_low
    (declare (salience 95))
    (ex_data ?applicant&:(fuzzy-match ?applicant "low"))
     =>
    (printout t "We see that you have low job experience. While this may not necessarily be a red flag on your profile and you can still apply, getting in may be difficult since the application process is highly competitive." crlf)
    )

; Rule 5-ii
(defrule ex_mod
    (declare (salience 95))
    (ex_data ?applicant&:(fuzzy-match ?applicant "moderate"))
     =>
    (printout t "We see that you have some job experience. Some experience on this side of life always comes in handy when your profile is being reviewed by the board." crlf)
    )

; Rule 5-iii
(defrule ex_exp
    (declare (salience 95))
    (ex_data ?applicant&:(fuzzy-match ?applicant "experienced"))
     =>
    (printout t "We see that you have extensive job experience. This boosts your overall profile and makes it stronger." crlf)
    )

; Rule 6
; The University requires an applicant to submit a Common App Personal Essay.
(defrule capeCheck
    (declare (salience 94))
    ?applicant <- (applicant (name ?name))
    =>
    (if (= ?applicant.cape No) then
        (printout t "All applicants are required to submit a Common App Personal Essay to complete their application" crlf)
        )
    )

	
; Rule 7
; If he/she is an international applicant, addmision will be tough
(defrule internationalCheck
    (declare (salience 93))
    ?applicant <- (applicant (name ?name))
    =>
    (if (= ?applicant.intrnatnl Yes) then
        (printout t "The international intake at our university is low and competition will be quite tough." crlf)
        )
    )

; Rule 8-i
; To validate a user's profile through their SAT score
(defrule satScore_low
    (declare (salience 92))
    (sat_data ?applicant&:(fuzzy-match ?applicant "low"))
     =>
    (printout t "You do not meet the university's criteria of minimum SAT score requirement." crlf)
    )

; Rule 8-ii
(defrule satScore_med
    (declare (salience 92))
    (sat_data ?applicant&:(fuzzy-match ?applicant "medium"))
     =>
    (printout t "While your SAT score meets the minimum criteria, generally the university admits students with higher SAT score." crlf)
    )

; Rule 8-iii
(defrule satScore_high
    (declare (salience 92))
    (sat_data ?applicant&:(fuzzy-match ?applicant "high"))
     =>
    (printout t "You exceed the university's criteria of minimum SAT score requirement." crlf)
    )

; Rule 9-i
; To validate the profile on the basis of TOEFL score
(defrule toeflScore_low
    (declare (salience 91))
    (toefl_data ?applicant&:(fuzzy-match ?applicant "low"))
     =>
    (printout t "You do not meet the university's criteria of minimum TOEFL score requirement." crlf)   
    )

; Rule 9-ii
(defrule toeflScore_med
    (declare (salience 91))
    (toefl_data ?applicant&:(fuzzy-match ?applicant "medium"))
     =>
    (printout t "You met the criteria of minimum TOEFL requirement but giving it another shot might be worth it." crlf)   
    )

; Rule 9-iii
(defrule toeflScore_high
    (declare (salience 91))
    (toefl_data ?applicant&:(fuzzy-match ?applicant "high"))
     =>
    (printout t "You exceed the university's criteria of minimum TOEFL score requirement." crlf)   
    )

; Rule 10
; The applicant  should submit 4 or more Letter of Recommendation
(defrule lorCheck
    (declare (salience 90))
    ?applicant <- (applicant (name ?name))
    =>
    (if (< ?applicant.letter 4) then
        (printout t "Your application will be under consideration but to foster your application it is encouraged to provide 4 LORs " crlf)
        )
    )

; Rule 11
; If the applicant is not able to provide school report currently,admission cannot be granted.
(defrule repCheck
	(declare (salience 96))
	?applicant <- (applicant(name ?name))
	=>
	(if (= ?applicant.schlrep No) then
		(printout t "We regret to inform you that university does not offer addmision without submission of School Report." crlf)
		)
)

; Rule 12
; If all criteria are met, we indicate the applicant to move further with his applicant and submit
(defrule proceedApp
    (declare (salience 88))
    ?app <- (applicant (name ?name))
    =>
    (if (and (> ?app.gpa 3.3) (= ?app.backlog No) (> ?app.sat 1150) (> ?app.toefl 80) (>= ?app.letter 4) (= ?app.cape Yes) (= ?app.schlrep Yes)) then
        (printout t "You are eligible to apply for the Undergraguate program at our University" crlf)
        )
    )

;Rule 13
(defrule printFacts
    (declare (salience 1))
    =>
    (facts)
    )
(reset)
       
(assert (applicant (name "Victor DeMello")
            (course Civil)
            (term spring)
            (gpa 3.7) 
            (backlog Yes)           
            (ex 1)
			(schlrep No)
            (intrnatnl Yes)
            (sat 1250)
            (toefl 106)
            (letter 4)
        	(cape Yes))
)   

(run)   