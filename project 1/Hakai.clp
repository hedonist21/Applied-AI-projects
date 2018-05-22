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
	(printout t "School Report availability: " ?applicant.schlrep crlf)
	)
        
; Rule 2
; To check whether the applicant's GPA satisfies the University's criteria  
(defrule checkGPA
    (declare (salience 99))
    ?applicant <- (applicant (name ?name))
    =>
    (if (< ?applicant.gpa 3.3) then
    	(printout t "We regret to inform you that you do not satisfy the academic performance criteria required." crlf)        
       	)
    )

; Rule 3
; To check whether the the applicant's intended course is offered by the university or not
(defrule checkSpecialization
    (declare (salience 98))
    ?applicant <- (applicant (name ?name))
    =>
    (if (and (<> ?applicant.course CS) (<> ?applicant.course ECE) (<> ?applicant.course Bioengineering) (<> ?applicant.course Civil) (<> ?applicant.course Mech) (<> ?applicant.course Chem) (<> ?applicant.course Elec)) then
        (printout t "We are sorry but the university does not offer the specialization you intend to apply for." crlf)
        )
    )  

; Rule 4
; If a student has any pending backlogs, he will not be accepted by the university
(defrule checkBacklog
    (declare (salience 97))
    ?applicant <- (applicant (name ?name))
    =>
    (if (= ?applicant.backlog Yes) then
        (printout t "Sorry but the university finds your academic performance unsatisfactory and cannot offer you admission at this time. (Pending Backlogs)" crlf)
        )
    )       
; Rule 5
; If he/she is an international applicant, addmision will be tough
(defrule internationalCheck
    (declare (salience 94))
    ?applicant <- (applicant (name ?name))
    =>
    (if (= ?applicant.intrnatnl Yes) then
        (printout t "The international intake at our university is low and competition will be quite tough." crlf)
        )
    )

; Rule 6
; The University requires an applicant to submit a Common App Personal Essay.
(defrule capeCheck
    (declare (salience 95))
    ?applicant <- (applicant (name ?name))
    =>
    (if (= ?applicant.cape No) then
        (printout t "All applicants are required to submit a Common App Personal Essay to complete their application" crlf)
        )
    )
 
; Rule 7
; If the applicant is not able to provide school report currently,admission cannot be granted.
(defrule repCheck
	(declare (salience 96))
	?applicant <- (applicant(name ?name))
	=>
	(if (= ?applicant.schlrep No) then
		(printout t "We regret to inform you that university does not offer addmision without submission of School Report." crlf)
		)
	)

; Rule 8
; The University has a minimum TOEFL requirement of 80
(defrule toeflCheck
    (declare (salience 93))
    ?applicant <- (applicant (name ?name))
    =>
    (if (< ?applicant.toefl 80) then
        (printout t "You do not meet the university's criteria of minimum TOEFL score requirement." crlf)
        )
    )

; Rule 9
; The University generally gives admit to candidates with SAT score of 1150 and above
(defrule satScore
    (declare (salience 92))
    ?applicant <- (applicant (name ?name))
    =>
    (if (< ?applicant.sat 1150) then
        (printout t "You can apply to the university but due to high competition a higher SAT score is desireable." crlf)
        )
    )
	
; Rule 10
; The applicant  should submit 4 or more Letter of Recommendation
(defrule lorCheck
    (declare (salience 91))
    ?applicant <- (applicant (name ?name))
    =>
    (if (< ?applicant.letter 4) then
        (printout t "Your application will be under consideration but to foster your application it is encouraged to provide 4 LORs " crlf)
        )
    )


; Rule 11
; If the applicant has no Out-of-school experiences, including summer activities, work, and hobbies then he is notified that getting in might be difficult but he can still apply
(defrule exCheck
    (declare (salience 90))
    ?applicant <- (applicant (name ?name))
    =>
    (if (= ?applicant.ex 0) then
        (printout t "We see that you have no Out-of-school experiences, including summer activities, work, and hobbies. While this is not a mandatory requirement and you can still apply, getting in may be difficult since the application process is highly competitive." crlf)
        )
    )


; Rule 12
; If all criteria are met, we indicate the applicant to move further with his applicant and submit
(defrule proceedApp
    (declare (salience 89))
    ?app <- (applicant (name ?name))
    =>
    (if (and (> ?app.gpa 3.3) (= ?app.backlog No) (> ?app.sat 1150) (> ?app.toefl 80) (>= ?app.letter 3) (= ?app.cape Yes) (= ?app.schlrep Yes)) then
        (printout t "You are eligible to apply for the Undergraguate program at our University" crlf)
        )
    )

;Rule 13
(defrule printFacts
    (declare (salience 10))
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
        	(cape Yes))) 
(run)                                     