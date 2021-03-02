;Jonathan Giraud 
;CMSC 403 Programming Languages
;Assignment 6


;Creates a list and returns it. 
(defun myList()
    (list 4 '(7 22) '"art" '("math"  (8) 99 ) 100) 
)

;Shows all years that are leapyears from 1800 to 2020.
(defun leapYear ()
  (labels ((leapList (Year YearList)
     ;Use labels to set up separate list for years that are leap years and the rejects.
    (cond ((< 2021 Year) YearList) ((and (/= (mod Year 400) 0) (= (mod Year 100) 0) (= (mod Year 4) 0)) (leapList (+ 4 Year) YearList))
	;Test if the Year being tested is divisible by 400, 100, and 4. If they are, they don't go into the leapList.
    (T (leapList (+ 4 Year) (append YearList (list Year)))))))
	;In they case they are leap years, add it to the leapList.
    (leapList 1800 '())
  )
)

;Merges two lists and deletes duplicates.
(defun union- (firstList secondList)
  	; Combine the lists then take out all of the duplicate items
	;Referenced this to confirm that remove-duplicates is NOT destructive. 
	;https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node144.html#:~:text=remove%2Dduplicates%20is%20the%20non,elements%20need%20to%20be%20removed.
	(remove-duplicates(append firstList secondList))	
)

;Gets the average of a list.
(defun avg (aList &optional (counter 0) (total 0))
    (
	if (null aList)   ;Make a branching path that checks if there are elements in the list and returns the avg when all elements are used.
        (
		if (= counter 0)
            (RETURN-FROM avg ()) 
            (RETURN-FROM avg (/ total counter))    			     ;Divide the total by the amount of elements in the list.
        )
        (avg (cdr aList) (+ counter 1) (+ (car aList) total))   ;Increment the counter and add the number in the list.
																							 ;Recursively keep doing avg calls until the whole list is used. 
    )
)

;Uses lambda function to test input for dataType.
(defun isType (dataType)
	( lambda(VALUE)     ;Anon function that creates a function that can return functions.
		(        					 ;Typeep returns true if an object is dataType typed, NIL otherwise.
		if (typep VALUE dataType) T NIL   
		)
	)
)

;Calculates for taxes that are over a limit.
(defun taxCalculator(limit rate values &optional taxes)
(
   ;Check if there's data in the list still. Stop if all the elements are checked.
    if(null values)
    (RETURN-FROM taxCalculator taxes)
    (
	    ;Checks if the element we're at is less than the limit.
        if(< (car values) limit)
        (
		   ;Do not calculate tax if true.
            taxCalculator limit rate (cdr values) (append taxes (cons (car values)()))
        )
        (
			;Calculate tax  recursively if false.
			taxCalculator limit rate (cdr values) (append taxes (cons (* rate (car values))()))
        )
    )
))

;;Uses a function as a parameter and returns a list if the original list passed in is acceptable.
(defun clean (aFunc aList &optional (List '()))
	(cond 
		((= (list-length aList) 0) List)
		;Check if the entire list has been checked through.
		((listp (first aList) )   (append List (list(clean aFunc (first aList)))  (clean aFunc (cdr aList))))
		;If the list is a sublist then keep cleaning up each sublist until every list and sublist is cleaned.
		((funcall aFunc (first aList))  (clean aFunc (cdr aList) (append List(list (first aList) ) ) ) )
		;When a value is not passable for aFunc, then skip over it.
		(T (clean aFunc (cdr aList) List))      	
	)
)


;Essentially a branched if statement that decides on which command to execute base on conditionals.
(defmacro threeWayBranch (x y toExecute)
	;Evaluates each list.
  	(defun execute (List)(dolist (X List)(eval X)))
	(cond
		((< x y) (execute (car toExecute)))    ;If the first value is greater than the second, then evaluate using List.
		((> x y) (execute (cadr toExecute))) ;If the first value is less than the second, then evaluate using the second List.
		(T (execute (caddr toExecute)))		;Otherwise, evaluate using the third List.
	)
)



