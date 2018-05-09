/** Classic blocks world actions. */

(defaction stack
	[precondition (clear ?a) (clear ?b)]
	[postcondition (on ?a ?b) 
				   (not (ontable ?a))
				   (not (clear ?b))
				   ])
	

(defaction unstack
	[precondition (clear ?a) (on ?a ?b)]
	[postcondition 
	    (not (on ?a ?b)) 
	    (clear ?b) 
	    (ontable ?a)])
	    