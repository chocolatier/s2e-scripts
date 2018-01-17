(set-option :produce-models true)
(set-logic QF_AUFBV )
(declare-fun sym_file () (Array (_ BitVec 32) (_ BitVec 8)))

(declare-const ?v (_ BitVec 8))

(assert 
<<<<<<< HEAD
	(let 
		(
		(?B1 (select sym_file (_ bv2 32))) 
	;	(?B2 (select sym_file (_ bv0 32)))
		)
	) 
	(bvsle ?B1 ?v)
=======
(let 
((?B1 (select sym_file (_ bv2 32))) (?B2 (select sym_file (_ bv0 32))))) 
(= false (bvsle ?B1 ?v))
>>>>>>> 7ae9b33c3a2e991dc06698cdf05bd84b9e09f12c
)

(check-sat)

