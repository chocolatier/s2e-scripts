(set-option :produce-models true)
(set-logic QF_AUFBV )
(declare-fun sym_file () (Array (_ BitVec 32) (_ BitVec 8)))

(declare-const ?v (_ BitVec 8))

(assert 
(let 
((?B1 (select sym_file (_ bv2 32))) (?B2 (select sym_file (_ bv0 32))))) 
(= false (bvsle ?B1 ?v))
)

(check-sat)

