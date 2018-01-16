(set-option :produce-models true) (set-logic QF_AUFBV ) (declare-fun
v0___symfile____tmp_input___0_1_symfile___0 () (Array (_ BitVec 32) (_ BitVec 8) ) )
(assert (let ( (?B1 (select  v0___symfile____tmp_input___0_1_symfile___0 (_ bv2
32) ) ) (?B2 (select  v0___symfile____tmp_input___0_1_symfile___0 (_ bv1 32) ) )
(?B3 (select  v0___symfile____tmp_input___0_1_symfile___0 (_ bv0 32) ) ) ) (let (
(?B6 ((_ zero_extend 24)  ?B3 ) ) (?B4 ((_ zero_extend 24)  ?B1 ) ) (?B7 ((_
sign_extend 24)  ?B3 ) ) (?B5 ((_ sign_extend 24)  ?B2 ) ) ) (and  (and  (and
(and  (and  (and  (and  (and  (and  (and  (and  (and  (and  (and  (and  (and
(and  (and  (and  (and  (and  (and  (and  (and  (=  (_ bv0 32) (bvand  (bvadd
(_ bv4294967255 32) ?B4 ) (_ bv255 32) ) ) (=  false (=  (_ bv0 32) (bvand
(bvadd  (_ bv4294967256 32) ?B4 ) (_ bv255 32) ) ) ) ) (=  false (=  (_ bv0 32)
(bvand  (bvadd  (_ bv4294967249 32) ?B4 ) (_ bv255 32) ) ) ) ) (=  false (=  (_
bv0 32) (bvand  (bvadd  (_ bv4294967254 32) ?B4 ) (_ bv255 32) ) ) ) ) (=  false (=
(_ bv0 32) (bvand  (bvadd  (_ bv4294967251 32) ?B4 ) (_ bv255 32) ) ) ) ) (=
false (=  (_ bv0 32) (bvand  (bvadd  (_ bv4294967253 32) ?B4 ) (_ bv255 32) ) ) ) ) (=
false (=  (_ bv0 32) (bvand  (bvadd  (_ bv4294967264 32) ?B4 ) (_ bv255 32) ) ) ) )
(bvsle  ((_ sign_extend 24)  ?B1 ) (_ bv47 32) ) ) (=  false (bvslt  (_ bv57 32)
?B5 ) ) ) (=  false (bvsle  ?B5 (_ bv47 32) ) ) ) (=  false (bvslt  (_ bv57 32)
?B7 ) ) ) (=  false (bvsle  ?B7 (_ bv47 32) ) ) ) (=  false (=  (_ bv0 32)
(bvand  (bvadd  (_ bv4294967255 32) ?B6 ) (_ bv255 32) ) ) ) ) (=  false (=  (_
bv0 32) (bvand  (bvadd  (_ bv4294967256 32) ?B6 ) (_ bv255 32) ) ) ) ) (=  false (=
(_ bv0 32) (bvand  (bvadd  (_ bv4294967249 32) ?B6 ) (_ bv255 32) ) ) ) ) (=
false (=  (_ bv0 32) (bvand  (bvadd  (_ bv4294967254 32) ?B6 ) (_ bv255 32) ) ) ) ) (=
false (=  (_ bv0 32) (bvand  (bvadd  (_ bv4294967251 32) ?B6 ) (_ bv255 32) ) ) ) ) (=
false (=  (_ bv0 32) (bvand  (bvadd  (_ bv4294967253 32) ?B6 ) (_ bv255 32) ) ) ) ) (=
false (=  (_ bv0 32) (bvand  (bvand  ?B6 ?B6 ) (_ bv255 32) ) ) ) ) (bvule  ?B1
(_ bv126 8) ) ) (bvule  (_ bv32 8) ?B1 ) ) (bvule  ?B2 (_ bv126 8) ) ) (bvule
(_ bv32 8) ?B2 ) ) (bvule  ?B3 (_ bv126 8) ) ) (bvule  (_ bv32 8) ?B3 ) ) ) ) )
(check-sat)
