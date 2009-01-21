
;; ---------------------------------------------------------------------------------------------------
;; test: this should run the interactive GSSM interface on the whole ARIES dataset for 
;; ---------------------------------------------------------------------------------------------------

(aries.demo/run-gssm-demo (tl/conc 'carbonService:ClimateStability))
(tl/alert "done")