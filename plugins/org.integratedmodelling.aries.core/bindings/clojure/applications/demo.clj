
;; ---------------------------------------------------------------------------------------------------
;; test: this should run the interactive GSSM interface on the whole ARIES dataset for climate stability
;; ---------------------------------------------------------------------------------------------------

(aries.demo/run-gssm-demo 'carbonService:ClimateStability 128)
(tl/alert "done")