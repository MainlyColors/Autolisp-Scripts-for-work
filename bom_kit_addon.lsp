;;;;;;;;;;;;;; taken from another file, just test code



; this gets the block defintion - returns a count of 14 because there are 14 objects in there
; does not return the block instances
(defun getbomblock (/)
  (setq blocks (vla-get-blocks (vla-get-activedocument (vlax-get-acad-object))))
  (setq cnt 0)
  
  (repeat (vla-get-count blocks)
    (if (or (= "bomlineboltedcustd" (vla-get-name (vla-item blocks cnt))) (= "bomlineboltedcustdLiq" (vla-get-name (vla-item blocks cnt))))
      (setq bom (vla-item blocks cnt))
    );if
    (setq cnt (1+ cnt))
  );repeat
  (princ)
);defun

;; block objects are in the model space
(defun getbomblockReferences (/)
  (setq model (vla-get-ModelSpace (vla-get-activedocument (vlax-get-acad-object))))
  (setq cnt 0)
  (setq blocklist '()) ;start empty
  
  (repeat (vla-get-count model)
    (if  (= "bomlineboltedcustdLiq" (vla-get-name (vla-item model cnt)))
      (setq blocklist (append (vla-item model cnt) blocklist))
    );if
    (setq cnt (1+ cnt))
  );repeat
  
  
  
  (princ)
);defun

(defun colorchange (v-object /)
  
  (setq cnt 0)
  (repeat (vla-get-count v-object)
    (vla-item)
  (setq cnt (1+ cnt))
  );repeat
  
);defun

(setq y (vla-get-blocks (vla-get-activedocument (vlax-get-acad-object))))
(setq cnt 0)
(setq namelist '("test"))
(repeat (vla-get-count y) (setq namelist (append namelist (list (vla-get-name (vla-item y cnt))))) (setq cnt (1+ cnt)))
; bomlineboltedcustd


(defun LM:vl-getattributevalues ( blk )
    (mapcar '(lambda ( att ) (cons (vla-get-tagstring att) (vla-get-textstring att))) 
             (vlax-invoke blk 'getattributes)
    );mapcar
)

(defun LM:vl-setattributevalue ( blk tag val )
    (setq tag (strcase tag))
    (vl-some
       '(lambda ( att )
            (if (= tag (strcase (vla-get-tagstring att)))
                (progn (vla-put-textstring att val) val)
            );if
        );lambda
        (vlax-invoke blk 'getattributes)
    );vl-some
);defun

;;;;;;;;;;;; Main question how come i can use lee mac's get attributes function to set attributtes but cant do it myself????
;http://www.lee-mac.com/attributefunctions.html
; set up
(setq text (entsel))
(setq test (vlax-ename->vla-object (car text)))

;prior to autocad 2000 was called vlax-invoke now called vlax-invoke-method
;returns safe array
(setq x (vlax-invoke-method test 'getattributes))
;returns whats inside the safe array
(setq x (vlax-invoke test 'getattributes))



;how to access block references
; they are in model space vla collection/object
(setq model (vla-get-ModelSpace (vla-get-activedocument (vlax-get-acad-object))))
(setq y (vla-item model 49)) ; block instance/reference
(vla-put-color y "8") ; this works even tho the object uses TrueColor property

;to check methods on objects/collections
(vlax-dump-object y T) ; the T dumps the methods and without the T propertys still dump