;|
  
--------------NOTES-----------------

block name to target: CST Title Block - Customer
by attribute: DRWN_BY, DRWN_DATE
checker attribute: CHKD_BY, CHKD_DATE
approver attribute: APPR_BY, APPR_DATE

-----------block------------
block name to target: CST Title Block - Shop
by attribute: DRWN_BY, DRWN_DATE
checker attribute: CHKD_BY, CHKD_DATE
approver attribute: APPR_BY, APPR_DATE

|;

(vl-load-com)


;|
  pre-steps:
  1. ask user if they want to fill in drawn by / checker / approver
  2. maybe give a way to exclude drawings from selection
  3. maybe keep track of which drawings got checked then give a report at the end
  
  steps: 
  1. get all active drawings 
  2. check modelSpace for BlockReference list
    - should check if drawing is read-only
  3. inside list find block instance by name
  4. determine if there is more then 1 somehow
  5. scrub the block and store the values
  6. if value is already filled in dont overwrite
  7.if empty, fill in 
  8. (idk if have to do this) - update/refresh the drawing
  9. zoom e
  10. quick save
  
  
  
  
  |;


; (defun updateTitleBlock (/)
  
;   (vlax-for document (vla-get-documents (vlax-get-acad-object))
    
;   );vlax-for
  
  
;   (setq documents (vla-get-documents (vlax-get-acad-object)))
  
;   (repeat
;   );repeat
; );defun

; for testing
; (setq modelSpaceCollection (vla-get-modelspace (vla-item (vla-get-documents (vlax-get-acad-object)) 1)))

(defun blockReferenceSearch (blkString modelSpaceCollection /)
  
  (setq test_list '())
  
  (vlax-for item modelSpaceCollection
            ; need to check if name property exists
            (if (vlax-property-available-p item 'name)
            (if (= (vla-get-name item) blkString) ;condition
              (setq test_list (append test_list (list item)))
            );if - nested
            );if
    
  ); vlax-for
);defun


(defun AttributeDump (vlaBlk / cnt)
  (setq cnt 0)
  (setq AttVariantArr (vla-GetAttributes vlaBlk))
  (setq AttSafeArr (vlax-variant-value AttVariantArr))
  (setq arrLength (+ 1 (vlax-safearray-get-u-bound AttSafeArr 1))) ; attribute arrays are 1 dimension always
  
  (repeat arrLength
    (princ (vla-get-textstring (vlax-safearray-get-element AttSafeArr cnt)))
    (princ "\n")
    
    (setq cnt (1+ cnt))
  );repeat
  
);defun

(defun titleBlkApprover (vlaBlk / cnt)
  
  (setq cnt 0)
  (setq AttVariantArr (vla-GetAttributes vlaBlk))
  (setq AttSafeArr (vlax-variant-value AttVariantArr))
  (setq arrLength (+ 1 (vlax-safearray-get-u-bound AttSafeArr 1))) ; attribute arrays are 1 dimension always
  
  (repeat arrLength
    (setq attObject (vlax-safearray-get-element AttSafeArr cnt))
    (setq tagString (vla-get-tagstring attObject))
    
    (if (= tagString "APPR_BY")
      (vla-put-textstring attObject "RM")
    );if
    (if (= tagString "APPR_DATE")
      (vla-put-textstring attObject "09/01/2021")
    );if

    
    (setq cnt (1+ cnt))
  );repeat
  
);defun

(defun UpdateApproverAllOpenDwg (/)
  ; (princ "acadObj set \n")
  (setq acadObj (vlax-get-acad-object))
  ; (princ "docs list set \n")
  (setq docs (vla-get-documents acadObj))
  ; (princ "cnt reset \n")
  (setq cnt 0);
  
  (repeat (vla-get-count docs)
    ; (princ (strcat "current count " (itoa cnt) "\n"))
    ; (princ "currentDoc set \n")
    (setq currentDoc (vla-item docs cnt))
    ; (princ (vla-get-fullname currentDoc))
    ; (princ "\n")
    
    ; (princ "modelSpaceCollection set \n")
    (setq modelSpaceCollection (vla-get-modelspace currentDoc))
    
    ; (princ "blockRefernce Function start \n")
    ;creates test_list
    (blockReferenceSearch "CST Title Block - Shop" modelSpaceCollection)
    ; (princ "blockRefernce Function end \n")
    ; (princ "test_list created \n")
    
    ; (princ "blkRef test check \n")
    (if (setq blkRef (car test_list))
      (progn
        ; (princ "blkRef set \n")
        ; (princ "titleBlkApprover start \n")
        (titleBlkApprover blkRef)
        ; (princ "titleBlkApprover end \n")
        ; (princ "AttributeDump start \n")
        (AttributeDump blkRef)
        ; (princ "AttributeDump end \n")
        ; (princ "vla-Regen start \n")
        (vla-Regen currentDoc acActiveViewport) ;regen's active vieport
        ; (princ "vla-Regen end \n")
      );progn
    );if
  ; (princ "add 1 to count \n")
    (setq cnt (1+ cnt))
    ; (princ "end repeat \n")
  );repeat
  
  ; (princ "end UpdateApproverAllOpenDwg \n")
);defun


(defun c:Update-Title-Block (/)
  (initget 1 "Drawer Checker Approver")
  (setq titleBlkPerson (getkword "\nWhich field do you want to update? [Drawer/Checker/Approver]: "))
  
  (initget 1 "Current All")
  (setq updateCoverage (getkword "\nUpdate only Current dwg or All open dwgs? [Current/All]: "))
  
  (initget 1 "Yes No")
  (setq Overwrite (getkword "\nOverwrite already filled in title block(s)? [Yes/No]: "))
  
    ; (initget 1 "Yes No")
  (setq intials (getstring "\nFill in intials: "))
  
  (initget 1 "Today Custom")
  (setq date (getkword "\nToday's Date or Custom? [Today/Custom]: "))
  
  
  (princ)
);defun
