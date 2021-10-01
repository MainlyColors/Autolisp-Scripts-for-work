;|
  
--------------NOTES-----------------

Blocks that need to be supported, common trend is the first 15 characters "CST Title Block"

-----------block------------
block name to target: CST Title Block - Shop A Size
by attribute: DRWN_BY, DRWN_DATE
checker attribute: CHKD_BY, CHKD_DATE
approver attribute: APPR_BY, APPR_DATE

-----------block------------
block name to target: CST Title Block - Shop A Size Portrait
by attribute: DRWN_BY, DRWN_DATE
checker attribute: CHKD_BY, CHKD_DATE
approver attribute: APPR_BY, APPR_DATE

-----------block------------
block name to target: CST Title Block - Shop
by attribute: DRWN_BY, DRWN_DATE
checker attribute: CHKD_BY, CHKD_DATE
approver attribute: APPR_BY, APPR_DATE

-----------block------------
block name to target: CST Title Block - Customer A Size
by attribute: DRWN_BY, DRWN_DATE
checker attribute: CHKD_BY, CHKD_DATE
approver attribute: APPR_BY, APPR_DATE

-----------block------------
block name to target: CST Title Block - Customer
by attribute: DRWN_BY, DRWN_DATE
checker attribute: CHKD_BY, CHKD_DATE
approver attribute: APPR_BY, APPR_DATE
|;

(vl-load-com)


;|
  pre-steps:
  1. ask user if they want to fill in drawn by / checker / approver --DONE--
  2. maybe give a way to exclude drawings from selection
  3. maybe keep track of which drawings got checked then give a report at the end
  
  steps: 
  1. get all active drawings --DONE--
  2. check modelSpace for BlockReference list --DONE--
    - should check if drawing is read-only
  3. inside list find block instance by name --DONE--
  4. determine if there is more then 1 somehow (i dont think this is needed)
  5. scrub the block and store the values (wasnt needed)
  6. if value is already filled in dont overwrite --DONE--
  7.if empty, fill in --DONE--
  8. (idk if have to do this) - update/refresh the drawing --DONE--
  9. zoom e
  10. quick save
  
  TODO: need a comparison function to check if anything changed and report back when drawings don't change
  
  
  
  
  |;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;blkString = is the block reference name in case sensitive string
;;;;modelSpaceCollection= full model space object collection to serach through
;;finds a list of reference blocks with the same name as blkstring
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;TODO make this more reusable with support contains, start, end, whole string searches
(defun blockReferenceSearch (blkString modelSpaceCollection /)
  
  (setq blkReferenceVlaList '()) ;needs to be global - should be local
  
  (vlax-for item modelSpaceCollection
            ; need to check if name property exists
            (if (vlax-property-available-p item 'name)
            ; the first 15 characters should be "CST Title Block" on all supported title blocks
            (if (= (substr (vla-get-name item) 1 15) blkString) ;condition
              (setq blkReferenceVlaList (append blkReferenceVlaList (list item)))
            );if - nested
            );if
    
  ); vlax-for
);defun

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;vlaBlk = Vla block reference from modelspace collection
;;prints out all attribute text strings to console
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;vlaBlk = Vla block reference from modelspace collection
;;;;person = string from getkwork
;;;;intials = string from user input
;;;;date = string from user input or function
;;Fills in Drawer/checker/approver aslong as the attribute tags match below
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun titleBlkIntialsFillIn (vlaBlk person intials date Overwrite / cnt ) ;attTag_By attTag_Date cnt AttVariantArr AttSafeArr arrLength attObject tagString

  (cond
    ((= person "Drawer") (setq attTag_By "DRWN_BY" attTag_Date "DRWN_DATE"))
    ((= person "Checker") (setq attTag_By "CHKD_BY" attTag_Date "CHKD_DATE"))
    ((= person "Approver") (setq attTag_By "APPR_BY" attTag_Date "APPR_DATE"))
  );cond
  
  (cond
    ((= Overwrite "No") (setq Overwrite nil))
    ((= Overwrite "Yes") (setq Overwrite T))
  );cond
  
  (setq cnt 0)
  (setq AttVariantArr (vla-GetAttributes vlaBlk))
  (setq AttSafeArr (vlax-variant-value AttVariantArr))
  (setq arrLength (+ 1 (vlax-safearray-get-u-bound AttSafeArr 1))) ; attribute arrays are 1 dimension always
  
  (repeat arrLength
    (setq attObject (vlax-safearray-get-element AttSafeArr cnt))
    (setq tagString (vla-get-tagstring attObject))

    (setq textString (vla-get-textstring attObject))

    (cond
      ((= Overwrite T)
        ;by attribute title block
        (if (= tagString attTag_By)
          (progn
          (vla-put-textstring attObject intials)
          (princ "\nChanging drawing: ")
          (princ (vla-get-windowtitle (vla-get-document attObject)))
          );progn
        );if
        ;date attribute title block
        (if (= tagString attTag_Date)
          (vla-put-textstring attObject date)
        );if
      );overwrite true
      ((= Overwrite nil)
       ;by attribute title block
       (if (and (= textString "") (= tagString attTag_By))
         (progn
          (vla-put-textstring attObject intials)
          (princ "\nChanging drawing: ")
          (princ (vla-get-windowtitle (vla-get-document attObject)))
         )
       );if
       ;date attribute title block
      (if (and (= textString "") (= tagString attTag_Date))
        (vla-put-textstring attObject date)
      );if
      );overwrite false
    );cond

    (setq cnt (1+ cnt))
  );repeat
  
);defun

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;person = string from user input
;;;;intials = string from user input 
;;;;date = string from user input or function
;;Fills in Drawer/checker/approver on all open drawings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun UpdateAllOpenDwgs (person intials date Overwrite / acadObj docs cnt Overwrite currentDoc modelSpaceCollection blkRef)
  (princ "\nStart Drawing Update....")
  (setq acadObj (vlax-get-acad-object))
  (setq docs (vla-get-documents acadObj))
  (setq cnt 0)
  
  (cond
    ((= Overwrite "No") (setq Overwrite nil))
    ((= Overwrite "Yes") (setq Overwrite T))
  );cond
  
  (repeat (vla-get-count docs)

    (setq currentDoc (vla-item docs cnt))
    (setq modelSpaceCollection (vla-get-modelspace currentDoc))
    
    (blockReferenceSearch "CST Title Block" modelSpaceCollection)

    (if (setq blkRef (car blkReferenceVlaList))
      (progn
        ; (changeReportAdd blkRef person);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (titleBlkIntialsFillIn blkRef person intials date Overwrite)

        ; (AttributeDump blkRef)
        (princ "\n")
        (vla-Regen currentDoc acActiveViewport) ;regen's active vieport
      );progn
    );if

    (setq cnt (1+ cnt))
  );repeat
  
  (princ "\nDrawing Update Finished")
  (princ) ;quiet exit
);defun


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;person = string from user input
;;;;intials = string from user input 
;;;;date = string from user input or function
;;Fills in Drawer/checker/approver on active drawing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun UpdateCurrentActiveDwg (person intials date Overwrite /) ;activeDoc modelSpaceCollection blkRef
  (princ "\nStart Current Drawing Update....")

  (setq activeDoc (vla-get-activedocument (vlax-get-acad-object)))
  (setq modelSpaceCollection (vla-get-modelspace activeDoc))
  
  (blockReferenceSearch "CST Title Block" modelSpaceCollection)
  
  (if (setq blkRef (car blkReferenceVlaList))
    (progn
      (titleBlkIntialsFillIn blkRef person intials date Overwrite)
      (vla-Regen activeDoc acActiveViewport) ;regen's active vieport
    );progn
  );if
  (princ "\nCurrent Drawing Update Finished")
);defun


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;gets current timestamp and converts it to a XX/XX/XXXX date format
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun TodayDate (/ dwgDate)
    (setq	dwgDate	(substr (rtos (getvar "cdate") 2 8) 1 8)
	dwgDate	(strcat	(substr dwgDate 5 2)
			"/"
			(substr dwgDate 7 2)
			"/"
			(substr dwgDate 1 4) ; year
		) ;strcat
  ) ;setq
);defun

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;asks a series of questions to fill out titleblock intials
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:Update-Title-Block (/ titleBlkPerson updateCoverage intials date Overwrite)
  (initget 1 "Drawer Checker Approver")
  (setq titleBlkPerson (getkword "\nWhich field do you want to update? [Drawer/Checker/Approver]: "))
  
  (initget 1 "Current All")
  (setq updateCoverage (getkword "\nUpdate only Current dwg or All open dwgs? [Current/All]: "))
  
  (setq intials "")
  (while (= (wcmatch intials "@@,@@@") nil)
    (setq intials (strcase (getstring "\nFill in intials (letters only in XX or XXX format): ")))
  );while
  
  (initget 1 "Today Custom")
  (setq date (getkword "\nToday's Date or Custom? [Today/Custom]: "))
  
  (cond
    ((= date "Today") (setq date (TodayDate)))
    ((= date "Custom")
     (while (= (wcmatch date "##/##/####") nil)
      (initget 1)
      (setq date (getstring "\nEnter the Custom date in xx/xx/xxxx format: ")) 
     );while
    );custom option
  );cond 
  
  
  (cond
    ((= updateCoverage "Current")
      (setq Overwrite "Yes")
      (UpdateCurrentActiveDwg titleBlkPerson intials date Overwrite)
     );current option
    ((= updateCoverage "All") 
      (initget 1 "Yes No")
      (setq Overwrite (getkword "\nOverwrite already filled in title block(s)? [Yes/No]: "))
      (UpdateAllOpenDwgs titleBlkPerson intials date Overwrite)
     );all option
  );cond
  
  
  (princ)
);defun
