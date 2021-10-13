;|
  Goal: Rotate any item about the center and use the same center point for reference point
  |;

(defun CircleRotate (/ userSelection oldsnap centerPt referencePt endPt)
  
  (setq userSelection (ssget)) ;no filters, should allow everything
  (setq oldsnap (getvar "osmode"))
  (setvar "osmode" 4) ; 4 is centerpoint
  (setq centerPt (getpoint "\nSelect Center Point for rotate (center snap is on): "))
  (setvar "osmode" oldsnap)
  (setq referencePt (getpoint "\nSelect reference point for rotate:"))
  (setq endPt (getpoint "\nSelect point to line up reference point for align:"))
  
  (command "Rotate" userSelection "" centerPt "_r" centerPt referencePt "_p" centerPt endPt)
  
  
);defun

(defun c:RRO (/)
  (CircleRotate)
)
