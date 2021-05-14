;|
  
--------------NOTES-----------------

 stuff reuqired:
 - center point for roll
 -  diameter or number of sheets to determine dia
 - select holes to convert
 - select line to base arc length off of
 - optional : select notch
 - color special holes


  Same Y value = horizontal
  Same X value = vertical
  
  horizontal bolt row - x determines order, y to check it all on the same line
  x,y,z
  example entitiy information - (10 -12431.5 1272.32 0.0) - 10 is the dxf code to look for

|;


(vl-load-com)


; (defun *error* (msg)
; (vl-bt)
; )

;---------------------------------------------------------------------------------------------------------
;prints number of holes above the hole
(defun c:flat-to-rolled (/ counter bolt_row_unordered current_list_item 
                         xyz_ent_data_OCS x_point y_point x_y_pt_list x_y_string_list 
                         xyz_ent_data_WCS entity_value
                        ) 



  (setq counter 0)
  (setq PtList nil) ;nil out the list outside repeat

  ; (setvar "osmode" 10407) ; turn on snap  //not used yet [check]
  ; (setq base_point (getpoint "select a starting point:\n")) //not used yet [check]
  ; (setvar "osmode" 0) ; turn off snap //not used yet [check]

  (setq roll_radius (getdist "Enter roll radius:\n"))
  (setq material_thickness (getdist "End material thickness:\n"))



  (princ "\nselect the bolt horizontal row")

  (if 
    (setq bolt_row_unordered (ssget 
                               '((-4 . "<or")
                                 (0 . "CIRCLE")
                                 (0 . "LINE")
                                 (-4 . "or>")
                                )
                             )
    ) ; will only select circles and lines in a window selction
    (progn 
      (sssetfirst nil bolt_row_unordered) ;the nil part is for gripsets no longer supported
      (setq ss_lines (ssget "_I" '((0 . "LINE"))))
      (sssetfirst nil bolt_row_unordered)
      (setq ss_circles (ssget "_I" '((0 . "CIRCLE"))))
    )
  )
  (sssetfirst nil nil) ; unselect
  (setq baseLeftXPt_MaxLength_list (object-max-horizontal-length-and-basePt 
                                     ss_lines
                                   )
  ) ;returns max length of object and base point in a list (baseXpt maxlength) ;custom support function
  (setq sheetNotchList (SheetNotchFinder 
                         ss_lines
                         (cadr baseLeftXPt_MaxLength_list)
                         (car baseLeftXPt_MaxLength_list)
                         roll_radius
                       )
  ) ; returns list of notch start end locations


  (setq baseLeftXPt (car baseLeftXPt_MaxLength_list))
  (setq base_arc_length (cadr baseLeftXPt_MaxLength_list))


  (setq top-bolt-row_ss (top-horizontal-bolt-row ss_circles)) ;returns selection set with top holes ;custom support function

  (repeat (sslength bolt_row_unordered) 
    (setq current_list_item (entget (ssname bolt_row_unordered counter))) ;grabs entity data based on counter index

    (setq xyz_ent_data_OCS (cdr (assoc 10 current_list_item))) ; OCS = object coordinate system
    (setq entity_value (cdr (assoc -1 current_list_item))) ; -1 is the actully entity data for the circle, don't know what dxf group 330 is
    (setq xyz_ent_data_WCS (trans xyz_ent_data_OCS entity_value 0))
    ; (setq OCS_data (assoc 210 current_list_item)) ; object coordinate system -not used

    ; (if (= counter 0)
    ;   (setq PtList (append xyz_ent_data_WCS))
    ;   (setq PtList (append PtList xyz_ent_data_WCS))
    ; )

    (if (= PtList nil) 
      (setq PtList (list xyz_ent_data_WCS)) ; if true
      (setq PtList (append 
                     (list xyz_ent_data_WCS)
                     PtList
                   )
      ) ; else
    )

    (setq x_point (car xyz_ent_data_WCS))
    (setq y_point (cadr xyz_ent_data_WCS))

    ; (setq x_y_pt_list (list (rtos x_point 2 8) (rtos (+ y_point 2) 2 8))) ; puts together a list of x and y points where y point is +2 for text to print above //[print text above holes]
    ; (setq x_y_string_list (strcat (car x_y_pt_list) "," (cadr x_y_pt_list))) ; builds string to put into text command//[print text above holes]

    ; (command "text" "j" "c" x_y_string_list "1.00" "0" counter) ; prints index of hole //[print text above holes]

    (setq counter (1+ counter))
  ) ;end repeat

  ;; sorts by lowest x then y according to https://forums.autodesk.com/t5/visual-lisp-autolisp-and-general/point-sort-problems/m-p/2028421#M243774,

  (setq PtList_sort (vl-sort 
                      PtList
                      '(lambda (a b) 
                         (if (equal (car a) (car b) 0.000001) 
                           (< (cadr a) (cadr b))
                           (< (car a) (car b))
                         )
                       )
                    )
  )

  ; (setq base_line (ssget '((0 . "CIRCLE"))))
  (setq oldsnap (getvar "osmode"))
  (setvar "osmode" 0)
  (setq center_point (getpoint "select insert center point"))

  (setq start-angle-dxf (find-start-angle-rad (/ base_arc_length 2) roll_radius))
  (setq end-angle-dxf (find-end-angle-rad (/ base_arc_length 2) roll_radius))
  (setq layerDXFObject (cons 8 "Object"))
  (setq layerDXFCenter (cons 8 "Center"))

  ;this works
  ; (entmake (list arc-dxf-0 center-dxf radius-dxf start-angle-dxf end-angle-dxf wcs-dxf)) ;main arc
  (Create-ARC-Entity center_point roll_radius start-angle-dxf end-angle-dxf 
                     layerDXFCenter
  ) ; main arc

  (setq offset_towards_roll_radius (- roll_radius (/ material_thickness 2)))
  (setq offset_away_roll_radius (+ roll_radius (/ material_thickness 2)))


  (Create-ARC-Entity center_point offset_towards_roll_radius start-angle-dxf 
                     end-angle-dxf layerDXFObject
  ) ;offset towards center arc
  (Create-ARC-Entity center_point offset_away_roll_radius start-angle-dxf 
                     end-angle-dxf layerDXFObject
  ) ;offset towards center arc

  ;left line outside
  (setq outsideP1 (polar center_point start-angle-dxf offset_towards_roll_radius))
  (setq outsideP2 (polar center_point start-angle-dxf offset_away_roll_radius))

  ;right line outside
  (setq outsideP3 (polar center_point end-angle-dxf offset_towards_roll_radius))
  (setq outsideP4 (polar center_point end-angle-dxf offset_away_roll_radius))

  
  (if (and (= (car sheetNotchList) nil) (= (cadr sheetNotchList) nil))
    (princ);if true
    (progn
        ;notch line 1
        (setq outsideP5 (polar center_point 
                              (+ start-angle-dxf (car sheetNotchList))
                              offset_towards_roll_radius
                        )
        )
        (setq outsideP6 (polar center_point 
                              (+ start-angle-dxf (car sheetNotchList))
                              offset_away_roll_radius
                        )
        )

        ;notch line 2
        (setq outsideP7 (polar center_point 
                              (+ start-angle-dxf (cadr sheetNotchList))
                              offset_towards_roll_radius
                        )
        )
        (setq outsideP8 (polar center_point 
                              (+ start-angle-dxf (cadr sheetNotchList))
                              offset_away_roll_radius
                        )
        )
        ;notch lines
        (line outsideP5 outsideP6 layerDXFObject)
        (line outsideP7 outsideP8 layerDXFObject)
    ); end else progn
  ); end if
  


  (line outsideP1 outsideP2 layerDXFObject)
  (line outsideP3 outsideP4 layerDXFObject)

  ;draw-hole-in-plan-view (ss thickness index objRollRadius ObjCenterPt baseXPt sAng / i ent_info radius)

  ; (draw-hole-in-plan-view top-bolt-row_ss material_thickness 0 roll_radius center_point baseLeftXPt start-angle-dxf)
  (setq cnt 0)
  (repeat (sslength top-bolt-row_ss)  ;creates all the holes


    (draw-hole-in-plan-view top-bolt-row_ss material_thickness cnt roll_radius 
                            center_point baseLeftXPt start-angle-dxf
    )

    (setq cnt (1+ cnt))
  ) ;end repeat


  ; (entmake (list arc-dxf-0 center-dxf radius-dxf start-angle-dxf end-angle-dxf wcs-dxf)) ; offset away arc

  ; (command "REGEN")
  (setvar "osmode" oldsnap)
) ; end defun
;---------------------------------------------------------------------------------------------------------

;---------------------------------------------------------------------------------------------------------
(defun c:del-text (/ all_text_selected) 
  (setq all_text_selected (ssget '((0 . "TEXT"))))

  (if all_text_selected 
    (command "_.Erase" all_text_selected "") ; if true
  ) ;end if
); end defun


(defun c:del-circle (/ all_circle_selected) 
  (setq all_circle_selected (ssget '((0 . "CIRCLE"))))

  (if all_circle_selected 
    (command "_.Erase" all_circle_selected "") ; if true
  ) ;end if
); end defun
;---------------------------------------------------------------------------------------------------------

;-----------------------------------
;-------support functions-----------
;-----------------------------------

;---------------------------------------------------------------------------------------------------------
;entmake an angle based on assumed WCS
(defun Create-ARC-Entity (centerPtInput radiusInput StartAngleRAD EndAngleRAD 
                          layerDXF / arc-dxf-0 center-dxf radius-dxf wcs-dxf 
                          start-angle-dxf end-angle-dxf
                         ) 

  (setq arc-dxf-0 (cons 0 "ARC"))
  ; (setq color-dxf (cons 62 256));256 for by layer
  (setq center-dxf (append '(10) centerPtInput)) ; center point
  (setq radius-dxf (cons 40 radiusInput))
  (setq wcs-dxf (list 210 0.0 0.0 1.0)) ;sets wcs for entity
  (setq start-angle-dxf (cons 50 StartAngleRAD))
  ;find-start-angle-rad ( bolt-radius-length radius-of-curve /)
  (setq end-angle-dxf (cons 51 EndAngleRAD))
  (entmake 
    (list arc-dxf-0 
          center-dxf
          radius-dxf
          start-angle-dxf
          end-angle-dxf
          wcs-dxf
          layerDXF
          '(39 . 0.1)
    )
  )
)
;---------------------------------------------------------------------------------------------------------
;---------------------------------------------------------------------------------------------------------
(defun Line (p1 p2 layerDXF)  ;from lee mac; entmakex returns entity name
  (entmakex 
    (list (cons 0 "LINE") 
          (cons 10 p1)
          (cons 11 p2)
          layerDXF
    )
  )
)
;---------------------------------------------------------------------------------------------------------
(defun LWPoly (lst cls layerDXF) 
  (entmakex 
    (append 
      (list (cons 0 "LWPOLYLINE") 
            (cons 100 "AcDbEntity")
            (cons 100 "AcDbPolyline")
            (cons 90 (length lst))
            (cons 70 cls)
            layerDXF
      )
      (mapcar (function (lambda (p) (cons 10 p))) lst)
    )
  )
)
;---------------------------------------------------------------------------------------------------------

;---------------------------------------------------------------------------------------------------------
(defun c:rolled-hole (/ radius)  ;not used yet

  ;ask user for values
  (setq center_pt (getpoint "select a center point for hole:\n"))
  (setq thickness (getdist "enter material thickness:\n"))
  (setq radius (getdist "enter radius"))
  (setq bolt-radius (getdist "enter bolt radius"))

  ;set entity values
  (setq arc-dxf-0 (cons 0 "ARC"))
  (setq color-dxf (cons 62 256)) ;256 for by layer
  (setq center-dxf (append '(10) center_pt)) ; center point
  (setq radius-dxf (cons 40 radius))
  (setq wcs-dxf (list 210 0.0 0.0 1.0)) ;sets wcs for entity
  (setq start-angle-dxf (cons 50 0))
  (setq end-angle-dxf (cons 51 1.5707963268))

  ;this works
  (entmake 
    (list arc-dxf-0 center-dxf radius-dxf start-angle-dxf end-angle-dxf wcs-dxf)
  )
)
;---------------------------------------------------------------------------------------------------------
;---------------------------------------------------------------------------------------------------------
;90 = 1.5707963268
(defun find-start-angle-rad (bolt-radius-length radius-of-curve /) 

  ; (setq bolt-diameter-x (* bolt-radius-length))
  ;arc length / radius = angle in rads
  (- (* 270 (/ pi 180)) (/ bolt-radius-length radius-of-curve)) ; this 270 could just be a variable
)
;---------------------------------------------------------------------------------------------------------
;---------------------------------------------------------------------------------------------------------
(defun find-end-angle-rad (bolt-radius-length radius-of-curve /) 
  ;arc length / radius = angle in rads
  (+ (* 270 (/ pi 180)) (/ bolt-radius-length radius-of-curve))
)
;---------------------------------------------------------------------------------------------------------

;---------------------------------------------------------------------------------------------------------
(defun object-max-horizontal-length-and-basePt (ss / cnt i start_point_WCS_X 
                                                end_point_WCS_X StartEndXPtList 
                                                StartEndXPtList_sort
                                               )  ; returns a maxium length of the object by looking for the limits of x-values

  (setq cnt 0)
  (setq StartEndXPtList nil) ; nil out sort list

  (repeat (sslength ss) 
    (setq i (entget (ssname ss cnt)))
    (setq start_point_WCS_X (car (cdr (assoc 10 i))))
    (setq end_point_WCS_X (car (cdr (assoc 11 i))))

    (if (= StartEndXPtList nil) 
      (setq StartEndXPtList (list start_point_WCS_X end_point_WCS_X)) ; if true
      (setq StartEndXPtList (append 
                              (list start_point_WCS_X end_point_WCS_X)
                              StartEndXPtList
                            )
      ) ; else
    )

    (setq cnt (1+ cnt)) ;index counter
  ) ;end repeat

  (setq StartEndXPtList_sort (vl-sort 
                               StartEndXPtList
                               '< ;less then function
                             ) ;end vl-sort
  ) ;end setq

  (list (car StartEndXPtList_sort) 
        (- (last StartEndXPtList_sort) (car StartEndXPtList_sort))
  ) ;return list of (leftXPt distance)
);end defun
;---------------------------------------------------------------------------------------------------------
;---------------------------------------------------------------------------------------------------------
(defun top-horizontal-bolt-row (ss / cnt i j)  ; takes circle ss and sorts out all holes except the highest y data
  (setq cnt 0)
  (setq centerYPTList nil) ; nil out sort list

  (repeat (sslength ss) 
    (setq i (entget (ssname ss cnt)))
    (setq center_point_WCS_Y (cadr (cdr (assoc 10 i)))) ; grabs just the y point
    (setq index cnt)
    (setq center_pt_index_pair (list center_point_WCS_Y cnt)) ;creats 2 element pair to find the poistion later example; (1272.32 . 0)

    (if (= centerYPTList nil) 
      (setq centerYPTList (list center_pt_index_pair)) ; if true
      (setq centerYPTList (append 
                            (list center_pt_index_pair)
                            centerYPTList
                          )
      ) ; else
    )
    (setq cnt (1+ cnt)) ;index counter
  ) ;end repear

  (setq centerYPTList_sort (vl-sort 
                             centerYPTList
                             '(lambda (a b) 
                                (if (not (equal (car a) (car b) 0.000001)) 
                                  (< (car a) (car b)) ; if true
                                ) ;less then function
                              ) ;end vl-sort
                           ) ;end setq
  )

  (setq top-bolt-hole-elevation (car (last centerYPTList_sort))) ;stores last point which should be the the top most bolt row unless one hole is higher then other - TODO way to check if there is an edge case for one hole

  (setq top-bolts (vl-remove-if-not 
                    '(lambda (a) (equal (car a) top-bolt-hole-elevation 0.000001))
                    centerYPTList_sort
                  )
  ) ; returns a list of top bolts


  (setq top_bolt_ss_list (ssadd)) ;start empty SS list

  (foreach n top-bolts 

    (setq index_value (cadr n))

    (setq ent_name (ssname ss index_value)) ; return ent name, entget not required
    ; (setq ent_name (cadr (assoc -1 i)))
    (ssadd ent_name top_bolt_ss_list)
  ) ;end repeat

  (princ top_bolt_ss_list) ;works like a return
); end defun
;---------------------------------------------------------------------------------------------------------

(defun draw-hole-in-plan-view (ss thickness index objRollRadius ObjCenterPt baseXPt 
                               sAng / i ent_info radius layerDXFCenter
                              )  ;created to be called in a repeat

  (setq ent_info (entget (ssname ss index)))
  (setq radius (cdr (assoc 40 ent_info))) ;just radius ;cadr doesn't work on dot lists
  (setq entNum (cdr (assoc -1 ent_info)))

  (if (= -1 (last (assoc 210 ent_info))) 
    (setq centerXPt (abs (car (cdr (assoc 10 ent_info))))) ;center point of bolt circle ;if 210 has -1 in the last spot then abs the x ; if true
    (setq centerXPt (car (cdr (assoc 10 ent_info)))) ;center point of bolt circle ;if false
  ) ;end if

  ; (setq centerXPt (car (cdr (assoc 10 ent_info)))) ;center point of bolt circle

  (setq HoleAngList (findStartCenterEndOnCurve baseXPt centerXPt sAng objRollRadius 
                                               radius
                    )
  ) ;returns bolt (sANG, CenANG, eANG)
  (setq CenterAng (cadr HoleAngList))



  (setq blkNameTemp (strcat "Script_Generated_Bolt_Hole_" 
                            (rtos (* radius 2) 2 5)
                            "_"
                            (rtos objRollRadius 2 8)
                            "_"
                            (rtos thickness 2 5)
                    )
  )

  (if (tblsearch "block" blkNameTemp)  ;condition
    (progn 
      (entmakex 
        (list 
          (cons 0 "INSERT")
          (cons 2 blkNameTemp)
          (cons 10 ObjCenterPt)
          (cons 50 CenterAng)
          (cons 8 "Center")
        )
      )
    ) ;end progn
    (progn  ;if it doesn't exist it will create a block definition
           (createBoltPunchPlanViewBlock thickness objRollRadius radius baseXPt 
                                         centerXPt
           )
           (entmakex 
             (list 
               (cons 0 "INSERT")
               (cons 2 blkNameTemp)
               (cons 10 ObjCenterPt)
               (cons 50 CenterAng)
               (cons 8 "Center")
             )
           )
    ) ;end prog
  ) ;end if
);end defun
;---------------------------------------------------------------------------------------------------------

;---------------------------------------------------------------------------------------------------------
(defun findStartCenterEndOnCurve (baseXPt CircleXPt sAng objRollRadius holeRadius /)  ;returns list of bolt hole (start, center, end) radian values on the curve ;dist dist_rads cenAng holeStartAng holeEndAng

  (setq dist (abs (- baseXPt CircleXPt))) ;absolute because center is a larger number so to avoid negative
  (setq dist_rads (arcLengthInRadians dist objRollRadius))
  (if (= sAng 0) 
    (setq cenAng sAng) ;if true
    (setq cenAng (+ dist_rads sAng)) ;if false
  )
  ; (setq cenAng (+ dist_rads sAng)) ;center of bolt circle arc
  (setq holeStartAng (- cenAng (arcLengthInRadians holeRadius objRollRadius)))
  (setq holeEndAng (+ cenAng (arcLengthInRadians holeRadius objRollRadius)))

  (list holeStartAng cenAng holeEndAng) ;should return the list since its the last line
)



(defun arcLengthInRadians (s r /)  ;returns arc length in radiuns
  ;|
    S=r*theta
    s=arc length in radians
    r= radius
    theta = angle in radians
    
    |;
  (/ s r)
)

(defun createBoltPunchPlanViewBlock (ObjThickness objRollRadius boltRadius baseXPt 
                                     centerXPt /
                                    )  ;always on center layer; draws bolt circle with center line on zero rad/ zero deg
  ;(tblsearch "block" "Script_Generated_Bolt_Hole") ;to check if it was made

  (setq ObjDiaStr (rtos (* boltRadius 2) 2 5))

  (setq blkName (strcat "Script_Generated_Bolt_Hole_" 
                        ObjDiaStr
                        "_"
                        (rtos objRollRadius 2 8)
                        "_"
                        (rtos ObjThickness 2 5)
                )
  )
  ; (setq blkName "Script_Generated_Bolt_Hole_")
  ;Script_Generated_Bolt_Hole_BoltDia_RollRadius_thickness
  (setq layerDXF '(8 . "Center"))
  (setq zeroPt (list 0 0 0))

  ;0 is starting angle to base off of, this might bug out when it gets a negative number
  (setq startHoleAngList (findStartCenterEndOnCurve baseXPt centerXPt 0 objRollRadius 
                                                    boltRadius
                         )
  )
  (setq holeSAng (car startHoleAngList))
  (setq holeCenterAng (cadr startHoleAngList))
  (setq holeEAng (last startHoleAngList))
  ;------------------------
  ;all objects needs to be drawn in relation the 0,0,0, not the current xyz of the object selected
  ;------------------------
  (setq intRadius (- objRollRadius (/ ObjThickness 2)))
  (setq extRadius (+ objRollRadius (/ ObjThickness 2)))

  ;left line
  (setq p1 (polar zeroPt holeSAng intRadius))
  (setq p2 (polar zeroPt holeSAng extRadius))

  ;center line
  (setq p3 (polar zeroPt holeCenterAng (- intRadius 0.125)))
  (setq p4 (polar zeroPt holeCenterAng (+ extRadius 0.125)))

  ;right line
  (setq p5 (polar zeroPt holeEAng intRadius))
  (setq p6 (polar zeroPt holeEAng extRadius))

  ;left LW polyline
  (setq leftLWPolyPtList (list p1 (polar zeroPt 0 objRollRadius) p2))

  ;right LW polyline
  (setq rightLWPolyPtList (list p5 (polar zeroPt 0 objRollRadius) p6))

  ; this works but the block doesn't show up ;new direction
  (entmake 
    (list 
      '(0 . "BLOCK")
      (cons 2 blkName)
      (cons 10 zeroPt)
      '(70 . 0)
      '(100 . "AcDbEntity")
      '(100 . "AcDbBlockBegin")
      '(8 . "0") ;block entity data is placed on layer 0
    )
  )
  (Create-ARC-Entity zeroPt intRadius holeSAng holeEAng layerDXF)
  (Create-ARC-Entity zeroPt extRadius holeSAng holeEAng layerDXF)
  (line p1 p2 layerDXF)
  (line p3 p4 layerDXF)
  (line p5 p6 layerDXF)
  (LWPoly leftLWPolyPtList 0 layerDXF) ;1 is for dxf 70
  (LWPoly rightLWPolyPtList 0 layerDXF) ;1 is for dxf 70
  (entmake 
    (list 
      '(0 . "ENDBLK")
      '(100 . "AcDbBlockEnd")
    )
  )
);end defun


(defun SheetNotchFinder (ss maxLength basePt radius / cnt StartEndXPtList2 i 
                         start_point_WCS_Y end_point_WCS_y
                        )  ;needs to be able to find notch in the top and bottom even with a notch on both sides
  (setq cnt 0)
  (setq StartEndXPtList2 nil) ; nil out sort list

  ;filter out vertical lines and leave horiztonal lines

  (setq horizontalLines (ssadd)) ;empty ss

  (repeat (sslength ss) 
    (setq i (entget (ssname ss cnt)))
    (setq startPT (cdr (assoc 10 i)))
    (setq endPT (cdr (assoc 11 i)))

    (if (equal (cadr startPT) (cadr endPT) 0.000001)  ;checks y pts are the same
      (ssadd (ssname ss cnt) horizontalLines) ; add to ss
      (princ) ; do nothing
    )
    (setq cnt (1+ cnt)) ;index counter
  ) ;end repeat

  (setq cnt 0)
  ;use horizontallines ss from this point on
  (repeat (sslength horizontalLines) 
    (setq i (entget (ssname horizontalLines cnt)))
    (setq start_point_WCS_Y (list (cdr (assoc 10 i)) cnt)) ;returns xyz pt with index value (12000.00 3)
    ; (setq end_point_WCS_y (list (cdr (assoc 11 i)) cnt)) ;returns xuz pt with index value (12000.00 3)

    (if (= StartEndXPtList2 nil) 
      (setq StartEndXPtList2 (list start_point_WCS_Y)) ; if true
      (setq StartEndXPtList2 (append 
                               (list start_point_WCS_Y)
                               StartEndXPtList2
                             )
      ) ; else
    ) ;end if

    (setq cnt (1+ cnt)) ;index counter
  ) ;end repeat

  (setq StartEndXPtList2_sort (vl-sort 
                                StartEndXPtList2
                                '(lambda (a b) 
                                   (> (cadr (car a)) (cadr (car b)))
                                 )
                              ) ;end vl-sort
  ) ;end setq

  (setq topLineSSIndex (cadr (car StartEndXPtList2_sort))) ;index

  (setq XDist (car (maxlengthLine (ssname horizontalLines topLineSSIndex)))) ; maxlengthline returns (list x_distance y_distance)
  ;basePt
  (if (not (equal XDist maxLength 0.000001)) 
    (progn  ; true progn
           (setq leftdistance (abs 
                                (- basePt 
                                   (car 
                                     (cdr 
                                       (assoc 10 
                                              (entget 
                                                (ssname horizontalLines 
                                                        topLineSSIndex
                                                )
                                              )
                                       )
                                     )
                                   )
                                )
                              )
           )
           (setq rightdistance (abs 
                                 (- basePt 
                                    (car 
                                      (cdr 
                                        (assoc 11 
                                               (entget 
                                                 (ssname horizontalLines 
                                                         topLineSSIndex
                                                 )
                                               )
                                        )
                                      )
                                    )
                                 )
                               )
           )
           (setq leftDistanceRad (arcLengthInRadians leftdistance radius))
           (setq rightDistanceRad (arcLengthInRadians rightdistance radius))
           ;  (setq output (list leftDistanceRad rightDistanceRad))
    ) ; end true progn
    (progn  ; false progn
           (princ "no notch")
    ) ; end false progn
  )
  (list leftDistanceRad rightDistanceRad) ;return output
);end defun


(defun maxlengthLine (ent / entInfo startPoint_X endPoint_X x_distance startPoint_y 
                      endPoint_y y_distance
                     ) 
  (setq entInfo (entget ent))

  ;x distance
  (setq startPoint_X (car (cdr (assoc 10 entInfo))))
  (setq endPoint_X (car (cdr (assoc 11 entInfo))))

  (setq x_distance (abs (- startPoint_X endPoint_X)))

  ;y distance
  (setq startPoint_y (cadr (cdr (assoc 10 entInfo))))
  (setq endPoint_y (cadr (cdr (assoc 11 entInfo))))

  (setq y_distance (abs (- startPoint_y endPoint_y)))

  (list x_distance y_distance) ;return
)




































(defun slotChecker (/) 
  (princ)
);end defun

(defun block->ss (/) 
  (princ)
);slot 1.25

;https://forums.autodesk.com/t5/visual-lisp-autolisp-and-general/get-entities-inside-a-block/td-p/2644829

;check out this link for entitys in block


;|
  
  (mapcar 'cdr

  (vl-remove-if-not '(lambda (x) (= (car x) 10));10 is the dxf code

    (entget (car (entsel "\nSelect lightweight Polyline: ")))

  )

)
  
  
  ;returns ((12394.5 1299.89) (12393.8 1299.89) (12393.8 1299.32) (12394.5 1299.32)) list of xy cant do z
  
  
  |;


(defun blockLoop (/ blkent blkname InsideBlkFirstEnt) 
  (setq blkent (entget (car (entsel))))
  (setq blkname (cdr (assoc 2 blkent)))
  (setq InsideBlkFirstEnt (entget (cdr (assoc -2 (tblsearch "block" blkname)))))

  (setq objects-ss nil) ;empty it every run
  (setq objects-ss (ssadd)) ;empty ss

  (setq firstEntName (cdr (assoc -1 InsideBlkFirstEnt)))
  (ssadd firstEntName objects-ss)

  (while (setq insideEnt (entnext firstEntName)) 

    (setq entName (cdr (assoc -1 (entget insideEnt))))
    (ssadd entName objects-ss)
  ) ;end while
)

;(entget (entnext (cdr (assoc -2 (tblsearch "block" "GIRDER ANGLE STRETCHOUT VIEW")))))




; test block with circle - 
; (entget (cdr (assoc -2 (tblsearch "block" "test"))))
  
; (entget (entnext (cdr (assoc -2 (tblsearch "block" "test")))))

; (entget (entnext (entnext (cdr (assoc -2 (tblsearch "block" "test"))))))



























































;;---------------------=={ Entity List }==--------------------;;
;;                                                            ;;
;;  Displays the DXF Information for an entity, a variation   ;;
;;  of the program by Michael Puckett of the same name.       ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac - www.lee-mac.com                         ;;
;;------------------------------------------------------------;;
;;  Arguments: entity - entity (ename) / DXF list             ;;
;;------------------------------------------------------------;;

(defun elist (entity / prin1x princx data xdata) 

  (defun prin1x (x i) (repeat i (princ "  ")) (prin1 x))
  (defun princx (x i) (repeat i (princ "  ")) (princ x))

  (cond 
    ((or 
       (and 
         (eq 'ENAME (type entity))
         (setq data (entget entity))
       )
       (and 
         (listp entity)
         (setq data entity)
         (setq entity (cdr (assoc -1 data)))
       )
     )
     (princ "\n\n  (\n")
     (foreach pair data 
       (prin1x pair 2)
       (princ "\n")
     )
     (if (setq xdata (assoc -3 (entget entity '("*")))) 
       (progn 
         (princx "(" 2)
         (prin1 (car xdata))
         (princ "\n")
         (foreach app (cdr xdata) 
           (princx "(" 3)
           (princ "\n")
           (foreach pair app (prin1x pair 4) (princ "\n"))
           (princx ")" 3)
           (princ "\n")
         )
         (princx ")" 2)
         (princ "\n")
       )
     )
     (princ "  )")
     (if (= 1 (cdr (assoc 66 data))) 
       (while 
         (progn 
           (elist (setq entity (entnext entity)))
           (not (eq "SEQEND" (cdr (assoc 0 (entget entity)))))
         )
       )
     )
    )
    ((print entity))
  )
  (princ)
)

(defun c:ee nil (elist (car (entsel))))
(defun c:eex nil (elist (car (nentsel))))

















;----------------------------------------------------------------
;----------------------------------------------------------------
;----------------------------------------------------------------
;afralisp polyline bulge functions - https://www.afralisp.net/autolisp/tutorials/polyline-bulges-part-1.php - https://www.afralisp.net/autolisp/tutorials/polyline-bulges-part-2.php

(defun getPolySegs (/ ent entl p1 pt bulge seg ptlst) 
  (setvar "ERRNO" 0)
  ;; repeat request for polyline until user either picks
  ;; a polyline or exits without picking
  (while (and (not ent) (/= (getvar "ERRNO") 52)) 
    (if 
      (and (setq ent (car (entsel "\nSelect polyline: "))) 
           (/= (cdr (assoc 0 (setq entl (entget ent)))) "LWPOLYLINE")
      )
      (setq ent nil)
    )
  )
  (cond 
    (ent
     ;; save start point if polyline is closed
     (if (= (logand (cdr (assoc 70 entl)) 1) 1) 
       (setq p1 (cdr (assoc 10 entl)))
     )
     ;; run thru entity list to collect list of segments
     (while (setq entl (member (assoc 10 entl) entl)) 
       ;; if segment then add to list
       (if (and pt bulge) 
         (setq seg (list pt bulge))
       )
       ;; save next point and bulge
       (setq pt    (cdr (assoc 10 entl))
             bulge (cdr (assoc 42 entl))
       )
       ;; if segment is build then add last point to segment
       ;; and add segment to list
       (if seg 
         (setq seg   (append seg (list pt))
               ptlst (cons seg ptlst)
         )
       )
       ;; reduce list and clear temporary segment
       (setq entl (cdr entl)
             seg  nil
       )
     )
    )
  )
  ;; if polyline is closed then add closing segment to list
  (if p1 (setq ptlst (cons (list pt bulge p1) ptlst)))
  ;; reverse and return list of segments
  (reverse ptlst)
)

(defun getArcInfo (segment / a p1 bulge p2 c c|2 gamma midp p phi r r2 s theta) 
  ;; assign variables to values in argument
  (mapcar 'set '(p1 bulge p2) segment)
  ;; find included angle
  ;; remember that bulge is negative if drawn clockwise
  (setq theta (* 4.0 (atan (abs bulge))))
  ;; output included angle
  (princ 
    (strcat "\n Included angle: " 
            (rtos theta)
            " rad ("
            (angtos theta 0)
            " degrees)"
    )
  )
  ;; find height of the arc
  (setq c (distance p1 p2)
        s (* (/ c 2.0) (abs bulge))
  )
  ;; output height of arc
  (princ (strcat "\n Height of arc:  " (rtos s)))
  ;; output chord length
  (princ (strcat "\n Chord length:   " (rtos c)))
  ;; If this function is used without making sure that the segment
  ;; is not simply a line segment (bulge = 0.0), it will produce
  ;; a division-by-zero error in the following. Therefore we want
  ;; to be sure that it doesn't process line segments.
  (cond 
    ((not (equal bulge 0.0 1E-6))
     ;; find radius of arc
     ;; first find half the chord length
     (setq c|2 (/ c 2.0) ;; find radius with Pythagoras (used as output)
           r   (/ (+ (expt c|2 2.0) (expt s 2.0)) (* s 2.0)) ;; find radius with trigonometry
           r2  (/ c|2 (sin (/ theta 2.0)))
     )
     (princ (strcat "\n Radius of arc:  " (rtos r)))
     ;; find center point of arc with angle arithmetic
     ;; (used as output)
     (setq gamma (/ (- pi theta) 2.0)
           phi   (if (>= bulge 0) 
                   (+ (angle p1 p2) gamma)
                   (- (angle p1 p2) gamma)
                 )
           p     (polar p1 phi r)
     )
     ;; find center point of arc with Pythagoras
     (setq a    (sqrt (- (expt r 2.0) (expt c|2 2.0)))
           midp (polar p1 (angle p1 p2) c|2)
           p2   (if (>= bulge 0) 
                  (polar midp (+ (angle p1 p2) (/ pi 2.0)) a)
                  (polar midp (- (angle p1 p2) (/ pi 2.0)) a)
                )
     )
     ;; output coordinates of center point
     (princ 
       (strcat "\n Center of arc:  " 
               (rtos (car p))
               ","
               (rtos (cadr p))
       )
     )
    )
    (T (princ "\n Segment has no arc info"))
  )
  (princ)
)

(defun C:POLYARCS (/ a polysegs seg) 
  ;; make a list of polyline segments of a
  ;; selected polyline
  (cond 
    ((setq polysegs (getPolySegs))
     ;; a is just an informative counter
     (setq a 0)
     ;; run thru each segment
     (foreach seg polysegs 
       (setq a (1+ a))
       ;; only process the segment if it's an arc
       ;; i.e. bulge /= 0.0
       (cond 
         ((not (zerop (cadr seg)))
          (princ (strcat "\nSegment " (itoa a) ": "))
          ;;
          (getArcInfo seg)
         )
       )
     )
    )
  )
)

;----------------------------------------------------------------
;----------------------------------------------------------------
;----------------------------------------------------------------