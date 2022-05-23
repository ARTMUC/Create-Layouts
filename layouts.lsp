(defun GridsToLayouts ( UseUndoMarks / GridLayer GridAttribute SourceLayout TitleBlockHeight KeyZoomFactor
                                       TitleBlockName TitleBlockSheetNumberAttribute TitleBlockTotalSheetsAttribute
                                       vl-GetAttributeValue
                                       ss i enam edata grids grid id previd ssvp1 vp1 vpno1 ssvp2 vp2 vbno2 ptmin ptmax)
  (vl-load-com)

  ;;;*SOME SETTINGS THAT CAN BE CUSTOMIZED
  (setq GridLayer                        "GRID")
  (setq GridAttribute                    "MAPSHTNUM")
  (setq SourceLayout                     "001")
  (setq TitleBlockHeight                 86)
  (setq KeyZoomFactor                    0.5)
  ;(setq TitleBlockName                   "XXX_2")
  ;(setq TitleBlockSheetNumberAttribute   "SHEET_NO")
  ;(setq TitleBlockTotalSheetsAttribute   "NO_OF_SHEETS")

  (defun vl-GetAttributeValue ( blk tag )
    (setq tag (strcase tag))
    (vl-some '(lambda ( att ) (if (= tag (strcase (vla-get-tagstring att))) (vla-get-textstring att)))
        (vlax-invoke blk 'getattributes)
    )
  )
 
  (cond
   ((not (setq ss (ssget "x" (list (cons 0 "INSERT") (cons 8 GridLayer)))))
    (princ (strcat "\nNo grid blocks on layer '" GridLayer "' found."))
   )
   ((not (member SourceLayout (layoutlist)))
    (princ (strcat "\nSource layout '" SourceLayout "' not found."))
   )
   ((> (length (layoutlist)) 1)
    (princ (strcat "\nOnly layouts 'Model' and '" SourceLayout "' should exist."))
   )
   (T
    (setq i 0)
    (while (< i (sslength ss))
	  (setq edata (entget (setq enam (ssname ss i))))
      (if (and
            (= (cdr (assoc 0 edata)) "INSERT")
            (setq attval (vl-GetAttributeValue (vlax-ename->vla-object (cdr (assoc -1 edata))) GridAttribute))
          )
        (setq grids (cons (cons attval enam) grids))
      )
      (setq i (1+ i))
    )
    (setq grids (vl-sort grids (function (lambda (e1 e2) (< (car e1) (car e2))))))

    
    (if UseUndoMarks (vla-StartUndoMark (vla-get-ActiveDocument (vlax-get-acad-object))))
    (if grids
      (princ "\nCreating layouts...")
      (princ "\nNo grids found...")
    )
    (foreach grid grids
      (if grids
        (progn
          (setq id (car grid) enam (cdr grid))
          (princ (strcat "\nCreating layout '" id "'... "))
          (if (not (member id (layoutlist)))
            (command "._layout" "c" previd id)
          )
          (command "._layout" "s" id "._pspace")
          (if (and
                (setq ssvp1 (ssget "x" (list (cons 0 "VIEWPORT") (cons -4 "*,>,*") (list 10 0 TitleBlockHeight 0))))
                (setq ssvp2 (ssget "x" (list (cons 0 "VIEWPORT") (cons -4 "*,<,*") (list 10 0 TitleBlockHeight 0))))
              )
            (progn
              (vla-getboundingbox (vlax-ename->vla-object enam) 'ptmin 'ptmax)
              (setq vpno1 (cdr (assoc 69 (entget (setq vp1 (ssname ssvp1 0))))))
              (setq vpno2 (cdr (assoc 69 (entget (setq vp2 (ssname ssvp2 0))))))
              (command "._mspace")
              (setvar "CVPORT" vpno1)
              (vla-zoomwindow (vlax-get-acad-object) ptmin ptmax)
              (setvar "CVPORT" vpno2)
              (vla-zoomwindow (vlax-get-acad-object) ptmin ptmax)
              (vla-zoomscaled (vlax-get-acad-object) KeyZoomFactor acZoomScaledRelative)
              (command "._pspace")
              (vla-zoomextents (vlax-get-acad-object))
            )
            (princ (strcat "\nUnable to find the two vieports needed for layout " id))
          )
          (setq previd id)
          (if (= (length (layoutlist)) 255)
            (progn
              (princ "\nMaximum number of layouts met.")
              (setq grids nil)
            )
          )
          (vla-eval (vlax-get-acad-object) "DoEvents")
        )       
      )
    )
    (princ "\... GridsToLayouts finished.")
    (if UseUndoMarks (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-acad-object))))
   )
  )
)

(defun C:GridsToLayouts nil (GridsToLayouts T) (princ))
      