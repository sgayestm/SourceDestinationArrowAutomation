;;--------------=={ ReDoRungs.lsp - RungIndexerForPages }==-------------;;
;;                                                                      ;;  
;;  This program lets you redo the rung numbers for each page by        ;;
;;  Checking the dwg file and only getting all connection pages, then   ;;
;;  updating the current rung number when it is on the current page is  ;;
;;  selected                                                            ;;
;;                                                                      ;;
;;----------------------------------------------------------------------;;
;;  Author:  Matthew Ayestaran										                    	;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2024-07-21                                      ;;
;;                                                                      ;;
;;  - First release.                                                    ;;
;;----------------------------------------------------------------------;;
(defun AddZero (Number)
  (if (< (atoi Number) 10)
    (strcat "0" Number)
    (strcat "" Number)
  )
)

(defun GetLastSourceFromLine (line)
  ;; Find the position of the hyphen
  (setq hyphenpos (vl-string-search "-" line))
  
  ;; Extract the substring after the hyphen
  (if hyphenpos
    (setq result (substr line (+ hyphenpos 2)))
    (setq result ""))
  result
)

(defun UpdateSourceArrowFirstRung (ArrowName FilePath ename)
  "Update the source arrow number based on the given file path and prefix."
  (setq aline 0)
  (setq matchFound nil)
  (setq file (open FilePath "r"))
  (if file
    (progn
        ;read log file into list, and match the line with arrow number
        ;if match update the list with new line instead
        (setq lines '())
        (while (setq CurrentLine (read-line file))
          (progn
          (if (wcmatch CurrentLine (strcat ArrowName "-*"))
            (progn
              (setq LastSource (GetLastSourceFromLine CurrentLine))
              (setq NewSource (rtos (+ (atoi LastSource) 1) 2 0))
              (setq CurrentLine (strcat ArrowName "-" NewSource))
              (setq matchFound t)
            )
          )
          (setq lines (append lines (list CurrentLine)))
          )
        )
        (if (not matchFound)
          (progn
          (setq lines (append lines (list (strcat ArrowName "-1"))))
          (setq NewSource "1")
          )
        )
        (close file)
        (setq UpdatedValue (AddZero NewSource))
        (setpropertyvalue ename "SIGCODE" (strcat ArrowName "-" UpdatedValue))
        (setq file (open FilePath "w"))
        (foreach line lines
          (write-line line file)
        )
        (close file)
    )
    ;if file doesnt exist create new file in location
    (progn
      (princ (strcat "\nError: Cannot open file " FilePath "\n"))
      (setq file (open FilePath "w"))
      (write-line (strcat ArrowName "-1") file)
      (close file)
      ;Run remaining commands as if the file existed
      (setq NewSource "1")
      (setq UpdatedValue (AddZero NewSource))
      (setpropertyvalue ename "SIGCODE" (strcat ArrowName "-" UpdatedValue))
      (princ (strcat "\nFile created and initialized: " FilePath "\n"))
    )
  )
)



(defun c:SourceArrowFirstRung (/ cnt obj atts ArrowName LastSource NewSource)
  "Find and update all instances of down facing destination arrows."
  (setq s (ssget "_X" '((0 . "INSERT")(66 . 1)(2 . "HA1S*"))))
  (if s
    (repeat (setq cnt (sslength s))
      (setq cnt (1- cnt))
      (setq obj (vlax-ename->vla-object (ssname s cnt)))
      (setq atts (vlax-invoke obj 'Getattributes))
      (setq ArrowName (vla-get-textstring (nth 0 atts)))

      (if ArrowName
        (if (wcmatch ArrowName "") 
          (princ "found blank")
          (if (< (nth 1 (assoc 10 (entget (ssname s cnt)))) 17)
            (UpdateSourceArrowFirstRung ArrowName "C:\\Users\\matth\\Documents\\Current_Project_Log.txt" (ssname s cnt))
          )
        )
      )
    )
    (princ "\nNo arrows found.\n")
  )
  (princ)
)