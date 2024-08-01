;;--------------=={ DestinationArrowsSecondRung.lsp - RungIndexerForPages }==-------------;;
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
;;  - Second release.                                                    ;;
;;----------------------------------------------------------------------;;
(defun AddZero (Number)
  (if (< (atoi Number) 10)
    (strcat "0" Number)
    Number
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

(defun UpdateDestinationArrowsSecondRung (ArrowName FilePath ename)
  "Update the source arrow number based on the given file path and prefix."
  (setq matchFound nil)
  (setq LastSource "0") ;; Default value if no line is matched
  
  ;; Open the file for reading
  (setq file (open FilePath "r"))
  (if file
    (progn
      ;; Read log file into list
      (setq lines '())
      (while (setq CurrentLine (read-line file))
        ;; Check if the line matches the ArrowName
        (if (wcmatch CurrentLine (strcat ArrowName "-*"))
          (progn
            ;; Found matching line, update it
            (setq LastSource (GetLastSourceFromLine CurrentLine))
            (setq lines (append lines (list CurrentLine)))
            (setq matchFound t)
          )
          ;; If no match, keep the original line
          (setq lines (append lines (list CurrentLine)))
        )
      )
      (close file)
      
      ;; If no match found, add new entry
      (if (not matchFound)
        (progn
        (setq lines (append lines (list (strcat ArrowName "-0"))))
        (setq LastSource "1")
        )
      )

      ;; Update entity property
      (setq UpdatedValue (AddZero LastSource))
      (setpropertyvalue ename "SIGCODE" (strcat ArrowName "-" UpdatedValue))

      ;; Write the updated content back to the file
      (setq file (open FilePath "w"))
      (foreach line lines
        (write-line line file)
      )
      (close file)
    )
    ;; If file doesn't exist, create it
    (progn
      (princ (strcat "\nError: Cannot open file " FilePath "\n"))
      (setq file (open FilePath "w"))
      (write-line (strcat ArrowName "-0") file)
      (close file)
      ;; Run remaining commands as if the file existed
      (setq UpdatedValue "01")
      (setpropertyvalue ename "SIGCODE" (strcat ArrowName "-" UpdatedValue))
      (princ (strcat "\nFile created and initialized: " FilePath "\n"))
    )
  )
)

(defun c:DestinationArrowsSecondRung (/ cnt obj atts ArrowName LastSource)
  "Find and update all instances of down facing destination arrows."
  (setq s (ssget "_X" '((0 . "INSERT")(66 . 1)(2 . "HA1D*"))))
  (if s
    (progn
      (repeat (setq cnt (sslength s))
        (setq cnt (1- cnt))
        (setq obj (vlax-ename->vla-object (ssname s cnt)))
        (setq atts (vlax-invoke obj 'Getattributes))
        (setq ArrowName (vla-get-textstring (nth 0 atts)))

        (if (and ArrowName (not (wcmatch ArrowName "")))
          (if (> (nth 1 (assoc 10 (entget (ssname s cnt)))) 17)
            (UpdateDestinationArrowsSecondRung ArrowName "C:\\Users\\matth\\Documents\\Current_Project_Log.txt" (ssname s cnt))
          )
        )
      )
    )
    (princ "\nNo arrows found.\n")
  )
  (princ)
)
