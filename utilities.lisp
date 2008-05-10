


(defun rename-files (from to)
 (dolist (file (directory from))
   (rename-file file (translate-pathname file from to))))

;; And thats it, 3 lines of code, or for our simple testing purposes

(defun show-rename-files (from to)
 (dolist (file (directory from))
   (format t "Renaming ~A to ~A~%" file
           (translate-pathname file from to))))

;; (show-rename-files "/usr/share/pixmaps/*.xpm" "/usr/share/pixmaps/backup-*.xpm")

