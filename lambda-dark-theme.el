;;; lambda-dark-theme.el --- Lambda-theme dark variant   -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Colin McLear

;; Author: Colin McLear <mclear@unl.edu>
;; Keywords: faces,


;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'lambda-themes)

(lambda-themes-deftheme
 lambda-dark
 "A custom theme."

 ((((class color) (min-colors #xFFFFFF))        ; col 1 GUI/24bit
   ((class color) (min-colors #xFF)))           ; col 2 Xterm/256

  (lambda-fg          "#EBE9E7" nil)
  (lambda-bg          "#141414" nil)

  (lambda-ultralight  "#1A1919" nil)
  (lambda-highlight   "#2C2C34" nil)
  (lambda-lowlight    "#3A3A41" nil)

  (lambda-urgent      "#CF6752" nil)
  (lambda-crucial     "#F2DA61" nil)
  (lambda-focus       "#4560E6" nil)
  (lambda-strong      "#F5F2F0" nil)
  (lambda-meek        "#AFADAF" nil)
  (lambda-mild        "#2D2D2E" nil)
  (lambda-faint       "#1D1E26" nil)

  (lambda-black       "#000000" nil)
  (lambda-white       "#FFFFFF" nil)
  (lambda-red         "#bf616a" nil)
  (lambda-green       "#8eb89d" nil)
  (lambda-blue        "#81a1c1" nil)
  (lambda-yellow      "#e9b85d" nil)
  (lambda-orange      "#d08770" nil)
  (lambda-aqua        "#85CCC6" nil)
  (lambda-purple      "#9D67E6"  nil)

  ;; (lambda-plain       "#CBCACB" nil)

  )

 (custom-theme-set-variables 'lambda-dark
                             `(ansi-color-names-vector
                               [,lambda-faint
                                ,lambda-red
                                ,lambda-green
                                ,lambda-yellow
                                ,lambda-blue
                                ,lambda-purple
                                ,lambda-aqua
                                ,lambda-strong]))
 )

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(run-hooks 'lambda-themes-after-load-theme-hook)

(provide-theme 'lambda-dark)

;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; End:

;;; lambda-dark-theme.el ends here
