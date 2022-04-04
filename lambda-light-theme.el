;;; lambda-light-theme.el --- Lambda-theme light variant   -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Colin McLear

;; Author: Colin McLear <mclear@unl.edu>
;; Keywords: faces,


;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'lambda-themes)

(lambda-themes-deftheme
 lambda-light
 "A custom theme."

 ((((class color) (min-colors #xFFFFFF))        ; col 1 GUI/24bit
   ((class color) (min-colors #xFF)))           ; col 2 Xterm/256

  (lambda-fg          "#0C0D0D" nil)
  (lambda-bg          "#FFFCFA" nil)

  (lambda-ultralight  "#E3E1E0" nil)
  (lambda-highlight   "#EBE9E7" nil)
  (lambda-lowlight    "#F8F6F4" nil)

  (lambda-urgent      "#B30000" nil)
  (lambda-crucial     "#5D00DA" nil)
  (lambda-focus       "#0044CC" nil)
  (lambda-strong      "#000000" nil)
  (lambda-meek        "#706F6F" nil)
  (lambda-mild        "#D1CFCF" nil)
  (lambda-faint       "#F5F2F0" nil)

  (lambda-black       "#000000" nil)
  (lambda-white       "#FFFFFF" nil)
  (lambda-gray        "#56565B" nil)
  (lambda-red         "#EC6A5E" nil)
  (lambda-green       "#61C554" nil)
  (lambda-blue        "#4C4CFF" nil)
  (lambda-yellow      "#F4BF4F" nil)
  (lambda-orange      "#ED8811" nil)
  (lambda-aqua        "#278C87" nil)
  (lambda-purple      "#833AE6" nil))

 (custom-theme-set-variables 'lambda-light
                             `(ansi-color-names-vector
                               [,lambda-faint
                                ,lambda-red
                                ,lambda-green
                                ,lambda-yellow
                                ,lambda-blue
                                ,lambda-purple
                                ,lambda-aqua
                                ,lambda-strong])))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))


(run-hooks 'lambda-themes-after-load-theme-hook)

(provide-theme 'lambda-light)

;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; End:

;;; lambda-light-theme.el ends here
