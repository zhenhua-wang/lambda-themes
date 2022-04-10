;;; lambda-dark-reg-theme.el --- Lambda-theme dark variant   -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Colin McLear

;; Author: Colin McLear <mclear@unl.edu>
;; Keywords: faces,


;;; Code:

(require 'lambda-themes)

(deftheme lambda-dark-reg "Lambda theme, the dark regular version")

(lambda-themes-create 'dark-reg 'lambda-dark-reg)

(run-hooks 'lambda-themes-after-load-theme-hook)

(provide-theme 'lambda-dark-reg)

;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; End:

;;; lambda-dark-reg-theme.el ends here
