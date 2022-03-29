;;; lambda-themes.el --- A custom theme  -*- lexical-binding: t; -*-
;; Copyright (C) 2020 Colin McLear
;; -------------------------------------------------------------------
;; Authors: Colin McLear
;; -------------------------------------------------------------------
;; URL: https://github.com/mclear-tools/lambda-themes
;; -------------------------------------------------------------------
;; Created: 2021-03-16
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
;; -------------------------------------------------------------------
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>
;; -------------------------------------------------------------------
;;; Commentary:
;; Lamda-themes provides a set of light and dark medium contrast themes that
;; offer a balance between readability and aesthetics. The main focus is on
;; keeping colors easily distinguishable and with enough contrast, while still
;; being aesthetically pleasing.
;; -------------------------------------------------------------------
;;; Code

;;;; Requirements
(eval-when-compile
  (require 'cl-lib))
(require 'autothemer)

(unless (>= emacs-major-version 25)
  (error "Requires Emacs 25 or later"))

;;;; Theme Options

(defcustom lambda-themes-set-theme 'light
  "Choose which theme variant, light or dark, to use."
  :group 'lambda-themes
  :type 'symbol)

;; Cursors
(defcustom lambda-themes-set-evil-cursors t
  "If t then use lambda evil cursor colors."
  :group 'lambda-themes
  :type 'boolean)

;; Font options
(defcustom lambda-themes-set-italic-comments t
  "If t then use italics for comments."
  :group 'lambda-themes
  :type 'boolean)

(defcustom lambda-themes-set-italic-keywords t
  "If t then use italics for keywords."
  :group 'lambda-themes
  :type 'boolean)

(defcustom lambda-themes-set-variable-pitch t
  "If t then use variable-pitch for headings."
  :group 'lambda-themes
  :type 'boolean)

;;;; Faces
(defface lambda-bg nil "")
(defface lambda-fg nil "")
(defface lambda-ultralight nil "")
(defface lambda-highlight nil "")
(defface lambda-lowlight nil "")
(defface lambda-urgent nil "")
(defface lambda-crucial nil "")
(defface lambda-focus nil "")
(defface lambda-strong nil "")
(defface lambda-mild nil "")
(defface lambda-faint nil "")
(defface lambda-blue nil "")
(defface lambda-green nil "")
(defface lambda-red nil "")
(defface lambda-yellow nil "")
(defface lambda-orange nil "")
(defface lambda-purple nil "")
(defface lambda-aqua nil "")
(defface lambda-gray nil "")

;;;; After Load Theme Hook
(defvar lambda-themes-after-load-theme-hook nil
  "Hook run after bespoke-theme is loaded using `load-theme'.")

;;;; Disable Theme Function
(defun lambda-themes--disable-all-themes ()
  "Disable all active themes."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

;;;; Theme Toggle
(defcustom lambda-themes-active-theme 'light "Variable for holding light/dark value of theme appearance."
  :group 'lambda-themes
  :type 'symbol)

;;;###autoload
(defun lambda-themes-toggle-theme ()
  "Toggle between dark and light variants."
  (interactive)
  (if (eq lambda-themes-active-theme 'light)
      (progn
        (lambda-themes--disable-all-themes)
        (load-theme 'lambda-dark t)
        (setq lambda-themes-active-theme 'dark)
        (run-hooks 'lambda-themes-after-load-theme-hook))
    (progn
      (lambda-themes--disable-all-themes)
      (load-theme 'lambda-light t)
      (setq lambda-themes-active-theme 'light)
      (run-hooks 'lambda-themes-after-load-theme-hook)
      )))

;;;; Define Theme
(defmacro lambda-themes-deftheme (name description palette &rest body)
  `(autothemer-deftheme
    ,name
    ,description
    ,palette

;;;;; Default
    ((default                                           (:background lambda-bg :foreground lambda-fg))
     (cursor                                            (:background lambda-fg))
     ;; (mode-line                                         (:background lambda-ultralight :box (:line-width 1 :color lambda-mild :style nil)))
     ;; (mode-line-inactive                                (:background lambda-bg :foreground lambda-faint (:line-width 1 :color lambda-mild :style nil)))
     (fringe                                            (:background lambda-bg :weight 'light))
     (hl-line                                           (:background lambda-highlight))
     (region                                            (:background lambda-mild))
     (secondary-selection                               (:background lambda-lowlight))
     (buffer-menu-buffer                                (:foreground lambda-strong))
     (minibuffer-prompt                                 (:background lambda-bg :foreground lambda-crucial))
     (vertical-border                                   (:foreground lambda-bg))
     (internal-border                                   (:background lambda-bg :foreground lambda-bg))
     (show-paren-match                                  (:foreground lambda-crucial :weight 'bold))
     (show-paren-mismatch                               (:foreground lambda-urgent :weight 'bold :box t))
     (link                                              (:foreground lambda-blue :underline t))
     (shadow                                            (:foreground lambda-lowlight))

     ;; NOTE: We want the lambda-themes- colors to be available as faces. It seems like there
     ;; should be a better way to do this but...
     (lambda-fg         (:foreground lambda-fg))
     (lambda-bg         (:background lambda-bg))
     (lambda-ultralight (:background lambda-ultralight))
     (lambda-highlight  (:foreground lambda-highlight))
     (lambda-lowlight   (:foreground lambda-lowlight))
     (lambda-urgent     (:foreground lambda-urgent))
     (lambda-focus      (:foreground lambda-focus))
     (lambda-strong     (:foreground lambda-strong))
     (lambda-crucial    (:foreground lambda-crucial))
     (lambda-mild       (:foreground lambda-mild))
     (lambda-faint      (:foreground lambda-faint))
     (lambda-blue       (:foreground lambda-blue))
     (lambda-green      (:foreground lambda-green))
     (lambda-red        (:foreground lambda-red))
     (lambda-orange     (:foreground lambda-orange))
     (lambda-yellow     (:foreground lambda-yellow))
     (lambda-aqua       (:foreground lambda-aqua))
     (lambda-purple     (:foreground lambda-purple))
     (lambda-gray       (:foreground lambda-gray))

;;;;; Bookmarks
     (bookmark-menu-heading                         (:foreground lambda-strong))
     (bookmark-menu-bookmark                        (:foreground lambda-focus))
     (bookmark-face                                 (:foreground lambda-focus))

;;;;; Childframes
;;;;;; Mini-Frame
     (mini-popup-background (:background lambda-faint))
     (mini-popup-border     (:background lambda-faint))

;;;;;; Mini-Popup (Childframe)
     (mini-popup-background (:background lambda-faint))
     (mini-popup-border     (:background lambda-faint))

;;;;;; Posframe

     (which-key-posframe                           (:background lambda-faint))
     (which-key-posframe-border                    (:background lambda-faint))
     (transient-posframe-border                    (:background lambda-faint))
     (transient-posframe                           (:foreground lambda-strong :background lambda-faint))

;;;;; Completion/Narrowing
;;;;;; Helm
     (helm-M-x-key                              (:foreground lambda-orange))
     (helm-action                               (:foreground lambda-strong :underline t))
     (helm-bookmark-addressbook                 (:foreground lambda-red))
     (helm-bookmark-directory                   (:foreground lambda-purple))
     (helm-bookmark-file                        (:foreground lambda-blue))
     (helm-bookmark-gnus                        (:foreground lambda-purple))
     (helm-bookmark-info                        (:foreground lambda-aqua))
     (helm-bookmark-man                         (:foreground lambda-orange))
     (helm-bookmark-w3m                         (:foreground lambda-yellow))
     (helm-buffer-directory                     (:foreground lambda-white :background lambda-blue))
     (helm-buffer-not-saved                     (:foreground lambda-red))
     (helm-buffer-process                       (:foreground lambda-yellow))
     (helm-buffer-saved-out                     (:foreground lambda-red))
     (helm-buffer-size                          (:foreground lambda-purple))
     (helm-candidate-number                     (:foreground lambda-green))
     (helm-eshell-prompts-buffer-name           (:foreground lambda-green))
     (helm-eshell-prompts-promptidx             (:foreground lambda-aqua))
     (helm-ff-directory                         (:foreground lambda-purple))
     (helm-ff-executable                        (:foreground lambda-aqua))
     (helm-ff-file                              (:foreground lambda-orange))
     (helm-ff-invalid-symlink                   (:foreground lambda-white :background lambda-red))
     (helm-ff-prefix                            (:foreground lambda-black :background lambda-yellow))
     (helm-ff-symlink                           (:foreground lambda-orange))
     (helm-grep-cmd-line                        (:foreground lambda-green))
     (helm-grep-file                            (:foreground lambda-purple))
     (helm-grep-finish                          (:foreground lambda-aqua))
     (helm-grep-lineno                          (:foreground lambda-orange))
     (helm-grep-match                           (:foreground lambda-yellow))
     (helm-grep-running                         (:foreground lambda-red))
     (helm-header                               (:foreground lambda-aqua))
     (helm-helper                               (:foreground lambda-aqua))
     (helm-history-deleted                      (:foreground lambda-black :background lambda-red))
     (helm-history-remote                       (:foreground lambda-red))
     (helm-lisp-completion-info                 (:foreground lambda-orange))
     (helm-lisp-show-completion                 (:foreground lambda-red))
     (helm-locate-finish                        (:foreground lambda-white :background lambda-aqua))
     (helm-match                                (:foreground lambda-orange))
     (helm-moccur-buffer                        (:foreground lambda-aqua :underline t))
     (helm-prefarg                              (:foreground lambda-aqua))
     (helm-selection                            (:foreground lambda-white :background lambda-faint))
     (helm-selection-line                       (:foreground lambda-white :background lambda-faint))
     (helm-separator                            (:foreground lambda-red))
     (helm-source-header                        (:foreground lambda-lowlight))
     (helm-visible-mark                         (:foreground lambda-black :background lambda-lowlight))

;;;;;; Helm-rg
     (helm-rg-preview-line-highlight              (:foreground lambda-black :background lambda-green))
     (helm-rg-base-rg-cmd-face                    (:foreground lambda-highlight))
     (helm-rg-extra-arg-face                      (:foreground lambda-yellow))
     (helm-rg-inactive-arg-face                   (:foreground lambda-aqua))
     (helm-rg-active-arg-face                     (:foreground lambda-green))
     (helm-rg-directory-cmd-face                  (:foreground lambda-orange :background lambda-black))
     (helm-rg-error-message                       (:foreground lambda-red))
     (helm-rg-title-face                          (:foreground lambda-purple))
     (helm-rg-directory-header-face               (:foreground lambda-white :background lambda-mild))
     (helm-rg-file-match-face                     (:foreground lambda-aqua))
     (helm-rg-colon-separator-ripgrep-output-face (:foreground lambda-faint :background lambda-bg))
     (helm-rg-line-number-match-face              (:foreground lambda-orange))
     (helm-rg-match-text-face                     (:foreground lambda-white :background lambda-purple))

;;;;;; Vertico
     (vertico-current                             (:weight 'regular :background lambda-highlight))
     (vertico-group-separator                     (:foreground lambda-lowlight :strike-through t))
     (vertico-multiline                           (:foreground lambda-gray))
     (vertico-group-title                         (:foreground lambda-gray))

;;;;;; Orderless

     (orderless-match-face-0                      (:weight 'bold :foreground lambda-crucial))
     (orderless-match-face-1                      (:weight 'bold :foreground lambda-crucial))
     (orderless-match-face-2                      (:weight 'bold :foreground lambda-crucial))
     (orderless-match-face-3                      (:weight 'bold :foreground lambda-crucial))

;;;;;; Corfu
     (corfu-annotations                           (:foreground lambda-mild))
     (corfu-bar                                   (:foreground lambda-ultralight))
     (corfu-border                                (:foreground lambda-faint))
     (corfu-current                               (:foreground lambda-crucial :background lambda-highlight))
     (corfu-default                               (:inherit 'default :background lambda-faint))
     (corfu-deprecated                            (:foreground lambda-mild))
     (corfu-echo                                  (:inherit 'default))

;;;;;; Company-mode
     (company-scrollbar-bg                        (:background lambda-faint))
     (company-scrollbar-fg                        (:background lambda-mild))
     (company-tooltip                             (:background lambda-mild))
     (company-tooltip-annotation                  (:foreground lambda-green))
     (company-tooltip-annotation-selection        (:inherit 'company-tooltip-annotation))
     (company-tooltip-selection                   (:foreground lambda-purple :background lambda-faint))
     (company-tooltip-common                      (:foreground lambda-blue :underline t))
     (company-tooltip-common-selection            (:foreground lambda-blue :underline t))
     (company-preview-common                      (:foreground lambda-highlight))
     (company-preview                             (:background lambda-blue))
     (company-preview-search                      (:background lambda-aqua))
     (company-template-field                      (:foreground lambda-black :background lambda-yellow))
     (company-echo-common                         (:foreground lambda-red))

;;;;;; Ivy
     (ivy-current-match                           (:foreground lambda-ultralight :weight 'bold :underline t))
     (ivy-minibuffer-match-face-1                 (:foreground lambda-orange))
     (ivy-minibuffer-match-face-2                 (:foreground lambda-yellow))
     (ivy-minibuffer-match-face-3                 (:foreground lambda-orange))
     (ivy-minibuffer-match-face-4                 (:foreground lambda-yellow))

;;;;;; Ido
     (ido-only-match                              (:inherit 'success))
     (ido-first-match                             (:foreground lambda-ultralight :weight 'bold :underline t))
     (ido-subdir                                  (:inherit 'dired-directory))

;;;;;; Consult
     (consult-separator                           (:foreground lambda-gray))
     (consult-file                                (:foreground lambda-focus))
     (consult-line-number                         (:foreground lambda-strong))
     (consult-help                                (:foreground lambda-gray))
     (consult-completing-read-multiple            (:foreground lambda-gray))
     (consult-grep-context                        (:foreground lambda-lowlight))

;;;;;; Selectrum
     (selectrum-current-candidate                   (:weight 'bold :background lambda-highlight))
     (selectrum-prescient-secondary-highlight       (:weight 'bold :foreground lambda-blue))
     (selectrum-prescient-primary-highlight         (:weight 'bold :foreground lambda-focus))
     (selectrum-completion-docsig                   (:slant 'italic :inherit 'selectrum-completion-annotation))
     (selectrum-completion-annotation               (:inherit 'completions-annotations))
     (selectrum-group-separator                     (:strike-through t :inherit 'shadow))
     (selectrum-group-title                         (:slant 'italic :inherit 'shadow))
     (selectrum-quick-keys-match                    (:inherit 'isearch))
     (selectrum-quick-keys-highlight                (:foreground lambda-crucial))

;;;;; Diffs & VC

;;;;;; Diff
     (diff-header                               (:foreground lambda-lowlight))
     (diff-file-header                          (:foreground lambda-fg))
     (diff-hunk-header                          (:background lambda-fg))
     (diff-context                              (:background lambda-mild :foreground lambda-fg))

     (diff-changed                              (:background nil :foreground lambda-blue))
     (diff-refine-changed                       (:foreground lambda-blue))
     (diff-added                                (:background nil :foreground lambda-green))
     (diff-refine-added                         (:background nil :foreground lambda-green))
     (diff-removed                              (:background nil :foreground lambda-red))
     (diff-refine-removed                       (:background nil :foreground lambda-red))

     (diff-indicator-changed                    (:inherit 'diff-changed))
     (diff-indicator-added                      (:inherit 'diff-added))
     (diff-indicator-removed                    (:inherit 'diff-removed))


;;;;;; Git-gutter
     (git-gutter:modified                       (:foreground lambda-blue))
     (git-gutter:added                          (:foreground lambda-green))
     (git-gutter:deleted                        (:foreground lambda-red))

;;;;;; Git-gutter+
     (git-gutter+-modified                      (:foreground lambda-blue))
     (git-gutter+-added                         (:foreground lambda-green))
     (git-gutter+-deleted                       (:foreground lambda-red))

;;;;;; Git-gutter-fringe
     (git-gutter-fr:modified                    (:inherit 'git-gutter:modified))
     (git-gutter-fr:added                       (:inherit 'git-gutter:added))
     (git-gutter-fr:deleted                     (:inherit 'git-gutter:deleted))

;;;;;; Diff-hl
     (diff-hl-change (:slant 'normal :weight 'normal  :foreground lambda-blue))
     (diff-hl-delete (:slant 'normal :weight 'normal  :foreground lambda-red))
     (diff-hl-insert (:slant 'normal :weight 'normal  :foreground lambda-green))

;;;;;; Ediff
     (ediff-even-diff-A                         (:background lambda-mild))
     (ediff-even-diff-B                         (:background lambda-mild))
     (ediff-even-diff-C                         (:background lambda-mild))
     (ediff-even-diff-Ancestor                  (:background lambda-mild))
     (ediff-odd-diff-A                          (:background lambda-faint))
     (ediff-odd-diff-B                          (:background lambda-faint))
     (ediff-odd-diff-C                          (:background lambda-faint))
     (ediff-odd-diff-Ancestor                   (:background lambda-faint))

     ;; TODO: Fix fine diffs
     ;; (ediff-fine-diff-A                         (:background lambda-ediff-fine-diff-A))
     ;; (ediff-fine-diff-Ancestor                  (:background lambda-ediff-fine-diff-Ancestor))
     ;; (ediff-fine-diff-B                         (:background lambda-ediff-fine-diff-B))
     ;; (ediff-fine-diff-C                         (:background lambda-ediff-fine-diff-C))
     ;; (ediff-current-diff-A                      (:background lambda-ediff-current-diff-A))
     ;; (ediff-current-diff-Ancestor               (:background lambda-ediff-current-diff-Ancestor))
     ;; (ediff-current-diff-B                      (:background lambda-ediff-current-diff-B))
     ;; (ediff-current-diff-C                      (:background lambda-ediff-current-diff-C))

     (js2-warning                               (:underline (:color lambda-yellow :style 'wave)))
     (js2-error                                 (:underline (:color lambda-red :style 'wave)))
     (js2-external-variable                     (:underline (:color lambda-aqua :style 'wave)))
     (js2-jsdoc-tag                             (:background nil :foreground lambda-lowlight))
     (js2-jsdoc-type                            (:background nil :foreground lambda-highlight))
     (js2-jsdoc-value                           (:background nil :foreground lambda-lowlight))
     (js2-function-param                        (:background nil :foreground lambda-aqua))
     (js2-function-call                         (:background nil :foreground lambda-blue))
     (js2-instance-member                       (:background nil :foreground lambda-orange))
     (js2-private-member                        (:background nil :foreground lambda-yellow))
     (js2-private-function-call                 (:background nil :foreground lambda-aqua))
     (js2-jsdoc-html-tag-name                   (:background nil :foreground lambda-highlight))
     (js2-jsdoc-html-tag-delimiter              (:background nil :foreground lambda-lowlight))

;;;;;; Magit
     (magit-bisect-bad                          (:foreground lambda-red))
     (magit-bisect-good                         (:foreground lambda-green))
     (magit-bisect-skip                         (:foreground lambda-yellow))
     (magit-blame-heading                       (:foreground lambda-ultralight :background lambda-faint))
     (magit-branch-local                        (:foreground lambda-blue))
     (magit-branch-current                      (:underline lambda-blue :inherit 'magit-branch-local))
     (magit-branch-remote                       (:foreground lambda-green))
     (magit-cherry-equivalent                   (:foreground lambda-purple))
     (magit-cherry-unmatched                    (:foreground lambda-aqua))
     (magit-diff-added                          (:foreground lambda-green))
     (magit-diff-added-highlight                (:foreground lambda-green :inherit 'magit-diff-context-highlight))
     (magit-diff-base                           (:background lambda-yellow :foreground lambda-lowlight))
     (magit-diff-base-highlight                 (:background lambda-yellow :foreground lambda-ultralight))
     (magit-diff-context                        (:foreground lambda-fg))
     (magit-diff-context-highlight              (:background lambda-faint  :foreground lambda-strong))
     (magit-diff-hunk-heading                   (:background lambda-faint  :foreground lambda-strong))
     (magit-diff-hunk-heading-highlight         (:background lambda-faint  :foreground lambda-strong))
     (magit-diff-hunk-heading-selection         (:background lambda-faint  :foreground lambda-orange))
     (magit-diff-lines-heading                  (:background lambda-orange :foreground lambda-strong))
     (magit-diff-removed                        (:foreground lambda-red))
     (magit-diff-removed-highlight              (:foreground lambda-red :inherit 'magit-diff-context-highlight))
     (magit-diffstat-added                      (:foreground lambda-green))
     (magit-diffstat-removed                    (:foreground lambda-red))
     (magit-dimmed                              (:foreground lambda-faint))
     (magit-hash                                (:foreground lambda-blue))
     (magit-log-author                          (:foreground lambda-red))
     (magit-log-date                            (:foreground lambda-aqua))
     (magit-log-graph                           (:foreground lambda-faint))
     (magit-process-ng                          (:foreground lambda-red :weight 'bold))
     (magit-process-ok                          (:foreground lambda-green :weight 'bold))
     (magit-reflog-amend                        (:foreground lambda-purple))
     (magit-reflog-checkout                     (:foreground lambda-blue))
     (magit-reflog-cherry-pick                  (:foreground lambda-green))
     (magit-reflog-commit                       (:foreground lambda-green))
     (magit-reflog-merge                        (:foreground lambda-green))
     (magit-reflog-other                        (:foreground lambda-aqua))
     (magit-reflog-rebase                       (:foreground lambda-purple))
     (magit-reflog-remote                       (:foreground lambda-blue))
     (magit-reflog-reset                        (:foreground lambda-red))
     (magit-refname                             (:foreground lambda-strong))
     (magit-section-heading                     (:foreground lambda-focus :weight 'bold))
     (magit-section-heading-selection           (:foreground lambda-focus))
     (magit-section-highlight                   (:background lambda-mild))
     (magit-sequence-drop                       (:foreground lambda-focus))
     (magit-sequence-head                       (:foreground lambda-aqua))
     (magit-sequence-part                       (:foreground lambda-focus))
     (magit-sequence-stop                       (:foreground lambda-green))
     (magit-signature-bad                       (:foreground lambda-red :weight 'bold))
     (magit-signature-error                     (:foreground lambda-red))
     (magit-signature-expired                   (:foreground lambda-orange))
     (magit-signature-good                      (:foreground lambda-green))
     (magit-signature-revoked                   (:foreground lambda-purple))
     (magit-signature-untrusted                 (:foreground lambda-blue))
     (magit-tag                                 (:foreground lambda-yellow))
     (magit-header-line         (:foreground lambda-fg :background lambda-faint
                                 :box (:line-width 3 :color lambda-faint :style nil) :overline nil :underline nil))
     (magit-header-line-log-select (:foreground lambda-fg :background lambda-faint
                                    :box (:line-width 3) :color lambda-faint :style nil :overline nil :underline nil))

;;;;; Directories
;;;;;; All The Icons Dired
     (all-the-icons-dired-dir-face              (:foreground lambda-focus))

;;;;;; dired+
     (diredp-file-name                          (:foreground lambda-strong))
     (diredp-file-suffix                        (:foreground lambda-lowlight))
     (diredp-compressed-file-suffix             (:foreground lambda-blue))
     (diredp-dir-name                           (:foreground lambda-blue))
     (diredp-dir-heading                        (:foreground lambda-blue))
     (diredp-symlink                            (:foreground lambda-orange))
     (diredp-date-time                          (:foreground lambda-lowlight))
     (diredp-number                             (:foreground lambda-blue))
     (diredp-no-priv                            (:foreground lambda-faint))
     (diredp-other-priv                         (:foreground lambda-faint))
     (diredp-rare-priv                          (:foreground lambda-faint))
     (diredp-ignored-file-name                  (:foreground lambda-faint))

     (diredp-dir-priv                           (:foreground lambda-blue  :background lambda-blue))
     (diredp-exec-priv                          (:foreground lambda-blue  :background lambda-blue))
     (diredp-link-priv                          (:foreground lambda-aqua  :background lambda-aqua))
     (diredp-read-priv                          (:foreground lambda-red  :background lambda-red))
     (diredp-write-priv                         (:foreground lambda-aqua :background lambda-aqua))

;;;;;; Dired Colors (Diredfl)
     (diredfl-write-priv                            (:foreground lambda-urgent))
     (diredfl-tagged-autofile-name                  (:foreground lambda-bg))
     (diredfl-symlink                               (:foreground lambda-urgent))
     (diredfl-read-priv                             (:foreground lambda-urgent))
     (diredfl-rare-priv                             (:foreground lambda-urgent :background lambda-urgent))
     (diredfl-other-priv                            (:background lambda-red))
     (diredfl-omit-file-name                        (:strike-through lambda-mild :foreground lambda-mild))
     (diredfl-number                                (:foreground lambda-focus))
     (diredfl-no-priv                               (:foreground lambda-urgent))
     (diredfl-mode-line-flagged                     (:foreground lambda-urgent))
     (diredfl-mode-line-marked                      (:foreground lambda-focus))
     (diredfl-link-priv                             (:foreground lambda-urgent))
     (diredfl-ignored-file-name                     (:foreground lambda-mild))
     (diredfl-flag-mark-line                        (:foreground lambda-urgent))
     (diredfl-flag-mark                             (:foreground lambda-urgent :background lambda-focus))
     (diredfl-file-suffix                           (:foreground lambda-lowlight))
     (diredfl-file-name                             (:foreground lambda-fg))
     (diredfl-executable-tag                        (:foreground lambda-urgent))
     (diredfl-exec-priv                             (:foreground lambda-urgent))
     (diredfl-dir-priv                              (:foreground lambda-mild))
     (diredfl-dir-name                              (:foreground lambda-green))
     (diredfl-dir-heading                           (:inherit 'variable-pitch :foreground lambda-blue :background lambda-faint))
     (diredfl-deletion-file-name                    (:foreground lambda-urgent))
     (diredfl-deletion                              (:foreground lambda-urgent :background lambda-urgent))
     (diredfl-date-time                             (:foreground lambda-green))
     (diredfl-compressed-file-suffix                (:foreground lambda-mild))
     (diredfl-compressed-file-name                  (:foreground lambda-bg))
     (diredfl-autofile-name                         (:background lambda-faint))


;;;;; Editing

;;;;;; Flycheck
     (flycheck-warning                          (:underline (:style 'wave :color lambda-yellow)))
     (flycheck-error                            (:underline (:style 'wave :color lambda-red)))
     (flycheck-info                             (:underline (:style 'wave :color lambda-blue)))
     (flycheck-fringe-warning                   (:foreground lambda-yellow))
     (flycheck-fringe-error                     (:foreground lambda-red))
     (flycheck-fringe-info                      (:foreground lambda-blue))
     (flycheck-error-list-warning               (:foreground lambda-yellow :bold t))
     (flycheck-error-list-error                 (:foreground lambda-red :bold t))
     (flycheck-error-list-info                  (:foreground lambda-blue :bold t))

;;;;;; Flyspell
     (flyspell-duplicate                        (:underline (:color lambda-red :style 'line)))
     (flyspell-incorrect                        (:underline (:color lambda-red :style 'line)))

;;;;;; Highlight indentation mode
     (highlight-indentation-current-column-face (:background lambda-faint))
     (highlight-indentation-face                (:background lambda-mild))




;;;;;; Hi-lock-mode
     (hi-black-b                                (:foreground lambda-black :weight 'bold))
     (hi-black-hb                               (:foreground lambda-black :weight 'bold :height 1.5))
     (hi-blue                                   (:foreground lambda-faint :background lambda-blue))
     (hi-blue-b                                 (:foreground lambda-blue :weight 'bold))
     (hi-green                                  (:foreground lambda-faint :background lambda-green))
     (hi-green-b                                (:foreground lambda-green :weight 'bold))
     (hi-pink                                   (:foreground lambda-faint :background lambda-purple))
     (hi-red-b                                  (:foreground lambda-red :weight 'bold))
     (hi-yellow                                 (:foreground lambda-faint :background lambda-yellow))

;;;;;; Line numbers
     (line-number                               (:foreground lambda-lowlight))
     (line-number-current-line                  (:foreground lambda-orange :background lambda-faint))
     (linum                                     (:foreground lambda-faint :background lambda-mild))
     (linum-highlight-face                      (:foreground lambda-orange :background lambda-faint))
     (linum-relative-current-face               (:foreground lambda-orange :background lambda-faint))

;;;;;; Undo-tree
     (undo-tree-visualizer-active-branch-face   (:foreground lambda-ultralight))
     (undo-tree-visualizer-current-face         (:foreground lambda-red))
     (undo-tree-visualizer-default-face         (:foreground lambda-faint))
     (undo-tree-visualizer-register-face        (:foreground lambda-yellow))
     (undo-tree-visualizer-unmodified-face      (:foreground lambda-aqua))

;;;;;; Whitespace-mode

     (whitespace-space                          (:background lambda-bg :foreground lambda-faint))
     (whitespace-hspace                         (:background lambda-bg :foreground lambda-faint))
     (whitespace-tab                            (:background lambda-bg :foreground lambda-faint))
     (whitespace-newline                        (:background lambda-bg :foreground lambda-faint))
     (whitespace-trailing                       (:background lambda-mild :foreground lambda-red))
     (whitespace-line                           (:background lambda-mild :foreground lambda-red))
     (whitespace-space-before-tab               (:background lambda-bg :foreground lambda-faint))
     (whitespace-indentation                    (:background lambda-bg :foreground lambda-faint))
     (whitespace-empty                          (:background nil :foreground nil))
     (whitespace-space-after-tab                (:background lambda-bg :foreground lambda-faint))

;;;;; Programming

;;;;;; Rainbow Delimiters

     (rainbow-delimiters-depth-1-face           (:foreground lambda-purple))
     (rainbow-delimiters-depth-2-face           (:foreground lambda-green))
     (rainbow-delimiters-depth-3-face           (:foreground lambda-red))
     (rainbow-delimiters-depth-4-face           (:foreground lambda-blue))
     (rainbow-delimiters-depth-5-face           (:foreground lambda-purple))
     (rainbow-delimiters-depth-6-face           (:foreground lambda-green))
     (rainbow-delimiters-depth-7-face           (:foreground lambda-red))
     (rainbow-delimiters-depth-8-face           (:foreground lambda-blue))
     (rainbow-delimiters-depth-9-face           (:foreground lambda-purple))
     (rainbow-delimiters-depth-10-face          (:foreground lambda-green))
     (rainbow-delimiters-depth-11-face          (:foreground lambda-red))
     (rainbow-delimiters-depth-12-face          (:foreground lambda-blue))
     (rainbow-delimiters-unmatched-face         (:background lambda-bg :foreground lambda-red :weight 'bold))

;;;;;; Langtool
     (langtool-errline                          (:foreground lambda-faint :background lambda-red))
     (langtool-correction-face                  (:foreground lambda-yellow :weight 'bold))

;;;;;; Smartparens
     (sp-pair-overlay-face                      (:background lambda-faint))
     (sp-show-pair-match-face                   (:background lambda-faint)) ;; Pair tags highlight
     (sp-show-pair-mismatch-face                (:background lambda-red)) ;; Highlight for bracket without pair
     ;;(sp-wrap-overlay-face                     (:inherit 'sp-wrap-overlay-face))
     ;;(sp-wrap-tag-overlay-face                 (:inherit 'sp-wrap-overlay-face))

;;;;;; Cider
     (cider-debug-code-overlay-face             (:background lambda-faint :foreground lambda-ultralight))
     (cider-deprecated-face                     (:background lambda-faint :foreground lambda-orange))
     (cider-enlightened-local-face              (:foreground lambda-orange :weight 'bold))
     (cider-error-highlight-face                (:foreground lambda-red :underline t :style 'wave))
     (cider-fringe-good-face                    (:foreground lambda-green))
     (cider-instrumented-face                   (:background lambda-mild :box (:line-width -1 :color lambda-red)))
     (cider-result-overlay-face                 (:background lambda-faint :box (:line-width -1 :color lambda-yellow)))
     (cider-test-error-face                     (:background lambda-red))
     (cider-test-error-face                     (:background lambda-orange))
     (cider-test-success-face                   (:background lambda-green))
     (cider-traced                              (:background lambda-aqua))
     (cider-warning-highlight-face              (:foreground lambda-yellow :underline t :style 'wave))

;;;;;; Latex
     (font-latex-bold-face                      (:foreground lambda-green :bold t))
     (font-latex-italic-face                    (:foreground lambda-green :underline t))
     (font-latex-math-face                      (:foreground lambda-strong))
     (font-latex-script-char-face               (:foreground lambda-aqua))
     (font-latex-sectioning-5-face              (:foreground lambda-yellow :bold t))
     (font-latex-sedate-face                    (:foreground lambda-strong))
     (font-latex-string-face                    (:foreground lambda-orange))
     (font-latex-verbatim-face                  (:foreground lambda-strong))
     (font-latex-warning-face                   (:foreground lambda-red :weight 'bold))
     (preview-face                              (:background lambda-mild))

;;;;;; Lsp
     (lsp-lsp-flycheck-warning-unnecessary-face (:underline (:color lambda-orange :style 'wave)
                                                 :foreground lambda-urgent))
     (lsp-ui-doc-background                     (:background lambda-mild))
     (lsp-ui-doc-header                         (:background lambda-blue))
     (lsp-ui-peek-filename                      (:foreground lambda-red))
     (lsp-ui-sideline-code-action               (:foreground lambda-yellow))
     (lsp-ui-sideline-current-symbol            (:foreground lambda-aqua))
     (lsp-ui-sideline-symbol                    (:foreground lambda-faint))

;;;;;; Web-mode
     (web-mode-doctype-face          (:foreground lambda-blue))
     (web-mode-html-tag-bracket-face (:foreground lambda-blue))
     (web-mode-html-tag-face         (:foreground lambda-blue))
     (web-mode-html-attr-name-face   (:foreground lambda-yellow))
     (web-mode-html-attr-equal-face  (:foreground lambda-yellow))
     (web-mode-html-attr-value-face  (:foreground lambda-green))


;;;;; UI (Frames, Windows, Buffers)

;;;;;; Ace-jump-mode
     (ace-jump-face-background                  (:foreground lambda-lowlight :background lambda-bg :inverse-video nil))
     (ace-jump-face-foreground                  (:foreground lambda-red :background lambda-bg :inverse-video nil))

;;;;;; Ace-window
     (aw-background-face                        (:foreground lambda-lowlight :background lambda-bg :inverse-video nil))
     (aw-leading-char-face                      (:foreground lambda-red :background lambda-bg :height 4.0))

;;;;;; Buttons
     (custom-button                                 (:foreground lambda-fg :background lambda-highlight :box nil))
     (custom-button-mouse                           (:foreground lambda-fg :background lambda-mild :box nil))
     (custom-button-pressed                         (:foreground lambda-bg :background lambda-fg :box nil))

;;;;;; Customize faces

     (custom-group-subtitle                         (:foreground lambda-fg :bold t))
     (custom-group-tag                              (:foreground lambda-fg :bold t))
     (custom-group-tag-1                            (:foreground lambda-fg :bold t))
     (custom-comment                                (:foreground lambda-mild))
     (custom-comment-tag                            (:foreground lambda-mild))
     (custom-changed                                (:foreground lambda-focus))
     (custom-modified                               (:foreground lambda-focus))
     (custom-face-tag                               (:foreground lambda-fg :bold t))
     (custom-variable-tag                           (:foreground lambda-fg :bold t))
     (custom-invalid                                (:foreground lambda-crucial))
     (custom-visibility                             (:foreground lambda-focus))
     (custom-state                                  (:foreground lambda-focus))
     (custom-link                                   (:foreground lambda-focus))
     (custom-button                                 (:foreground lambda-mild :background lambda-bg :box (:line-width 1 :color lambda-mild :style nil)))
     (custom-button-mouse                           (:foreground lambda-mild :background lambda-faint :box (:line-width 1 :color lambda-mild :style nil)))
     (custom-button-pressed                         (:foreground lambda-fg :background lambda-focus :inverse-video nil :box (:line-width 1 :color lambda-focus :style nil)))


;;;;;; Elscreen
     (elscreen-tab-background-face              (:background lambda-bg :box nil)) ;; Tab bar, not the tabs
     (elscreen-tab-control-face                 (:background lambda-faint :foreground lambda-red :underline nil :box nil)) ;; The controls
     (elscreen-tab-current-screen-face          (:background lambda-faint :foreground lambda-strong :box nil)) ;; Current tab
     (elscreen-tab-other-screen-face            (:background lambda-faint :foreground lambda-lowlight :underline nil :box nil)) ;; Inactive tab


;;;;;; Highlight-Indentation
     (highlight-indentation-face (:inherit lambda-highlight))
     (highlight-indentation-current-column-face (:background lambda-yellow))

;;;;;; Highlight Indentation Guides
     (highlight-indent-guides-stack-odd-face        (:foreground lambda-orange))
     (highlight-indent-guides-stack-even-face       (:foreground lambda-yellow))
     (highlight-indent-guides-top-odd-face          (:foreground lambda-orange))
     (highlight-indent-guides-top-even-face         (:foreground lambda-yellow))
     (highlight-indent-guides-odd-face              (:foreground lambda-orange))
     (highlight-indent-guides-even-face             (:foreground lambda-yellow))
     (highlight-indent-guides-character-face        (:foreground lambda-highlight))
     (highlight-indent-guides-top-character-face    (:foreground lambda-highlight))
     (highlight-indent-guides-stack-character-face  (:foreground lambda-highlight))


;;;;;; Popup
     (popup-face                                (:underline nil :foreground lambda-highlight :background lambda-mild))
     (popup-menu-mouse-face                     (:underline nil :foreground lambda-white :background lambda-green))
     (popup-menu-selection-face                 (:underline nil :foreground lambda-white :background lambda-green))
     (popup-tip-face                            (:underline nil :foreground lambda-lowlight :background lambda-faint))

;;;;;; Splash Faces

     (lem-splash-menu-face     (:foreground lambda-purple :weight 'light))
     (lem-splash-footer-face   (:foreground lambda-gray))
     (lem-splash-image-face    (:foreground lambda-gray))

;;;;;; Tabbar
     (tabbar-default                             (:foreground lambda-ultralight :background lambda-mild :bold nil :height 1.0 :box (:line-width -5 :color lambda-mild)))
     (tabbar-separator                           (:foreground lambda-ultralight :background lambda-mild))
     (tabbar-highlight                           (:inherit 'highlight))
     (tabbar-button                              (:foreground lambda-mild :background lambda-mild :box nil :line-width 0))
     (tabbar-button-highlight                    (:inherit 'tabbar-button :inverse-video t))
     (tabbar-modified                            (:foreground lambda-green :background lambda-mild :box (:line-width -5 :color lambda-mild)))
     (tabbar-unselected                          (:inherit 'tabbar-default))
     (tabbar-unselected-modified                 (:inherit 'tabbar-modified))
     (tabbar-selected                            (:inherit 'tabbar-default :foreground lambda-yellow))
     (tabbar-selected-modified                   (:inherit 'tabbar-selected))

;;;;;; Tab-bar
     (tab-bar-tab-inactive (:background lambda-bg :foreground lambda-ultralight))
     (tab-bar-tab (:background lambda-faint :foreground lambda-ultralight))
     (tab-bar (:background lambda-bg :foreground lambda-ultralight))

;;;;;; Tab-bar Echo
     (tab-bar-echo-area-tab                      (:foreground lambda-strong :underline t :weight 'bold))
     (tab-bar-echo-area-tab-group-current        (:foreground lambda-strong))
     (tab-bar-echo-area-tab-ungrouped            (:foreground lambda-strong :weight 'light))

;;;;;; Tool tips
     (tooltip                                   (:foreground lambda-highlight :background lambda-mild))

;;;;;; Widget faces
     (widget-button-pressed-face                (:foreground lambda-red))
     (widget-documentation-face                 (:foreground lambda-green))
     (widget-field                              (:background lambda-faint))
     (widget-button                             (:foreground lambda-fg :bold t))
     (widget-single-line-field                  (:background lambda-faint))

;;;;;; Window Divs
     ;; divide windows more attractively
     (window-divider                               (:foreground lambda-bg))
     (window-divider-first-pixel                   (:foreground lambda-bg))
     (window-divider-last-pixel                    (:foreground lambda-bg))
     ;; divide windows better in terminal
     ;; see https://www.reddit.com/r/emacs/comments/3u0d0u/how_do_i_make_the_vertical_window_divider_more/
     ;; (when (not (display-graphic-p))
     ;;   (set-face-background 'vertical-border lambda-bg)
     ;;   (set-face-foreground 'vertical-border (face-background 'vertical-border)))

;;;;; Help, Info, & Menus

;;;;;; Help(ful)

     (helpful-heading (:inherit'variable-pitch :foreground lambda-blue))

;;;;;; Hydra
     (hydra-face-red (:foreground lambda-red :weight 'bold))
     (hydra-face-blue (:foreground lambda-blue :weight 'bold))
     (hydra-face-amaranth (:foreground lambda-yellow :weight 'bold))
     (hydra-face-pink (:foreground lambda-purple :weight 'bold))
     (hydra-face-teal (:foreground lambda-aqua :weight 'bold))

;;;;;; Imenu List
     (imenu-list-entry-face-0                       (:inherit 'imenu-list-entry-face :foreground lambda-mild))
     (imenu-list-entry-face-1                       (:inherit 'imenu-list-entry-face :foreground lambda-mild))
     (imenu-list-entry-face-2                       (:inherit 'imenu-list-entry-face :foreground lambda-mild))
     (imenu-list-entry-face-3                       (:inherit 'imenu-list-entry-face :foreground lambda-mild))

;;;;;; Info (Documentation)
     (info-menu-header                              (:foreground lambda-strong))
     (info-header-node                              (:foreground lambda-green))
     (info-index-match                              (:foreground lambda-focus))
     (Info-quoted                                   (:foreground lambda-mild))
     (info-title-1                                  (:foreground lambda-strong))
     (info-title-2                                  (:foreground lambda-strong))
     (info-title-3                                  (:foreground lambda-strong))
     (info-title-4                                  (:foreground lambda-strong))

;;;;;; Marginalia
     (marginalia-documentation                  (:italic t :foreground lambda-strong))

;;;;;; Message-mode
     (message-header-to                         (:inherit 'font-lock-variable-name-face))
     (message-header-cc                         (:inherit 'font-lock-variable-name-face))
     (message-header-subject                    (:foreground lambda-orange :weight 'bold))
     (message-header-newsgroups                 (:foreground lambda-yellow :weight 'bold))
     (message-header-other                      (:inherit 'font-lock-variable-name-face))
     (message-header-name                       (:inherit 'font-lock-keyword-face))
     (message-header-xheader                    (:foreground lambda-blue))
     (message-separator                         (:inherit 'font-lock-comment-face))
     (message-cited-text                        (:inherit 'font-lock-comment-face))
     (message-mml                               (:foreground lambda-green :weight 'bold))

;;;;;; Which-function-mode
     (which-func                                 (:foreground lambda-blue))

;;;;; Built-in syntax

     (font-lock-builtin-face                            (:foreground lambda-strong :weight 'light))
     (font-lock-constant-face                           (:foreground lambda-strong))
     (font-lock-comment-face                            (:foreground lambda-gray :slant 'italic :weight 'light))
     (font-lock-function-name-face                      (:foreground lambda-strong :weight 'bold))
     (font-lock-keyword-face                            (:foreground lambda-strong :weight 'light))
     (font-lock-string-face                             (:foreground lambda-strong :background lambda-faint))
     (font-lock-variable-name-face                      (:foreground lambda-strong))
     (font-lock-type-face                               (:foreground lambda-strong))
     (font-lock-warning-face                            (:foreground lambda-urgent :weight 'bold))

;;;;; Basic faces
     (error                                             (:foreground lambda-urgent :bold t))
     (success                                           (:foreground lambda-green :bold t))
     (warning                                           (:foreground lambda-crucial :bold t))
     (alert-low-face                                    (:foreground lambda-orange))
     (trailing-whitespace                               (:background lambda-red))
     (escape-glyph                                      (:foreground lambda-aqua))
     (highlight                                         (:background lambda-highlight))
     (homoglyph                                         (:foreground lambda-focus))
     (match                                             (:foreground lambda-lowlight :background lambda-focus))

;;;;; Writing
;;;;; Outline
     (outline-minor-0      (:background lambda-ultralight))
     (outline-1            (:inherit 'variable-pitch :foreground lambda-green))
     (outline-2            (:inherit 'variable-pitch :foreground lambda-blue))
     (outline-3            (:inherit 'variable-pitch :foreground lambda-red))
     (outline-4            (:inherit 'variable-pitch :foreground lambda-purple))
     (outline-5            (:inherit 'outline-1))
     (outline-6            (:inherit 'outline-2))
     (outline-7            (:inherit 'outline-3))
     (outline-8            (:inherit 'outline-4))

;;;;; Markdown-mode
     (markdown-header-face-1                    (:inherit 'outline-1))
     (markdown-header-face-2                    (:inherit 'outline-2))
     (markdown-header-face-3                    (:inherit 'outline-3))
     (markdown-header-face-4                    (:inherit 'outline-4))
     (markdown-header-face-5                    (:inherit 'outline-5))
     (markdown-header-face-6                    (:inherit 'outline-6))

;;;;; Org-mode
     (org-hide                                  (:foreground lambda-faint))
     (org-level-1                               (:inherit 'outline-1))
     (org-level-2                               (:inherit 'outline-2))
     (org-level-3                               (:inherit 'outline-3))
     (org-level-4                               (:inherit 'outline-4))
     (org-level-5                               (:inherit 'outline-5))
     (org-level-6                               (:inherit 'outline-6))
     (org-level-7                               (:inherit 'outline-7))
     (org-level-8                               (:inherit 'outline-8))
     (org-special-keyword                       (:inherit 'font-lock-comment-face))
     (org-drawer                                (:inherit 'font-lock-function-name-face))
     (org-column                                (:background lambda-faint))
     (org-column-title                          (:background lambda-faint :underline t :weight 'bold))
     (org-warning                               (:foreground lambda-red :weight 'bold :underline nil :bold t))
     (org-archived                              (:foreground lambda-ultralight :weight 'bold))
     (org-link                                  (:foreground lambda-aqua :underline t))
     (org-footnote                              (:foreground lambda-aqua :underline t))
     (org-ellipsis                              (:foreground lambda-lowlight))
     (org-date                                  (:foreground lambda-blue :underline t))
     (org-sexp-date                             (:foreground lambda-blue :underline t))
     (org-tag                                   (:bold t :weight 'bold))
     (org-list-dt                               (:bold t :weight 'bold))
     (org-todo                                  (:foreground lambda-red :weight 'bold :bold t))
     (org-done                                  (:foreground lambda-aqua :weight 'bold :bold t))
     (org-agenda-done                           (:foreground lambda-aqua))
     (org-headline-done                         (:foreground lambda-aqua))
     (org-table                                 (:foreground lambda-blue))
     (org-block                                 (:background lambda-faint))
     (org-block-begin-line                      (:background lambda-mild))
     (org-block-end-line                        (:background lambda-mild))
     (org-formula                               (:foreground lambda-yellow))
     (org-document-title                        (:foreground lambda-blue))
     (org-document-info                         (:foreground lambda-blue))
     (org-agenda-structure                      (:inherit 'font-lock-comment-face))
     (org-agenda-date-today                     (:foreground lambda-ultralight :weight 'bold :italic t))
     (org-scheduled                             (:foreground lambda-yellow))
     (org-scheduled-today                       (:foreground lambda-blue))
     (org-scheduled-previously                  (:foreground lambda-red))
     (org-upcoming-deadline                     (:inherit 'font-lock-keyword-face))
     (org-deadline-announce                     (:foreground lambda-red))
     (org-time-grid                             (:foreground lambda-orange))
     (org-latex-and-related                     (:foreground lambda-blue))

;;;;; Org-habit
     (org-habit-clear-face                      (:background lambda-blue))
     (org-habit-clear-future-face               (:background lambda-blue))
     (org-habit-ready-face                      (:background lambda-green))
     (org-habit-ready-future-face               (:background lambda-green))
     (org-habit-alert-face                      (:background lambda-yellow))
     (org-habit-alert-future-face               (:background lambda-yellow))
     (org-habit-overdue-face                    (:background lambda-red))
     (org-habit-overdue-future-face             (:background lambda-red))


;;;;; term
     (term-color-black                          (:foreground lambda-faint :background lambda-mild))
     (term-color-blue                           (:foreground lambda-blue :background lambda-blue))
     (term-color-cyan                           (:foreground lambda-aqua :background lambda-aqua))
     (term-color-green                          (:foreground lambda-green :background lambda-green))
     (term-color-magenta                        (:foreground lambda-purple :background lambda-purple))
     (term-color-red                            (:foreground lambda-red :background lambda-red))
     (term-color-white                          (:foreground lambda-highlight :background lambda-highlight))
     (term-color-yellow                         (:foreground lambda-yellow :background lambda-yellow))
     (term-default-fg-color                     (:foreground lambda-ultralight))
     (term-default-bg-color                     (:background lambda-bg))

;;;;; ag (The Silver Searcher)
     (ag-hit-face                               (:foreground lambda-blue))
     (ag-match-face                             (:foreground lambda-red))

;;;;; elfeed
     (elfeed-search-title-face                  (:foreground lambda-lowlight  ))
     (elfeed-search-unread-title-face           (:foreground lambda-ultralight))
     (elfeed-search-date-face                   (:inherit 'font-lock-builtin-face :underline t))
     (elfeed-search-feed-face                   (:inherit 'font-lock-variable-name-face))
     (elfeed-search-tag-face                    (:inherit 'font-lock-keyword-face))
     (elfeed-search-last-update-face            (:inherit 'font-lock-comment-face))
     (elfeed-search-unread-count-face           (:inherit 'font-lock-comment-face))
     (elfeed-search-filter-face                 (:inherit 'font-lock-string-face))

;;;;; isearch
     (isearch                                   (:foreground lambda-black :background lambda-orange))
     (lazy-highlight                            (:foreground lambda-black :background lambda-yellow))
     (isearch-fail                              (:foreground lambda-ultralight :background lambda-red))

;;;;; anzu-mode
     (anzu-mode-line                            (:foreground lambda-yellow :weight 'bold))
     (anzu-match-1                              (:background lambda-green))
     (anzu-match-2                              (:background lambda-yellow))
     (anzu-match-3                              (:background lambda-aqua))
     (anzu-replace-to                           (:foreground lambda-yellow))
     (anzu-replace-highlight                    (:inherit 'isearch))

;;;;; mu4e
     (mu4e-header-key-face                      (:foreground lambda-green :weight 'bold ))
     (mu4e-unread-face                          (:foreground lambda-blue :weight 'bold ))
     (mu4e-highlight-face                       (:foreground lambda-green))

;;;;; shell script
     (sh-quoted-exec                            (:foreground lambda-purple))
     (sh-heredoc                                (:foreground lambda-orange))

;;;;; neotree
     (neo-banner-face                           (:foreground lambda-purple :bold t))
     (neo-dir-link-face                         (:foreground lambda-yellow))
     (neo-expand-btn-face                       (:foreground lambda-orange))
     (neo-file-link-face                        (:foreground lambda-ultralight))
     (neo-header-face                           (:foreground lambda-purple))
     (neo-root-dir-face                         (:foreground lambda-purple :bold t))

;;;;; eshell
     (eshell-prompt                              (:foreground lambda-aqua))
     (eshell-ls-archive                          (:foreground lambda-highlight))
     (eshell-ls-backup                           (:foreground lambda-lowlight))
     (eshell-ls-clutter                          (:foreground lambda-orange :weight 'bold))
     (eshell-ls-directory                        (:foreground lambda-yellow))
     (eshell-ls-executable                       (:weight 'bold))
     (eshell-ls-missing                          (:foreground lambda-red :bold t))
     (eshell-ls-product                          (:foreground lambda-red))
     (eshell-ls-readonly                         (:foreground lambda-highlight))
     (eshell-ls-special                          (:foreground lambda-yellow :bold t))
     (eshell-ls-symlink                          (:foreground lambda-red))
     (eshell-ls-unreadable                       (:foreground lambda-red :bold t))

;;;;; wgrep
     (wgrep-delete-face                          (:strike-through lambda-red))
     (wgrep-done-face                            (:foreground lambda-aqua))
     (wgrep-face                                 (:underline (:color lambda-yellow :style 'line)))
     (wgrep-file-face                            (:inherit 'highlight))
     (wgrep-reject-face                          (:foreground lambda-red :bold t))

;;;;; circe
     (circe-prompt-face               (:foreground lambda-aqua))
     (circe-fool                      (:foreground lambda-faint))
     (circe-highlight-nick-face       (:foreground lambda-yellow))
     (circe-server-face               (:foreground lambda-faint))
     (circe-my-message-face           (:foreground lambda-aqua))
     (lui-time-stamp-face             (:foreground lambda-blue))

;;;;; erc
     (erc-action-face            (:inherit 'erc-default-face))
     (erc-bold-face              (:weight 'bold))
     (erc-current-nick-face      (:foreground lambda-aqua))
     (erc-dangerous-host-face    (:inherit 'font-lock-warning-face))
     (erc-default-face           (:inherit 'default))
     (erc-direct-msg-face        (:inherit 'erc-default-face))
     (erc-error-face             (:inherit 'font-lock-warning-face))
     (erc-fool-face              (:inherit 'erc-default-face))
     (erc-input-face             (:foreground lambda-aqua))
     (erc-my-nick-face           (:foreground lambda-aqua))
     (erc-nick-msg-face          (:inherit 'erc-default-face))
     (erc-notice-face            (:foreground lambda-faint))
     (erc-timestamp-face         (:foreground lambda-green))
     (erc-underline-face         (:underline t))
     (erc-prompt-face            (:foreground lambda-aqua))
     (erc-pal-face               (:foreground lambda-yellow :weight 'bold))
     (erc-keyword-face           (:foreground lambda-orange :weight 'bold))
     (erc-nick-default-face      (:weight 'regular))
     (erc-button                 (:weight 'bold  :underline t))

;;;;; gnus
     (gnus-group-mail-1           (:weight 'bold :foreground lambda-ultralight))
     (gnus-group-mail-2           (:inherit 'gnus-group-mail-1))
     (gnus-group-mail-3           (:inherit 'gnus-group-mail-1))
     (gnus-group-mail-1-empty     (:foreground lambda-faint))
     (gnus-group-mail-2-empty     (:inherit 'gnus-group-mail-1-empty))
     (gnus-group-mail-3-empty     (:inherit 'gnus-group-mail-1-empty))
     (gnus-group-news-1           (:inherit 'gnus-group-mail-1))
     (gnus-group-news-2           (:inherit 'gnus-group-news-1))
     (gnus-group-news-3           (:inherit 'gnus-group-news-1))
     (gnus-group-news-4           (:inherit 'gnus-group-news-1))
     (gnus-group-news-5           (:inherit 'gnus-group-news-1))
     (gnus-group-news-6           (:inherit 'gnus-group-news-1))
     (gnus-group-news-1-empty     (:inherit 'gnus-group-mail-1-empty))
     (gnus-group-news-2-empty     (:inherit 'gnus-group-news-1-empty))
     (gnus-group-news-3-empty     (:inherit 'gnus-group-news-1-empty))
     (gnus-group-news-4-empty     (:inherit 'gnus-group-news-1-empty))
     (gnus-group-news-5-empty     (:inherit 'gnus-group-news-1-empty))
     (gnus-group-news-6-empty     (:inherit 'gnus-group-news-1-empty))
     (gnus-group-mail-low         (:inherit 'gnus-group-mail-1 :weight 'normal))
     (gnus-group-mail-low-empty   (:inherit 'gnus-group-mail-1-empty))
     (gnus-group-news-low         (:inherit 'gnus-group-mail-1 :foreground lambda-faint))
     (gnus-group-news-low-empty   (:inherit 'gnus-group-news-low :weight 'normal))
     (gnus-header-content         (:inherit 'message-header-other))
     (gnus-header-from            (:inherit 'message-header-other))
     (gnus-header-name            (:inherit 'message-header-name))
     (gnus-header-newsgroups      (:inherit 'message-header-other))
     (gnus-header-subject         (:inherit 'message-header-subject))
     (gnus-summary-cancelled      (:foreground lambda-red :strike-through t))
     (gnus-summary-normal-ancient (:foreground lambda-faint :inherit 'italic))
     (gnus-summary-normal-read    (:foreground lambda-ultralight))
     (gnus-summary-normal-ticked  (:foreground lambda-purple))
     (gnus-summary-normal-unread  (:foreground lambda-green :inherit 'bold))
     (gnus-summary-selected       (:foreground lambda-blue :weight 'bold))
     (gnus-cite-1                 (:foreground lambda-purple))
     (gnus-cite-2                 (:foreground lambda-purple))
     (gnus-cite-3                 (:foreground lambda-purple))
     (gnus-cite-4                 (:foreground lambda-green))
     (gnus-cite-5                 (:foreground lambda-green))
     (gnus-cite-6                 (:foreground lambda-green))
     (gnus-cite-7                 (:foreground lambda-purple))
     (gnus-cite-8                 (:foreground lambda-purple))
     (gnus-cite-9                 (:foreground lambda-purple))
     (gnus-cite-10                (:foreground lambda-orange))
     (gnus-cite-11                (:foreground lambda-orange))
     (gnus-signature              (:foreground lambda-orange))
     (gnus-x-face                 (:background lambda-faint :foreground lambda-ultralight))

;;;;; Coq
     (coq-solve-tactics-face      (:inherit 'font-lock-constant-face))
     (coq-cheat-face              (:box (:line-width -1 :color lambda-red :style nil)
                                   :foreground lambda-red))
     (coq-button-face             (:background lambda-bg))
     (coq-button-face-active      (:background lambda-mild))
     (coq-button-face-pressed     (:background lambda-bg))

;;;;; Proof General
     (proof-active-area-face      (:underline t))
     (proof-tacticals-name-face   (:inherit 'font-lock-constant-face))
     (proof-tactics-name-face     (:inherit 'font-lock-constant-face))
     (proof-locked-face           (:background lambda-mild))
     (proof-queue-face            (:background lambda-faint))
     (proof-warning-face          (:background lambda-red))
     (proof-error-face            (:background lambda-bg :foreground lambda-red))

;;;;; ledger-mode
     (ledger-font-xact-highlight-face  (:background lambda-mild))

;;;;; Modeline/Headerline
;;;;;; Basic Modeline/Headerline
     (header-line (:foreground lambda-fg :background lambda-faint))
     (mode-line   (:background lambda-faint :height .1))
     (mode-line-inactive (:background lambda-ultralight))

;;;;;; Smart-mode-line
     (sml/global                                (:foreground lambda-strong :inverse-video nil))
     (sml/modes                                 (:foreground lambda-green))
     (sml/filename                              (:foreground lambda-red :weight 'bold))
     (sml/prefix                                (:foreground lambda-ultralight))
     (sml/read-only                             (:foreground lambda-blue))
     (persp-selected-face                       (:foreground lambda-orange))

;;;;;; Powerline
     (powerline-active0                         (:background lambda-faint :foreground lambda-ultralight))
     (powerline-active1                         (:background lambda-strong :foreground lambda-ultralight))
     (powerline-active2                         (:background lambda-faint :foreground lambda-ultralight))
     (powerline-inactive0                       (:background lambda-faint :foreground lambda-ultralight))
     (powerline-inactive1                       (:background lambda-mild  :foreground lambda-ultralight))
     (powerline-inactive2                       (:background lambda-faint :foreground lambda-ultralight))

;;;;;; Bespoke Modeline

     (bespoke-modeline-active               (:foreground lambda-fg :background lambda-faint
                                             :box (:line-width 1 :color lambda-faint :style nil) :overline nil :underline nil))
     (bespoke-modeline-active-name          (:background lambda-faint :foreground lambda-fg))
     (bespoke-modeline-active-primary       (:foreground lambda-faint :weight 'light))
     (bespoke-modeline-active-secondary     (:foreground lambda-fg))
     (bespoke-modeline-active-status-RW     (:weight 'bold :foreground lambda-fg :background lambda-green :box (:line-width 1 :color lambda-green :style nil)))
     (bespoke-modeline-active-status-**     (:weight 'bold :foreground lambda-fg :background lambda-red :box (:line-width 1 :color lambda-red :style nil)))
     (bespoke-modeline-active-status-RO     (:weight 'bold :foreground lambda-fg :background lambda-yellow :box (:line-width 1 :color lambda-yellow :style nil)))

     ;; Inactive
     (bespoke-modeline-inactive             (:foreground lambda-mild :background lambda-ultralight :box (:line-width 1 :color lambda-faint) :overline nil :underline nil))
     (bespoke-modeline-inactive-name        (:foreground lambda-mild :background lambda-ultralight :weight 'light :box nil :overline nil :underline nil))
     (bespoke-modeline-inactive-primary     (:foreground lambda-mild :background lambda-ultralight :weight 'light :box nil :overline nil :underline nil))
     (bespoke-modeline-inactive-secondary   (:foreground lambda-mild :background lambda-ultralight :weight 'light :box nil :overline nil :underline nil))
     (bespoke-modeline-inactive-status-RO   (:foreground lambda-mild :background lambda-faint
                                             :box (:line-width 1 :color lambda-faint :style nil) :overline nil :underline nil))
     (bespoke-modeline-inactive-status-RW (:foreground lambda-mild :background lambda-faint
                                           :box (:line-width 1 :color lambda-faint :style nil) :overline nil :underline nil))
     (bespoke-modeline-inactive-status-**  (:foreground lambda-mild :background lambda-faint
                                            :box (:line-width 1 :color lambda-faint :style nil) :overline nil :underline nil))

     ) ,@body))


;;;; Set Minibuffer & Echo Area
(defun lambda-themes--minibuffer ()
  "Derive minibuffer / echo area faces from lambda faces."
  ;; Minibuffer / echo area
  (dolist (buffer (list " *Minibuf-0*" " *Echo Area 0*"
                        " *Minibuf-1*" " *Echo Area 1*"))
    (when (get-buffer buffer)
      (with-current-buffer buffer
        (face-remap-add-relative 'default 'fringe)))))
(lambda-themes--minibuffer)


;;; Provide file
;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide 'lambda-themes)

;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; End:
;;; lambda-themes.el ends here
