;; lambda-themes.el --- A custom theme  -*- lexical-binding: t; -*-
;; Copyright (C) 2020 Colin McLear
;; -------------------------------------------------------------------
;; Authors: Colin McLear
;; -------------------------------------------------------------------
;; URL: https://github.com/mclear-tools/lambda-themes
;; -------------------------------------------------------------------
;; Created: 2021-03-16
;; Version: 0.6
;; Package-Requires: ((emacs "26.1"))
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
;; This theme started as a fork of nano-emacs.
;; See https://github.com/rougier/nano-emacs.
;; Color palatte has been expanded and face definitions revised
;; -------------------------------------------------------------------
;;; Code


;;;; Theme Variables

(defvar evil-emacs-state-cursor)
(defvar evil-normal-state-cursor)
(defvar evil-visual-state-cursor)
(defvar evil-insert-state-cursor)
(defvar evil-replace-state-cursor)
(defvar evil-motion-state-cursor)
(defvar evil-operator-state-cursor)
(defvar hl-todo-keyword-faces)

;;;; Theme Options

(defcustom lambda-set-theme 'light
  "Choose which theme variant, light or dark, to use."
  :group 'lambda-themes
  :type 'symbol)

;; Cursors
(defcustom lambda-set-evil-cursors t
  "If t then use lambda evil cursor colors."
  :group 'lambda-themes
  :type 'boolean)

;; Font options
(defcustom lambda-set-italic-comments t
  "If t then use italics for comments."
  :group 'lambda-themes
  :type 'boolean)

(defcustom lambda-set-italic-keywords t
  "If t then use italics for keywords."
  :group 'lambda-themes
  :type 'boolean)

(defcustom lambda-set-variable-pitch t
  "If t then use variable-pitch for headings."
  :group 'lambda-themes
  :type 'boolean)

;;;; After Load Theme Hook
(defvar lambda-after-load-theme-hook nil
  "Hook run after lambda-theme is loaded using `load-theme'.")

;;;; Disable Theme Function
(defun lambda--disable-all-themes ()
  "Disable all active themes."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

;;;; Theme Toggle
;;;###autoload
(defun lambda/toggle-theme ()
  "Toggle between dark and light variants"
  (interactive)
  (if (eq lambda-set-theme 'light)
      (progn
        (lambda--disable-all-themes)
        (setq lambda-set-theme 'dark)
        (load-theme 'lambda t)
        (run-hooks 'lambda-after-load-theme-hook))
    (progn
      (lambda--disable-all-themes)
      (setq lambda-set-theme 'light)
      (load-theme 'lambda t)
      (run-hooks 'lambda-after-load-theme-hook)
      )))

;;;; Call Theme Functions
;;;###autoload
(defun lambda/light-theme ()
  "Set light variant of lambda-theme"
  (interactive)
  (lambda--disable-all-themes)
  (setq lambda-set-theme 'light)
  (load-theme 'lambda t)
  (run-hooks 'lambda-after-load-theme-hook))

;;;###autoload
(defun lambda/dark-theme ()
  "Set dark variant of lambda-theme"
  (interactive)
  (lambda--disable-all-themes)
  (setq lambda-set-theme 'dark)
  (load-theme 'lambda t)
  (run-hooks 'lambda-after-load-theme-hook))


;;;; Define group & colors

(defgroup lambda-themes nil
  "Faces and colors for lambda themes"
  :group 'faces)

;; Derive our default color set from core Emacs faces.
;; This allows use of lambda colors in independently themed Emacsen
;;
;; We memorize the default colorset in this var in order not to confuse
;; customize: the STANDARD argument of defcustom gets re-evaluated by customize
;; to determine if the current value is default or not.
(defvar lambda-base-colors--defaults
  `((foreground . ,(face-foreground 'default nil t))
    (background . ,(face-background 'default nil t))
    (highlight . ,(face-background 'fringe nil t))
    (critical . ,(face-foreground 'error nil t))
    (salient . ,(face-foreground 'font-lock-keyword-face nil t))
    (strong . ,(face-foreground 'default nil t))
    (popout . ,(face-foreground 'font-lock-string-face nil t))
    (subtle . ,(face-background 'mode-line-inactive nil t))
    (faded . ,(face-foreground 'shadow nil t))))

(defun lambda-base-colors--get (name)
  "Get default color associated with symbol NAME."
  (cdr (assoc name lambda-base-colors--defaults)))

(defcustom lambda-foreground (lambda-base-colors--get 'foreground)
  ""
  :type 'color
  :group 'lambda-themes)

(defcustom lambda-background (lambda-base-colors--get 'background)
  ""
  :type 'color
  :group 'lambda-themes)

(defcustom lambda-highlight (lambda-base-colors--get 'highlight)
  ""
  :type 'color
  :group 'lambda-themes)

(defcustom lambda-critical (lambda-base-colors--get 'critical)
  ""
  :type 'color
  :group 'lambda-themes)

(defcustom lambda-salient (lambda-base-colors--get 'salient)
  ""
  :type 'color
  :group 'lambda-themes)

(defcustom lambda-strong (lambda-base-colors--get 'strong)
  ""
  :type 'color
  :group 'lambda-themes)

(defcustom lambda-popout (lambda-base-colors--get 'popout)
  ""
  :type 'color
  :group 'lambda-themes)

(defcustom lambda-subtle (lambda-base-colors--get 'subtle)
  ""
  :type 'color
  :group 'lambda-themes)

(defcustom lambda-faded (lambda-base-colors--get 'faded)
  ""
  :type 'color
  :group 'lambda-themes)


;;;; Define Faces
;; The themes are fully defined by these faces

;;;;; Core faces
(defface lambda-default nil
  "Default face is for regular use."
  :group 'faces)

(defface lambda-critical nil
  "Critical face is for information that requires action---e.g.,
syntax or spelling errors. It should be of high constrast when
compared to other faces. This can be realized (for example) by
setting an intense background color, typically a shade of red or
orange. It should be used rarely."
  :group 'faces)

(defface lambda-popout nil
  "Popout face is used for information that needs attention.
To achieve such effect, the hue of the face has to be
sufficiently different from other faces such that it attracts
attention through the popout effect (see
https://metapraxis.com/blog/blog/the-pop-out-effect/)."
  :group 'faces)

(defface lambda-strong nil
  "Strong face is used for information of a structural nature.
It is the same color as the default color. Only the
weight differs by one level (e.g., light/regular or
regular/bold). Usage might include titles, keywords,
directory, etc."
  :group 'faces)

(set-face-attribute 'lambda-strong nil
                    :foreground (face-foreground 'lambda-default)
                    :weight 'bold)

(defface lambda-salient nil
  "Salient face is used for important information, though not
necessarily that which needs immediate action or attention. To
suggest the information is important, the face uses a different
hue with approximately the same intensity as the default face.
This might be used, e.g., for links."
  :group 'faces)

(defface lambda-faded nil
  "Faded face is for less (immediately) important information. It
is made by using the same hue as the default but with a lesser
intensity than the default. It can be used for comments,
secondary information."
  :group 'faces)

(defface lambda-subtle nil
  "Subtle face is used to suggest a physical area on the screen.
It's main use is for differentiating regions without drawing a
significant amount of attention. It is also closely related in
shade to the modeline color and to the highlight color."
  :group 'faces)

;;;;; Accent faces
;; The accent colors are used to fill out the color palatte. They are meant to be
;; used for attention or contrast with the core colors. Readability is important.

(defface lambda-highlight nil
  "This should be used primarily for highlighting. It is meant
subtlety stand out from the mode line and other adjacent faces."
  :group 'faces)

(defface lambda-modeline nil
  "Default face for the mode line."
  :group 'faces)

(defface lambda-inactive nil
  "Face for the inactive mode line"
  :group 'faces)

(defface lambda-red nil
  "A reddish accent face"
  :group 'faces)

(defface lambda-green nil
  "A greenish accent face"
  :group 'faces)

(defface lambda-blue nil
  "A bluish accent face"
  :group 'faces)

(defface lambda-yellow nil
  "A yellowish accent face"
  :group 'faces)

(defface lambda-brown nil
  "A brownish accent face"
  :group 'faces)

;;;; Define Theme
(deftheme lambda "A custom theme for yak shaving, with light and dark variants")

;;;; Set Colors

(defun lambda-theme--light-dark (light dark)
  "Determine theme using the LIGHT or the DARK color variants of lambda-theme."
  (if (eq lambda-set-theme 'light)
      light
    dark))
(defalias '--l/d #'lambda-theme--light-dark)

(defun lambda--set-theme-variant ()
  "Set theme colors according to LIGHT or DARK variant"
  (setq lambda-foreground (--l/d "#282b35" "#eceff1"))
  (setq lambda-background (--l/d "#fffef9" "#282b35"))

  (setq lambda-modeline   (--l/d "#e3e7ef" "#3c4353"))
  (setq lambda-headline   (--l/d ""        ""       ))
  (setq lambda-highlight  (--l/d "#dbe1eb" "#444B5c"))
  (setq lambda-active     (--l/d ""         ""      ))
  (setq lambda-inactive   (--l/d "#cbd3e1" "#525868"))

  (setq lambda-critical   (--l/d "#f53137" "#f46715"))
  (setq lambda-salient    (--l/d "#303db4" "#88c0d0"))
  (setq lambda-strong     (--l/d "#000000" "#ffffff"))
  (setq lambda-popout     (--l/d "#940b96" "#bc85cf"))
  (setq lambda-subtle     (--l/d "#eceff1" "#333a47"))
  (setq lambda-faded      (--l/d "#727d97" "#959eb1"))

  (setq lambda-blue       (--l/d "#30608c" "#81a1c1"))
  (setq lambda-green      (--l/d "#00796b" "#8eb89d"))
  (setq lambda-red        (--l/d "#960d36" "#bf616a"))
  (setq lambda-brown      (--l/d "#966e53" "#d08770"))
  (setq lambda-yellow     (--l/d "#e0a500" "#e9b85d")))

;;;; Customize Faces

;; Call color settings
(lambda--set-theme-variant)

;; Declare class and set faces
(let ((class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   `lambda
   `(default ((,class :foreground ,lambda-foreground :background ,lambda-background)))


;;;;; Basic Faces
   `(buffer-menu-buffer                            ((,class :foreground ,lambda-strong)))
   `(minibuffer-prompt                             ((,class :foreground ,lambda-green)))
   `(link                                          ((,class :foreground ,lambda-salient)))
   `(region                                        ((,class :background ,lambda-highlight)))
   `(fringe                                        ((,class :foreground ,lambda-faded :weight light)))
   `(highlight                                     ((,class :background ,lambda-subtle)))
   `(lazy-highlight                                ((,class :foreground ,lambda-green)))
   `(trailing-whitespace                           ((,class :foreground ,lambda-faded)))
   `(secondary-selection                           ((,class :foreground ,lambda-yellow :background ,lambda-subtle)))
   `(show-paren-match                              ((,class :foreground ,lambda-yellow :weight bold)))
   `(show-paren-mismatch                           ((,class :foreground ,lambda-critical :weight bold :box t)))
   `(tooltip nil                                   ((,class :height 0.85)))

;;;;; Lambda Faces
   ;; NOTE: We want the lambda colors to be available as faces. It seems like there
   ;; should be a better way to do this but...
   `(lambda-foreground ((,class :foreground ,lambda-foreground)))
   `(lambda-background ((,class :background ,lambda-background)))
   `(lambda-modeline   ((,class :background ,lambda-modeline)))
   `(lambda-highlight  ((,class :foreground ,lambda-highlight)))
   `(lambda-inactive   ((,class :foreground ,lambda-inactive)))
   `(lambda-critical   ((,class :foreground ,lambda-critical)))
   `(lambda-salient    ((,class :foreground ,lambda-salient)))
   `(lambda-strong     ((,class :foreground ,lambda-strong)))
   `(lambda-popout     ((,class :foreground ,lambda-popout)))
   `(lambda-subtle     ((,class :foreground ,lambda-subtle)))
   `(lambda-faded      ((,class :foreground ,lambda-faded)))
   `(lambda-blue       ((,class :foreground ,lambda-blue)))
   `(lambda-green      ((,class :foreground ,lambda-green)))
   `(lambda-red        ((,class :foreground ,lambda-red)))
   `(lambda-brown      ((,class :foreground ,lambda-brown)))
   `(lambda-yellow     ((,class :foreground ,lambda-yellow)))

;;;;; Buttons
   `(custom-button                                 ((,class :foreground ,lambda-foreground :background ,lambda-highlight :box nil)))
   `(custom-button-mouse                           ((,class :foreground ,lambda-foreground :background ,lambda-subtle :box nil)))
   `(custom-button-pressed                         ((,class :foreground ,lambda-background :background ,lambda-foreground :box nil)))

;;;;; Bookmarks
   `(bookmark-menu-heading                         ((,class :foreground ,lambda-strong)))
   `(bookmark-menu-bookmark                        ((,class :foreground ,lambda-salient)))
   `(bookmark-face                                 ((,class :foreground ,lambda-salient)))

;;;;; Childframes
;;;;;; Mini-Frame
   `(mini-popup-background ((,class :background ,lambda-subtle)))
   `(mini-popup-border     ((,class :background ,lambda-subtle)))

;;;;;; Mini-Popup (Childframe)
   `(mini-popup-background ((,class :background ,lambda-subtle)))
   `(mini-popup-border     ((,class :background ,lambda-subtle)))

;;;;;; Posframe

   `(which-key-posframe                           ((,class :background ,lambda-subtle)))
   `(which-key-posframe-border                    ((,class :background ,lambda-subtle)))
   `(transient-posframe-border                    ((,class :background ,lambda-subtle)))
   `(transient-posframe                           ((,class :foreground ,lambda-strong :background ,lambda-subtle)))

;;;;; Completion/Narrowing

;;;;;; Company
   `(company-scrollbar-fg                          ((,class :foreground ,lambda-faded)))
   `(company-scrollbar-bg                          ((,class :foreground ,lambda-faded)))
   `(company-preview                               ((,class :foreground ,lambda-faded :weight bold)))
   `(company-preview-common                        ((,class :foreground ,lambda-faded)))
   `(company-tooltip-selection                     ((,class :foreground ,lambda-salient)))
   `(company-tooltip                               ((,class :background ,lambda-subtle)))
   `(company-tooltip-common                        ((,class :background ,lambda-subtle)))
   `(company-tooltip-common-selection              ((,class :foreground ,lambda-salient)))
   `(company-tooltip-annotation                    ((,class :foreground ,lambda-faded)))
   `(company-tooltip-annotation-selection          ((,class :foreground ,lambda-salient)))

;;;;;; Corfu
   `(corfu-annotations                             ((,class :foreground ,lambda-faded)))
   `(corfu-bar                                     ((,class :foreground ,lambda-modeline)))
   `(corfu-border                                  ((,class :foreground ,lambda-subtle)))
   `(corfu-current                                 ((,class :foreground ,lambda-popout :background ,lambda-highlight)))
   `(corfu-default                                 ((,class :inherit default :background ,lambda-subtle)))
   `(corfu-deprecated                              ((,class :foreground ,lambda-faded)))
   `(corfu-echo                                    ((,class :inherit default)))

;;;;;; Counsel
   `(counsel-active-mode                           ((,class :foreground ,lambda-salient)))
   `(counsel-application-name                      ((,class :foreground ,lambda-red)))
   `(counsel-key-binding                           ((,class :inherit default)))
   `(counsel-outline-1                             ((,class :inherit org-level-1)))
   `(counsel-outline-2                             ((,class :inherit org-level-2)))
   `(counsel-outline-3                             ((,class :inherit org-level-3)))
   `(counsel-outline-4                             ((,class :inherit org-level-4)))
   `(counsel-outline-5                             ((,class :inherit org-level-5)))
   `(counsel-outline-6                             ((,class :inherit org-level-6)))
   `(counsel-outline-7                             ((,class :inherit org-level-7)))
   `(counsel-outline-8                             ((,class :inherit org-level-8)))
   `(counsel-outline-default                       ((,class :foreground ,lambda-foreground)))
   `(counsel-variable-documentation                ((,class :inherit default :foreground ,lambda-yellow)))

;;;;;; Helm
   `(helm-selection                                ((,class :foreground ,lambda-subtle :weight bold)))
   `(helm-match                                    ((,class :foreground ,lambda-strong)))
   `(helm-source-header                            ((,class :foreground ,lambda-salient)))
   `(helm-visible-mark                             ((,class :foreground ,lambda-strong)))
   `(helm-swoop-target-line-face                   ((,class :foreground ,lambda-subtle :weight bold)))
   `(helm-moccur-buffer                            ((,class :foreground ,lambda-strong)))
   `(helm-ff-file                                  ((,class :foreground ,lambda-faded)))
   `(helm-ff-prefix                                ((,class :foreground ,lambda-strong)))
   `(helm-ff-dotted-directory                      ((,class :foreground ,lambda-faded)))
   `(helm-ff-directory                             ((,class :foreground ,lambda-strong)))
   `(helm-ff-executable                            ((,class :foreground ,lambda-popout)))
   `(helm-grep-match                               ((,class :foreground ,lambda-strong)))
   `(helm-grep-file                                ((,class :foreground ,lambda-faded)))
   `(helm-grep-lineno                              ((,class :foreground ,lambda-faded)))
   `(helm-grep-finish                              ((,class :foreground ,lambda-foreground)))


;;;;;; Ivy
   `(ivy-action                                    ((,class :foreground ,lambda-faded)))
   `(ivy-completions-annotations                   ((,class :foreground ,lambda-faded)))
   `(ivy-confirm-face                              ((,class :foreground ,lambda-faded)))
   `(ivy-current-match                             ((,class :foreground ,lambda-strong :weight bold :background ,lambda-highlight)))
   `(ivy-cursor                                    ((,class :inherit default)))
   `(ivy-grep-info                                 ((,class :foreground ,lambda-strong)))
   `(ivy-grep-line-number                          ((,class :foreground ,lambda-faded)))
   `(ivy-highlight-face                            ((,class :foreground ,lambda-strong)))
   `(ivy-match-required-face                       ((,class :foreground ,lambda-faded)))
   `(ivy-minibuffer-match-face-1                   ((,class :foreground ,lambda-popout)))
   `(ivy-minibuffer-match-face-2                   ((,class :foreground ,lambda-popout)))
   `(ivy-minibuffer-match-face-3                   ((,class :foreground ,lambda-popout)))
   `(ivy-minibuffer-match-face-4                   ((,class :foreground ,lambda-popout)))
   `(ivy-minibuffer-match-highlight                ((,class :foreground ,lambda-strong)))
   `(ivy-modified-buffer                           ((,class :foreground ,lambda-popout)))
   `(ivy-modified-outside-buffer                   ((,class :foreground ,lambda-strong)))
   `(ivy-org                                       ((,class :foreground ,lambda-faded)))
   `(ivy-prompt-match                              ((,class :foreground ,lambda-faded)))
   `(ivy-remote                                    ((,class :inherit default)))
   `(ivy-separator                                 ((,class :foreground ,lambda-faded)))
   `(ivy-subdir                                    ((,class :foreground ,lambda-faded)))
   `(ivy-virtual                                   ((,class :foreground ,lambda-faded)))
   `(ivy-yanked-word                               ((,class :foreground ,lambda-faded)))

;;;;;; Ido
   `(ido-first-match                               ((,class :foreground ,lambda-salient)))
   `(ido-only-match                                ((,class :foreground ,lambda-faded)))
   `(ido-subdir                                    ((,class :foreground ,lambda-strong)))

;;;;;; Selectrum
   `(selectrum-current-candidate                   ((,class :weight bold :background ,lambda-highlight)))
   `(selectrum-prescient-secondary-highlight       ((,class :weight bold :foreground ,lambda-blue)))
   `(selectrum-prescient-primary-highlight         ((,class :weight bold :foreground ,lambda-salient)))
   `(selectrum-completion-docsig                   ((,class :slant  italic :inherit selectrum-completion-annotation)))
   `(selectrum-completion-annotation               ((,class :inherit completions-annotations)))
   `(selectrum-group-separator                     ((,class :strike-through t :inherit shadow)))
   `(selectrum-group-title                         ((,class :slant  italic :inherit shadow)))
   `(selectrum-quick-keys-match                    ((,class :inherit isearch)))
   `(selectrum-quick-keys-highlight                ((,class :foreground ,lambda-popout)))

;;;;;; Vertico
   `(vertico-current                               ((,class :weight regular :background ,lambda-highlight)))

;;;;;; Orderless

   `(orderless-match-face-0                        ((,class :weight bold :foreground ,lambda-yellow)))
   `(orderless-match-face-1                        ((,class :weight bold :foreground ,lambda-yellow)))
   `(orderless-match-face-2                        ((,class :weight bold :foreground ,lambda-yellow)))
   `(orderless-match-face-3                        ((,class :weight bold :foreground ,lambda-yellow)))



;;;;; Customize
   `(widget-field                                  ((,class :background ,lambda-subtle)))
   `(widget-button                                 ((,class :foreground ,lambda-foreground :bold t)))
   `(widget-single-line-field                      ((,class :background ,lambda-subtle)))
   `(custom-group-subtitle                         ((,class :foreground ,lambda-foreground :bold t)))
   `(custom-group-tag                              ((,class :foreground ,lambda-foreground :bold t)))
   `(custom-group-tag-1                            ((,class :foreground ,lambda-foreground :bold t)))
   `(custom-comment                                ((,class :foreground ,lambda-faded)))
   `(custom-comment-tag                            ((,class :foreground ,lambda-faded)))
   `(custom-changed                                ((,class :foreground ,lambda-salient)))
   `(custom-modified                               ((,class :foreground ,lambda-salient)))
   `(custom-face-tag                               ((,class :foreground ,lambda-foreground :bold t)))
   `(custom-variable-tag                           ((,class :foreground ,lambda-foreground :bold t)))
   `(custom-invalid                                ((,class :foreground ,lambda-popout)))
   `(custom-visibility                             ((,class :foreground ,lambda-salient)))
   `(custom-state                                  ((,class :foreground ,lambda-salient)))
   `(custom-link                                   ((,class :foreground ,lambda-salient)))
   `(custom-button                                 ((,class :foreground ,lambda-faded :background ,lambda-background :box `(:line-width 1 :color ,(face-foreground 'lambda-faded) :style nil))))
   `(custom-button-mouse                           ((,class :foreground ,lambda-faded :background ,lambda-subtle :box `(:line-width 1 :color ,(face-foreground 'lambda-faded) :style nil))))
   `(custom-button-pressed                         ((,class :foreground ,lambda-foreground :background ,lambda-salient :inverse-video nil :box `(:line-width 1 :color ,(face-foreground 'lambda-salient) :style nil))))

;;;;; Deft
   `(deft-filter-string-error-face                 ((,class :foreground ,lambda-popout)))
   `(deft-filter-string-face                       ((,class :foreground ,lambda-yellow)))
   `(deft-header-face                              ((,class :foreground ,lambda-salient)))
   `(deft-separator-face                           ((,class :foreground ,lambda-faded)))
   `(deft-summary-face                             ((,class :foreground ,lambda-faded)))
   `(deft-time-face                                ((,class :foreground ,lambda-salient)))
   `(deft-title-face                               ((,class :foreground ,lambda-strong :weight semi-bold)))

;;;;; Diff
   `(diff-header                                   ((,class :foreground ,lambda-faded)))
   `(diff-file-header                              ((,class :foreground ,lambda-strong)))
   `(diff-context                                  ((,class :inherit    default)))
   `(diff-removed                                  ((,class :foreground ,lambda-faded)))
   `(diff-changed                                  ((,class :foreground ,lambda-popout)))
   `(diff-added                                    ((,class :foreground ,lambda-salient)))
   `(diff-refine-added                             ((,class :foreground ,lambda-strong)))
   `(diff-refine-changed                           ((,class :foreground ,lambda-popout)))
   `(diff-refine-removed                           ((,class :foreground ,lambda-faded :strike-through t)))
   `(magit-section-highlight                       ((,class :background ,lambda-subtle)))


;;;;; Dired
;;;;;; All The Icons Dired
   `(all-the-icons-dired-dir-face                  ((,class :forground ,lambda-salient)))

;;;;;; Dired (plus)
   `(diredp-write-priv                             ((,class :foreground ,lambda-critical)))
   `(diredp-tagged-autofile-name                   ((,class :foreground ,lambda-background)))
   `(diredp-symlink                                ((,class :foreground ,lambda-popout)))
   `(diredp-read-priv                              ((,class :foreground ,lambda-popout)))
   `(diredp-rare-priv                              ((,class :foreground ,lambda-popout :background ,lambda-critical)))
   `(diredp-other-priv                             ((,class :background ,lambda-red)))
   `(diredp-omit-file-name                         ((,class :strike-through ,lambda-faded :inherit diredp-ignored-file-name)))
   `(diredp-number                                 ((,class :foreground ,lambda-salient)))
   `(diredp-no-priv                                ((,class :foreground ,lambda-critical)))
   `(diredp-mode-line-flagged                      ((,class :foreground ,lambda-critical)))
   `(diredp-mode-line-marked                       ((,class :foreground ,lambda-salient)))
   `(diredp-link-priv                              ((,class :foreground ,lambda-popout)))
   `(diredp-ignored-file-name                      ((,class :foreground ,lambda-faded)))
   `(diredp-flag-mark-line                         ((,class :foreground ,lambda-popout)))
   `(diredp-flag-mark                              ((,class :foreground ,lambda-popout :background ,lambda-salient)))
   `(diredp-file-suffix                            ((,class :foreground ,lambda-faded)))
   `(diredp-file-name                              ((,class :foreground ,lambda-foreground)))
   `(diredp-executable-tag                         ((,class :foreground ,lambda-critical)))
   `(diredp-exec-priv                              ((,class :foreground ,lambda-critical)))
   `(diredp-dir-priv                               ((,class :foreground ,lambda-faded)))
   `(diredp-dir-name                               ((,class :foreground ,lambda-green)))
   `(diredp-dir-heading                            ((,class :inherit ,(if lambda-set-variable-pitch 'variable-pitch 'default) :foreground ,lambda-blue :background ,lambda-subtle)))
   `(diredp-deletion-file-name                     ((,class :foreground ,lambda-critical)))
   `(diredp-deletion                               ((,class :foreground ,lambda-popout :background ,lambda-critical)))
   `(diredp-date-time                              ((,class :foreground ,lambda-faded)))
   `(diredp-compressed-file-suffix                 ((,class :foreground ,lambda-faded)))
   `(diredp-compressed-file-name                   ((,class :foreground ,lambda-background)))
   `(diredp-autofile-name                          ((,class :background ,lambda-subtle)))

;;;;;; Dired Colors (Diredfl)
   `(diredfl-write-priv                            ((,class :foreground ,lambda-critical)))
   `(diredfl-tagged-autofile-name                  ((,class :foreground ,lambda-background)))
   `(diredfl-symlink                               ((,class :foreground ,lambda-popout)))
   `(diredfl-read-priv                             ((,class :foreground ,lambda-popout)))
   `(diredfl-rare-priv                             ((,class :foreground ,lambda-popout :background ,lambda-critical)))
   `(diredfl-other-priv                            ((,class :background ,lambda-red)))
   `(diredfl-omit-file-name                        ((,class :strike-through ,lambda-faded :inherit diredp-ignored-file-name)))
   `(diredfl-number                                ((,class :foreground ,lambda-salient)))
   `(diredfl-no-priv                               ((,class :foreground ,lambda-critical)))
   `(diredfl-mode-line-flagged                     ((,class :foreground ,lambda-critical)))
   `(diredfl-mode-line-marked                      ((,class :foreground ,lambda-salient)))
   `(diredfl-link-priv                             ((,class :foreground ,lambda-popout)))
   `(diredfl-ignored-file-name                     ((,class :foreground ,lambda-faded)))
   `(diredfl-flag-mark-line                        ((,class :foreground ,lambda-popout)))
   `(diredfl-flag-mark                             ((,class :foreground ,lambda-popout :background ,lambda-salient)))
   `(diredfl-file-suffix                           ((,class :foreground ,lambda-faded)))
   `(diredfl-file-name                             ((,class :foreground ,lambda-foreground)))
   `(diredfl-executable-tag                        ((,class :foreground ,lambda-critical)))
   `(diredfl-exec-priv                             ((,class :foreground ,lambda-critical)))
   `(diredfl-dir-priv                              ((,class :foreground ,lambda-faded)))
   `(diredfl-dir-name                              ((,class :foreground ,lambda-green)))
   `(diredfl-dir-heading                           ((,class :inherit ,(if lambda-set-variable-pitch 'variable-pitch 'default) :foreground ,lambda-blue :background ,lambda-subtle)))
   `(diredfl-deletion-file-name                    ((,class :foreground ,lambda-critical)))
   `(diredfl-deletion                              ((,class :foreground ,lambda-popout :background ,lambda-critical)))
   `(diredfl-date-time                             ((,class :foreground ,lambda-faded)))
   `(diredfl-compressed-file-suffix                ((,class :foreground ,lambda-faded)))
   `(diredfl-compressed-file-name                  ((,class :foreground ,lambda-background)))
   `(diredfl-autofile-name                         ((,class :background ,lambda-subtle)))

;;;;; Flyspell
   `(flyspell-duplicate                            ((,class :foreground ,lambda-red)))
   `(flyspell-incorrect                            ((,class :foreground ,lambda-critical)))

;;;;; Font Lock
   `(font-lock-comment-face                        ((,class :foreground ,lambda-faded :slant ,(if lambda-set-italic-comments 'italic 'normal))))
   `(font-lock-comment-delimiter-face              ((,class :foreground ,lambda-faded :weight bold :slant ,(if lambda-set-italic-comments 'italic 'normal))))
   `(font-lock-doc-face                            ((,class :foreground ,lambda-faded)))
   `(font-lock-string-face                         ((,class :foreground ,lambda-popout)))
   `(font-lock-constant-face                       ((,class :foreground ,lambda-green)))
   `(font-lock-builtin-face                        ((,class :foreground ,lambda-green)))
   `(font-lock-function-name-face                  ((,class :foreground ,lambda-strong :weight semi-bold)))
   `(font-lock-variable-name-face                  ((,class :foreground ,lambda-yellow)))
   `(font-lock-type-face                           ((,class :foreground ,lambda-salient)))
   `(font-lock-keyword-face                        ((,class :foreground ,lambda-salient :slant ,(if lambda-set-italic-keywords 'italic 'normal))))
   `(font-lock-reference-face                      ((,class :foreground ,lambda-salient)))
   `(font-lock-warning-face                        ((,class :foreground ,lambda-critical)))
   `(font-lock-regexp-grouping-backslash           ((,class :foreground ,lambda-critical)))
   `(font-lock-regexp-grouping-construct           ((,class :foreground ,lambda-critical)))

;;;;; Git
;;;;;; Git-gutter
   `(git-gutter:added        ((,class :foreground ,lambda-green)))
   `(git-gutter:deleted      ((,class :foreground ,lambda-red)))
   `(git-gutter:modified     ((,class :foreground ,lambda-popout)))
   `(git-gutter:separator    ((,class :foreground ,lambda-subtle)))
   `(git-gutter:unchanged    ((,class :foreground ,lambda-background)))
;;;;;; Git-gutter-fr
   `(git-gutter-fr:added        ((,class :foreground ,lambda-green)))
   `(git-gutter-fr:deleted      ((,class :foreground ,lambda-red)))
   `(git-gutter-fr:modified     ((,class :foreground ,lambda-popout)))

;;;;; Goggles
   `(goggles-added   ((,class :background ,lambda-green)))
   `(goggles-changed ((,class :background ,lambda-popout)))
   `(goggles-removed ((,class :background ,lambda-red)))

;;;;; Help(ful)

   `(helpful-heading ((,class :inherit ,(if lambda-set-variable-pitch 'variable-pitch 'default) :foreground ,lambda-blue :height 1.25)))


;;;;; Highlight-Indentation
   `(highlight-indentation-face ((,class :inherit ,lambda-highlight)))
   `(highlight-indentation-current-column-face ((,class :background ,lambda-yellow)))

;;;;; Highlight Indentation Guides
   `(highlight-indent-guides-stack-odd-face        ((,class :foreground ,lambda-brown)))
   `(highlight-indent-guides-stack-even-face       ((,class :foreground ,lambda-yellow)))
   `(highlight-indent-guides-top-odd-face          ((,class :foreground ,lambda-brown)))
   `(highlight-indent-guides-top-even-face         ((,class :foreground ,lambda-yellow)))
   `(highlight-indent-guides-odd-face              ((,class :foreground ,lambda-brown)))
   `(highlight-indent-guides-even-face             ((,class :foreground ,lambda-yellow)))
   `(highlight-indent-guides-character-face        ((,class :foreground ,lambda-highlight)))
   `(highlight-indent-guides-top-character-face    ((,class :foreground ,lambda-highlight)))
   `(highlight-indent-guides-stack-character-face  ((,class :foreground ,lambda-highlight)))

;;;;; Imenu List
   `(imenu-list-entry-face-0                       ((,class :inherit imenu-list-entry-face :foreground ,lambda-faded)))
   `(imenu-list-entry-face-1                       ((,class :inherit imenu-list-entry-face :foreground ,lambda-faded)))
   `(imenu-list-entry-face-2                       ((,class :inherit imenu-list-entry-face :foreground ,lambda-faded)))
   `(imenu-list-entry-face-3                       ((,class :inherit imenu-list-entry-face :foreground ,lambda-faded)))

;;;;; Info (Documentation)
   `(info-menu-header                              ((,class :foreground ,lambda-strong)))
   `(info-header-node                              ((,class :foreground ,lambda-green)))
   `(info-index-match                              ((,class :foreground ,lambda-salient)))
   `(Info-quoted                                   ((,class :foreground ,lambda-faded)))
   `(info-title-1                                  ((,class :foreground ,lambda-strong)))
   `(info-title-2                                  ((,class :foreground ,lambda-strong)))
   `(info-title-3                                  ((,class :foreground ,lambda-strong)))
   `(info-title-4                                  ((,class :foreground ,lambda-strong)))

;;;;; Interface
   `(widget-field                                  ((,class :background ,lambda-subtle)))
   `(widget-button                                 ((,class :foreground ,lambda-strong)))
   `(widget-single-line-field                      ((,class :foreground ,lambda-subtle)))
   `(custom-group-subtitle                         ((,class :foreground ,lambda-strong)))
   `(custom-group-tag                              ((,class :foreground ,lambda-strong)))
   `(custom-group-tag-1                            ((,class :foreground ,lambda-strong)))
   `(custom-comment                                ((,class :foreground ,lambda-faded)))
   `(custom-comment-tag                            ((,class :foreground ,lambda-faded)))
   `(custom-changed                                ((,class :foreground ,lambda-salient)))
   `(custom-modified                               ((,class :foreground ,lambda-salient)))
   `(custom-face-tag                               ((,class :foreground ,lambda-strong)))
   `(custom-variable-tag                           ((,class :inherit    default)))
   `(custom-invalid                                ((,class :foreground ,lambda-popout)))
   `(custom-visibility                             ((,class :foreground ,lambda-salient)))
   `(custom-state                                  ((,class :foreground ,lambda-salient)))
   `(custom-link                                   ((,class :foreground ,lambda-salient)))

;;;;; Markdown Mode
   `(markdown-blockquote-face                      ((,class :foreground ,lambda-salient)))
   `(markdown-bold-face                            ((,class :foreground ,lambda-strong :weight bold)))
   `(markdown-code-face                            ((,class :inherit    default)))
   `(markdown-comment-face                         ((,class :foreground ,lambda-faded)))
   `(markdown-footnote-marker-face                 ((,class :inherit    default)))
   `(markdown-footnote-text-face                   ((,class :inherit    default)))
   `(markdown-gfm-checkbox-face                    ((,class :inherit    default)))
   `(markdown-header-delimiter-face                ((,class :foreground ,lambda-faded)))
   `(markdown-header-face                          ((,class :inherit ,(if lambda-set-variable-pitch 'variable-pitch 'default))))
   `(markdown-header-face-1                        ((,class :inherit outline-1)))
   `(markdown-header-face-2                        ((,class :inherit outline-2)))
   `(markdown-header-face-3                        ((,class :inherit outline-1)))
   `(markdown-header-face-4                        ((,class :inherit outline-2)))
   `(markdown-header-face-5                        ((,class :inherit outline-1)))
   `(markdown-header-face-6                        ((,class :inherit outline-2)))
   `(markdown-header-rule-face                     ((,class :inherit default)))
   `(markdown-highlight-face                       ((,class :inherit default)))
   `(markdown-hr-face                              ((,class :inherit default)))
   `(markdown-html-attr-name-face                  ((,class :inherit default)))
   `(markdown-html-attr-value-face                 ((,class :inherit default)))
   `(markdown-html-entity-face                     ((,class :inherit default)))
   `(markdown-html-tag-delimiter-face              ((,class :inherit default)))
   `(markdown-html-tag-name-face                   ((,class :inherit default)))
   `(markdown-inline-code-face                     ((,class :foreground ,lambda-popout)))
   `(markdown-italic-face                          ((,class :foreground ,lambda-strong :slant italic)))
   `(markdown-language-info-face                   ((,class :inherit   default)))
   `(markdown-language-keyword-face                ((,class :inherit   default)))
   `(markdown-line-break-face                      ((,class :inherit   default)))
   `(markdown-link-face                            ((,class :foreground ,lambda-salient)))
   `(markdown-link-title-face                      ((,class :inherit    default)))
   `(markdown-list-face                            ((,class :foreground ,lambda-faded)))
   `(markdown-markup-face                          ((,class :foreground ,lambda-faded)))
   `(markdown-math-face                            ((,class :inherit    default)))
   `(markdown-metadata-key-face                    ((,class :foreground ,lambda-faded)))
   `(markdown-metadata-value-face                  ((,class :foreground ,lambda-faded)))
   `(markdown-missing-link-face                    ((,class :inherit    default)))
   `(markdown-plain-url-face                       ((,class :inherit    default)))
   `(markdown-pre-face                             ((,class :inherit    default)))
   `(markdown-reference-face                       ((,class :foreground ,lambda-salient)))
   `(markdown-strike-through-face                  ((,class :foreground ,lambda-faded)))
   `(markdown-table-face                           ((,class :inherit    default)))
   `(markdown-url-face                             ((,class :foreground ,lambda-salient)))

;;;;; Magit
   `(magit-branch-current      ((,class :foreground ,lambda-salient :box t :weight semi-bold)))
   `(magit-branch-local        ((,class :foreground ,lambda-salient :weight semi-bold)))
   `(magit-branch-remote       ((,class :foreground ,lambda-green :weight semi-bold)))
   `(magit-branch-remote-head  ((,class :foreground ,lambda-popout :box t)))
   `(magit-branch-upstream     ((,class :inherit italic)))
   `(magit-cherry-equivalent   ((,class :background ,lambda-background :foreground ,lambda-popout)))
   `(magit-cherry-unmatched ((,class :background ,lambda-background :foreground ,lambda-salient)))
   `(magit-head                ((,class :inherit magit-branch-local)))
   `(magit-header-line ((,class :foreground ,lambda-foreground)))
   `(magit-header-line-key ((,class :foregrond ,lambda-green)))
   `(magit-header-line-log-select ((,class :foreground ,lambda-foreground)))
   `(magit-keyword ((,class :foreground ,lambda-popout)))
   `(magit-keyword-squash ((,class :inherit bold :foreground ,lambda-yellow)))
   `(magit-section ((,class :background ,lambda-subtle :foreground ,lambda-foreground)))
   `(magit-section-heading     ((,class :weight semi-bold :foreground ,lambda-yellow)))
   `(magit-section-heading-selection ((,class :foreground ,lambda-salient)))
   `(magit-section-highlight ((,class :background ,lambda-highlight)))
   `(magit-tag                 ((,class :foreground ,lambda-yellow)))
   `(magit-header-line         ((,class :foreground ,lambda-foreground
                                        :background ,lambda-modeline
                                        :box (:line-width (if (fboundp 'lambda-modeline) lambda-modeline-size 3))
                                        :color ,lambda-modeline
                                        :style nil)
                                :overline nil
                                :underline nil))
   `(magit-header-line-log-select ((,class :foreground ,lambda-foreground
                                           :background ,lambda-modeline
                                           :box (:line-width (if (fboundp 'lambda-modeline) lambda-modeline-size 3))
                                           :color ,lambda-modeline
                                           :style nil)
                                   :overline nil
                                   :underline nil))




;;;;; Message
   `(message-cited-text                            ((,class :foreground ,lambda-faded)))
   `(message-header-cc                             ((,class :inherit default)))
   `(message-header-name                           ((,class :foreground ,lambda-strong)))
   `(message-header-newsgroups                     ((,class :inherit default)))
   `(message-header-other                          ((,class :inherit default)))
   `(message-header-subject                        ((,class :foreground ,lambda-salient)))
   `(message-header-to                             ((,class :foreground ,lambda-salient)))
   `(message-header-xheader                        ((,class :inherit default)))
   `(message-mml                                   ((,class :foreground ,lambda-popout)))
   `(message-separator                             ((,class :foreground ,lambda-faded)))

;;;;; Meow
   `(meow-normal-cursor         ((,class :background ,lambda-yellow)))
   `(meow-insert-cursor         ((,class :background ,lambda-critical)))
   `(meow-keypad-cursor         ((,class :background ,lambda-brown)))
   `(meow-motion-cursor         ((,class :background ,lambda-green)))
   `(meow-kmacro-cursor         ((,class :background ,lambda-salient)))
   `(meow-beacon-cursor         ((,class :background ,lambda-yellow)))
   `(meow-beacon-fake-selection ((,class :background ,lambda-modeline)))
   `(meow-beacon-fake-cursor    ((,class :background ,lambda-yellow)))

;;;;; Mode line/Header line
;;;;;; Conditional Loading
   ;; NOTE: these settings are specifically for lambda-modeline
   ;; See https://github.com/mclear-tools/lambda-modeline
   ;; Mode line settings based on position
   (when (fboundp 'lambda-modeline)
     (when (eq lambda-modeline-position 'top)
       `(header-line ((,class :foreground ,lambda-foreground
                              :background ,lambda-modeline
                              :box (:line-width ,lambda-modeline-size
                                    :color ,lambda-modeline
                                    :style nil)
                              :overline nil
                              :underline nil)))))

   (when (fboundp 'lambda-modeline)
     (when (eq lambda-modeline-position 'top)
       `(mode-line  ((,class :height 0.1
                             :underline ,lambda-subtle
                             :overline nil
                             :box nil)))))


   (when (fboundp 'lambda-modeline)
     (when (eq lambda-modeline-position 'top)
       `(mode-line-inactive  ((,class :height 0.1
                                      :underline ,lambda-subtle
                                      :overline nil
                                      :box nil)))))


   (when (fboundp 'lambda-modeline)
     (when (eq lambda-modeline-position 'bottom)
       `(mode-line ((,class :foreground ,lambda-foreground
                            :background ,lambda-modeline
                            :box (:line-width ,lambda-modeline-size
                                  :color ,lambda-modeline
                                  :style nil)
                            :overline nil
                            :underline nil)))))

   (when (fboundp 'lambda-modeline)
     (when (eq lambda-modeline-position 'bottom)
       `(mode-line-inactive ((,class :foreground ,lambda-subtle
                                     :background ,lambda-modeline
                                     :box (:line-width ,lambda-modeline-size
                                           :color ,lambda-modeline
                                           :style nil)
                                     :overline nil
                                     :underline nil)))))

   ;; No underline in terminal
   ;; FIXME: for some reason this seems necessary
   ;; to disable underline in terminal
   (when (not (display-graphic-p))
     (set-face-attribute 'mode-line nil
                         :underline nil)
     (set-face-attribute 'mode-line-inactive nil
                         :underline nil))


   (when (fboundp 'lambda-modeline)
     (when (eq lambda-modeline-position nil)
       `(mode-line ((,class :foreground ,lambda-foreground
                            :background ,lambda-modeline
                            :box (:line-width ,lambda-modeline-size
                                  :color ,lambda-modeline
                                  :style nil)
                            :overline nil
                            :underline nil)))))

   (when (fboundp 'lambda-modeline)
     (when (eq lambda-modeline-position nil)
       `(mode-line-inactive ((,class :foreground ,lambda-faded
                                     :background ,lambda-modeline
                                     :box (:line-width ,lambda-modeline-size
                                           :color ,lambda-modeline
                                           :style nil)
                                     :overline nil
                                     :underline nil)))))

;;;;;; Mode line indicators

   ;; Active
   (when (fboundp 'lambda-modeline)
     `(lambda-modeline-active               ((,class (:foreground ,lambda-foreground
                                                       :background ,lambda-modeline
                                                       :box (:line-width ,lambda-modeline-size
                                                             :color ,lambda-modeline
                                                             :style nil)
                                                       :overline nil
                                                       :underline nil)))))

   `(lambda-modeline-active-name          ((,class (:background ,lambda-modeline
                                                     :foreground ,lambda-foreground))))
   `(lambda-modeline-active-primary       ((,class (:foreground ,lambda-faded :weight light))))
   `(lambda-modeline-active-secondary     ((,class (:foreground ,lambda-foreground))))
   `(lambda-modeline-active-status-RW ((,class :foreground ,lambda-background
                                                :background ,lambda-blue
                                                :box (:line-width 1 :color ,lambda-blue :style nil))))

   `(lambda-modeline-active-status-** ((,class :foreground ,lambda-background
                                                :background ,lambda-red
                                                :box (:line-width 1 :color ,lambda-red :style nil))))

   `(lambda-modeline-active-status-RO ((,class :foreground ,lambda-background
                                                :background ,lambda-yellow
                                                :box (:line-width 1 :color ,lambda-yellow :style nil))))

   ;; Inactive
   (when (fboundp 'lambda-modeline)
     `(lambda-modeline-inactive             ((,class (:foreground ,lambda-subtle
                                                       :background ,lambda-modeline
                                                       :box (:line-width ,lambda-modeline-size
                                                             :color ,lambda-modeline
                                                             :style nil)
                                                       :overline nil
                                                       :underline nil)))))
   `(lambda-modeline-inactive-name        ((,class (:foreground ,lambda-faded :background ,lambda-modeline :weight light))))
   `(lambda-modeline-inactive-primary     ((,class (:foreground ,lambda-faded :background ,lambda-modeline :weight light))))
   `(lambda-modeline-inactive-secondary   ((,class (:foreground ,lambda-faded :background ,lambda-modeline :weight light))))

   `(lambda-modeline-inactive-status-RO   ((,class :foreground ,lambda-subtle
                                                    :background ,lambda-inactive
                                                    :box (:line-width 1
                                                          :color ,lambda-inactive
                                                          :style nil)
                                                    :overline nil
                                                    :underline nil)))

   `(lambda-modeline-inactive-status-RW ((,class :foreground ,lambda-subtle
                                                  :background ,lambda-inactive
                                                  :box (:line-width 1
                                                        :color ,lambda-inactive
                                                        :style nil)
                                                  :overline nil
                                                  :underline nil)))

   `(lambda-modeline-inactive-status-**  ((,class :foreground ,lambda-subtle
                                                   :background ,lambda-inactive
                                                   :box (:line-width 1
                                                         :color ,lambda-inactive
                                                         :style nil)
                                                   :overline nil
                                                   :underline nil)))

   (when (not (fboundp 'lambda-modeline))
     `(mode-line ((,class :foreground ,lambda-foreground
                          :background ,lambda-modeline
                          :box (:line-width 3
                                :color ,lambda-modeline
                                :style nil)
                          :overline nil
                          :underline nil))))

   (when (not (fboundp 'lambda-modeline))
     `(mode-line-inactive ((,class :foreground ,lambda-faded
                                   :background ,lambda-modeline
                                   :box (:line-width 3
                                         :color ,lambda-modeline
                                         :style nil)
                                   :overline nil
                                   :underline nil))))

;;;;; Mu4e
   `(mu4e-attach-number-face                      ((,class :foreground ,lambda-strong)))
   `(mu4e-cited-1-face                            ((,class :foreground ,lambda-faded)))
   `(mu4e-cited-2-face                            ((,class :foreground ,lambda-faded)))
   `(mu4e-cited-3-face                            ((,class :foreground ,lambda-faded)))
   `(mu4e-cited-4-face                            ((,class :foreground ,lambda-faded)))
   `(mu4e-cited-5-face                            ((,class :foreground ,lambda-faded)))
   `(mu4e-cited-6-face                            ((,class :foreground ,lambda-faded)))
   `(mu4e-cited-7-face                            ((,class :foreground ,lambda-faded)))
   `(mu4e-compose-header-face                     ((,class :foreground ,lambda-faded)))
   `(mu4e-compose-separator-face                  ((,class :foreground ,lambda-faded)))
   `(mu4e-contact-face                            ((,class :foreground ,lambda-salient)))
   `(mu4e-context-face                            ((,class :foreground ,lambda-faded)))
   `(mu4e-draft-face                              ((,class :foreground ,lambda-faded :weight light :slant italic)))
   `(mu4e-flagged-face                            ((,class :foreground ,lambda-yellow)))
   `(mu4e-footer-face                             ((,class :foreground ,lambda-faded)))
   `(mu4e-forwarded-face                          ((,class :inherit    default)))
   `(mu4e-header-face                             ((,class :inherit    default)))
   `(mu4e-header-highlight-face                   ((,class :inherit highlight)))
   `(mu4e-header-key-face                         ((,class :foreground ,lambda-strong :weight bold)))
   `(mu4e-header-marks-face                       ((,class :foreground ,lambda-faded)))
   `(mu4e-header-title-face                       ((,class :foreground ,lambda-strong)))
   `(mu4e-header-value-face                       ((,class :inherit    default)))
   `(mu4e-highlight-face                          ((,class :foreground ,lambda-salient)))
   `(mu4e-link-face                               ((,class :foreground ,lambda-salient)))
   `(mu4e-modeline-face                           ((,class :foreground ,lambda-modeline)))
   `(mu4e-moved-face                              ((,class :foreground ,lambda-faded)))
   `(mu4e-ok-face                                 ((,class :foreground ,lambda-faded)))
   `(mu4e-region-code                             ((,class :foreground ,lambda-faded)))
   `(mu4e-replied-face                            ((,class :foreground ,lambda-popout)))
   `(mu4e-special-header-value-face               ((,class :inherit    default)))
   `(mu4e-system-face                             ((,class :foreground ,lambda-faded)))
   `(mu4e-title-face                              ((,class :weight bold :foreground ,lambda-popout)))
   `(mu4e-trashed-face                            ((,class :foreground ,lambda-inactive :weight light)))
   `(mu4e-unread-face                             ((,class :inherit    bold)))
   `(mu4e-url-number-face                         ((,class :foreground ,lambda-faded)))
   `(mu4e-view-body-face                          ((,class :inherit    default)))
   `(mu4e-warning-face                            ((,class :foreground ,lambda-critical)))

;;;;; Org-agenda
   `(org-agenda-calendar-event                    ((,class :inherit default)))
   `(org-agenda-calendar-sexp                     ((,class :foreground ,lambda-faded)))
   `(org-agenda-clocking                          ((,class :foreground ,lambda-faded)))
   `(org-agenda-column-dateline                   ((,class :foreground ,lambda-faded)))
   `(org-agenda-current-time                      ((,class :foreground ,lambda-faded)))
   `(org-agenda-date                              ((,class :foreground ,lambda-salient)))
   `(org-agenda-date-today                        ((,class :inherit ,(if lambda-set-variable-pitch 'variable-pitch 'default) :height 1.25 :foreground ,lambda-blue)))
   `(org-super-agenda-header                      ((,class :inherit ,(if lambda-set-variable-pitch 'variable-pitch 'default) :foreground ,lambda-blue)))
   `(org-agenda-date-weekend                      ((,class :foreground ,lambda-faded)))
   `(org-agenda-diary                             ((,class :foreground ,lambda-faded)))
   `(org-agenda-dimmed-todo-face                  ((,class :foreground ,lambda-faded)))
   `(org-agenda-done                              ((,class :foreground ,lambda-faded :strike-through t)))
   `(org-agenda-filter-category                   ((,class :foreground ,lambda-faded)))
   `(org-agenda-filter-effort                     ((,class :foreground ,lambda-faded)))
   `(org-agenda-filter-regexp                     ((,class :foreground ,lambda-faded)))
   `(org-agenda-filter-tags                       ((,class :foreground ,lambda-faded)))
   `(org-agenda-restriction-lock                  ((,class :foreground ,lambda-faded)))
   `(org-agenda-structure                         ((,class :foreground ,lambda-faded)))

;;;;; Org mode
   `(org-archived                                 ((,class :foreground ,lambda-faded)))
   `(org-block                                    ((,class :foreground ,lambda-faded)))
   `(org-block-begin-line                         ((,class :foreground ,lambda-faded)))
   `(org-block-end-line                           ((,class :foreground ,lambda-faded)))
   `(org-checkbox                                 ((,class :foreground ,lambda-faded)))
   `(org-checkbox-statistics-done                 ((,class :foreground ,lambda-faded)))
   `(org-checkbox-statistics-todo                 ((,class :foreground ,lambda-faded)))
   `(org-cite                                     ((,class :foreground ,lambda-salient)))
   `(org-cite-key                                 ((,class :foreground ,lambda-green)))
   `(org-clock-overlay                            ((,class :foreground ,lambda-faded)))
   `(org-code                                     ((,class :foreground ,lambda-faded)))
   `(org-column                                   ((,class :foreground ,lambda-faded)))
   `(org-column-title                             ((,class :foreground ,lambda-faded)))
   `(org-date                                     ((,class :foreground ,lambda-faded)))
   `(org-date-selected                            ((,class :foreground ,lambda-faded)))
   `(org-default                                  ((,class :foreground ,lambda-faded)))
   `(org-document-info                            ((,class :foreground ,lambda-faded :weight light)))
   `(org-document-info-keyword                    ((,class :foreground ,lambda-faded :weight light)))
   `(org-document-title                           ((,class :inherit ,(if lambda-set-variable-pitch 'variable-pitch 'default) :height 1.1 :foreground ,lambda-salient)))
   `(org-done                                     ((,class :foreground ,lambda-faded :strike-through t)))
   `(org-drawer                                   ((,class :foreground ,lambda-faded :weight light)))
   `(org-ellipsis                                 ((,class :foreground ,lambda-faded)))
   `(org-footnote                                 ((,class :foreground ,lambda-faded)))
   `(org-formula                                  ((,class :foreground ,lambda-faded)))
   `(org-habit-alert-face                         ((,class :inherit default)))
   `(org-headline-done                            ((,class :foreground ,lambda-faded)))
   `(org-latex-and-related                        ((,class :foreground ,lambda-faded)))
   `(org-level-1                                  ((,class :inherit 'outline-1)))
   `(org-level-2                                  ((,class :inherit 'outline-2)))
   `(org-level-3                                  ((,class :inherit 'outline-3)))
   `(org-level-4                                  ((,class :inherit 'outline-4)))
   `(org-level-5                                  ((,class :inherit 'outline-5)))
   `(org-level-6                                  ((,class :inherit 'outline-6)))
   `(org-level-7                                  ((,class :inherit 'outline-7)))
   `(org-level-8                                  ((,class :inherit 'outline-8)))
   `(org-link                                     ((,class :foreground ,lambda-salient)))
   `(org-list-dt                                  ((,class :foreground ,lambda-blue)))
   `(org-macro                                    ((,class :foreground ,lambda-faded)))
   `(org-meta-line                                ((,class :foreground ,lambda-faded :weight light)))
   `(org-mode-line-clock                          ((,class :foreground ,lambda-faded)))
   `(org-mode-line-clock-overrun                  ((,class :foreground ,lambda-faded)))
   `(org-priority                                 ((,class :foreground ,lambda-faded)))
   `(org-property-value                           ((,class :foreground ,lambda-faded :weight light)))
   `(org-quote                                    ((,class :foreground ,lambda-salient)))
   `(org-scheduled                                ((,class :foreground ,lambda-salient)))
   `(org-scheduled-previously                     ((,class :foreground ,lambda-salient)))
   `(org-scheduled-today                          ((,class :foreground ,lambda-salient)))
   `(org-sexp-date                                ((,class :foreground ,lambda-faded)))
   `(org-special-keyword                          ((,class :foreground ,lambda-faded :weight light)))
   `(org-table                                    ((,class :inherit    default)))
   `(org-tag                                      ((,class :foreground ,lambda-faded)))
   `(org-tag-group                                ((,class :foreground ,lambda-faded)))
   `(org-target                                   ((,class :foreground ,lambda-faded)))
   `(org-time-grid                                ((,class :foreground ,lambda-faded)))
   `(org-todo                                     ((,class :weight normal :foreground ,lambda-yellow)))
   `(org-upcoming-deadline                        ((,class :foreground ,lambda-strong)))
   `(org-upcoming-distant-deadline                ((,class :foreground ,lambda-foreground)))
   `(org-verbatim                                 ((,class :foreground ,lambda-faded)))
   `(org-verse                                    ((,class :foreground ,lambda-faded)))
   `(org-warning                                  ((,class :foreground ,lambda-popout)))

;;;;; Outline
   `(outline-minor-0      ((,class :background ,lambda-highlight)))
   `(outline-1            ((,class :inherit ,(if lambda-set-variable-pitch 'variable-pitch 'default) :weight normal :foreground ,lambda-green)))
   `(outline-2            ((,class :inherit ,(if lambda-set-variable-pitch 'variable-pitch 'default) :weight normal :foreground ,lambda-blue)))
   `(outline-3            ((,class :inherit ,(if lambda-set-variable-pitch 'variable-pitch 'default) :weight normal :foreground ,lambda-brown)))
   `(outline-4            ((,class :inherit ,(if lambda-set-variable-pitch 'variable-pitch 'default) :weight normal :foreground ,lambda-yellow)))
   `(outline-5            ((,class :inherit outline-1)))
   `(outline-6            ((,class :inherit outline-2)))
   `(outline-7            ((,class :inherit outline-3)))
   `(outline-8            ((,class :inherit outline-4)))

;;;;; Rainbow Delimiters
   `(rainbow-delimiters-depth-1-face ((,class :foreground ,lambda-popout     :weight medium)))
   `(rainbow-delimiters-depth-2-face ((,class :foreground ,lambda-salient    :weight light)))
   `(rainbow-delimiters-depth-3-face ((,class :foreground ,lambda-brown      :weight light)))
   `(rainbow-delimiters-depth-4-face ((,class :foreground ,lambda-yellow     :weight light)))
   `(rainbow-delimiters-depth-5-face ((,class :foreground ,lambda-green      :weight light)))
   `(rainbow-delimiters-depth-6-face ((,class :foreground ,lambda-red        :weight light)))
   `(rainbow-delimiters-depth-7-face ((,class :foreground ,lambda-blue       :weight light)))
   `(rainbow-delimiters-depth-8-face ((,class :foreground ,lambda-faded      :weight light)))
   `(rainbow-delimiters-depth-9-face ((,class :foreground ,lambda-foreground :weight light)))

;;;;; Search
   `(evil-ex-search                               ((,class :background ,lambda-popout)))
   `(isearch                                      ((,class :background ,lambda-popout :foreground ,lambda-highlight :weight bold)))
   `(isearch-fail                                 ((,class :background ,lambda-critical)))
   `(isearch-group-1                              ((,class :background ,lambda-blue)))
   `(isearch-group-2                              ((,class :background ,lambda-red)))
   `(query-replace                                ((,class :background ,lambda-yellow)))

;;;;; Semantic
   `(italic                                       ((,class :slant italic)))
   `(bold                                         ((,class :foreground ,lambda-strong :weight bold)))
   `(bold-italic                                  ((,class :foreground ,lambda-strong :weight bold :slant italic)))
   `(underline                                    ((,class :underline t)))
   `(shadow                                       ((,class :foreground ,lambda-faded)))
   `(success                                      ((,class :foreground ,lambda-salient)))
   `(warning                                      ((,class :foreground ,lambda-popout)))
   `(error                                        ((,class :foreground ,lambda-critical)))
   `(match                                        ((,class :forgeround ,lambda-popout :weight bold)))

;;;;; Speed Bar

   `(speedbar-button-face                         ((,class :foreground ,lambda-faded)))
   `(speedbar-directory-face                      ((,class :foreground ,lambda-foreground :bold t)))
   `(speedbar-file-face                           ((,class :foreground ,lambda-foreground :background ,lambda-background)))
   `(speedbar-highlight-face                      ((,class :foreground ,lambda-highlight)))
   `(speedbar-selected-face                       ((,class :background ,lambda-subtle :bold t)))
   `(speedbar-separator-face                      ((,class :foreground ,lambda-faded)))
   `(speedbar-tag-face                            ((,class :foreground ,lambda-faded)))

;;;;; Tabs
   `(tab-bar-echo-area-tab               ((,class :foreground ,lambda-faded :underline t :weight bold)))
   `(tab-bar-echo-area-tab-group-current ((,class :foreground ,lambda-faded)))

;;;;; Term
   `(term-bold                                    ((,class :foreground ,lambda-strong :weight semi-bold)))
   `(term-color-black                             ((,class :foreground ,lambda-background :background ,lambda-background)))
   `(term-color-white                             ((,class :foreground ,lambda-foreground :background ,lambda-foreground)))
   `(term-color-blue                              ((,class :foreground ,lambda-blue :background ,lambda-blue)))
   `(term-color-cyan                              ((,class :foreground ,lambda-salient :background ,lambda-salient)))
   `(term-color-green                             ((,class :foreground ,lambda-green :background ,lambda-green)))
   `(term-color-magenta                           ((,class :foreground ,lambda-popout :background ,lambda-popout)))
   `(term-color-red                               ((,class :foreground ,lambda-critical :background ,lambda-critical)))
   `(term-color-yellow                            ((,class :foreground ,lambda-yellow :background ,lambda-yellow)))

;;;;; Window Divs
   ;; divide windows more attractively
   `(window-divider                               ((,class :foreground ,lambda-background)))
   `(window-divider-first-pixel                   ((,class :foreground ,lambda-background)))
   `(window-divider-last-pixel                    ((,class :foreground ,lambda-background)))
   ;; divide windows better in terminal
   ;; see https://www.reddit.com/r/emacs/comments/3u0d0u/how_do_i_make_the_vertical_window_divider_more/
   (when (not (display-graphic-p))
     (set-face-background 'vertical-border lambda-background)
     (set-face-foreground 'vertical-border (face-background 'vertical-border)))

;;;;; End Custom faces
   ))

;;;; Define evil cursor colors
(defun lambda--evil-load-cursors ()
  "Load theme specific cursor colors"
  (setq evil-emacs-state-cursor    `(,lambda-salient box))
  (setq evil-normal-state-cursor   `(,lambda-yellow box))
  (setq evil-visual-state-cursor   `(,lambda-faded box))
  (setq evil-insert-state-cursor   `(,lambda-critical (bar . 2)))
  (setq evil-replace-state-cursor  `(,lambda-critical hbar))
  (setq evil-motion-state-cursor   `(,lambda-green box))
  (setq evil-operator-state-cursor `(,lambda-brown hollow)))

(when lambda-set-evil-cursors
  (add-hook 'lambda-after-load-theme-hook #'lambda--evil-load-cursors))

;;;; Set Hl-Todo
;; inherit faces
(setq hl-todo-keyword-faces
      '(("HOLD" .       query-replace)
        ("TODO" .       warning)
        ("NEXT" .       highlight)
        ("OKAY" .       success)
        ("DONT" .       error)
        ("FAIL" .       error)
        ("DONE" .       shadow)
        ("NOTE" .       warning)
        ("KLUDGE" .     warning)
        ("HACK" .       warning)
        ("TEMP" .       warning)
        ("FIXME" .      error)
        ("XXX+" .       error)
        ("BUG" .        error)
        ("REVIEW" .     shadow)
        ("DEPRECATED" . shadow)))

;;;; Set Minibuffer & Echo Area
(defun lambda-theme--minibuffer ()
  "Derive minibuffer / echo area faces from lambda faces."
  ;; Minibuffer / echo area
  (dolist (buffer (list " *Minibuf-0*" " *Echo Area 0*"
                        " *Minibuf-1*" " *Echo Area 1*"))
    (when (get-buffer buffer)
      (with-current-buffer buffer
        (face-remap-add-relative 'default 'fringe)))))
(lambda-theme--minibuffer)

;;; Provide theme

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'lambda)
(provide 'lambda-faces-colors)


;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode 1))
;; End:
;;; lambda-themes.el ends here
