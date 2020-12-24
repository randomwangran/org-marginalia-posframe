;;; org-marginalia-posframe.el --- Show the margin note at point  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Ran Wang

;; Author: Ran Wang
;; URL: https://github.com/randomwangran/org-marginalia-posframe
;; Version: 0.0.0
;; Last Modified: 
;; Package-Requires: ((emacs "27.1") (org "9.4"))
;; Keywords: org-mode, annotation, writing, note-taking, margin-notes

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package shows your margin notes (marginalia) at point

;;;; Installation

;;;;; MELPA

;; This package is not available on MELPA. Manual installation required.

;;;;; Manual

;; Install these required packages:

;; Ensure to have Org Mode 9.4 or later (tested on 9.4).  This package uses
;; `org-collect-keywords', which does not exist in an earlier version.

;; Then put this file in your load-path, and put this in your init
;; file:
;; (require 'org-marginalia-posframe)

(require 'posframe)
(require 'org-marginalia)

(defvar org-marginalia-posframe-buffer " *org-marginalia-posframe-buffer*")

(defun org-marginalia-show-posframe (point)
  (interactive "d")
  (let ((id (get-char-property point 'om/id)))
    (progn
     (org-marginalia--get-contents id)
     (with-current-buffer (get-buffer-create org-marginalia-posframe-buffer)
       (erase-buffer)
       (insert org-marginalia--contents))))
  (when (posframe-workable-p)
    (posframe-show org-marginalia-posframe-buffer
                   :position (point)
                   :internal-border-width 2
                   :background-color "#93937070DBDB"
                   ))
  ;; if user press C-g, it will not clear the posframe
  ;;
  ;; if the posframe stick to your screen, please use
  ;;
  ;; M-x posframe-delete-all
  ;;
  ;; to clear them.
  (sit-for 2)
  (posframe-delete " *org-marginalia-posframe-buffer*"))

(defun org-marginalia--get-contents (id)
  (switch-to-buffer
   (find-file-noselect "~/Documents/marginalia.org"))
  (save-excursion
    (goto-char (org-find-property om/prop-id id))
    (save-excursion
     ;; If inside heading contents, move the point back to the heading
     ;; otherwise `org-agenda-get-some-entry-text' won't work.
     (unless (org-on-heading-p) (org-previous-visible-heading 1))
     (setq org-marginalia--contents (substring-no-properties
                      (org-agenda-get-some-entry-text
                       (point-marker)
                       most-positive-fixnum)))))
  (previous-buffer))

(defun org-marginalia--check-has-marginalia-note-p (point)
  (interactive "d")
  (get-char-property point 'om/id))


(defun om/next (point)
  "Look at the current point, and move to the next highlight, if any.
If there is none below the point, but there is a highlight in the
buffer, go back to the first one."
  (interactive "d")
  (let ((after-exec-point))
    (if (not om/highlights) (message "No highlights present in this buffer.")
      (if (om/find-next-highlight)
          (progn
            (goto-char (om/find-next-highlight))
            (setq after-exec-point (point))
            (org-marginalia-show-posframe after-exec-point))
               (message "Nothing done. No more visible highlights exist")))))

(defun om/prev (point)
  "Look at the current point, and move to the previous highlight, if any.
If there is none above the point, but there is a highlight in the
buffer, go back to the last one."
  (interactive "d")
  (if (not om/highlights) (message "No highlights present in this buffer.")
    (if (om/find-prev-highlight)
        (progn (goto-char (om/find-prev-highlight))
               (setq after-exec-point (point))
               (org-marginalia-show-posframe after-exec-point))
      (message "Nothing done. No more visible highlights exist"))))

(define-key org-marginalia-mode-map (kbd "C-c }") #'om/next)
(define-key org-marginalia-mode-map (kbd "C-c {") #'om/prev)

(provide 'org-marginalia-posframe)

;;; org-marginalia-posframe.el ends here
