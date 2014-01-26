;;; moomin.el --- Edit MoinMoin with Emacs

;; Copyright (C) 2014  Toshiyuki Takahashi

;; Author: Toshiyuki Takahashi (@tototoshi)
;; Keywords: MoinMoin

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

;;; Usage:
;;;
;;; (require 'moomin)
;;;
;;; (setq moomin-wiki-url-base "http://your.moinmoin/wiki")
;;;
;;; (setq moomin-user "user")
;;; (setq moomin-password "password")
;;;
;;; ;; When your moin wiki requires basic authentication
;;; (setq moomin-basic-auth-user "user")
;;; (setq moomin-basic-auth-password "password")
;;;
;;; ;; Assign keybind to 'helm-moomin and 'moomin-save-current-buffer as you like
;;; (global-set-key (kbd "C-x w") 'helm-moomin)
;;; (add-hook 'moinmoin-mode-hook
;;;   (lambda ()
;;;     (define-key moinmoin-mode-map (kbd "C-c C-c") 'moomin-save-current-buffer)))
;;;
;;;
;;; Code:
;;;

(require 'screen-lines) ;; moinmoin-mode requires this. available from elpa.
(require 'moinmoin-mode)
(require 'request) ;; available from elpa
(require 'helm)    ;; available from elpa

(defvar moomin-basic-auth-user nil)
(defvar moomin-basic-auth-password nil)
(defvar moomin-wiki-url-base nil)
(defvar moomin-user nil)
(defvar moomin-password nil)

;; This function is picked up from http-get.el
;;
;; URL encoding for parameters
(defun moomin-http-url-encode (str content-type)
  "URL encode STR using CONTENT-TYPE as the coding system."
  (apply 'concat
         (mapcar (lambda (c)
                   (if (or (and (>= c ?a) (<= c ?z))
                           (and (>= c ?A) (<= c ?Z))
                           (and (>= c ?0) (<= c ?9)))
                       (string c)
                     (format "%%%02x" c)))
                 (encode-coding-string str content-type))))

(defun moomin-http-url-encode-alist (alist)
  (loop for sep = "" then "&"
          for (k . v) in alist
          concat sep
          concat (moomin-http-url-encode (format "%s" k) 'utf-8)
          concat "="
          concat (moomin-http-url-encode v 'utf-8)))

(defun moomin-basic-auth-header ()
  (concat "Basic" " "
          (base64-encode-string
           (concat moomin-basic-auth-user ":" moomin-basic-auth-password))))

(defun moomin-request-headers ()
  (cond ((and moomin-basic-auth-user moomin-basic-auth-password)
         `(("Authorization" . ,(moomin-basic-auth-header))
           ("User-Agent" . "emacs-request")))
        (t '((("User-Agent" . "emacs-request"))))))

(defun moomin-make-local-variables (page rev ticket)
  (set (make-local-variable 'moomin-current-buffer-page-name) page)
  (set (make-local-variable 'moomin-current-buffer-rev) rev)
  (set (make-local-variable 'moomin-current-buffer-ticket-token) ticket))

(defun moomin-html-unescape ()
  (let ((mapping-table-assoc '(("&lt;" . "<")
                               ("&gt;" . ">")
                               ("&amp;" . "&"))))
    (dolist (pair mapping-table-assoc)
      (goto-char (point-min))
      (replace-string (car pair) (cdr pair))))
  (goto-char (point-min)))

(defun moomin-extract-textarea ()
  (goto-char (point-min))
  (re-search-forward "<textarea")
  (re-search-forward ">")
  (delete-region (point-min) (point))
  (goto-char (point-max))
  (re-search-backward "</textarea>")
  (delete-region (point) (point-max))
  (goto-char (point-min))
  (moomin-html-unescape))

(defun moomin-extract-ticket ()
  (save-excursion
    (let ((start (progn (goto-char (point-min))
                      (re-search-forward "name=\"ticket\"")
                      (re-search-forward "value=\"")
                      (point)))
        (end (progn
               (re-search-forward "\"")
               (- (point) 1))))
      (buffer-substring-no-properties start end))))

(defun moomin-extract-rev-token ()
  (save-excursion
    (let ((start (progn (goto-char (point-min))
                      (re-search-forward "name=\"rev\"")
                      (re-search-forward "value=\"")
                      (point)))
        (end (progn
               (re-search-forward "\"")
               (- (point) 1))))
      (buffer-substring-no-properties start end))))

(defun moomin-flash-buffer-with-response-data (page data)
  (switch-to-buffer (get-buffer-create (concat "MoinMoin:" page)))
  (insert data)
  (let ((ticket (moomin-extract-ticket))
        (rev (moomin-extract-rev-token)))
    (moomin-extract-textarea)
    (moinmoin-mode)
    (moomin-make-local-variables page rev ticket)))

(defun moomin-wiki-url (page)
  (concat moomin-wiki-url-base "/"
          (moomin-http-url-encode "Arch Linux" 'utf-8)))

(defun moomin-login ()
  (request
   moomin-wiki-url-base
   :type "POST"
   :data (moomin-http-url-encode-alist `((action . "login")
                                         (name . ,moomin-user)
                                         (password . ,moomin-password)
                                         (login . "Login")
                                         (login . "Login")))
   :headers (moomin-request-headers)
   :parser 'buffer-string
   :sync t))

(defun moomin-get-current-revision (page)
  (let ((rev nil))
    (request
     (moomin-wiki-url page)
     :type "GET"
     :params '((action . "edit")
               (editor . "text"))
     :headers (moomin-request-headers)
     :parser 'buffer-string
     :sync t
     :success (function*
               (lambda (&key data &allow-other-keys)
                 (with-temp-buffer
                   (progn
                     (insert data)
                     (setq rev (moomin-extract-rev-token)))))))
    rev))

(defun moomin-get-page (page)
  (moomin-login)
  (request
   (moomin-wiki-url page)
   :type "GET"
   :params '((action . "edit")
             (editor . "text"))
   :headers (moomin-request-headers)
   :parser 'buffer-string
   :sync t
   :success (function*
             (lambda (&key data &allow-other-keys)
               (moomin-flash-buffer-with-response-data page data)))))

(defun moomin-save-page (page text rev ticket)
  (moomin-login)
  (cond ((not (string= moomin-current-buffer-rev (moomin-get-current-revision page)))
         (message "Oops! Failed to save changes. Someone has already editted this page"))
        (t (request
            (moomin-wiki-url page)
            :type "POST"
            :data (moomin-http-url-encode-alist
                   `((action . "edit")
                     (rev . ,rev)
                     (ticket . ,ticket)
                     (button_save . "Save+Changes")
                     (editor . "text")
                     (savetext . ,text)
                     (comment . "")
                     (category . "")))
            :headers (moomin-request-headers)
            :parser 'buffer-string
            :sync t
            :success (function*
                      (lambda (&key data &allow-other-keys)
                        (let ((p (point)))
                          (moomin-get-page page)
                          (goto-char p))
                        (message "Save changes.")))))))

(defun moomin-save-current-buffer ()
  (interactive)
  (moomin-save-page
   moomin-current-buffer-page-name
   (buffer-substring-no-properties (point-min) (point-max))
   moomin-current-buffer-rev
   moomin-current-buffer-ticket-token))

(defun moomin-browse-url (page)
  (browse-url (concat moomin-wiki-url-base "/" page)))

(defun moomin-get-page-list ()
  (moomin-login)
  (request
   (concat moomin-wiki-url-base "?action=titleindex")
   :type "GET"
   :headers (moomin-request-headers)
   :parser 'buffer-string
   :sync t
   :success (function*
             (lambda (&key data &allow-other-keys)
               (insert data)
               (goto-char (point-min))
               (delete-trailing-whitespace)))))

(setq helm-c-source-moomin-page
      '((name . "MoinMoin wiki page list")
        (init . (lambda ()
                  (with-current-buffer (helm-candidate-buffer 'global)
                    (moomin-get-page-list))))
        (candidates-in-buffer)
        (action
         . (("Edit with emacs" . moomin-get-page)
            ("View" . moomin-browse-url)))))

(defun moomin-create-new-page (page)
  (interactive "sNewPage: ")
  (moomin-get-page page))

(defun helm-moomin ()
  (interactive)
  (helm '(helm-c-source-moomin-page)))

(provide 'moomin)

;;; moomin.el ends here
