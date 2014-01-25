# moomin.el

Edit MoinMoin with Emacs

## Dependency

 * [request](https://github.com/tkf/emacs-request) (Available from ELPA.)
 * [screen-lines](https://github.com/emacsmirror/screen-lines) (moinmoin-mode requires this. Available from ELPA.)
 * [moinmoin-mode](http://moinmo.in/action/raw/EmacsForMoinMoin/MoinMoinMode?action=raw)
 * [helm](https://github.com/emacs-helm/helm) (Available from ELPA.)

## Usage

```lisp
(require 'moomin)

(setq moomin-wiki-url-base "http://your.moinmoin/wiki")

(setq moomin-user "user")
(setq moomin-password "password")

;; When your moin wiki requires basic authentication
(setq moomin-basic-auth-user "user")
(setq moomin-basic-auth-password "password")

;; Assign keybind to 'helm-moomin and 'moomin-save-current-buffer as you like
(global-set-key (kbd "C-x w") 'helm-moomin)
(add-hook 'moinmoin-mode-hook
  (lambda ()
    (define-key moinmoin-mode-map (kbd "C-c C-c") 'moomin-save-current-buffer)))
```
