;;; notion.el --- Tight integration of emacs with the notion window manager

;; Copyright (C) 2005-2006 by Stefan Reichör

;; Filename: notion.el
;; Author: Stefan Reichör, <stefan@xsteve.at>

;; notion.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; notion.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary

;; notion.el is an emacs interface for the notion window manager

;; You need mod_notionflux-3 (at least from 2005-04-21)
;; mod_notionflux-3 can be found here: http://modeemi.fi/~tuomov/repos/

;; Put the following in your .emacs to make the scrollwm-mode function available:
;; (autoload 'scrollwm-mode "notion" "Major mode to edit notion config files" t)

;; The latest version of notion.el can be found at http://www.xsteve.at/prg/emacs/notion.el

;; Comments / suggestions welcome!

;;; Todo
;;  * Better error handling - at the moment they are only shown on the
;;    terminal, where notion was started

;;; History:
;;

;;; Code:

;; --------------------------------------------------------------------------------
;; notion interaction via notionflux
;; --------------------------------------------------------------------------------

(defvar scrollwm-display-target ":0"
  "The DISPLAY to target. Useful when debugging a separate notion in eg. a Xephyr server")

(defconst scrollwm--helper-path
  (concat (file-name-directory (or load-file-name buffer-file-name))
          "emacs.js"))

(defconst scrollwm-documentation-url
  "http://notion.sourceforge.net/notionconf/")

(defun scrollwm--name-at-point ()
  "Get current Name { ['.'|':'} Name } sequence."
  ;; Taken from lua-mode.el
  ;; NB: copying/modifying syntax table for each call may incur a penalty
  (with-syntax-table (copy-syntax-table)
    (modify-syntax-entry ?. "_")
    (modify-syntax-entry ?: "_")
    (current-word t)))

(defun scrollwm-run-interactively (cmd insert-result show-result)
  "Helper that handles common options relevant for interactive commands"

  (let ((result (scrollwm-run cmd)))

    (when insert-result
      (save-excursion
        (end-of-line)

        (if (eobp)
            (newline)
          (forward-line))

        (while (looking-at "//: ")
          (kill-whole-line))

        (newline)
        (forward-line -1)

        (insert (replace-regexp-in-string "^" "//: " result))
        ))

    (when show-result
      (message result))

    result))

(defun scrollwm-run (cmd)
  (unless (car (scrollwm--run "emacs"))
    ;; send init code
    (with-temp-buffer
      (insert-file-contents scrollwm--helper-path)
      (scrollwm--run (buffer-string))))

  (let* ((response (scrollwm--run cmd))
         (success (car response))
         (result (cadr response)))

    (if (string-empty-p result)
        (pp-to-string success)
      result)))

(defun scrollwm--run (cmd)
  (dbus-call-method :session "org.scrollwm.Shell" "/org/scrollwm/Shell"
                    "org.scrollwm.Shell" "Eval" cmd))

(defun scrollwm-send-string (str)
  "Send STR to notion, using the notionflux program."
  (scrollwm-run str))

(defun scrollwm-send-region (start end &optional insert-result)
  "Send send the region to notion, using the notionflux program."
  (interactive "r\nP")
  (scrollwm-run-interactively (buffer-substring start end)
                                          insert-result (called-interactively-p)))

(defun scrollwm-send-current-line (&optional insert-result)
  "Send send the actual line to notion, using the notionflux program."
  (interactive "P")
  (scrollwm-run-interactively (buffer-substring (line-beginning-position) (line-end-position))
                                          insert-result (called-interactively-p)))

(defun scrollwm-repl ()
  (interactive)
  (let (a b)
    (if (region-active-p)
        (setq a (min (point) (mark))
              b (max (point) (mark)))
      (setq a (line-beginning-position)
            b (line-end-position)))

    (let ((cmd (buffer-substring a b)))
      (save-excursion
        (goto-char b)

        (when (eq b (line-beginning-position))
          ;; What I actually want to check for is if the region is active and is
          ;; in "line mode". Then b will be at the line _after_ the last code
          ;; line selected

          ;; Maybe simply back up all blank lines too?
          (forward-line -1))

        (beginning-of-line)
        
        (scrollwm-run-interactively cmd t nil))
      )))

(defun scrollwm-send-proc ()
  "Send proc around point to notion."
  (interactive)
  (let (start end)
    (save-excursion
      (lua-beginning-of-proc)
      (setq start (point))
      (lua-end-of-proc)
      (setq end (point)))
    (scrollwm-send-region start end)))

(defun scrollwm-send-buffer ()
  "Send send the buffer content to notion, using the notionflux program."
  (interactive)
  (scrollwm-send-region (point-min) (point-max)))


(defun scrollwm-cmd (cmd &optional insert-result)
  "Send a command to notion.
The command is prefixed by a return statement."
  (interactive "sNotion cmd: \nP")
  (scrollwm-run-interactively cmd insert-result (called-interactively-p)))


;; --------------------------------------------------------------------------------
;; Utility functions that need scrollwm-emacs.lua
;; --------------------------------------------------------------------------------

(defun scrollwm-client-list ()
  "Return the list of the notion clients."
  (let* ((s (scrollwm-cmd "emacs.list_clients()"))
         (s0 (substring s 1 (- (length s) 2)))
         (client-list (split-string s0 "\\\\\n")))
    client-list))


;; (ido-completing-read "notion window: " (scrollwm-client-list) t t nil nil (car (scrollwm-client-list)))

(defun scrollwm-goto-client (name)
  ;;(interactive (list (ido-completing-read "select: " '("a" "aaab" "a/b" "a/b/c" "x/z"))))
  (interactive (list (ido-completing-read "select: " (scrollwm-client-list))))
  (scrollwm-send-string (concat "WRegion.goto(ioncore.lookup_clientwin(\"" name "\"))")))

(defun scrollwm-look-up-function-at-point ()
  (interactive)
  ;; Documentation still uses ioncore instead of notioncore
  (let* ((funcname (replace-regexp-in-string "^notioncore\\." "ioncore."
                                             (scrollwm--name-at-point)))
         (lua-req (format "return emacs.canonical_funcname(\"%s\")" funcname))
         (canonical-funcname (read (scrollwm-send-string lua-req))) ;; CLEANUP
         (url (concat scrollwm-documentation-url
                      "node7.html#fn:" canonical-funcname)))
    (browse-url url))
  )

(defun scrollwm--resolve-lua-source-file (relative-path)
  ;; Byte compiled lua files contain file _name_ at best
  (let* ((candidates
          (remove-if-not (lambda (project-file-path)
                           (string-suffix-p relative-path project-file-path))
                         (projectile-current-project-files)))
         (project-file (if (rest candidates)
                           (completing-read "Source file" candidates)
                         (first candidates))))
    (if project-file
        (concat (projectile-project-root) project-file)
      (helm-find-files-1 relative-path))))

(defun scrollwm-goto-definition (function-name)
  (interactive (list (scrollwm--name-at-point)))
  ;; Hackety-hack...
  (let* ((raw (scrollwm-send-string (format "return emacs.defined_at(\"%s\")" function-name)))
         (as-string (and raw (read raw)))
         (location (and as-string (read as-string))))

    (when location
      (let* ((path          (car location))
             (line-number   (cadr location))
             (resolved-path (if (file-name-absolute-p path)
                                path
                              (scrollwm--resolve-lua-source-file path))))
        (when resolved-path
          (find-file resolved-path)
          (goto-line line-number)
          resolved-path)))))

;; --------------------------------------------------------------------------------
;; The notion edit mode, based on lua mode
;; --------------------------------------------------------------------------------

(defvar scrollwm-mode-map () "Keymap used in `scrollwm-mode' buffers.")

(when (not scrollwm-mode-map)
  (setq scrollwm-mode-map (make-sparse-keymap))
  (define-key scrollwm-mode-map [(control ?c) (control ?p)] 'scrollwm-send-proc)
  (define-key scrollwm-mode-map [(control ?c) (control ?r)] 'scrollwm-send-region)
  (define-key scrollwm-mode-map [(control ?c) (control ?b)] 'scrollwm-send-buffer)
  (define-key scrollwm-mode-map [(control ?c) (control ?l)] 'scrollwm-send-line)
  (define-key scrollwm-mode-map (kbd "C-<return>") 'scrollwm-repl)
  )

(easy-menu-define scrollwm-mode-menu scrollwm-mode-map
"'scrollwm-mode' menu"
                  '("Notion"
                    ("Interaction"
                    ["Send Procedure" scrollwm-send-proc t]
                    ["Send Region" scrollwm-send-region t]
                    ["Send Buffer" scrollwm-send-buffer t]
                    ["Send String" scrollwm-send-string t]
                    ["Send Line" scrollwm-send-line t]
                    )
                    ["Goto client" scrollwm-goto-client t]
                    ))

(define-derived-mode scrollwm-mode js2-mode "scrollwm"
  "scrollwm-mode provides a tight integration of emacs and scrollwm.
"
  (use-local-map scrollwm-mode-map))

;; --------------------------------------------------------------------------------
;; various stuff for testing purposes
;; --------------------------------------------------------------------------------


;; (scrollwm-send-string "ioncore.goto_next_screen()")
;; (scrollwm-cmd "ioncore.goto_next_screen()")

;;(defun scrollwm-show-message-for-cmd (cmd)
;;  (interactive "snotion command: ")
;;  (scrollwm-run (concat "mod_query.message(ioncore.find_screen_id(0)," cmd ")")))


;; (scrollwm-client-list)


;; (scrollwm-show-message-for-cmd "ioncore.version()")
;; (scrollwm-send-string "return ioncore.version()")
;; (scrollwm-send-string "return 4+5")

;; (scrollwm-cmd "ioncore.version()")
;; (scrollwm-cmd "4+5")

 ;; (setenv "NOTIONFLUX_SOCKET" "/tmp/fileM5J57y")

;; things to support
;;table ioncore.clientwin_list()

;; WClientWin ioncore.lookup_clientwin(string name)

;; bool WRegion.goto(WRegion reg)

(provide 'scrollwm-mode)

;;; notion.el ends here

;; arch-tag: 17c5fcf9-ea23-4ca5-b7d5-a0635b8b4230


