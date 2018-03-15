;;; fuo.el --- feeluown client. -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019 Cosven and contributors
;;
;; Author: cosven <yinshaowen241@gmail.com>
;; URL: http://github.com/cosven/emacs-fuo
;; Package-Requires: ((emacs "24.4"))
;; Version: 1.0beta
;; Keywords: feeluown, multimedia, unix

;; This file is not part of GNU Emacs.

;;; License:

;; Licensed under the same terms as Emacs.

;;; Commentary:

;; Quick Start:
;; start feeluown daemon
;; run fuo-* command

;;; Code:

(require 'subr-x)

(defvar fuo-daemon-proc-name nil "Fuo daemon connection name.")
(defvar fuo-live-lyric-proc-name nil "Fuo live lyric connection name.")

(setq fuo-daemon-proc-name "fuo-conn")
(setq fuo-live-lyric-proc-name "fuo-live-lyric")

(defun fuo--write-to-fuo-buffer (output)
  "Show OUTPUT in *fuo* buffer."
  (switch-to-buffer "*fuo*")
  (fuo-mode)
  (fuoc-mode 1)
  (setq buffer-read-only nil)
  (erase-buffer)
  (insert output)
  (setq buffer-read-only t)
  (goto-char 0)
  )

(defun fuo--append-to-fuo-buffer (output)
  "Append OUTPUT to *fuo* buffer."
  (switch-to-buffer "*fuo*")
  (setq buffer-read-only nil)
  (goto-char (point-max))
  (insert output)
  (setq buffer-read-only t)
  (goto-char 0)
  )

(defun fuo-daemon-proc-filter (_proc string)
  "Fuo daemon connection filter function.
Write PROC STRING to fuo buffer."
  (if (or (string-prefix-p "ACK" string) (string-prefix-p "Oops" string))
      (fuo--write-to-fuo-buffer string)
    (fuo--append-to-fuo-buffer string))
  ;; (message string)
  )

(defun fuo-create-connection ()
  "Create a persistent connection to fuo daemon."
  (interactive)
  (if (not (process-status fuo-daemon-proc-name))
      (progn
        (message "Try to create a connection to fuo daemon...")
        (condition-case nil
            (progn
              (open-network-stream fuo-daemon-proc-name nil "localhost" 23333)
              (message (concat (current-message) "done"))
              (set-process-filter (get-process fuo-daemon-proc-name) 'fuo-daemon-proc-filter)
              )
          (error (message (concat (current-message) "failed")))))
    (message "Connection already exists.")))

(defun fuo-close-connection ()
  "Close the connection to fuo daemon."
  (interactive)
  (delete-process fuo-daemon-proc-name))

(defun fuo-get-or-create-connection ()
  "A convenience function for getting connection.
Create a connection if needed."
  (interactive)
  (when (not (process-status fuo-daemon-proc-name))
      (fuo-create-connection))
  (get-process fuo-daemon-proc-name))

;;;###autoload
(defun fuo-run-command (command)
  "Run fuo COMMAND."
  (process-send-string (fuo-get-or-create-connection) (concat command "\n"))
  (process-send-eof))

(defun fuo--is-current-word-uri ()
  "Judge if the current word is a valid uri."
  (string-prefix-p "fuo" (string-trim (thing-at-point 'word t))))

(defun fuo-play-uri ()
  "Play song from `thing-at-point'.
Try to parse a fuo uri from current line and play it."
  (interactive)
  (if (string-prefix-p "fuo" (thing-at-point 'word t))
      (progn
        (message (format "Will play: %s" (thing-at-point 'word t)))
        (fuo-run-command (format "play %s" (thing-at-point 'word t))))
    (message (format "No song found under cursor"))))

(defun fuo-add-uri ()
  "Add uri to current playlist at point;
print msg in the echo area."
  (interactive)
  (if (fuo--is-current-word-uri)
      (progn
        (message
         (format "Add %s to current playlist." (thing-at-point 'word)))
        (fuo-run-command (format "add %s" (thing-at-point 'word)))
        (fuo-list)  ;; TODO: more elegant way to refresh?
        )
    (message "No song found under cursor.")))

(defun fuo-remove-uri ()
  "Remove uri from current playlist at point;
print msg in the echo area."
  (interactive)
  (if (fuo--is-current-word-uri)
      (progn
        (message
         (format "Remove %s from current playlist." (thing-at-point 'word)))
        (fuo-run-command (format "remove %s" (thing-at-point 'word)))
        (fuo-list)
        )
    (message "No song found under cursor.")))

(defun fuo-show-uri ()
  "Show detail of furi.
Parse a fuo uri from current word and show info about it."
  (interactive)
  (when (string-prefix-p "fuo" (thing-at-point 'word t))
    (message (format "Show: %s" (thing-at-point 'word t)))
    (fuo--write-to-fuo-buffer
     (fuo-run-command (format "show %s" (thing-at-point 'word))))))

(defun fuo-status ()
  "Show status of fuo daemon."
  (interactive)
  (fuo-run-command "status"))

(defun fuo-next ()
  "Play next."
  (interactive)
  (message "Will play next song.")
  (fuo-run-command "next"))

(defun fuo-previous ()
  "Play previous."
  (interactive)
  (message "Will play previous song.")
  (fuo-run-command "previous"))

;;;###autoload
(defun fuo-list ()
  "List current playlist."
  (interactive)
  (fuo-run-command "list")
)

(defun fuo-pause ()
  "Pause player."
  (interactive)
  (message "Pause player.")
  (fuo-run-command "pause"))

(defun fuo-resume ()
  "Resum player."
  (interactive)
  (message "Resume player.")
  (fuo-run-command "resume"))

(defun fuo-toggle ()
  "Pause."
  (interactive)
  (message "Toggle player.")
  (fuo-run-command "toggle"))

(defun fuo-clear ()
  "Clear current playlist."
  (interactive)
  (message "Clear current playlist.")
  (fuo-run-command "clear"))

;;;###autoload
(defun fuo-search ()
  "Search songs."
  (interactive)
  (fuo-run-command
   (format "search %s" (read-string "Fuo search: "))))

;;;###autoload
(defun fuo-live-lyric ()
  "Live lyric."
  (interactive)
  (if (not (process-status fuo-live-lyric-proc-name))
      (progn
        (message "Try to create a connection to fuo live lyric...")
        (condition-case nil
            (progn
              (open-network-stream fuo-live-lyric-proc-name "*fuo-live-lyric*" "localhost" 23334)
              (message (concat (current-message) "done"))
              (process-send-string (get-process fuo-live-lyric-proc-name) "sub topic.live_lyric\n")
              (switch-to-buffer "*fuo-live-lyric*")
              )
          (error (message (concat (current-message) "failed")))))
    (message "Connection already exists.")))



(defvar fuo-mode-hook nil)
(defvar fuo-mode-syntax-table nil "Syntax table for `fuo-mode'.")
(defvar fuo-highlights nil)

(setq fuo-highlights
      '(("ACK \\(\\w+\\) \\(.*\\)" . ((1 font-lock-function-name-face) (2 font-lock-doc-face)))
        ("ACK\\|Oops\\|OK" . font-lock-constant-face)))

(setq fuo-mode-syntax-table
      (let ( (syntax-table (make-syntax-table)))
        ;; python style comment: “# …”
        (modify-syntax-entry ?# "<" syntax-table)
        (modify-syntax-entry ?\n ">" syntax-table)
        (modify-syntax-entry ?: "w" syntax-table)
        (modify-syntax-entry ?/ "w" syntax-table)
        syntax-table))

;;;###autoload
(define-derived-mode fuo-mode special-mode "Fuo"
  "A mode for fuo."
  ;; (use-local-map fuo-mode-map)
  (setq font-lock-defaults '(fuo-highlights))
  (set-syntax-table fuo-mode-syntax-table))


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.fuo\\'" . fuo-mode))

;; -----------------------------------
;; -----------------------------------
;; -----------------------------------

(defvar fuoc-mode-map nil "Keymap for `fuoc-mode'.")

(progn
  (setq fuoc-mode-map (make-sparse-keymap))
  ;; FIXME: let user do the customization themsevles?
  ;; or some other more reasonable shortcuts?
  (define-key fuoc-mode-map (kbd "<return>") 'fuo-play-uri)
  (define-key fuoc-mode-map (kbd "RET") 'fuo-play-uri)
  (define-key fuoc-mode-map (kbd "SPC") 'fuo-show-uri)
  (define-key fuoc-mode-map (kbd "a") 'fuo-add-uri)
  (define-key fuoc-mode-map (kbd "d") 'fuo-remove-uri)
  (define-key fuoc-mode-map (kbd "s") 'fuo-search)
  ;; NOTE: put these in README is enough?
  ;; (define-key fuo-mode-map (kbd "n") 'fuo-next)
  ;; (define-key fuo-mode-map (kbd "N") 'fuo-previous)
  (define-key fuoc-mode-map (kbd "t") 'fuo-toggle)
  ;; (define-key fuo-mode-map (kbd "r") 'fuo-resume)
  ;; (define-key fuo-mode-map (kbd "p") 'fuo-pause)
  (define-key fuoc-mode-map (kbd "l") 'fuo-list)
)

;;;###autoload
(define-minor-mode fuoc-mode
  "Fuo control minor mode."
  :lighter " Fuoc"
  :keymap fuoc-mode-map)

(provide 'fuo)
;;; fuo.el ends here
