;;; fuo.el --- fuo client.

;;; Commentary:
; nothing here.

;;; Code:

(defun fuo-run-command (command)
  "Run fuo COMMAND."
  (shell-command-to-string (format "fuocli %s" command)))

(defun fuo--write-to-fuo-buffer (output)
  "Show OUTPUT in *fuo* buffer."
  (switch-to-buffer "*fuo*")
  (fuo-mode)
  (setq buffer-read-only nil)
  (erase-buffer)
  (insert output)
  (goto-char 0)
  (setq buffer-read-only t)
  )

(defun fuo--play-current-line-song ()
  "Play song from `thing-at-point'."
  (interactive)
  (when (string-prefix-p "fuo" (thing-at-point 'line t))
    (fuo-run-command
     (format "play %s"
             (elt (split-string (thing-at-point 'line t)) 0)))))

(defun fuo-play-next ()
  "Play next."
  (interactive)
  (shell-command-to-string "fuocli next"))

(defun fuo-show-current-playlist ()
  "Show fuo current playlist."
  (interactive)
  (let ((result (shell-command-to-string "fuocli list")))
    (fuo--write-to-fuo-buffer result)))

(defun fuo-search ()
  "Search songs."
  (interactive)
  (fuo--write-to-fuo-buffer
   (fuo-run-command
    (format "search %s" (read-string "Fuo search: ")))))

(defvar fuo-mode-map nil "Keymap for `fuo-mode'.")
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
        syntax-table))

(progn
  (setq fuo-mode-map (make-sparse-keymap))
  (define-key fuo-mode-map (kbd "<return>") 'fuo--play-current-line-song)
  (define-key fuo-mode-map (kbd "s") 'fuo-search)
  (define-key fuo-mode-map (kbd "n") 'fuo-play-next)
  (define-key fuo-mode-map (kbd "l") 'fuo-show-current-playlist)
  )

;;;###autoload
(define-derived-mode fuo-mode special-mode "Fuo"
  "A mode for fuo."
  (use-local-map fuo-mode-map)
  (setq font-lock-defaults '(fuo-highlights))
  (set-syntax-table fuo-mode-syntax-table))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.fuo\\'" . fuo-mode))

(provide 'fuo)
;;; fuo.el ends here
