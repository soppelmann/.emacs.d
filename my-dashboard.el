;; Config

(dashboard-setup-startup-hook)

(setq dashboard-footer-messages
      (with-temp-buffer
        (insert-file-contents "~/.emacs.d/theo")
        (split-string (buffer-string) "\n" t)))

(setq dashboard-item-shortcuts
      '((recents . "r")
        (bookmarks . "m")
        (projects . "p")
        (agenda . "a")
        (registers . "e")
        (commands . "c")))


(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
(setq dashboard-center-content t)
;(split-window-config)
(defun dashboard-insert-custom (list-size)
  (dashboard-insert-heading
   "Commands:"
   "c") ; Optional heading for the widget
  (insert "\n")
  (dashboard-insert-shortcut 'commands "c" "Commands:")

  (let ((button-text "    Layout\n"))
    (put-text-property 0 (length button-text) 'mouse-face 'highlight
                       button-text)
    (put-text-property 0 (length button-text) 'keymap
                       (let ((map (make-sparse-keymap)))
                         (define-key map (kbd "RET") 'split-window-config)
                         map)
                       button-text)
    (insert button-text))
  (let ((button-text "    Refresh dashboard\n"))
    (put-text-property 0 (length button-text) 'mouse-face 'highlight
                       button-text)
    (put-text-property 0 (length button-text) 'keymap
                       (let ((map (make-sparse-keymap)))
                         (define-key
                          map (kbd "RET") 'dashboard-refresh-buffer)
                         map)
                       button-text)
    (insert button-text))
  (let ((button-text "    Ranger"))
    (put-text-property 0 (length button-text) 'mouse-face 'highlight
                       button-text)
    (put-text-property 0 (length button-text) 'keymap
                       (let ((map (make-sparse-keymap)))
                         (define-key
                          map (kbd "RET") 'ranger)
                         map)
                       button-text)
    (insert button-text))
  )


(add-to-list 'dashboard-item-generators '(commands . dashboard-insert-custom))
(add-to-list 'dashboard-items '(commands) t)
(dashboard-insert-shortcut (dashboard-get-shortcut 'commands) "c" "Commands:")

(setq dashboard-items
      '((commands . 5)
        (recents . 5)
        (bookmarks . 5)
        (registers . 5)))

(setq dashboard-set-navigator t)
