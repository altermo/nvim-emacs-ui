;; -*- lexical-binding: t; -*-

(require 'mpack)
(require 'nvim-key-conv)
(require 'seq)
(defun nvim-ui--get-size ()
  (let ((wins (get-buffer-window-list (process-buffer nvim-ui-proc))))
    (let ((width (frame-width)) (height (frame-height)))
      (dolist (win wins (progn
                          (setq width (1- width))
                          (setq height (1- height))
                          (setq-local nvim-ui-width width)
                          (setq-local nvim-ui-height height)
                          (cons width height)))
        (if (< (window-width win) width)
          (setq width (window-width win)))
        (if (< (window-height win) height)
          (setq height (window-height win)))))))

(defun nvim-ui--handle-event-queue ()
  (dolist (event (nreverse nvim-ui-event-queue))
    (let ((m (aref event 0))
          (args (seq-subseq event 1)))
      (cond
        ((equal m "grid_line")
         (seq-doseq
           (arg args)
           (let* ((grid (aref arg 0))
                  (row (aref arg 1))
                  (col_start (aref arg 2))
                  (cells (aref arg 3))
                  (wrap (aref arg 4))
                  (pos (+ (* (1+ nvim-ui-width) row) col_start 1))
                  face)
             (with-current-buffer
               (process-buffer nvim-ui-proc)
               (let ((inhibit-read-only t))
                 (seq-doseq
                   (cell cells)
                   (goto-char pos)
                   (let ((text (aref cell 0))
                         (hl-id (if (>= (length cell) 2) (aref cell 1)))
                         (rep (if (>= (length cell) 3) (aref cell 2) 1)))
                     (if hl-id
                       (setq face (gethash hl-id nvim-ui-hl-tbl)))
                     (delete-char rep)
                     (dotimes (_ rep)
                       (insert (propertize text 'face face)))
                     (setq pos (+ pos rep))
                     )))))))
        ((equal m "grid_scroll")
         (seq-doseq
           (arg args)
           (let* ((gird (aref arg 0))
                  (top (aref arg 1))
                  (bot (aref arg 2))
                  (left (aref arg 3))
                  (right (aref arg 4))
                  (rows (aref arg 5))
                  (cols (aref arg 6)))
             (with-current-buffer
               (process-buffer nvim-ui-proc)
               (let ((inhibit-read-only t))
                 (dotimes (row (- bot top (abs rows)))
                   (let* ((dst-row (if (< rows 0) (- bot 1 row) (+ top row)))
                          (src-row (+ dst-row rows))
                          (dst-pos (+ (* dst-row (1+ nvim-ui-width)) left 1))
                          (src-pos (+ (* src-row (1+ nvim-ui-width)) left 1))
                          (str (buffer-substring src-pos (+ src-pos (- right left)))))
                     (goto-char dst-pos)
                     (delete-char (- right left))
                     (insert str)
                     )))))))
        ((equal m "grid_cursor_goto")
         (seq-doseq
           (arg args)
           (let* ((grid (aref arg 0))
                  (row (aref arg 1))
                  (col (aref arg 2))
                  (pos (+ (* (1+ nvim-ui-width) row) col 1)))
             (setq-local nvim-ui-point pos)
             )))
        ((equal m "hl_attr_define")
         (seq-doseq
           (arg args)
           (let* ((id (aref arg 0))
                  (rgb_attr (aref arg 1))
                  (cterm_attr (aref arg 2))
                  (info (aref arg 3))

                  (foreground (plist-get rgb_attr "foreground" 'equal))
                  (background (plist-get rgb_attr "background" 'equal))
                  (special (plist-get rgb_attr "special" 'equal))
                  underline
                  face)
             (if foreground
               (setq foreground (format "#%06x" foreground)))
             (if background
               (setq background (format "#%06x" background)))
             (if special
               (setq special (format "#%06x" special)))
             (if (plist-get rgb_attr "reverse" 'equal)
               (let ((tmp foreground))
                 (setq foreground background)
                 (setq background tmp)))
             (if (plist-get rgb_attr "italic" 'equal)
               (setq face (plist-put face :slant 'italic)))
             (if (plist-get rgb_attr "bold" 'equal)
               (setq face (plist-put face :weight 'bold)))
             (if (plist-get rgb_attr "strikethrough" 'equal)
               (setq face (plist-put face :strike-through t)))
             (cond
               ((plist-get rgb_attr "underline" 'equal)
                (setq underline 'line))
               ((plist-get rgb_attr "undercurl" 'equal)
                (setq underline 'wave)))
             (if underline
               (setq face (plist-put face :underline
                                     (list
                                       :color (or special 'foreground-color)
                                       :style underline))))
             (if foreground
               (setq face (plist-put face :foreground foreground)))
             (if background
               (setq face (plist-put face :background background)))
             (puthash id face nvim-ui-hl-tbl)
             )))
        ((equal m "default_colors_set")
         (seq-doseq
           (arg args)
           (let ((rgb_fg (aref arg 0))
                 (rgb_bg (aref arg 1))
                 (rgb_sp (aref arg 2))
                 (cterm_fg (aref arg 3))
                 (cterm_bg (aref arg 4)))
             (with-current-buffer
               (process-buffer nvim-ui-proc)
               (setq-local face-remapping-alist
                           `((default
                               :foreground ,(format "#%06x" rgb_fg)
                               :background ,(format "#%06x" rgb_bg))))
               ))))
        )))
  (if (boundp 'nvim-ui-point)
    (with-current-buffer
      (process-buffer nvim-ui-proc)
      (goto-char nvim-ui-point)))
  (setq nvim-ui-event-queue '()))

(defun nvim-ui--proc-filter (proc out)
  (dolist (data (mpack-decode-multi out))
    (when (and (vectorp data) (= (length data) 3)
               (= (aref data 0) 2)
               (equal (aref data 1) "redraw")
               (vectorp (aref data 2)))
      (seq-doseq
        (event (aref data 2))
        (if (equal (aref event 0) "flush")
          (nvim-ui--handle-event-queue)
          (setq nvim-ui-event-queue
                (cons event nvim-ui-event-queue)))))))
; (defun nvim-ui--proc-filter (proc out)
;   (internal-default-process-filter proc (prin1-to-string (mpack-decode-multi out))))

(defun nvim-ui-notify (command &rest args)
  (process-send-string nvim-ui-proc (mpack-encode (vector 2 command (vconcat args)))))

(defvar nvim-ui-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key
      map [t]
      (lambda ()
        (interactive)
        (let ((key (nvim-key-conv (this-command-keys-vector))))
          (if (> (length key) 0)
            (nvim-ui-notify "nvim_input" key)))))
    (define-key
      map (kbd "C-1 C-2")
      (lambda ()
        (interactive)
        (use-local-map nvim-ui-mode-normal-map)))
    (define-key
      map (kbd "C-1 C-1")
      (lambda ()
        (interactive)
        (nvim-ui-notify "nvim_input" "<C-1>")))
    map))

(defvar nvim-ui-mode-normal-map
  (let ((map (make-sparse-keymap)))
    (define-key
      map (kbd "C-1")
      (lambda ()
        (interactive)
        (use-local-map nvim-ui-mode-map)))
    map))

(define-derived-mode
  nvim-ui-mode fundamental-mode "nvim-ui" ""
  (buffer-disable-undo)
  (setq buffer-read-only t)
  (setq-local nvim-ui-proc (make-process
                             :name "nvim"
                             :buffer (current-buffer)
                             :filter #'nvim-ui--proc-filter
                             :connection-type 'pipe
                             :coding 'no-conversion
                             :command '("nvim" "--embed")))
  (setq-local nvim-ui-event-queue '())
  (setq-local nvim-ui-hl-tbl (make-hash-table))

  (let* ((size (nvim-ui--get-size))
         (width (car size))
         (height (cdr size)))
    (let ((inhibit-read-only t))
      (dotimes (_ height)
        (insert (make-string width 32))
        (insert "\n")))
    (beginning-of-buffer)
    (nvim-ui-notify
      "nvim_ui_attach" width height '(
                                      "ext_linegrid" t
                                      )))
  ; TODO: a hook to refresh size
  )

(defun nvim ()
  (interactive)
  (switch-to-buffer (generate-new-buffer "*nvim*"))
  (nvim-ui-mode))

(provide 'nvim-ui)
