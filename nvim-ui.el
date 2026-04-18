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
                  (pos (+ (* (1+ nvim-ui-width) row) col_start 1)))
             (with-current-buffer
               (process-buffer nvim-ui-proc)
               (let ((inhibit-read-only t))
                 (seq-doseq
                   (cell cells)
                   (goto-char pos)
                   (let ((text (aref cell 0))
                         (rep (if (>= (length cell) 3) (aref cell 2) 1)))
                     (delete-char rep)
                     (dotimes (_ rep)
                       (insert text))
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
    map))

(define-derived-mode
  nvim-ui-mode fundamental-mode "nvim-ui" ""
  (setq-local nvim-ui-proc (make-process
                             :name "nvim"
                             :buffer (current-buffer)
                             :filter #'nvim-ui--proc-filter
                             :connection-type 'pipe
                             :coding 'no-conversion
                             :command '("nvim" "--embed")))
  (setq-local nvim-ui-event-queue '())

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
  ; TODO: a hook to close the proces on mode exit/buffer delete
  ; TODO: a hook to refresh size
  )

(provide 'nvim-ui)
