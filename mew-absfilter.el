;;; mew-absfilter.el --- spam filter with bsfilter for Mew

;; Author: SAITO Takuya <tabmore@gmail.com>
;; $Id$

;; You can use, copy, distribute, and/or modify this file for any purpose.
;; There is NO WARRANTY.

;;; Commentary:

;; You can find bsfilter at http://bsfilter.org/

;; To enable spam check after retrieve, put below into your ~/.emacs:
;; (mew-absfilter-mode 1)

;; If you want to do spam checking after shimbun retrieve,
;; do not use `mew-shimbun-retrieve-all' because it kills the shimbun buffer.

;; When you find bsfilter marks the clean message as spam,
;; use "bc" (mew-absfilter-learn-clean) instead of "u" (mew-summary-undo)

;; With "bx" (mew-absfilter-summary-exec-spam), you can process spam mark
;; even in nntp.

;;; History:
;;  v1.38, 2006-06-11
;;   Masayuki Ataka <masayuki.ataka@gmail.com> Support Mew 5
;;  v1.39, 2007-01-17
;;   Masayuki Ataka <masayuki.ataka@gmail.com> Support non-nil mew-use-suffix
;;  v1.40, 2007-01-28
;;   SAITO Takuya <tabmore@gmail.com> tiny fix

;;; Code:

(require 'mew)

;;; spam mark
(defvar mew-absfilter-mark-spam ?\;)

(defvar mew-absfilter-spam-folder "+spam"
  "*Spam folder. Must be a local folder.")

(defface mew-absfilter-face-mark-spam
  '((((class color) (type tty)) (:foreground "green"))
    (((class color) (background light)) (:foreground "DimGray"))
    (((class color) (background dark)) (:foreground "gray"))
    (t nil))
  "*Face to highlight the spam mark"
  :group 'mew-highlight)

(defun mew-absfilter-mark-kill-spam (src msg)
  "Return t if kill summary line."
  (not (string= src mew-absfilter-spam-folder)))

(defun mew-absfilter-mark-exec-spam (src msgs)
  "Refile MSGs from the SRC folder to `mew-absfilter-spam-folder'."
  (unless (string= src mew-absfilter-spam-folder)
    (let ((mew-trash-folder mew-absfilter-spam-folder)
	  (mew-trash-folder-list nil))
      (mew-mark-exec-delete src msgs))))

(defun mew-absfilter-summary-spam-one (&optional no-msg)
  "Put the spam mark(default is ';') on this message."
  (mew-mark-put-mark mew-absfilter-mark-spam no-msg 'valid-only))

;; register spam mark
(add-to-list 'mew-mark-afterstep-spec
	     (list mew-absfilter-mark-spam 2 2 2 2 0 2 0))
(add-to-list 'mew-mark-spec
	     (list mew-absfilter-mark-spam "spam" 2 nil
		   'mew-absfilter-mark-kill-spam nil
		   'mew-absfilter-mark-exec-spam nil))
(add-to-list 'mew-highlight-mark-keywords
	     (cons mew-absfilter-mark-spam 'mew-absfilter-face-mark-spam))


(defvar mew-absfilter-program "bsfilter")
(defvar mew-absfilter-arg-check '("--quiet" "--list-spam"))
(defvar mew-absfilter-arg-clean '("--sub-spam" "--add-clean" "--update"))
(defvar mew-absfilter-arg-spam '("--sub-clean" "--add-spam" "--update"))

(defvar mew-absfilter-spam-folder-max-msgs 3000)

;; like mew-prog-grep-max-msgs
(defvar mew-absfilter-max-msgs 10000)

(defvar mew-absfilter-map
  (let ((map (make-sparse-keymap)))
    (define-key map "c" 'mew-absfilter-learn-clean)
    (define-key map "s" 'mew-absfilter-learn-spam)
    (define-key map "C" 'mew-absfilter-mark-learn-clean)
    (define-key map "S" 'mew-absfilter-mark-learn-spam)
    (define-key map "b" 'mew-absfilter-check-spam)
    (define-key map "x" 'mew-absfilter-summary-exec-spam)
    (define-key map "D" 'mew-absfilter-clean-spam-folder)
    map))

(define-key mew-summary-mode-map "b" mew-absfilter-map)
;; (define-key mew-summary-mode-map
;;   [remap mew-summary-learn-spam] 'mew-absfilter-learn-spam)
;; (define-key mew-summary-mode-map
;;   [remap mew-summary-learn-ham] 'mew-absfilter-learn-clean)


(defvar mew-absfilter-summary-buffer-process nil)
(make-variable-buffer-local 'mew-absfilter-summary-buffer-process)
;; Use buffer-local-variable in process-buffer.
;; process-{put,get} is avairable only in Emacs-21.4 or above.
(defvar mew-absfilter-process-folder nil)

(defun mew-absfilter-add-clean (files)
  (apply 'call-process
	 mew-absfilter-program nil 0 nil
	 (append mew-absfilter-arg-clean files)))

(defun mew-absfilter-add-spam (files)
  (apply 'call-process
	 mew-absfilter-program nil 0 nil
	 (append mew-absfilter-arg-spam files)))

(defmacro mew-absfilter-expand-msg (folder msg)
  "Expand message MSG in FOLDER.
Function `mew-expand-msg' is defined after Mew 4.2.53.
Use `mew-expand-folder' iff `mew-expand-msg' is not available."
  (if (fboundp 'mew-expand-msg)
      `(mew-expand-msg ,folder ,msg)
    `(mew-expand-folder ,folder ,msg)))

(defmacro mew-absfilter-sumsyn-filename ()
  "Get filename"
  (if (fboundp 'mew-msg-get-filename)
      `(mew-msg-get-filename (mew-sumsyn-message-number))
    `(mew-sumsyn-message-number)))

(defun mew-absfilter-match-string-message-number ()
  (if (and (boundp 'mew-use-suffix)
	   (boundp 'mew-suffix))
      (mew-match-string 1)
    (mew-match-string 0)))

;; spam check
(defun mew-absfilter-collect-message-region (begin end)
  "Returns a list of message file name in region."
  (when (> end begin)
    (let (msgs)
      (save-excursion
	(save-restriction
	  (narrow-to-region begin end)
	  (goto-char (point-min))
	  (while (not (eobp))
	    (when (and (mew-summary-markable)
		       (mew-sumsyn-match mew-regex-sumsyn-short))
	      (push (mew-absfilter-sumsyn-filename) msgs))
	    (forward-line))))
      (nreverse msgs))))

(defun mew-absfilter-collect-spam-message ()
  (let (spam)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
	(when (looking-at mew-regex-message-files2)
	  (push (mew-absfilter-match-string-message-number) spam))
	(forward-line)))
    (nreverse spam)))

(defun mew-absfilter-check-spam-region (case:folder begin end)
  (with-current-buffer case:folder
    (mew-pickable
     (let ((msgs (mew-absfilter-collect-message-region begin end))
	   nxt)
       (when msgs
	 (message "Spam checking %s..." case:folder))
       (while msgs
	 (let ((buf (get-buffer-create
		     (generate-new-buffer-name " *mew bsfilter*")))
	       process)
	   (with-current-buffer buf
	     (cd (mew-expand-folder case:folder))
	     (mew-erase-buffer)
	     (set (make-local-variable 'mew-absfilter-process-folder)
		  case:folder))
	   (setq nxt (nthcdr mew-absfilter-max-msgs msgs))
	   (when nxt
	     (setcdr (nthcdr (1- mew-absfilter-max-msgs) msgs) nil))
	   (setq process (apply 'start-process "mew-absfilter" buf
				mew-absfilter-program
				(append mew-absfilter-arg-check msgs)))
	   (set-process-sentinel process 'mew-absfilter-sentinel)
	   (add-to-list 'mew-absfilter-summary-buffer-process process))
	 (setq msgs nxt)))))
  (when mew-absfilter-summary-buffer-process
    (force-mode-line-update)))

(defun mew-absfilter-apply-spam-action (case:folder spam)
  (when (and spam
	     (get-buffer case:folder))
    (save-excursion
      (let ((vfolder (mew-folder-to-thread case:folder)))
	;; mark in thread if exists
	(when (and (get-buffer vfolder)
		   (mew-virtual-thread-p vfolder)
		   (with-current-buffer case:folder
		     (mew-thread-cache-valid-p vfolder)))
	  (let ((msgs spam))
	    (setq spam nil)
	    (set-buffer vfolder)
	    (save-excursion
	      (dolist (msg msgs)
		(if (or (re-search-forward (mew-regex-sumsyn-msg msg) nil t)
			(re-search-backward (mew-regex-sumsyn-msg msg) nil t))
		    (mew-absfilter-summary-spam-one 'no-msg)
		  ;; if msg is not found, try to mark in physical folder
		  (push msg spam)))))))
      (when spam
	(set-buffer case:folder)
	(save-excursion
	  (dolist (msg spam)
	    (when (or (re-search-forward (mew-regex-sumsyn-msg msg) nil t)
		      (re-search-backward (mew-regex-sumsyn-msg msg) nil t))
	      (mew-absfilter-summary-spam-one 'no-msg))))))))

(defun mew-absfilter-sentinel (process event)
  ;; exit status of "bsfilter --list-spam"
  ;;  0: some spams are found
  ;;  1: spam not found
  (mew-filter
   (let ((status (process-exit-status process))
	 (case:folder mew-absfilter-process-folder)
	 (spam (mew-absfilter-collect-spam-message)))
     (when (zerop status)
       (mew-absfilter-apply-spam-action case:folder spam))
     (with-current-buffer case:folder
       (setq mew-absfilter-summary-buffer-process
	     (delq process mew-absfilter-summary-buffer-process)))
     (message "Spam checking %s...%s"
	      case:folder
	      (cond
	       ((= status 0)
		(format "done (%d spam)" (length spam)))
	       ((= status 1)
		(concat "done (spam not found)"))
	       (t
		(concat "failed. " event))))
     (kill-buffer (current-buffer)))))

;; commands
(defun mew-absfilter-learn-clean (&optional mark-only)
  "Learn this message as clean (not spam)."
  (interactive "P")
  (mew-summary-msg-or-part
   (mew-summary-goto-message)
   (when (mew-sumsyn-match mew-regex-sumsyn-short)
     (let* ((msg (mew-sumsyn-message-number))
	    (case:folder (mew-sumsyn-folder-name))
	    (file (mew-absfilter-expand-msg case:folder msg)))
       (when (eq (mew-summary-get-mark) mew-absfilter-mark-spam)
	 (mew-summary-undo))
       (unless mark-only
	 (mew-absfilter-add-clean (list file))
	 (message "Learned as clean"))))))

(defun mew-absfilter-learn-spam (&optional mark-only)
  "Learn this message as spam."
  (interactive "P")
  (mew-summary-msg-or-part
   (mew-summary-goto-message)
   (when (mew-sumsyn-match mew-regex-sumsyn-short)
     (let* ((msg (mew-sumsyn-message-number))
	    (case:folder (mew-sumsyn-folder-name))
	    (file (mew-absfilter-expand-msg case:folder msg)))
       (mew-absfilter-summary-spam-one)
       (unless mark-only
	 (mew-absfilter-add-spam (list file))
	 (message "Learned as spam"))))))

(defun mew-absfilter-mark-learn-clean (&optional mark-only)
  "Learn all messages marked with '*' as clean (not spam)."
  (interactive "P")
  (mew-summary-multi-msgs
   (mew-mark-undo-mark mew-mark-review)
   (unless mark-only
     (message "Learning as clean...")
     (mew-absfilter-add-clean FILES)
     (message "Learning as clean...done"))))
   
(defun mew-absfilter-mark-learn-spam (&optional mark-only)
  "Learn all messages marked with '*' as spam."
  (interactive "P")
  (mew-summary-multi-msgs
   (mew-mark-undo-mark mew-mark-review)   
   (unless mark-only
     (message "Learning as spam...")
     (mew-absfilter-add-spam FILES)
     (message "Learning as spam...done"))))
   
;; (defun mew-absfilter-thread-mark-learn-spam ()
;;   "Put the ';' mark on all messages of the current sub-thread."
;;   (interactive)
;;   (mew-thread-mark mew-absfilter-mark-spam 'valid-only))

(defun mew-absfilter-summary-exec-spam ()
  "Process messages marked with ';'."
  (interactive)
  (let* ((ent (assoc mew-absfilter-mark-spam mew-mark-spec))
	 (mew-mark-spec (list ent)))
    ;; call `mew-summary-exec-local' even for imap or nntp
    (cond
     ((mew-virtual-p)
      (mew-summary-go-back-summary
       (mew-substitute-for-summary "\\[mew-summary-exec]")))
     (t
      ;; This message can not be changed because
      ;; (message "Refiling and deleting...done") is called in
      ;; `mew-summary-exec-local'.
      (message "Refiling and deleting...")
      (force-mode-line-update)
      (mew-summary-exec-local (point-min) (point-max))))))

(defun mew-absfilter-check-spam (&optional arg)
  "Check spam messages with bsfilter."
  (interactive "P")
  (let ((region (if (or arg (mew-mark-active-p))
		    (mew-summary-get-region)
		  (cons (point-min) (point-max)))))
    (mew-absfilter-check-spam-region (mew-summary-folder-name 'ext)
				     (car region) (cdr region))))

(defun mew-absfilter-clean-spam-folder (&optional unlink)
  "Remove old spam.
Save `mew-absfilter-spam-folder-max-msgs' messages."
  (interactive "P")
  (mew-summary-visit-folder mew-absfilter-spam-folder)
  (mew-rendezvous mew-summary-buffer-process)
  (mew-decode-syntax-delete)
  (save-excursion
    (goto-char (point-max))
    (forward-line (- mew-absfilter-spam-folder-max-msgs))
    (let ((pos (point)))
      (while (zerop (forward-line -1))
	(mew-summary-mark-as (if unlink mew-mark-unlink mew-mark-delete)))
      (mew-summary-exec-region (point) pos))))


;;; Check after retrieve
(defvar mew-absfilter-check t
  "When to check with bsfilter.
If t, do full check. Otherwise, the value should be a list whose element
is one of `local', `pop', `imap', `nntp', or `shimbun'.")

;; biff scan inc sync exec get list jobs
(defvar mew-absfilter-check-directive-list '(("+" inc)
					     ("$" . nil)
					     ("%" inc scan)
					     ("-" scan)))

;; Suppress byte-compiler warning.
;; bnm and directive is local variable which can be used in
;; mew-{local,pop,imap,nntp}-sentinel.
(defvar bnm)
(defvar directive)

(defun mew-absfilter-check-spam-after-retrieve ()
  "Check spam messages with bsfilter after retrieve."
  (when (stringp bnm)
    (let* ((proto (mew-folder-prefix (mew-case:folder-folder bnm)))
	   (check (cdr (assoc proto mew-absfilter-check-directive-list))))
      (when (memq directive check)
	(with-current-buffer bnm
	  (mew-absfilter-check-spam-region bnm
					   (mew-sinfo-get-start-point)
					   (point-max)))))))

;; mew-local-sentinel does not let-bind `directive'
;; and that information is lost by (mew-info-clean-up pnm)
;; when mew-scan-sentinel-hook is called.
(defadvice mew-local-sentinel (around absfilter-check disable)
  "Bind `directive' for spam checking.
Advised in mew-absfilter.el"
  (let ((directive (mew-local-get-directive (process-name process))))
    ad-do-it))

;; Check after `mew-shimbun-retrieve'
(defun mew-absfilter-shimbun-retrieve-set-start-point ()
  "Set retrieve start point."
  (mew-sinfo-set-start-point (point-max)))

(defun mew-absfilter-check-spam-after-shimbun-retrieve ()
  "Check spam messages with absfilter after shimbun-retrieve."
  (mew-absfilter-check-spam-region (mew-summary-folder-name 'ext)
				   (mew-sinfo-get-start-point) (point-max)))

;; modeline
(defadvice mew-summary-setup-mode-line (after absfilter-process disable)
  "Display \"bsfilter\" in mode line.
Advised in mew-absfilter.el"
  (add-to-list 'mode-line-process
	       (list 'mew-absfilter-summary-buffer-process " bsfilter")))

;; inhibit pack, exec
(defadvice mew-summary-exclusive-p (after absfilter-process disable)
  "Return nil when operation may break marking spam.
`mew-absfilter-apply-spam-action' may put spam mark on the wrong message
if message number is changed during bsfilter is running.
The example of such operations are:
 - \"O\" (mew-summary-pack)
 - 'Refile' + \"i\" (mew-summary-retrieve)

Advised in mew-absfilter.el"
  (when (and mew-absfilter-summary-buffer-process
	     (memq this-command '(mew-summary-exec mew-summary-pack)))
    (unless no-msg
      (message "bsfilter is running. Try again later"))
    (setq ad-return-value nil)))

(defun mew-absfilter-mode-activate (initialize)
  (let ((all '(local pop imap nntp shimbun))
	ad-func hook-func)
    (when (eq initialize t)
      (setq initialize all))
    (if initialize
	(setq ad-func 'ad-enable-advice
	      hook-func 'add-hook)
      (setq initialize all
	    ad-func 'ad-disable-advice
	    hook-func 'remove-hook))
    (when (memq 'local initialize)
      (funcall ad-func 'mew-local-sentinel 'around 'absfilter-check)
      (ad-activate 'mew-local-sentinel)
      (funcall hook-func
	       'mew-scan-sentinel-hook
	       'mew-absfilter-check-spam-after-retrieve))
    (when (memq 'pop initialize)
      (funcall hook-func
	       'mew-pop-sentinel-non-biff-hook
	       'mew-absfilter-check-spam-after-retrieve))
    (when (memq 'imap initialize)
      (funcall hook-func
	       'mew-imap-sentinel-non-biff-hook
	       'mew-absfilter-check-spam-after-retrieve))
    (when (memq 'nntp initialize)
      (funcall hook-func
	       'mew-nntp-sentinel-hook
	       'mew-absfilter-check-spam-after-retrieve))
    (when (memq 'shimbun initialize)
      (funcall hook-func
	       'mew-shimbun-before-retrieve-hook
	       'mew-absfilter-shimbun-retrieve-set-start-point)
      (funcall hook-func
	       'mew-shimbun-retrieve-hook
	       'mew-absfilter-check-spam-after-shimbun-retrieve))
    (funcall ad-func 'mew-summary-setup-mode-line 'after 'absfilter-process)
    (funcall ad-func 'mew-summary-exclusive-p 'after 'absfilter-process)
    (ad-activate 'mew-summary-setup-mode-line)
    (ad-activate 'mew-summary-exclusive-p)))

(defvar mew-absfilter-mode nil)
;;;###autoload
(defun mew-absfilter-mode (&optional arg)
  "Enable or disable bsfilter checking.
See `mew-absfilter-check' when bsfilter is run."
  (interactive "P")
  (let ((mode (if arg
		  (> (prefix-numeric-value arg) 0)
		(not mew-absfilter-mode))))
    (when (and mode
	       (not (mew-which-exec mew-absfilter-program)))
      (error "`%s' not found" mew-absfilter-program))
    (setq mew-absfilter-mode mode)
    (mew-absfilter-mode-activate (if mode mew-absfilter-check))
    (when (interactive-p)
      (message "bsfilter checking is %s" (if mode "enabled" "disabled")))))

(provide 'mew-absfilter)

;;; mew-absfilter.el ends here
