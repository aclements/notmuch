;; notmuch-go.el --- notmuch is going places.
;;
;; Copyright © Austin Clements
;;
;; This file is part of Notmuch.
;;
;; Notmuch is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Notmuch is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Notmuch.  If not, see <http://www.gnu.org/licenses/>.
;;
;; Authors: Austin Clements <aclements@csail.mit.edu>

;; XXX Make this a defcustom
;; XXX Or derive somehow from saved searches?
(defvar notmuch-go-searches
  `((,(kbd "i") "inbox" "tag:inbox")
    (,(kbd "u") "unread" "tag:unread")
    (,(kbd "f") "flagged" "tag:flagged")
    (,(kbd "t") "sent" "tag:sent")
    (,(kbd "d") "drafts" "tag:draft")
    (,(kbd "a") "all mail" "*")

    ;; (,(kbd "z") "test" "test")
    ;; (,(kbd "z") "test" "test")
    ;; (,(kbd "z") "test" "test")
    ;; (,(kbd "z") "test" "test")
    ;; (,(kbd "z") "test" "test")
    ;; (,(kbd "z") "test" "test")
    ;; (,(kbd "z") "test" "test")
    ;; (,(kbd "z") "test" "test")
    ))
;; (makunbound 'notmuch-go-searches)

(defun notmuch-go-search ()
  (interactive)
  (notmuch-go
   (mapcar (lambda (search)
	     ;; XXX Add counts, but do it asynchronously so this stays
	     ;; snappy
	     (list (first search) (second search)
		   `(lambda ()
		      (notmuch-search ',(third search)
				      notmuch-search-oldest-first))))
	   notmuch-go-searches)
   "Go to "))

(defvar notmuch-go--action nil)

(defvar notmuch-go--can-split-root 'unknown)

(defun notmuch-go (action-map prompt)
  "Interactively prompt for one of the keys in ACTION-MAP.

Displays a pop-up temporary buffer with a summary of all bindings
in ACTION-MAP, reads a key from the minibuffer, and performs the
corresponding action.  The prompt can be canceled with C-g.
PROMPT must be a string to use for the prompt if this command was
not invoked directly by a key binding (e.g., it was invoked
through M-x).  PROMPT should include a space at the end.

ACTION-MAP must be a list of triples of the form
  (KEY LABEL ACTION)
where KEY is a key binding, LABEL is a string label to display in
the buffer, and ACTION is a nullary function to call.  LABEL may
be null, in which case the action will still be bound, but will
not appear in the pop-up buffer.
"

  ;; Emacs 24 always lets us split the frame root window, but Emacs 23
  ;; and older don't if it's a container window.  Unfortunately,
  ;; there's no good way to tell whether this is supported other than
  ;; just trying it.  So try it.
  (when (eq notmuch-go--can-split-root 'unknown)
    (condition-case nil
	(save-window-excursion
	  (when (window-live-p (frame-root-window))
	    ;; Make sure the frame root is a container window
	    (split-window))
	  (split-window (frame-root-window))
	  (setq notmuch-go--can-split-root t))
      (wrong-type-argument
       (setq notmuch-go--can-split-root nil))))

  (setq notmuch-go--action nil)
  (let ((items (notmuch-go--format-actions action-map))
	buffer window height)
    (unwind-protect
	(progn
	  (setq buffer (get-buffer-create "*notmuch-go*"))

	  ;; Fill the action buffer
	  ;; XXX This should act like a completion buffer and allow
	  ;; mouse and keyboard selection
	  (with-current-buffer buffer
	    (setq buffer-read-only t)
	    (let ((window-width (if notmuch-go--can-split-root
				    (window-body-width (minibuffer-window))
				  (window-body-width)))
		  (inhibit-read-only t))
	      (erase-buffer)
	      (remove-overlays)
	      (notmuch-go--insert-items window-width items))
	    (goto-char (point-min))

	    (setq height (count-lines (point-min) (point-max)))

	    ;; If we can show the action window right over the
	    ;; minibuffer, then hide the mode line to make it look
	    ;; like this blends right in to the minibuffer.
	    (if notmuch-go--can-split-root
		(setq mode-line-format nil)
	      ;; Increase height to allow for mode-line in window
	      (setq height (+ 1 height)))

	    ;; Unless the user selects this window, don't show the
	    ;; cursor in it because it will cover up the bindings.
	    (setq cursor-in-non-selected-windows nil))

	  ;; Show the action window
	  (let ((window-min-height 1))
	    (setq window
		  (if notmuch-go--can-split-root
		      (split-window (frame-root-window) (- height))
		    (split-window nil (- (window-height) height)))))
	  (select-window window)
	  (switch-to-buffer buffer)

	  ;; Read the action
	  (let ((minibuffer-map (notmuch-go--make-keymap action-map))
		(prompt
		 (if (eq this-original-command this-command)
		     ;; Make it look like we're just part of a
		     ;; submap prompt
		     (concat (format-kbd-macro (this-command-keys)) " ")
		   ;; We were invoked through something like M-x
		   prompt)))
	    (read-from-minibuffer prompt nil minibuffer-map)))

      ;; Tear down the buffer and window
      ;; XXX Use save-window-excursion to guarantee we return to the
      ;; same window configuration?
      (when window (delete-window window))
      (when buffer (kill-buffer buffer))))

  ;; If we got an action, do it
  (when notmuch-go--action
    (funcall notmuch-go--action)))

(defun notmuch-go--format-actions (action-map)
  "Format the actions in ACTION-MAP.

Returns a list of strings, one for each item with a label in
ACTION-MAP.  These strings can be inserted into a tabular
buffer."

  ;; Compute the maximum key description width
  (let ((key-width 1))
    (dolist (action action-map)
      (setq key-width
	    (max key-width
		 (string-width (format-kbd-macro (first action))))))
    ;; Format each action
    (mapcar (lambda (action)
	      (let ((key (format-kbd-macro (first action)))
		    (desc (second action)))
		(concat
		 (propertize key 'face 'minibuffer-prompt)
		 (make-string (- key-width (length key)) ? )
		 " " desc)))
	    action-map)))

(defun notmuch-go--insert-items (width items)
  "Make a table of ITEMS up to WIDTH wide in the current buffer."
  (let* ((nitems (length items))
	 (col-width (+ 3 (apply #'max (mapcar #'string-width items))))
	 (ncols (if (> (* col-width nitems) width)
		    (max 1 (/ width col-width))
		  ;; Items fit on one line.  Space them out
		  (setq col-width (/ width nitems))
		  (length items))))
    (while items
      (dotimes (col ncols)
	(when items
	  (let ((item (pop items)))
	    (insert item)
	    (when (and items (< col (- ncols 1)))
	      (insert (make-string (- col-width (string-width item)) ? ))))))
      (when items
	(insert "\n")))))

(defun notmuch-go--make-keymap (action-map)
  "Translate ACTION-MAP into a minibuffer keymap."
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map notmuch-go-minibuffer-map)
    (dolist (action action-map)
      (define-key map (first action)
	`(lambda () (interactive)
	   (setq notmuch-go--action ',(third action))
	   (exit-minibuffer))))
    map))

(defvar notmuch-go-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    ;; Make this like a special-mode keymap, with no self-insert-command
    (suppress-keymap map)
    map))

(unless (fboundp 'window-body-width)
  ;; Compatibility for Emacs pre-24
  (defun window-body-width (&optional window)
    (let ((edges (window-inside-edges window)))
      (- (caddr edges) (car edges)))))

;; XXX Unused
(unless (fboundp 'window-body-height)
  (defun window-body-height (&optional window)
    (let ((edges (window-inside-edges window)))
      (- (cadddr edges) (cadr edges)))))

;;(global-set-key (kbd "C-c g") #'notmuch-go-search)
(define-key notmuch-common-keymap (kbd "g") #'notmuch-go-search)
