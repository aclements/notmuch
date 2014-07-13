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

(defun notmuch-go-search ()
  "Jump to a saved search by shortcut key.

This prompts for and performs a saved search using the shortcut
keys configured in the :key property of `notmuch-saved-searches'.
Typically these shortcuts are a single key long, so this is a
fast way to jump to a saved search from anywhere in Notmuch."
  (interactive)

  ;; Build the action map
  (let (action-map)
    (dolist (saved-search notmuch-saved-searches)
      (let* ((saved-search (notmuch-hello-saved-search-to-plist saved-search))
	     (key (plist-get saved-search :key)))
	(when key
	  (let ((name (plist-get saved-search :name))
		(query (plist-get saved-search :query))
		(oldest-first
		 (case (plist-get saved-search :sort-order)
		   (newest-first nil)
		   (oldest-first t)
		   (otherwise (default-value notmuch-search-oldest-first)))))
	    (push (list key name
			`(lambda () (notmuch-search ',query ',oldest-first)))
		  action-map)))))
    (setq action-map (nreverse action-map))

    (if action-map
	(notmuch-go action-map "Go to ")
      (error "No shortcut keys for saved searches.  Please customize notmuch-saved-searches."))))

(defvar notmuch-go--action nil)

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

  (let* ((items (notmuch-go--format-actions action-map))
	 ;; Format the table of bindings and the full prompt
	 (table
	  (with-temp-buffer
	    (notmuch-go--insert-items (window-body-width) items)
	    (buffer-string)))
	 (prompt-text
	  (if (eq this-original-command this-command)
	      ;; Make it look like we're just part of any regular
	      ;; submap prompt (like C-x, C-c, etc.)
	      (concat (format-kbd-macro (this-command-keys)) "-")
	    ;; We were invoked through something like M-x
	    prompt))
	 (full-prompt
	  (concat table "\n\n"
		  (propertize prompt-text 'face 'minibuffer-prompt)))
	 ;; By default, the minibuffer applies the minibuffer face to
	 ;; the entire prompt.  However, we want to clearly
	 ;; distinguish bindings (which we put in the prompt face
	 ;; ourselves) from their labels, so disable the minibuffer's
	 ;; own re-face-ing.
	 (minibuffer-prompt-properties
	  (notmuch-go--plist-delete (copy-sequence minibuffer-prompt-properties)
				    'face))
	 ;; Build the keymap with our bindings
	 (minibuffer-map (notmuch-go--make-keymap action-map))
	 ;; The bindings save the the action in notmuch-go--action
	 (notmuch-go--action nil))
    ;; Read the action
    (read-from-minibuffer full-prompt nil minibuffer-map)

    ;; If we got an action, do it
    (when notmuch-go--action
      (funcall notmuch-go--action))))

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

(defun notmuch-go--plist-delete (plist property)
  (let* ((xplist (cons nil plist))
	 (pred xplist))
    (while (cdr pred)
      (when (eq (cadr pred) property)
	(setcdr pred (cdddr pred)))
      (setq pred (cddr pred)))
    (cdr xplist)))

(unless (fboundp 'window-body-width)
  ;; Compatibility for Emacs pre-24
  (defun window-body-width (&optional window)
    (let ((edges (window-inside-edges window)))
      (- (caddr edges) (car edges)))))

(defvar notmuch-go-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    ;; Make this like a special-mode keymap, with no self-insert-command
    (suppress-keymap map)
    map)
  "Base keymap for notmuch-go's minibuffer keymap.")

(define-key notmuch-common-keymap (kbd "g") #'notmuch-go-search)
