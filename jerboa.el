;;; jerboa.el --- keyboards shortcut focused, universal bookmarks -*- lexical-binding: t; -*-

;; Author: Daniel Nicolai
;; Version: 0.1
;; Package-Requires: ((emacs "27"))

;;; Commentary:

;; This library provides `jerboa-read-key-sequence`, a function that
;; reads a key sequence from the user while displaying the current
;; bindings using `which-key`.
;;
;; It also provides `jerboa-bind` to easily create bookmarks or other
;; bindings in `jerboa-map`.
;;
;; It saves/loads bindings to/from a persistent file.

;;; Code:

;; NOTE: These requires could be moved inside the relevant commands/functions
;; that use them to improve load time (e.g. desktop and tab-bar are only
;; used for specific binding types).
(require 'which-key)
(require 'bookmark)
(require 'tab-bar)
(require 'desktop)
(require 'seq)

(defgroup jerboa nil
  "Jerboa customization."
  :group 'convenience)

(defcustom jerboa-file (locate-user-emacs-file "jerboa-maps")
  "File to save jerboa maps."
  :type 'file
  :group 'jerboa)

;;; Key Sequence Reading

;; NOTE: This function could be moved into `which-key.el` (e.g. as
;; `which-key-read-key-sequence`) so it could be used for other purposes
;; like `global-set-key`.
(defun jerboa-read-key-sequence (prompt &optional keymap)
  "Read a key sequence, showing `which-key` bindings as you type.
PROMPT is passed to `read-key` (though which-key display might obscure it).
If KEYMAP is non-nil, use it to lookup bindings (for checking prefixes),
otherwise use the current active keymaps."
  (let ((keys [])
	(key nil))
    ;; Show top-level bindings initially
    (which-key--create-buffer-and-show nil keymap)
    (unwind-protect
	(while (progn
		 (setq key (read-key prompt))
		 (when (eq key ?\C-g)
		   (keyboard-quit))
		 (setq keys (vconcat keys (vector key)))
		 (let ((binding (if keymap
				    (lookup-key keymap keys)
				  (key-binding keys))))
		   (if (keymapp binding)
		       (progn
			 (which-key--create-buffer-and-show keys keymap)
			 t) ;; Continue loop
		     nil)))) ;; Stop loop
      (which-key--hide-popup))
    keys))

;;; Jerboa Map and Commands

(defvar jerboa-map (make-sparse-keymap "jerboa")
  "Keymap for Jerboa commands.")

(defun jerboa--keys-to-name (keys)
  "Convert KEYS vector to string, e.g. \"s-a\"."
  (replace-regexp-in-string " " "-" (key-description keys)))

;;;###autoload
(defun jerboa-define-bookmark (keys)
  "Bind KEYS to a bookmark at current location."
  (let* ((name-suffix (jerboa--keys-to-name keys))
	 (cmd-name (intern (concat "jb-" name-suffix)))
	 (bmk-name (concat "jerboa-" name-suffix))
	 (doc (format "%s:%d" (buffer-name) (line-number-at-pos))))
    (bookmark-set bmk-name)
    (defalias cmd-name
      (lambda () (interactive) (bookmark-jump bmk-name))
      doc)
    (define-key jerboa-map keys cmd-name)
    (message "Jerboa: bound %s to %s (%s)" (key-description keys) cmd-name doc)))

;;;###autoload
(defun jerboa-define-prefix (keys)
  "Bind KEYS to a new prefix map."
  (let* ((name (read-string "Prefix name: "))
	 (map-name (intern name)))
    (define-prefix-command map-name)
    (define-key jerboa-map keys map-name)
    (message "Jerboa: bound %s to prefix %s" (key-description keys) name)))

;;;###autoload
(defun jerboa-define-tab (keys)
  "Store current tab and bind KEYS to switch to it by NAME."
  (unless desktop-save-mode
    (when (y-or-n-p "Desktop save mode is not active. Tabs might not persist. Enable it? ")
      (desktop-save-mode 1)))
  (let* ((tab-name (read-string "Tab name: "))
	 (name-suffix (jerboa--keys-to-name keys))
	 (cmd-name (intern (concat "jb-" name-suffix)))
	 (doc (concat "<t>" tab-name)))
    ;; Rename current tab
    (tab-bar-rename-tab tab-name)
    ;; Define command
    (defalias cmd-name
      (lambda () (interactive) (tab-bar-switch-to-tab tab-name))
      doc)
    (define-key jerboa-map keys cmd-name)
    (message "Jerboa: bound %s to switch to tab \"%s\"" (key-description keys) tab-name)))

(defvar jerboa-handlers
  '(("Define Prefix" . jerboa-define-prefix)
    ("Define Tab" . jerboa-define-tab))
  "Handlers for `jerboa-bind`.
Each entry is (NAME . FUNCTION), where FUNCTION takes KEYS as argument.
Can be extended with `add-to-list`.")

;;;###autoload
(defun jerboa-bind (arg)
  "Bind a key sequence in `jerboa-map`.
Without prefix ARG, bind to a bookmark at current location.
With prefix ARG, select a handler from `jerboa-handlers`.
This reads keys within `jerboa-map`, respecting existing prefixes."
  (interactive "P")
  (let ((keys (jerboa-read-key-sequence "Jerboa key: " jerboa-map)))
    (if arg
	(let* ((choice (completing-read "Handler: " jerboa-handlers))
	       (handler (cdr (assoc choice jerboa-handlers))))
	  (funcall handler keys))
      (jerboa-define-bookmark keys))))

(defun jerboa--get-all-bindings (&optional map prefix)
  "Return alist of (DISPLAY-STRING . KEYS) for all bindings in MAP."
  (unless map (setq map jerboa-map))
  (let ((res nil))
    (map-keymap
     (lambda (ev binding)
       (let ((keys (vconcat prefix (vector ev))))
	 (let ((doc (condition-case nil
			(documentation binding)
		      (error nil))))
	   ;; If no docstring, fallback to symbol name or string rep
	   (when (and (not doc) (symbolp binding))
	     (setq doc (symbol-name binding)))
	   (when (and (not doc) (keymapp binding))
	     (setq doc "Prefix Map"))
	   ;; Take first line
	   (when doc
	     (setq doc (car (split-string doc "\n"))))
	   (push (cons (format "%-12s %s" (key-description keys) (or doc "")) keys) res))
	 (when (keymapp binding)
	   (setq res (append (jerboa--get-all-bindings binding keys) res)))))
     map)
    res))

;;;###autoload
(defun jerboa-unbind ()
  "Unbind a key sequence in `jerboa-map` using completion.
Lists all bindings and allows selecting one to remove."
  (interactive)
  (let* ((candidates (jerboa--get-all-bindings))
	 (selection (completing-read "Unbind: " candidates))
	 (keys (cdr (assoc selection candidates))))
    (when keys
      (let ((binding (lookup-key jerboa-map keys)))
	 (define-key jerboa-map keys nil)
	 ;; Bookmark handling
	 (when (and (symbolp binding)
		    (string-prefix-p "jb-" (symbol-name binding)))
	   (let ((bmk-name (concat "jerboa-" (substring (symbol-name binding) 3))))
	     (when (bookmark-get-bookmark bmk-name t)
	       (bookmark-delete bmk-name)
	       (message "Jerboa: Unbound %s and deleted bookmark %s"
			(key-description keys) bmk-name))))
	 (unless (and (symbolp binding) (string-prefix-p "jb-" (symbol-name binding)))
	     (message "Jerboa: Unbound %s" (key-description keys)))))))

;;;###autoload
(defun jerboa-visit-bookmark ()
  "Visit a jerboa bookmark selected by name."
  (interactive)
  (let* ((all-bindings (jerboa--get-all-bindings))
	 (candidates (seq-filter
		      (lambda (cand)
			(let* ((keys (cdr cand))
			       (cmd (lookup-key jerboa-map keys)))
			  (and (symbolp cmd)
			       (string-prefix-p "jb-" (symbol-name cmd)))))
		      all-bindings))
	 (selection (completing-read "Visit: " candidates))
	 (keys (cdr (assoc selection candidates))))
    (when keys
      (let ((cmd (lookup-key jerboa-map keys)))
	(if (commandp cmd)
	    (call-interactively cmd)
	  (message "Jerboa: %s is not a command" (key-description keys)))))))

;;; Persistence

(defvar jerboa-loaders
  '((:prefix   . jerboa--restore-prefix)
    (:bookmark . jerboa--restore-bookmark)
    (:tab      . jerboa--restore-tab))
  "Alist mapping binding types to restore functions.
Function takes (KEYS SPEC).")

(defun jerboa--restore-prefix (keys spec)
  (let* ((name (plist-get spec :name))
	 (map-name (intern name)))
    (define-prefix-command map-name)
    (define-key jerboa-map keys map-name)))

(defun jerboa--restore-bookmark (keys spec)
  (let* ((name (plist-get spec :name))
	 (cmd-name (intern name))
	 (name-suffix (jerboa--keys-to-name keys))
	 (bmk-name (concat "jerboa-" name-suffix)))
    (defalias cmd-name
      (lambda () (interactive) (bookmark-jump bmk-name))
      (format "Bookmark: %s" bmk-name))
    (define-key jerboa-map keys cmd-name)))

(defun jerboa--restore-tab (keys spec)
  (let* ((name (plist-get spec :name))
	 (cmd-name (intern (concat "jb-" (jerboa--keys-to-name keys))))
	 (tab-name (if (string-prefix-p "<t>" name)
		       (substring name 3)
		     name)))
    (defalias cmd-name
      (lambda () (interactive) (tab-bar-switch-to-tab tab-name))
      name)
    (define-key jerboa-map keys cmd-name)))

(defvar jerboa-serializers
  '(jerboa--serialize-prefix
    jerboa--serialize-tab
    jerboa--serialize-bookmark)
  "List of functions to serialize bindings.
Each function takes (BINDING KEYS) and returns a plist
(:type TYPE :name NAME ...) or nil if it cannot handle the binding.")

(defun jerboa--serialize-prefix (binding keys)
  (when (and (symbolp binding)
	     (boundp binding)
	     (keymapp (symbol-value binding)))
    (list :type :prefix :keys keys :name (symbol-name binding))))

(defun jerboa--serialize-tab (binding keys)
  (when (symbolp binding)
    (let ((doc (documentation binding)))
      (when (and doc (string-prefix-p "<t>" doc))
	(list :type :tab :keys keys :name doc)))))

(defun jerboa--serialize-bookmark (binding keys)
  (when (and (symbolp binding)
	     (string-prefix-p "jb-" (symbol-name binding)))
    (list :type :bookmark :keys keys :name (symbol-name binding))))

(defun jerboa--serialize-map (map &optional prefix)
  "Serialize MAP to a list of binding specifications."
  (let ((res nil))
    (map-keymap
     (lambda (ev binding)
       (let ((keys (vconcat prefix (vector ev))))
	 (cond
	  ((keymapp binding)
	   (setq res (append res (jerboa--serialize-map binding keys))))
	  (t
	   (let ((spec (seq-some (lambda (fn) (funcall fn binding keys))
				jerboa-serializers)))
	     (when spec
	       (push spec res)))))))
     map)
    res))

(defun jerboa-save-map (name)
  "Save current `jerboa-map` as NAME to `jerboa-file`."
  (interactive (list (read-string "Save profile as (blank for default): " nil nil "default")))
  (let* ((specs (jerboa--serialize-map jerboa-map))
	 (maps (if (file-exists-p jerboa-file)
		   (with-temp-buffer
		     (insert-file-contents jerboa-file)
		     (read (current-buffer)))
		 nil))
	 (entry (assoc name maps)))
    (if entry
	(setcdr entry specs)
      (push (cons name specs) maps))
    (with-temp-file jerboa-file
      (insert ";; Jerboa maps file\n")
      (pp maps (current-buffer)))))

(defun jerboa-load-map (name)
  "Load profile NAME from `jerboa-file` into `jerboa-map`."
  (interactive
   (list (let ((maps (if (file-exists-p jerboa-file)
			 (with-temp-buffer
			   (insert-file-contents jerboa-file)
			   (read (current-buffer)))
		       nil)))
	   (completing-read "Load profile: " (mapcar #'car maps)))))
  ;; Clear current map
  (setq jerboa-map (make-sparse-keymap "jerboa"))
  (let* ((maps (with-temp-buffer
		 (insert-file-contents jerboa-file)
		 (read (current-buffer))))
	 (specs (cdr (assoc name maps))))
    (dolist (spec specs)
      (let* ((type (plist-get spec :type))
	     (keys (plist-get spec :keys))
	     (loader (cdr (assoc type jerboa-loaders))))
	(if loader
	    (funcall loader keys spec)
	  (message "Jerboa: No loader found for type %s" type))))
    (message "Jerboa: Loaded profile '%s'" name)))

(defun jerboa-save-default ()
  "Save \"default\" map."
  (jerboa-save-map "default"))

(defun jerboa-load-default ()
  "Load \"default\" map if available."
  (when (file-exists-p jerboa-file)
    (condition-case nil
	(jerboa-load-map "default")
      (error (message "Jerboa: No default profile found or error loading.")))))

(add-hook 'kill-emacs-hook #'jerboa-save-default)
(jerboa-load-default)

(provide 'jerboa)
;;; jerboa.el ends here
