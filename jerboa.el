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

(defun jerboa--switch-to-buffer-or-file (target)
  "Switch to buffer or file TARGET."
  (if (and (stringp target) (file-name-absolute-p target))
      (find-file target)
    (switch-to-buffer target)))

(defun jerboa--generate-label (&optional buffer line)
  "Generate label. If LINE is non-nil, append :LINE."
  (let* ((buf (or buffer (current-buffer)))
         (path (buffer-file-name buf))
         (name (buffer-name buf))
         (base (if path
                   (let* ((dir (file-name-directory path))
                          (parent (file-name-nondirectory (directory-file-name dir)))
                          (file (file-name-nondirectory path)))
                     (format "%s(%s)" file parent))
                 name)))
    (if line
        (format "%s:%d" base line)
      base)))

;;;###autoload
(defun jerboa-define-bookmark (keys)
  "Bind KEYS to a bookmark at current location."
  (let* ((name-suffix (jerboa--keys-to-name keys))
	 (bmk-name (concat "jerboa-" name-suffix))
	 (label (jerboa--generate-label nil (line-number-at-pos))))
    (bookmark-set bmk-name)
    (define-key jerboa-map keys
      (cons label `(lambda () (interactive) (bookmark-jump ,bmk-name))))
    (message "Jerboa: bound %s to %s" (key-description keys) label)))

;;;###autoload
(defun jerboa-define-prefix (keys)
  "Bind KEYS to a new prefix map."
  (let ((name (read-string "Prefix name: ")))
    (define-key jerboa-map keys (cons name (make-sparse-keymap)))
    (message "Jerboa: bound %s to prefix %s" (key-description keys) name)))

;;;###autoload
(defun jerboa-define-tab (keys)
  "Store current tab and bind KEYS to switch to it by NAME."
  (unless desktop-save-mode
    (when (y-or-n-p "Desktop save mode is not active. Tabs might not persist. Enable it? ")
      (desktop-save-mode 1)))
  (let* ((tab-name (read-string "Tab name: "))
	 (label (concat "<t>" tab-name)))
    ;; Rename current tab
    (tab-bar-rename-tab tab-name)
    ;; Define command
    (define-key jerboa-map keys
      (cons label `(lambda () (interactive) (tab-bar-switch-to-tab ,tab-name))))
    (message "Jerboa: bound %s to switch to tab \"%s\"" (key-description keys) tab-name)))

;;;###autoload
(defun jerboa-define-buffer (keys)
  "Bind KEYS to switch to current buffer or file."
  (let* ((buf (current-buffer))
	 (path (buffer-file-name buf))
	 (target (or path (buffer-name buf)))
	 (label (jerboa--generate-label buf)))
    (define-key jerboa-map keys
      (cons label `(lambda () (interactive) (jerboa--switch-to-buffer-or-file ,target))))
    (message "Jerboa: bound %s to %s \"%s\"" (key-description keys) (if path "file" "buffer") target)))

;;;###autoload
(defun jerboa-define-desktop (keys)
  "Save current desktop configuration and bind KEYS to load it."
  (let* ((name (read-string "Desktop name: "))
	 (dir (locate-user-emacs-file (file-name-as-directory
                                       (concat "jerboa-desktops/" name))))
	 (label (concat "<d>" name)))
    (make-directory dir t)
    (desktop-save dir)
    (define-key jerboa-map keys
      (cons label `(lambda () (interactive) (desktop-change-dir ,dir))))
    (message "Jerboa: bound %s to load desktop \"%s\"" (key-description keys) name)))

(defvar jerboa-handlers
  '(("Define Prefix" . jerboa-define-prefix)
    ("Define Tab" . jerboa-define-tab)
    ("Define Buffer" . jerboa-define-buffer)
    ("Define Desktop" . jerboa-define-desktop))
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
	 (cond
	  ;; (LABEL . CMD) or (LABEL . KEYMAP)
	  ((and (consp binding) (stringp (car binding)))
	   (push (cons (format "%-12s %s" (key-description keys) (car binding)) keys) res)
	   ;; If it's a prefix map, recurse
	   (when (keymapp (cdr binding))
	     (setq res (append (jerboa--get-all-bindings (cdr binding) keys) res))))
	  ;; Raw Keymap
	  ((keymapp binding)
	   (push (cons (format "%-12s %s" (key-description keys) "Prefix Map") keys) res)
	   (setq res (append (jerboa--get-all-bindings binding keys) res)))
	  (t
	   (let ((doc (condition-case nil (documentation binding) (error nil))))
	     (when (and (not doc) (symbolp binding)) (setq doc (symbol-name binding)))
	     (push (cons (format "%-12s %s" (key-description keys) (or doc "")) keys) res))))))
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
      (define-key jerboa-map keys nil)
      ;; Delete bookmark if it exists
      (let ((bmk-name (concat "jerboa-" (jerboa--keys-to-name keys))))
	(when (bookmark-get-bookmark bmk-name t)
	  (bookmark-delete bmk-name)
	  (message "Jerboa: Unbound %s and deleted bookmark %s"
		   (key-description keys) bmk-name)))
      (message "Jerboa: Unbound %s" (key-description keys)))))

;;;###autoload
(defun jerboa-visit-bookmark ()
  "Visit a jerboa bookmark selected by name."
  (interactive)
  (let* ((all-bindings (jerboa--get-all-bindings))
	 (candidates (seq-filter
		      (lambda (cand)
			(let* ((keys (cdr cand))
			       (binding (lookup-key jerboa-map keys))
			       (cmd (if (consp binding) (cdr binding) binding)))
			  ;; Check if it is a bookmark lambda
			  (and (listp cmd)
			       (eq (car cmd) 'lambda)
			       (let ((body (nth 2 cmd)))
				 (and (eq (car-safe body) 'interactive)
				      (eq (car-safe (nth 3 cmd)) 'bookmark-jump))))))
		      all-bindings))
	 (selection (completing-read "Visit: " candidates))
	 (keys (cdr (assoc selection candidates))))
    (when keys
      (let* ((binding (lookup-key jerboa-map keys))
	     (cmd (if (consp binding) (cdr binding) binding)))
	(when (commandp cmd)
	  (call-interactively cmd))))))

;;; Persistence

(defvar jerboa-loaders
  '((:prefix   . jerboa--restore-prefix)
    (:bookmark . jerboa--restore-bookmark)
    (:tab      . jerboa--restore-tab)
    (:buffer   . jerboa--restore-buffer)
    (:desktop  . jerboa--restore-desktop))
  "Alist mapping binding types to restore functions.
Function takes (KEYS SPEC).")

(defun jerboa--restore-prefix (keys spec)
  (let ((name (or (plist-get spec :label) (plist-get spec :name) "Prefix")))
    (define-key jerboa-map keys (cons name (make-sparse-keymap)))))

(defun jerboa--restore-bookmark (keys spec)
  (let* ((name-suffix (jerboa--keys-to-name keys))
	 (bmk-name (concat "jerboa-" name-suffix))
	 (label (or (plist-get spec :label) (plist-get spec :doc) bmk-name)))
    (define-key jerboa-map keys
      (cons label `(lambda () (interactive) (bookmark-jump ,bmk-name))))))

(defun jerboa--restore-tab (keys spec)
  (let ((tab-name (plist-get spec :name))
	(label (or (plist-get spec :label) (plist-get spec :doc))))
    (define-key jerboa-map keys
      (cons label `(lambda () (interactive) (tab-bar-switch-to-tab ,tab-name))))))

(defun jerboa--restore-buffer (keys spec)
  (let ((target (plist-get spec :target))
	(label (or (plist-get spec :label) (plist-get spec :doc))))
    (define-key jerboa-map keys
      (cons label `(lambda () (interactive) (jerboa--switch-to-buffer-or-file ,target))))))

(defun jerboa--restore-desktop (keys spec)
  (let ((dir (plist-get spec :dir))
	(label (or (plist-get spec :label) (plist-get spec :doc))))
    (define-key jerboa-map keys
      (cons label `(lambda () (interactive) (desktop-change-dir ,dir))))))

(defun jerboa--check-lambda (cmd fn-symbol)
  "Check if CMD is a lambda calling FN-SYMBOL."
  (and (listp cmd) (eq (car cmd) 'lambda)
       (let ((body (if (eq (car-safe (nth 2 cmd)) 'interactive)
		       (nth 3 cmd)
		     (nth 2 cmd))))
	 (eq (car-safe body) fn-symbol))))

(defun jerboa--extract-arg (cmd)
  "Extract the first argument from the function call in CMD lambda."
  (let ((body (if (eq (car-safe (nth 2 cmd)) 'interactive)
		  (nth 3 cmd)
		(nth 2 cmd))))
    (nth 1 body)))

(defvar jerboa-serializers
  '(jerboa--serialize-prefix
    jerboa--serialize-tab
    jerboa--serialize-buffer
    jerboa--serialize-bookmark
    jerboa--serialize-desktop)
  "List of functions to serialize bindings.
Each function takes (BINDING KEYS) and returns a plist
(:type TYPE :name NAME ...) or nil if it cannot handle the binding.")

(defun jerboa--serialize-prefix (binding keys)
  (let ((label (if (consp binding) (car binding) nil))
	(cmd (if (consp binding) (cdr binding) binding)))
    (when (or (keymapp cmd)
	      (and (symbolp cmd) (boundp cmd) (keymapp (symbol-value cmd))))
      (list :type :prefix :keys keys :label (or label (if (symbolp cmd) (symbol-name cmd) "Prefix"))))))

(defun jerboa--serialize-tab (binding keys)
  (let ((label (if (consp binding) (car binding) nil))
	(cmd (if (consp binding) (cdr binding) binding)))
    (when (jerboa--check-lambda cmd 'tab-bar-switch-to-tab)
      (list :type :tab :keys keys :name (jerboa--extract-arg cmd) :label label))))

(defun jerboa--serialize-buffer (binding keys)
  (let ((label (if (consp binding) (car binding) nil))
	(cmd (if (consp binding) (cdr binding) binding)))
    (when (jerboa--check-lambda cmd 'jerboa--switch-to-buffer-or-file)
      (list :type :buffer :keys keys :target (jerboa--extract-arg cmd) :label label))))

(defun jerboa--serialize-desktop (binding keys)
  (let ((label (if (consp binding) (car binding) nil))
	(cmd (if (consp binding) (cdr binding) binding)))
    (when (jerboa--check-lambda cmd 'desktop-change-dir)
      (list :type :desktop :keys keys :dir (jerboa--extract-arg cmd) :label label))))

(defun jerboa--serialize-bookmark (binding keys)
  (let ((label (if (consp binding) (car binding) nil))
	(cmd (if (consp binding) (cdr binding) binding)))
    (when (jerboa--check-lambda cmd 'bookmark-jump)
      (list :type :bookmark :keys keys :label label))))

(defun jerboa--serialize-map (map &optional prefix)
  "Serialize MAP to a list of binding specifications."
  (let ((res nil))
    (map-keymap
     (lambda (ev binding)
       (let ((keys (vconcat prefix (vector ev))))
	 (let ((cmd (if (consp binding) (cdr binding) binding)))
	   (if (keymapp cmd)
	       (let ((prefix-spec (seq-some (lambda (fn) (funcall fn binding keys)) jerboa-serializers))
		     (children (jerboa--serialize-map cmd keys)))
		 (when prefix-spec (push prefix-spec children))
		 (setq res (append children res)))
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
(fset 'jerboa-root jerboa-map)

(provide 'jerboa)
;;; jerboa.el ends here
