;;; doom-lookup.el -*- lexical-binding: t; -*-

;;; Extract lookup module from Doom Emacs
;;; https://github.com/hlissner/doom-emacs/tree/develop/modules/tools/lookup

;;;
;;; Prerequisite
;;; Some Doom Emacs core helper functions
;;;
;;;###autoload
(defun doom-region-active-p ()
  "Return non-nil if selection is active.
Detects evil visual mode as well."
  (declare (side-effect-free t))
  (or (use-region-p)
      (and (bound-and-true-p evil-local-mode)
           (evil-visual-state-p))))

;;;###autoload
(defun doom-region-beginning ()
  "Return beginning position of selection.
Uses `evil-visual-beginning' if available."
  (declare (side-effect-free t))
  (or (and (bound-and-true-p evil-local-mode)
           (markerp evil-visual-beginning)
           (marker-position evil-visual-beginning))
      (region-beginning)))

;;;###autoload
(defun doom-region-end ()
  "Return end position of selection.
Uses `evil-visual-end' if available."
  (declare (side-effect-free t))
  (if (bound-and-true-p evil-local-mode)
      evil-visual-end
    (region-end)))

;;;###autoload
(defun doom-thing-at-point-or-region (&optional thing prompt)
  "Grab the current selection, THING at point, or xref identifier at point.
Returns THING if it is a string. Otherwise, if nothing is found at point and
PROMPT is non-nil, prompt for a string (if PROMPT is a string it'll be used as
the prompting string). Returns nil if all else fails.
NOTE: Don't use THING for grabbing symbol-at-point. The xref fallback is smarter
in some cases."
  (declare (side-effect-free t))
  (cond ((stringp thing)
         thing)
        ((doom-region-active-p)
         (buffer-substring-no-properties
          (doom-region-beginning)
          (doom-region-end)))
        (thing
         (thing-at-point thing t))
        ((require 'xref nil t)
         ;; Eglot, nox (a fork of eglot), and elpy implementations for
         ;; `xref-backend-identifier-at-point' betray the documented purpose of
         ;; the interface. Eglot/nox return a hardcoded string and elpy prepends
         ;; the line number to the symbol.
         (if (memq (xref-find-backend) '(eglot elpy nox))
             (thing-at-point 'symbol t)
           ;; A little smarter than using `symbol-at-point', though in most
           ;; cases, xref ends up using `symbol-at-point' anyway.
           (xref-backend-identifier-at-point (xref-find-backend))))
        (prompt
         (read-string (if (stringp prompt) prompt "")))))

(defun doom-enlist (exp)
  "Return EXP wrapped in a list, or as-is if already a list."
  (declare (pure t) (side-effect-free t))
  (if (proper-list-p exp) exp (list exp)))



;;;===============
;;; Main contents
;;;===============


;;;###autodef
(defun set-lookup-handlers! (modes &rest plist)
  "Define jump handlers for major or minor MODES.

A handler is either an interactive command that changes the current buffer
and/or location of the cursor, or a function that takes one argument: the
identifier being looked up, and returns either nil (failed to find it), t
(succeeded at changing the buffer/moving the cursor), or 'deferred (assume this
handler has succeeded, but expect changes not to be visible yet).

There are several kinds of handlers, which can be defined with the following
properties:

:definition FN
  Run when jumping to a symbol's definition. Used by `+lookup/definition'.
:implementations FN
  Run when looking for implementations of a symbol in the current project. Used
  by `+lookup/implementations'.
:type-definition FN
  Run when jumping to a symbol's type definition. Used by
  `+lookup/type-definition'.
:references FN
  Run when looking for usage references of a symbol in the current project. Used
  by `+lookup/references'.
:documentation FN
  Run when looking up documentation for a symbol. Used by
  `+lookup/documentation'.
:file FN
  Run when looking up the file for a symbol/string. Typically a file path. Used
  by `+lookup/file'.
:xref-backend FN
  Defines an xref backend for a major-mode. A :definition and :references
  handler isn't necessary with a :xref-backend, but will have higher precedence
  if they exist.
:async BOOL
  Indicates that *all* supplied FNs are asynchronous. Note: lookups will not try
  any handlers after async ones, due to their nature. To get around this, you
  must write a specialized wrapper to await the async response, or use a
  different heuristic to determine, ahead of time, whether the async call will
  succeed or not.

  If you only want to specify one FN is async, declare it inline instead:

    (set-lookup-handlers! 'rust-mode
      :definition '(racer-find-definition :async t))

Handlers can either be interactive or non-interactive. Non-interactive handlers
must take one argument: the identifier being looked up. This function must
change the current buffer or window or return non-nil when it succeeds.

If it doesn't change the current buffer, or it returns nil, the lookup module
will fall back to the next handler in `+lookup-definition-functions',
`+lookup-implementations-functions', `+lookup-type-definition-functions',
`+lookup-references-functions', `+lookup-file-functions' or
`+lookup-documentation-functions'.

Consecutive `set-lookup-handlers!' calls will overwrite previously defined
handlers for MODES. If used on minor modes, they are stacked onto handlers
defined for other minor modes or the major mode it's activated in.

This can be passed nil as its second argument to unset handlers for MODES. e.g.

  (set-lookup-handlers! 'python-mode nil)

\(fn MODES &key DEFINITION IMPLEMENTATIONS TYPE-DEFINITION REFERENCES DOCUMENTATION FILE XREF-BACKEND ASYNC)"
  (declare (indent defun))
  (dolist (mode (doom-enlist modes))
    (let ((hook (intern (format "%s-hook" mode)))
          (fn   (intern (format "+lookup--init-%s-handlers-h" mode))))
      (if (null (car plist))
          (progn
            (remove-hook hook fn)
            (unintern fn nil))
        (fset
         fn
         (lambda ()
           (cl-destructuring-bind (&key definition implementations type-definition references documentation file xref-backend async)
               plist
             (cl-mapc #'+lookup--set-handler
                      (list definition
                            implementations
                            type-definition
                            references
                            documentation
                            file
                            xref-backend)
                      (list '+lookup-definition-functions
                            '+lookup-implementations-functions
                            '+lookup-type-definition-functions
                            '+lookup-references-functions
                            '+lookup-documentation-functions
                            '+lookup-file-functions
                            'xref-backend-functions)
                      (make-list 5 async)
                      (make-list 5 (or (eq major-mode mode)
                                       (and (boundp mode)
                                            (symbol-value mode))))))))
        (add-hook hook fn)))))


;;
;;; Helpers

(defun +lookup--set-handler (spec functions-var &optional async enable)
  (when spec
    (cl-destructuring-bind (fn . plist)
        (doom-enlist spec)
      (if (not enable)
          (remove-hook functions-var fn 'local)
        (put fn '+lookup-async (or (plist-get plist :async) async))
        (add-hook functions-var fn nil 'local)))))

(defun +lookup--run-handler (handler identifier)
  (if (commandp handler)
      (call-interactively handler)
    (funcall handler identifier)))

(defun +lookup--run-handlers (handler identifier origin)
  (message "Looking up '%s' with '%s'" identifier handler)
  (condition-case-unless-debug e
      (let ((wconf (current-window-configuration))
            (result (condition-case-unless-debug e
                        (+lookup--run-handler handler identifier)
                      (error
                       (message "Lookup handler %S threw an error: %s" handler e)
                       'fail))))
        (cond ((eq result 'fail)
               (set-window-configuration wconf)
               nil)
              ((or (get handler '+lookup-async)
                   (eq result 'deferred)))
              ((or result
                   (null origin)
                   (/= (point-marker) origin))
               (prog1 (point-marker)
                 (set-window-configuration wconf)))))
    ((error user-error)
     (message "Lookup handler %S: %s" handler e)
     nil)))

(defun +lookup--jump-to (prop identifier &optional display-fn arg)
  (let* ((origin (point-marker))
         (handlers
          (plist-get (list :definition '+lookup-definition-functions
                           :implementations '+lookup-implementations-functions
                           :type-definition '+lookup-type-definition-functions
                           :references '+lookup-references-functions
                           :documentation '+lookup-documentation-functions
                           :file '+lookup-file-functions)
                     prop))
         (result
          (if arg
              (if-let
                  (handler
                   (intern-soft
                    (completing-read "Select lookup handler: "
                                     (delete-dups
                                      (remq t (append (symbol-value handlers)
                                                      (default-value handlers))))
                                     nil t)))
                  (+lookup--run-handlers handler identifier origin)
                (user-error "No lookup handler selected"))
            (run-hook-wrapped handlers #'+lookup--run-handlers identifier origin))))
    (unwind-protect
        (when (cond ((null result)
                     (message "No lookup handler could find %S" identifier)
                     nil)
                    ((markerp result)
                     (funcall (or display-fn #'switch-to-buffer)
                              (marker-buffer result))
                     (goto-char result)
                     result)
                    (result))
          (with-current-buffer (marker-buffer origin)
            (better-jumper-set-jump (marker-position origin)))
          result)
      (set-marker origin nil))))


;;
;;; Lookup backends

(autoload 'xref--show-defs "xref")
(defun +lookup--xref-show (fn identifier &optional show-fn)
  (let ((xrefs (funcall fn
                        (xref-find-backend)
                        identifier)))
    (when xrefs
      (let* ((jumped nil)
             (xref-after-jump-hook
              (cons (lambda () (setq jumped t))
                    xref-after-jump-hook)))
        (funcall (or show-fn #'xref--show-defs)
                 (lambda () xrefs)
                 nil)
        (if (cdr xrefs)
            'deferred
          jumped)))))


(defun +lookup-xref-definitions-backend-fn (identifier)
  "Non-interactive wrapper for `xref-find-definitions'"
  (condition-case _
      (+lookup--xref-show 'xref-backend-definitions identifier #'xref--show-defs)
    (cl-no-applicable-method nil)))

(defun +lookup-xref-references-backend-fn (identifier)
  "Non-interactive wrapper for `xref-find-references'"
  (condition-case _
      (+lookup--xref-show 'xref-backend-references identifier #'xref--show-xrefs)
    (cl-no-applicable-method nil)))

(defun +lookup-dumb-jump-backend-fn (_identifier)
  "Look up the symbol at point (or selection) with `dumb-jump', which conducts a
project search with ag, rg, pt, or git-grep, combined with extra heuristics to
reduce false positives.

This backend prefers \"just working\" over accuracy."
  (and (require 'dumb-jump nil t)
       (dumb-jump-go)))

(defun +lookup-project-search-backend-fn (identifier)
  "Conducts a simple project text search for IDENTIFIER.

Uses and requires `+ivy-file-search', `+helm-file-search', or `+vertico-file-search'.
Will return nil if neither is available. These require ripgrep to be installed."
  (unless identifier
    (let ((query (rxt-quote-pcre identifier)))
      (ignore-errors
        (+ivy-file-search :query query)))))

;;;###autoload
(cl-defun +ivy-file-search (&key query in all-files (recursive t) prompt args)
  "Conduct a file search using ripgrep.
:query STRING
  Determines the initial input to search for.
:in PATH
  Sets what directory to base the search out of. Defaults to the current
  project's root.
:recursive BOOL
  Whether or not to search files recursively from the base directory."
  (declare (indent defun))
  (unless (executable-find "rg")
    (user-error "Couldn't find ripgrep in your PATH"))
  (require 'counsel)
  (let* ((this-command 'counsel-rg)
         (project-root (or (cdr (project-current)) default-directory))
         (directory (or in project-root))
         (args (concat (if all-files " -uu")
                       (unless recursive " --maxdepth 1")
                       " --hidden -g!.git "
                       (mapconcat #'shell-quote-argument args " "))))
    (setq deactivate-mark t)
    (counsel-rg
     (or query
         (when (doom-region-active-p)
           (replace-regexp-in-string
            "[! |]" (lambda (substr)
                      (cond ((and (string= substr " ")
                                  (not (featurep! +fuzzy)))
                             "  ")
                            ((string= substr "|")
                             "\\\\\\\\|")
                            ((concat "\\\\" substr))))
            (rxt-quote-pcre (doom-thing-at-point-or-region)))))
     directory args
     (or prompt
         (format "Search project [%s]: "
                 (cond ((equal directory default-directory)
                        "./")
                       ((equal directory project-root)
                        (project-current))
                       ((file-relative-name directory project-root)))
                 (string-trim args))))))

;;;###autoload
(defun +ivy/project-search (&optional arg initial-query directory)
  "Performs a live project search from the project root using ripgrep.
If ARG (universal argument), include all files, even hidden or compressed ones,
in the search."
  (interactive "P")
  (+ivy-file-search :query initial-query :in directory :all-files arg))

(defun +lookup-mu-rgrep-search-backend-fn (_identifier)
  "Use `mu-recursive-grep' function to search specific directory or specific
project recursively."
  (when (commandp 'mu-recursive-grep t)
    (call-interactively 'mu-recursive-grep)))

(defun +lookup-evil-goto-definition-backend-fn (_identifier)
  "Uses `evil-goto-definition' to conduct a text search for IDENTIFIER in the
current buffer."
  (when (fboundp 'evil-goto-definition)
    (ignore-errors
      (cl-destructuring-bind (beg . end)
          (bounds-of-thing-at-point 'symbol)
        (evil-goto-definition)
        (let ((pt (point)))
          (not (and (>= pt beg)
                    (<  pt end))))))))

(defun my/--project-p ()
  "Determine whether is a project"
  (and (project-current)
       t))

(defun my/--project-root ()
  "Return project root directory, if not a project, return `default-directory'"
  (let ((root default-directory)
        (project (project-current)))
    (when project
      (setq root (cdr project)))
    root))

(defun +lookup-ffap-backend-fn (identifier)
  "Tries to locate the file at point (or in active selection).
Uses find-in-project functionality (provided by ivy, helm, or project),
otherwise falling back to ffap.el (find-file-at-point)."
  (let ((guess
         (cond (identifier)
               ((doom-region-active-p)
                (buffer-substring-no-properties
                 (doom-region-beginning)
                 (doom-region-end)))
               ((if (require 'ffap) (ffap-guesser)))
               ((thing-at-point 'filename t)))))
    (cond ((and (stringp guess)
                (or (file-exists-p guess)
                    (ffap-url-p guess)))
           (find-file-at-point guess))
          ((and (featurep 'ivy)
                (my/--project-p))
           (counsel-file-jump guess (my/--project-root)))
          ((find-file-at-point (ffap-prompter guess))))
    t))

(defun +lookup-bug-reference-backend-fn (_identifier)
  "Searches for a bug reference in user/repo#123 or #123 format and opens it in
the browser."
  (require 'bug-reference)
  (when (fboundp 'bug-reference-try-setup-from-vc)
    (let ((old-bug-reference-mode bug-reference-mode)
          (old-bug-reference-prog-mode bug-reference-prog-mode)
          (bug-reference-url-format bug-reference-url-format)
          (bug-reference-bug-regexp bug-reference-bug-regexp))
      (bug-reference-try-setup-from-vc)
      (unwind-protect
          (let ((bug-reference-mode t)
                (bug-reference-prog-mode nil))
            (catch 'found
              (bug-reference-fontify (line-beginning-position) (line-end-position))
              (dolist (o (overlays-at (point)))
                ;; It should only be possible to have one URL overlay.
                (when-let (url (overlay-get o 'bug-reference-url))
                  (browse-url url)

                  (throw 'found t)))))
        ;; Restore any messed up fontification as a result of this.
        (bug-reference-unfontify (line-beginning-position) (line-end-position))
        (if (or old-bug-reference-mode
                old-bug-reference-prog-mode)
            (bug-reference-fontify (line-beginning-position) (line-end-position)))))))


;;
;;; Main commands

;;;###autoload
(defun +lookup/definition (identifier &optional arg)
  "Jump to the definition of IDENTIFIER (defaults to the symbol at point).

Each function in `+lookup-definition-functions' is tried until one changes the
point or current buffer. Falls back to dumb-jump, naive
ripgrep/the_silver_searcher text search, then `evil-goto-definition' if
evil-mode is active."
  (interactive (list (doom-thing-at-point-or-region)
                     current-prefix-arg))
  (cond ((null identifier) (user-error "Nothing under point"))
        ((+lookup--jump-to :definition identifier nil arg))
        ((user-error "Couldn't find the definition of %S" (substring-no-properties identifier)))))

;;;###autoload
(defun +lookup/implementations (identifier &optional arg)
  "Jump to the implementations of IDENTIFIER (defaults to the symbol at point).

Each function in `+lookup-implementations-functions' is tried until one changes
the point or current buffer."
  (interactive (list (doom-thing-at-point-or-region)
                     current-prefix-arg))
  (cond ((null identifier) (user-error "Nothing under point"))
        ((+lookup--jump-to :implementations identifier nil arg))
        ((user-error "Couldn't find the implementations of %S" (substring-no-properties identifier)))))

;;;###autoload
(defun +lookup/type-definition (identifier &optional arg)
  "Jump to the type definition of IDENTIFIER (defaults to the symbol at point).

Each function in `+lookup-type-definition-functions' is tried until one changes
the point or current buffer."
  (interactive (list (doom-thing-at-point-or-region)
                     current-prefix-arg))
  (cond ((null identifier) (user-error "Nothing under point"))
        ((+lookup--jump-to :type-definition identifier nil arg))
        ((user-error "Couldn't find the definition of %S" (substring-no-properties identifier)))))

;;;###autoload
(defun +lookup/references (identifier &optional arg)
  "Show a list of usages of IDENTIFIER (defaults to the symbol at point)

Tries each function in `+lookup-references-functions' until one changes the
point and/or current buffer. Falls back to a naive ripgrep/the_silver_searcher
search otherwise."
  (interactive (list (doom-thing-at-point-or-region)
                     current-prefix-arg))
  (cond ((null identifier) (user-error "Nothing under point"))
        ((+lookup--jump-to :references identifier nil arg))
        ((user-error "Couldn't find references of %S" (substring-no-properties identifier)))))

;;;###autoload
(defun +lookup/documentation (identifier &optional arg)
  "Show documentation for IDENTIFIER (defaults to symbol at point or selection.

First attempts the :documentation handler specified with `set-lookup-handlers!'
for the current mode/buffer (if any), then falls back to the backends in
`+lookup-documentation-functions'."
  (interactive (list (doom-thing-at-point-or-region)
                     current-prefix-arg))
  (cond ((+lookup--jump-to :documentation identifier #'pop-to-buffer arg))
        ((user-error "Couldn't find documentation for %S" (substring-no-properties identifier)))))

;;;###autoload
(defun +lookup/file (&optional path)
  "Figure out PATH from whatever is at point and open it.

Each function in `+lookup-file-functions' is tried until one changes the point
or the current buffer.

Otherwise, falls back on `find-file-at-point'."
  (interactive)
  (cond ((and path
              buffer-file-name
              (file-equal-p path buffer-file-name)
              (user-error "Already here")))

        ((+lookup--jump-to :file path))

        ((user-error "Couldn't find any files here"))))



;;; Config for lookup
;;;==================

;; "What am I looking at?" This module helps you answer this question.
;;
;;   + `+lookup/definition': a jump-to-definition that should 'just work'
;;   + `+lookup/implementations': find a symbol's implementations in the current
;;                                project
;;   + `+lookup/references': find a symbol's references in the current project
;;   + `+lookup/file': open the file referenced at point
;;   + `+lookup/online'; look up a symbol on online resources
;;   + `+lookup/in-docsets': look up in Dash docsets
;;
;; This module uses `xref', an experimental new library in Emacs. It may change
;; in the future. When xref can't be depended on it will fall back to
;; `dumb-jump' to find what you want.

(defvar +lookup-definition-functions
  '(+lookup-xref-definitions-backend-fn
    +lookup-dumb-jump-backend-fn
    +lookup-project-search-backend-fn
    +lookup-evil-goto-definition-backend-fn)
  "Functions for `+lookup/definition' to try, before resorting to `dumb-jump'.
Stops at the first function to return non-nil or change the current
window/point.
If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point. See `set-lookup-handlers!' about adding to
this list.")

(defvar +lookup-implementations-functions ()
  "Function for `+lookup/implementations' to try. Stops at the first function to
return non-nil or change the current window/point.
If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point. See `set-lookup-handlers!' about adding to
this list.")

(defvar +lookup-type-definition-functions ()
  "Functions for `+lookup/type-definition' to try. Stops at the first function to
return non-nil or change the current window/point.
If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point. See `set-lookup-handlers!' about adding to
this list.")

(defvar +lookup-references-functions
  '(+lookup-xref-references-backend-fn
    +lookup-project-search-backend-fn
    +lookup-mu-rgrep-search-backend-fn)
  "Functions for `+lookup/references' to try, before resorting to `dumb-jump'.
Stops at the first function to return non-nil or change the current
window/point.
If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point. See `set-lookup-handlers!' about adding to
this list.")

(defvar +lookup-documentation-functions
  '(+lookup-online-backend-fn)
  "Functions for `+lookup/documentation' to try, before resorting to
`dumb-jump'. Stops at the first function to return non-nil or change the current
window/point.
If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point. See `set-lookup-handlers!' about adding to
this list.")

(defvar +lookup-file-functions
  '(+lookup-bug-reference-backend-fn
    +lookup-ffap-backend-fn)
  "Function for `+lookup/file' to try, before restoring to `find-file-at-point'.
Stops at the first function to return non-nil or change the current
window/point.
If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point. See `set-lookup-handlers!' about adding to
this list.")

;;
;;; xref

;; The lookup commands are superior, and will consult xref if there are no
;; better backends available.
(global-set-key [remap xref-find-definitions] #'+lookup/definition)
(global-set-key [remap xref-find-references]  #'+lookup/references)

(provide 'doom-lookup)
