;;; magit-stgit.el --- StGit extension for Magit

;; Copyright (C) 2011-2014  The Magit Project Developers

;; Author: Lluís Vilanova <vilanova@ac.upc.edu>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>
;; Keywords: vc tools
;; Package: magit-stgit
;; Package-Requires: ((cl-lib "0.5") (magit "2.1.0"))

;; Magit is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Magit is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Magit.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; This package provides very basic support for StGit.
;;
;;   StGit (Stacked Git) is an application that aims to provide a
;;   convenient way to maintain a patch stack on top of a Git branch.
;;
;; For information about StGit see http://www.procode.org/stgit.
;;
;; If you are looking for full fledged StGit support in Emacs, then
;; have a look at `stgit.el' which is distributed with StGit.

;; When `magit-stgit-mode' is turned on then the current patch series
;; is displayed in the status buffer.  While point is on a patch the
;; changes it introduces can be shown using `RET', it can be selected
;; as the current patch using `a', and it can be discarded using `k'.
;; Other StGit commands are available from the StGit popup on `Y'.

;; To enable the mode in a particular repository use:
;;
;;   cd /path/to/repository
;;   git config --add magit.extension stgit
;;
;; To enable the mode for all repositories use:
;;
;;   git config --global --add magit.extension stgit
;;
;; To enable the mode globally without dropping to a shell:
;;
;;   (add-hook 'magit-mode-hook 'magit-stgit-mode)

;;; Code:

(require 'cl-lib)

(require 'magit)
(require 'dash)
(require 's)

;;; Options
;;;; Variables

(defgroup magit-stgit nil
  "StGit support for Magit."
  :group 'magit-extensions)

(defcustom magit-stgit-executable "stg"
  "The name of the StGit executable."
  :group 'magit-stgit
  :type 'string)

(defcustom magit-stgit-show-patch-name t
  "Whether to prefix patch messages with the patch name, in patch series."
  :group 'magit-stgit
  :type 'boolean)

(defcustom magit-stgit-mode-lighter " Stg"
  "Mode-line lighter for Magit-Stgit mode."
  :group 'magit-stgit
  :type 'string)

;;;; Faces

(defgroup magit-stgit-faces nil
  "Faces used by Magit-StGit."
  :group 'magit-stgit
  :group 'magit-faces)

(defface magit-stgit-patch
  '((t :inherit magit-hash))
  "Face for name of a stgit patch."
  :group 'magit-stgit-faces)

(add-to-list 'magit-ref-namespaces
             '("^refs/patches/\\(.+\\)" magit-stgit-patch nil))

(defface magit-stgit-current
  '((((background dark)) (:weight bold :foreground "yellow"))
    (((background light)) (:weight bold :foreground "purple"))
    (t (:weight bold)))
  "Face for the current stgit patch."
  :group 'magit-stgit-faces)

(defface magit-stgit-applied
  '((((background dark)) (:foreground "light yellow"))
    (((background light)) (:foreground "purple"))
    (t ()))
  "Face for an applied stgit patch."
  :group 'magit-stgit-faces)

(defface magit-stgit-unapplied
  '((((background dark)) (:foreground "gray80"))
    (((background light)) (:foreground "orchid"))
    (t ()))
  "Face for an unapplied stgit patch."
  :group 'magit-stgit-faces)

(defface magit-stgit-empty
  '((t :inherit magit-diff-removed))
  "Face for an empty stgit patch."
  :group 'magit-stgit-faces)

(defface magit-stgit-hidden
  '((t :inherit magit-diff-context))
  "Face for an hidden stgit patch."
  :group 'magit-stgit-faces)

;;; Utilities

(defun magit-run-stgit (&rest args)
  (apply #'magit-call-process magit-stgit-executable (-flatten args))
  (magit-refresh))

(defun magit-stgit-lines (&rest args)
  (with-temp-buffer
    (apply 'process-file magit-stgit-executable nil (list t nil) nil args)
    (split-string (buffer-string) "\n" 'omit-nulls)))

(defvar magit-stgit-read-patch-history nil)

(defun magit-stgit-read-patch (prompt &optional require-match)
  (magit-completing-read prompt (magit-stgit-lines "series" "--noprefix")
                         nil require-match
                         nil 'magit-stgit-read-patch-history))

(defun magit-stgit-read-args (prompt)
  (list (or (magit-section-when stgit-patch)
            (magit-stgit-read-patch prompt t))))

;;; Commands

(magit-define-popup magit-stgit-popup
  "Popup console for StGit commands."
  'magit-popups
  :actions '((?\r "Show"      magit-stgit-show)
             (?a  "Goto"      magit-stgit-goto-popup)
             (?k  "Delete"    magit-stgit-delete-popup)
             (?r  "Rebase"    magit-stgit-rebase-popup)
             (?g  "Refresh"   magit-stgit-refresh-popup)
             (?R  "Repair"    magit-stgit-repair)

             (?i  "Init"      magit-stgit-init)
             (?n  "New"       magit-stgit-new)
             (?p  "Pop"       magit-stgit-pop-popup)
             (?P  "Push"      magit-stgit-push-popup)
             (?u  "Undo"      magit-stgit-undo-popup)
             (?X  "Redo"      magit-stgit-redo-popup)
             (?c  "Commit"    magit-stgit-commit-popup)
             (?U  "Uncommit"  magit-stgit-uncommit-popup)
             (?C  "Rename"    magit-stgit-rename)

             (?S  "Sink"      magit-stgit-sink-popup)
             (?F  "Float"     magit-stgit-float-popup)
             (?h  "Hide"      magit-stgit-hide)
             (?H  "Unhide"    magit-stgit-unhide)))

(magit-define-popup magit-stgit-undo-popup
  "Popup console for StGit 'undo' command."
  'magit-stgit-popup
  :actions '((?u "Undo" magit-stgit-undo))
  :options '((?n "Undo the last N commands" "--number=" read-number))
  :switches '((?h "Discard index/worktree changes" "--hard")))

(magit-define-popup magit-stgit-redo-popup
  "Popup console for StGit 'redo' command."
  'magit-stgit-popup
  :actions '((?r "Redo" magit-stgit-redo))
  :options '((?n "Undo the last N undos" "--number=" read-number))
  :switches '((?h "Discard index/worktree changes" "--hard")))

(magit-define-popup magit-stgit-goto-popup
  "Popup console for StGit 'goto' command."
  'magit-stgit-popup
  :actions '((?g "Goto" magit-stgit-goto))
  :switches '((?k "Keep local changes" "--keep")))

(magit-define-popup magit-stgit-refresh-popup
  "Popup console for StGit 'refresh' command."
  'magit-stgit-popup
  :actions '((?r "Refresh" magit-stgit-refresh))
  :switches '((?u "Only update current patch files" "--update")
              (?i "Refresh from index only" "--index"))
  :options '((?p "Refresh patch by name" "--patch=" magit-stgit-read-patch)))

(magit-define-popup magit-stgit-commit-popup
  "Popup console for StGit 'commit' command."
  'magit-stgit-popup
  :actions '((?c "Commit" magit-stgit-commit))
  :switches '((?a "Commit all applied patches" "--all"))
  :options '((?n "Commit N patches" "--number=" read-number)))

(magit-define-popup magit-stgit-uncommit-popup
  "Popup console for StGit 'uncommit' command."
  'magit-stgit-popup
  :actions '((?u "Uncommit" magit-stgit-uncommit))
  :switches '((?x "Exclude the commit specified by --to" "--exclusive"))
  :options '((?n "Uncommit N commits" "--number=" read-number)
             (?t "Uncommit to the specified commit" "--to="  read-from-minibufffer ; FIXME
                 )))

(magit-define-popup magit-stgit-delete-popup
  "Popup console for StGit 'delete' command."
  'magit-stgit-popup
  :actions '((?d "Delete" magit-stgit-delete))
  :switches '((?s "Spill patch contents to index and worktree" "--spill")))

(magit-define-popup magit-stgit-rebase-popup
  "Popup console for StGit 'rebase' command."
  'magit-stgit-popup
  :actions '((?r "Rebase" magit-stgit-rebase))
  :switches '((?n "Do not push the patches back after rebase" "--nopush")
              (?m "Check for patches merged upstream" "--merged")))

(magit-define-popup magit-stgit-rebase-popup
  "Popup console for StGit 'rebase' command."
  'magit-stgit-popup
  :actions '((?r "Rebase" magit-stgit-rebase))
  :switches '((?n "Do not push the patches back after rebase" "--nopush")
              (?m "Check for patches merged upstream" "--merged")))

(magit-define-popup magit-stgit-pop-popup
  "Popup console for StGit 'pop' command."
  'magit-stgit-popup
  :actions '((?p "Pop" magit-stgit-pop))
  :switches '((?a "Pop all the applied patches" "--all")
              (?k "Keep the local changes" "--keep"))
  :options '((?n "Pop N patches" "--number=" read-number)))

(magit-define-popup magit-stgit-push-popup
  "Popup console for StGit 'push' command."
  'magit-stgit-popup
  :actions '((?p "Push" magit-stgit-push))
  :switches '((?a "Push all the applied patches" "--all")
              (?k "Keep the local changes" "--keep")
              (?r "Push the patches in reverse order" "--reverse")
              (?s "Push the patch with the original tree" "--set-tree")
              (?m "Check for patches merged upstream" "--merged"))
  :options '((?n "Push N patches" "--number=" read-number)))

(magit-define-popup magit-stgit-sink-popup
  "Popup console for Stgit 'sink' command."
  'magit-stgit-popup
  :actions '((?s "Sink" magit-stgit-sink))
  :switches '((?n "Do not push the patches back after rebase" "--nopush")
              (?k "Keep the local changes" "--keep"))
  :options '((?t "Sink patch below the target patch" "--to=" magit-stgit-read-patch)))

(magit-define-popup magit-stgit-float-popup
  "Popup console for Stgit 'float' command."
  'magit-stgit-popup
  :actions '((?f "Float" magit-stgit-float))
  :switches '((?k "Keep the local changes" "--keep")))

(defun magit-stgit-run-refresh (&rest args)
  (magit-run-stgit "refresh" args))

;;;###autoload
(defun magit-stgit-refresh ()
  "Refresh a StGit PATCH."
  (interactive)
  (magit-stgit-run-refresh magit-current-popup-args))

(defun magit-stgit-quick-refresh ()
  "Refresh the StGit patch at point."
  (interactive)
  (let ((patch (magit-section-when stgit-patch)))
    (when patch
      (magit-stgit-run-refresh "-i" "-p" patch))))

;;;###autoload
(defun magit-stgit-repair ()
  "Repair StGit metadata if branch was modified with git commands.
In the case of Git commits these will be imported as new patches
into the series."
  (interactive)
  (message "Repairing series...")
  (magit-run-stgit "repair")
  (message "Repairing series...done"))

;;;###autoload
(defun magit-stgit-rebase ()
  "Rebase a StGit patch series."
  (interactive)
  (let ((remote (magit-get-current-remote))
        (branch (magit-get-current-branch)))
    (if (not (and remote branch))
        (user-error "Branch has no upstream")
      (when (y-or-n-p "Update remote first? ")
        (message "Updating remote...")
        (magit-run-git-async "remote" "update" remote)
        (message "Updating remote...done"))
      (magit-run-stgit "rebase" magit-current-popup-args (format "remotes/%s/%s" remote branch)))))


;;;###autoload
(defun magit-stgit-init ()
  "Initialize StGit support for the current branch."
  (interactive)
  (magit-run-stgit "init"))

;;;###autoload
(defun magit-stgit-new (patch message)
  "Create new StGit patch."
  (interactive "sNew patch: \nsMessage: ")
  (magit-run-stgit "new" "-m" message patch))

;;;###autoload
(defun magit-stgit-float (patch)
  "Float StGit patch to the top of the stack."
  (interactive (magit-stgit-read-args "Float patch"))
  (magit-run-stgit "float" magit-current-popup-args patch))

;;;###autoload
(defun magit-stgit-sink (patch)
  "Sink StGit patch down the stack."
  (interactive (magit-stgit-read-args "Sink patch"))
  (magit-run-stgit "sink" magit-current-popup-args patch))

(defun magit-stgit-sink-1 (patch)
  "Sink StGit patch one position down the stack."
  (interactive (magit-stgit-read-args "Sink patch"))
  (let* ((series (magit-stgit-lines "series" "--noprefix"))
         (patch-position (-elem-index patch series)))
    (when (and patch-position (> patch-position 0))
      (magit-run-stgit "sink" "-t" (elt series (1- patch-position)) patch))))

;;;###autoload
(defun magit-stgit-pop (patch)
  "Pop the topmost StGit patch from the stack."
  (interactive (magit-stgit-read-args "Pop patch"))
  (magit-run-stgit "pop" magit-current-popup-args patch))

;;;###autoload
(defun magit-stgit-push (patch)
  "Push a StGit patch to the stack."
  (interactive (magit-stgit-read-args "Push patch"))
  (magit-run-stgit "push" magit-current-popup-args patch))

;;;###autoload
(defun magit-stgit-undo ()
  "Undo the last operation."
  (interactive)
  (magit-run-stgit "undo" magit-current-popup-args))

;;;###autoload
(defun magit-stgit-redo ()
  "Undo the last operation."
  (interactive)
  (magit-run-stgit "redo" magit-current-popup-args))

;; TODO: edit, squash, clean

(defun magit-stgit-run-commit (&rest args)
  (magit-run-stgit "commit" args))

;;;###autoload
(defun magit-stgit-commit ()
  "Permanently commit the applied patches."
  (interactive)
  (magit-stgit-run-commit magit-current-popup-args))

(defun magit-stgit-quick-commit ()
  "Commit the patch at point."
  (interactive)
  (let ((patch (magit-section-when stgit-patch)))
    (when patch
      (magit-stgit-run-commit patch))))

(defun magit-stgit-uncommit ()
  "Turn Git commits into StGit patches."
  (interactive)
  (magit-run-stgit "uncommit" magit-current-popup-args))

(defun magit-stgit-rename (patch newpatch)
  "Rename a patch."
  (interactive (let* ((orig (car (magit-stgit-read-args "Patch to rename")))
                      (new (read-from-minibuffer (format "Rename '%s' to: " orig))))
                 (list orig new)))
  (magit-run-stgit "rename" patch newpatch))



;;;###autoload
(defun magit-stgit-spill (patch)
  "Discard a StGit patch, spill the diff."
  (interactive (magit-stgit-read-args "Spill patch"))
  (when (yes-or-no-p (format "Discard and spill patch `%s'? " patch))
    (magit-run-stgit "delete" "--spill" patch)))

;;;###autoload
(defun magit-stgit-delete (patch)
  "Delete a StGit patch."
  (interactive (magit-stgit-read-args "Delete patch"))
  (when (yes-or-no-p (format "Delete patch `%s'? " patch))
    (magit-run-stgit "delete" magit-current-popup-args patch)))

;;;###autoload
(defun magit-stgit-goto (patch)
  "Set PATCH as target of StGit push and pop operations."
  (interactive (magit-stgit-read-args "Goto patch"))
  (magit-run-stgit "goto" patch magit-current-popup-args))

;;;###autoload
(defun magit-stgit-show (patch)
  "Show diff of a StGit patch."
  (interactive (magit-stgit-read-args "Show patch"))
  (magit-show-commit (magit-stgit-lines "id" patch)))

(defun magit-stgit-hide (patch)
  "Hide a StGit patch."
  (interactive (magit-stgit-read-args "Hide patch"))
  (magit-run-stgit "hide" patch))

(defun magit-stgit-unhide (patch)
  "Unhide a StGit patch."
  (interactive (magit-stgit-read-args "Unhide patch"))
  (magit-run-stgit "unhide" patch))

;;; Mode

(defvar magit-stgit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "Y" 'magit-stgit-popup)
    map))

;;;###autoload
(define-minor-mode magit-stgit-mode
  "StGit support for Magit."
  :lighter magit-stgit-mode-lighter
  :keymap  magit-stgit-mode-map
  (unless (derived-mode-p 'magit-mode)
    (user-error "This mode only makes sense with Magit"))
  (if magit-stgit-mode
      (magit-add-section-hook 'magit-status-sections-hook
                              'magit-insert-stgit-series
                              'magit-insert-stashes t t)
    (remove-hook 'magit-status-sections-hook
                 'magit-insert-stgit-series t))
  (when (called-interactively-p 'any)
    (magit-refresh)))

;;;###autoload
(custom-add-option 'magit-mode-hook #'magit-stgit-mode)

(easy-menu-define magit-stgit-mode-menu nil "Magit-Stgit mode menu"
  '("StGit" :visible magit-stgit-mode
    ["Initialize" magit-stgit-init
     :help "Initialize StGit in the current branch"]
    "---"
    ["New patch" magit-stgit-new
     :help "Create new patch"]
    ["Refresh patch" magit-stgit-refresh
     :help "Refresh the contents of a patch in an StGit series"]
    ["Repair" magit-stgit-repair
     :help "Repair StGit metadata if branch was modified with git commands"]
    ["Rebase series" magit-stgit-rebase
     :help "Rebase an StGit patch series"]
    "---"
    ["Undo" magit-stgit-undo
     :help "Undo the last operation"]
    ["Redo" magit-stgit-redo
     :help "Undo the last undo operation"]))

(easy-menu-add-item 'magit-mode-menu '("Extensions") magit-stgit-mode-menu)

;;; Sections

(defconst magit-stgit-patch-re
  "^\\(.\\)\\([-+>!]\\) \\([^ ]+\\) +# \\(.*\\)$")

(defvar magit-stgit-patch-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n"  'magit-stgit-new)
    (define-key map "k"  'magit-stgit-delete)
    (define-key map "a"  'magit-stgit-goto)
    (define-key map "\r" 'magit-stgit-show)
    (define-key map "s"  'magit-stgit-spill)
    (define-key map "r"  'magit-stgit-rename)
    (define-key map "c"  'magit-stgit-commit)
    (define-key map "S"  'magit-stgit-sink-1)
    (define-key map "F"  'magit-stgit-float)
    (define-key map "f"  'magit-stgit-quick-refresh)
    (define-key map "h"  'magit-stgit-hide)
    (define-key map "u"  'magit-stgit-unhide)
    map))

(defun magit-insert-stgit-series ()
  (when magit-stgit-mode
    (magit-insert-section (series)
      (magit-insert-heading "Patch series:")
      (let ((beg (point)))
        (process-file magit-stgit-executable nil (list t nil) nil
                      "series" "--all" "--empty" "--description")
        (if (= (point) beg)
            (magit-cancel-section)
          (save-restriction
            (narrow-to-region beg (point))
            (goto-char beg)
            (magit-wash-sequence #'magit-stgit-wash-patch)))
        (insert ?\n)))))

(defun magit-stgit-wash-patch ()
  (when (looking-at magit-stgit-patch-re)
    (magit-bind-match-strings (empty state patch msg) nil
      (delete-region (point) (point-at-eol))
      (magit-insert-section (stgit-patch patch)
        (let ((patch-face (cond ((equal state ">") 'magit-stgit-current)
                                ((equal state "+") 'magit-stgit-applied)
                                ((equal state "-") 'magit-stgit-unapplied)
                                ((equal state "!") 'magit-stgit-hidden)
                                (t (user-error "Unknown stgit patch state: %s"
                                               state)))))
          (magit-insert state patch-face)
          (magit-insert empty 'magit-stgit-empty ?\s)
          (when magit-stgit-show-patch-name
            (magit-insert patch patch-face (s-repeat (+ 4 (- 30 (length patch))) " ")))
          (insert msg)
          (put-text-property (line-beginning-position) (1+ (line-end-position))
                             'keymap 'magit-stgit-patch-map)
          (forward-line))))))

;;; magit-stgit.el ends soon

(define-obsolete-function-alias 'turn-on-magit-stgit 'magit-stgit-mode)

(provide 'magit-stgit)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-stgit.el ends here
