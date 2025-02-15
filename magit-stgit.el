;;; magit-stgit.el --- StGit extension for Magit  -*- lexical-binding: t; -*-

;; Copyright (C) 2011-2025  The Magit Project Contributors

;; Author: Lluís Vilanova <vilanova@ac.upc.edu>
;; Maintainer: Peter Grayson <pete@jpgrayson.net>
;; Homepage: https://github.com/stacked-git/magit-stgit
;; Keywords: git tools vc

;; Package-Requires: (
;;     (emacs "27.1")
;;     (llama "0.6.0")
;;     (magit "4.3.0")
;;     (transient "0.8.4"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Magit-StGit is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Magit-StGit is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Magit-StGit.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; This package is no longer maintained.

;; This package provides very basic support for StGit.
;;
;;   StGit (Stacked Git) is an application that aims to provide a
;;   convenient way to maintain a patch stack on top of a Git branch.
;;
;; For information about StGit see http://stacked-git.github.io.
;;
;; If you are looking for full fledged StGit support in Emacs, then
;; have a look at `stgit.el' which is distributed with StGit.

;; When `magit-stgit-mode' is turned on then the current patch series
;; is displayed in the status buffer.  While point is on a patch the
;; changes it introduces can be shown using `RET', it can be selected
;; as the current patch using `a', and it can be discarded using `k'.
;; Other StGit commands are available from the StGit transient on `/'.

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
(require 'llama)
(require 'seq)
(require 'subr-x)

(require 'magit)
(require 'transient)

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
             (cons "^refs/patches/\\(.+\\)" 'magit-stgit-patch))

(defface magit-stgit-current
  '((((background dark)) (:weight bold :foreground "yellow"))
    (((background light)) (:weight bold :foreground "purple"))
    (t (:weight bold)))
  "Face for the current stgit patch."
  :group 'magit-stgit-faces)

(defface magit-stgit-applied
  '((t :inherit magit-cherry-equivalent))
  "Face for an applied stgit patch."
  :group 'magit-stgit-faces)

(defface magit-stgit-unapplied
  '((t :inherit magit-cherry-unmatched))
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
  "Run StGit command with given arguments.
Any list in ARGS is flattened."
  (magit-run-stgit-callback nil args))

(defun magit-run-stgit-async (&rest args)
  "Asynchronously run StGit command with given arguments.
Any list in ARGS is flattened."
  (with-editor "GIT_EDITOR"
    (let ((magit-process-popup-time -1))
      (message "Running %s %s" magit-stgit-executable
               (mapconcat 'identity (flatten-tree args) " "))
      (apply #'magit-start-process magit-stgit-executable nil
             (flatten-tree args)))))

(defun magit-run-stgit-and-mark-remove (patches &rest args)
  "Run `magit-run-stgit' and `magit-stgit-mark-remove'.
Argument PATCHES sets the marks to remove, and ARGS the arguments to StGit."
  (magit-run-stgit-callback (##magit-stgit-mark-remove patches) args))

(defun magit-run-stgit-callback (callback &rest args)
  "Run StGit command with given arguments.
Function CALLBACK will be executed before refreshing the buffer.
Any list in ARGS is flattened."
  (apply #'magit-call-process magit-stgit-executable (-flatten args))
  (when callback
    (funcall callback))
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

(defun magit-stgit-patches-sorted (patches)
  "Return elements in PATCHES with the same partial order as the series."
  (seq-keep (##car (member % patches))
            (magit-stgit-lines "series" "--noprefix")))

;;; Marking

(defvar-local magit-stgit--marked-patches nil
  "List of currently marked patches.")

(defun magit-stgit-read-patches (use-region use-marks use-point
                                 require-match prompt)
  "Return a list of patches selected according to the arguments.

If USE-REGION is non-nil and there is an active region, return
marked patches in the region if USE-MARKS is non-nil, or all
patches in the region if USE-MARKS is nil.

Otherwise, if USE-MARKS is non-nil and some patches are marked,
return those.

Otherwise, if USE-POINT is non-nil, return the patch at point.

Otherwise, if PROMPT is non-nil, ask the user for the name of a
patch using `magit-stgit-read-patch', passing PROMPT and
REQUIRE-MATCH."
  (let* ((region (and use-region (magit-region-values 'stgit-patch)))
         (intersection (cl-intersection region magit-stgit--marked-patches
                                        :test #'equal)))
    (or (and use-marks intersection)
        region
        (and use-marks (magit-stgit-patches-sorted magit-stgit--marked-patches))
        (and use-point (append (magit-section-value-if 'stgit-patch) nil))
        (and prompt (append (magit-stgit-read-patch prompt require-match) nil)))))

(defun magit-stgit-mark-contains (patch)
  "Return whether the given PATCH is marked."
  (member patch magit-stgit--marked-patches))

(defun magit-stgit-mark-add (patches)
  "Add PATCHES to the set of marked patches.

If called interactively, mark the patches around point or read
one from the minibuffer, and move to the next line."
  (interactive (list (magit-stgit-read-patches t nil t t "Patch name")))
  (setq magit-stgit--marked-patches
        (cl-union magit-stgit--marked-patches patches :test #'equal))
  (when (called-interactively-p 'any)
    (forward-line)
    (magit-refresh)))

(defun magit-stgit-mark-remove (patches)
  "Remove PATCHES from the set of marked patches.

If called interactively, unmark the patches around point or read
one from the minibuffer, and move to the next line."
  (interactive (list (magit-stgit-read-patches t nil t t "Patch name")))
  (setq magit-stgit--marked-patches
        (cl-set-difference magit-stgit--marked-patches patches :test #'equal))
  (when (called-interactively-p 'any)
    (forward-line)
    (magit-refresh)))

(defun magit-stgit-mark-toggle (patches)
  "Toggle the marked status of PATCHES, adding or removing each
from the set of marked patches as necessary.

If called interactively, toggle the patches around point or read
one from the minibuffer, and move to the next line."
  (interactive (list (magit-stgit-read-patches t nil t t "Patch name")))
  (setq magit-stgit--marked-patches
        (cl-set-exclusive-or magit-stgit--marked-patches patches :test #'equal))
  (when (called-interactively-p 'any)
    (forward-line)
    (magit-refresh)))

;;; Commands

(transient-define-prefix magit-stgit-dispatch ()
  "Manipulate StGit patches."
  :man-page "stg"
  ["Stack"
   [("i"   "Init"         magit-stgit-init)
    ("f"   "Float"        magit-stgit-float)
    ("s"   "Sink"         magit-stgit-sink)
    ("a"   "Goto"         magit-stgit-goto)]
   [("c"   "Commit"       magit-stgit-commit)
    ("C"   "Uncommit"     magit-stgit-uncommit)
    ("b"   "Branch"       magit-stgit-branch)
    ("r"   "Repair"       magit-stgit-repair)
    ("R"   "Rebase"       magit-stgit-rebase)]
   [("z"   "Undo"         magit-stgit-undo)
    ("Z"   "Redo"         magit-stgit-redo)]]
  ["Patch"
   [("N"   "New"          magit-stgit-new)
    ("g"   "Refresh"      magit-stgit-refresh)
    ("RET" "Show"         magit-stgit-show)]
   [("e"   "Edit"         magit-stgit-edit)
    ("n"   "Rename"       magit-stgit-rename)
    ("k"   "Delete"       magit-stgit-delete)]
   [("m"   "Mail patches" magit-stgit-mail)]])

;;;###autoload
(defun magit-stgit-init ()
  "Initialize StGit support for the current branch."
  (interactive)
  (magit-run-stgit "init"))

(defvar magit-stgit-new-filename-regexp ".stgit-\\(new\\|edit\\).txt")

(defun magit-stgit-new-check-buffer ()
  "Check if buffer is an StGit commit message."
  ;; TODO: must remove the stray file on cancel
  (and buffer-file-name
       (string-match-p magit-stgit-new-filename-regexp buffer-file-name)
       (git-commit-setup)))

(transient-define-prefix magit-stgit-new ()
  "Create a new empty patch on top of the stack."
  :man-page "stg-new"
  ["Arguments"
   ("-a" "Add Acked-by" "--ack")
   ("-s" "Add Signed-off-by" "--sign")]
  ["Actions"
   ("N" "New" magit-stgit--new)])

;;;###autoload
(defun magit-stgit--new (&optional name &rest args)
  "Invoke `stg new ARGS... NAME'.

If NAME is nil, let `stg new' read the name using the editor.

If called interactively, read NAME from the minibuffer."
  (interactive (cons (let ((name (read-string "Patch name: ")))
                       (and (not (string= name "")) name))
                     (transient-args 'magit-stgit-new)))
  (magit-run-stgit-async "new" args name))

(transient-define-prefix magit-stgit-edit ()
  "Edit the description of an existing patch."
  :man-page "stg-edit"
  ["Arguments"
   ("-s" "Add Signed-off-by" "--sign")
   ("-a" "Add Acked-by" "--ack")
   ("-r" "Add Reviewed-by" "--review")]
  ["Actions"
   ("e" "Edit" magit-stgit--edit)])

;;;###autoload
(defun magit-stgit--edit (&optional patch &rest args)
  "Invoke `stg edit ARGS... PATCH'.

If PATCH is nil, edit the current patch.

If called interactively, edit the patch at point or read one from
the minibuffer."
  (interactive (cons (car (magit-stgit-read-patches nil nil t t "Edit patch"))
                     (transient-args 'magit-stgit-edit)))
  (magit-run-stgit-async "edit" "--edit" args patch))

(transient-define-prefix magit-stgit-float ()
  "Move a set of patches toward the top of the stack."
  :man-page "stg-float"
  ["Arguments"
   ("-k" "Keep the local changes" "--keep")]
  ["Actions"
   ("f" "Float" magit-stgit--float)])

;;;###autoload
(defun magit-stgit--float (patches &rest args)
  "Invoke `stg float ARGS... PATCHES...'.

If called interactively, float the patches around point or read
one from the minibuffer."
  (interactive (cons (magit-stgit-read-patches t t t t "Float patch")
                     (transient-args 'magit-stgit-float)))
  (magit-run-stgit-and-mark-remove patches "float" args patches))

;;;###autoload
(defun magit-stgit-rename (oldname newname)
  "Invoke `stg rename OLDNAME NEWNAME'.

If called interactively, read OLDNAME and NEWNAME from the
minibuffer."
  (interactive
   (list (magit-stgit-read-patch "Patch to rename" t)
         (read-from-minibuffer "New name: ")))
  (magit-run-stgit "rename" oldname newname))

(transient-define-prefix magit-stgit-sink ()
  "Move a set of patches toward the bottom of the stack."
  :man-page "stg-sink"
  ["Arguments"
   ("-k" "Keep the local changes" "--keep")
   ("-t" "Sink patches below target" "--to="
    :reader (lambda (_prompt _initial-input _history)
              (magit-stgit-read-patch "Sink below" t)))]
  ["Actions"
   ("s" "Sink" magit-stgit--sink)])

;;;###autoload
(defun magit-stgit--sink (&optional patches &rest args)
  "Invoke `stg sink ARGS... PATCHES...'.

If PATCHES is nil, sink the current patch.

If called interactively, sink the patches around point or read
one from the minibuffer. Read the target patch from the
minibuffer as well."
  (interactive (cons (magit-stgit-read-patches t t t t "Sink patch")
                     (transient-args 'magit-stgit-sink)))
  (let ((target (and (called-interactively-p 'any)
                     (not transient-current-prefix)
                     (magit-stgit-read-patch "Sink below" t))))
    (magit-run-stgit-and-mark-remove
     patches (and target (list "-t" target)) "sink" args patches)))

(transient-define-prefix magit-stgit-commit ()
  "Commit a set of patches."
  :man-page "stg-commit"
  ["Arguments"
   ("-a" "Commit all applied patches" "--all")
   ("-e" "Allow empty patches to be committed" "--allow-empty")
   ("-n" "Commit the first N patches from the bottom up" "--number="
    :reader (lambda (prompt _initial-input history)
              (number-to-string (read-number prompt nil history))))]
  ["Actions"
   ("c" "Commit" magit-stgit--commit)])

(defun magit-stgit--commit-need-patches-p (args)
  (and (not (member "--all" args))
       (not (member "--number" args))
       (not (transient-arg-value "--number=" args))))

;;;###autoload
(defun magit-stgit--commit (patches &rest args)
  "Invoke `stg commit ARGS... PATCHES...'.

If PATCHES is nil, commit the bottommost patch.

PATCHES is ignored if ARGS contains `--all' or `--number'.

If called interactively, commit the patches around point or read
one from the minibuffer."
  (interactive (let ((args (transient-args 'magit-stgit-commit)))
                 (cons (and (magit-stgit--commit-need-patches-p args)
                            (magit-stgit-read-patches t t t t "Commit patch"))
                       args)))
  (let ((patches (and (magit-stgit--commit-need-patches-p args)
                      (or patches (error "No patches provided")))))
    (magit-run-stgit-and-mark-remove patches "commit" args patches)))

(transient-define-prefix magit-stgit-uncommit ()
  "Uncommit a set of patches."
  :man-page "stg-uncommit"
  ["Arguments"
   ("-n" "Uncommit the first N commits from the base down" "--number="
    :reader (lambda (prompt _initial-input history)
              (number-to-string (read-number prompt nil history))))]
  ["Actions"
   ("C" "Uncommit" magit-stgit--uncommit)])

;;;###autoload
(defun magit-stgit--uncommit (&rest args)
  "Invoke `stg uncommit ARGS...'."
  (interactive (transient-args 'magit-stgit-uncommit))
  (magit-run-stgit "uncommit" args))

(transient-define-prefix magit-stgit-refresh ()
  "Include the latest changes into a patch."
  :man-page "stg-refresh"
  ["Arguments"
   ("-u" "Only update the current patch files" "--update")
   ("-i" "Refresh from index instead of worktree" "--index")
   ("-F" "Force refresh even if index is dirty" "--force")
   ("-e" "Edit the patch description" "--edit")
   ("-s" "Add Signed-off-by" "--sign")
   ("-a" "Add Acked-by" "--ack")]
  ["Actions"
   ("g" "Refresh" magit-stgit--refresh)])

;;;###autoload
(defun magit-stgit--refresh (&optional patch &rest args)
  "Invoke `stg refresh --patch PATCH ARGS...'.

If PATCH is nil, refresh the current patch.

If called interactively, refresh the patch at point or read one
from the minibuffer."
  (interactive (cons (car (magit-stgit-read-patches
                           nil nil t t "Refresh patch"))
                     (transient-args 'magit-stgit-refresh)))
  (magit-run-stgit-async "refresh" (and patch (list "--patch" patch)) args))

;;;###autoload
(defun magit-stgit-repair ()
  "Repair StGit metadata if branch was modified with git commands.
In the case of Git commits these will be imported as new patches
into the series."
  (interactive)
  (message "Repairing series...")
  (magit-run-stgit "repair")
  (message "Repairing series...done"))

(transient-define-prefix magit-stgit-branch ()
  "Popup console for StGit branch."
  :man-page "stg-branch"
  ["Arguments"
   ("-f" "Force cleanup or deletion even if a branch has patches" "--force")]
  ["Branch action"
   ("c"  "Create"  magit-stgit-branch-create)
   ("C"  "Clone"  magit-stgit-branch-clone)
   ("m"  "Rename"  magit-stgit-branch-rename)
   ("p"  "Protect"  magit-stgit-branch-protect)
   ("u"  "Unprotect"  magit-stgit-branch-unprotect)
   ("k"  "Delete"  magit-stgit-branch-delete)
   ("K"  "Cleanup"  magit-stgit-branch-cleanup)])

;;;###autoload
(defun magit-stgit-branch-create (start branch-name)
  "Create and switch to a new branch"
  (interactive (list (magit-read-starting-point "Create and checkout branch")
                     (read-from-minibuffer "Name for new branch: ")))
  (magit-run-stgit "branch" "--create" branch-name start))

;;;###autoload
(defun magit-stgit-branch-clone (target)
  "Clone the contents of the current branch"
  (interactive (list (read-from-minibuffer "New branch name: " (magit-get-current-branch))))
  (magit-run-stgit "branch" "--clone" target))

;;;###autoload
(defun magit-stgit-branch-rename (old-name new-name)
  "Rename an existing branch"
  (interactive (list (magit-read-local-branch "Branch to rename")
                     (read-from-minibuffer "New branch name: ")))
  (magit-run-stgit "branch" "--rename" old-name new-name))

;;;###autoload
(defun magit-stgit-branch-protect (target)
  "Prevent StGit from modifying a branch"
  (interactive (list (magit-read-local-branch "Branch to protect")))
  (magit-run-stgit "branch" "--protect" target))

;;;###autoload
(defun magit-stgit-branch-unprotect (target)
  "Allow StGit to modify a previously protected branch"
  (interactive (list (magit-read-local-branch "Branch to unprotect")))
  (magit-run-stgit "branch" "--unprotect" target))

;;;###autoload
(defun magit-stgit-branch-delete (target &rest args)
  "Invoke `stg uncommit ARGS...'."
  (interactive (append (list (magit-read-local-branch "Branch to delete"))
                       (transient-args 'magit-stgit-branch)))
  (magit-run-stgit "branch" "--delete" target args))

;;;###autoload
(defun magit-stgit-branch-cleanup (target &rest args)
  "Invoke `stg uncommit ARGS...'."
  (interactive (append (list (magit-read-local-branch "Branch to clean up"))
                       (transient-args 'magit-stgit-branch)))
  (magit-run-stgit "branch" "--cleanup" target args))


(transient-define-prefix magit-stgit-rebase ()
  "Rebase the stack."
  :man-page "stg-rebase"
  ["Arguments"
   ("-n" "Do not push the patches back after rebasing" "--nopush")
   ("-m" "Check for patches merged upstream" "--merged")]
  ["Actions"
   ("R" "Rebase" magit-stgit--rebase)])

;;;###autoload
(defun magit-stgit--rebase (remote branch &rest args)
  "Invoke `stg rebase ARGS... remotes/REMOTE/BRANCH'.

If called interactively, use the current branch and its remote,
and ask whether to update the remote first."
  (interactive (append (let* ((branch (magit-get-current-branch))
                              (remote (magit-get-remote branch)))
                         (if (and remote branch)
                             (list remote branch)
                           (user-error "Branch has no upstream")))
                       (transient-args 'magit-stgit-rebase)))
  (when (and (called-interactively-p 'any)
             (y-or-n-p "Update remote first? "))
    (message "Updating remote...")
    (magit-run-git-async "remote" "update" remote)
    (message "Updating remote...done"))
  (magit-run-stgit "rebase" args (format "remotes/%s/%s" remote branch)))

(transient-define-prefix magit-stgit-delete ()
  "Delete a set of patches."
  :man-page "stg-delete"
  ["Arguments"
   ("-s" "Spill patch contents to worktree and index" "--spill")]
  ["Actions"
   ("k" "Delete" magit-stgit--delete)])

;;;###autoload
(defun magit-stgit--delete (patches &rest args)
  "Invoke `stg delete ARGS... PATCHES...'.

If called interactively, delete the patches around point or read
one from the minibuffer. Ask whether to spill the contents and
ask for confirmation before deleting."
  (interactive (cons (magit-stgit-read-patches t t t t "Delete patch")
                     (transient-args 'magit-stgit-delete)))
  (let* ((non-empty-p (seq-some (##magit-stgit-lines "files" "--bare" %)
                                patches))
         (args (if (and (called-interactively-p 'any)
                        (not transient-current-prefix)
                        non-empty-p
                        (y-or-n-p "Spill contents? "))
                   (cons "--spill" args)
                 args))
         (spillp (member "--spill" args)))
    (when (or (not (called-interactively-p 'any))
              (yes-or-no-p
               (format "Delete%s patch%s %s? "
                       (if spillp " and spill" "")
                       (if (> (length patches) 1) "es" "")
                       (mapconcat (##format "`%s'" %) patches ", "))))
      (magit-run-stgit-and-mark-remove patches "delete" args patches))))

(transient-define-prefix magit-stgit-goto ()
  "Make an arbitrary patch current."
  :man-page "stg-goto"
  ["Arguments"
   ("-k" "Keep the local changes" "--keep")
   ("-m" "Check for patches merged upstream" "--merged")]
  ["Actions"
   ("a" "Goto" magit-stgit--goto)])

;;;###autoload
(defun magit-stgit--goto (patch &rest args)
  "Invoke `stg goto ARGS... PATCH'."
  (interactive (cons (car (magit-stgit-read-patches nil nil t t "Goto patch"))
                     (transient-args 'magit-stgit-goto)))
  (magit-run-stgit "goto" args patch))

;;;###autoload
(defun magit-stgit-show (patch)
  "Show diff of a StGit patch."
  (interactive (magit-stgit-read-patches nil nil t t "Show patch"))
  (magit-show-commit (car (magit-stgit-lines "id" patch))))

(transient-define-prefix magit-stgit-undo ()
  "Undo a previous stack operation."
  :man-page "stg-undo"
  ["Arguments"
   ("-n" "Undo the last N operations" "--number="
    :reader (lambda (prompt _initial-input history)
              (number-to-string (read-number prompt nil history))))
   ("-h" "Discard changes in index/worktree" "--hard")]
  ["Actions"
   ("z"  "Undo" magit-stgit--undo)])

;;;###autoload
(defun magit-stgit--undo (&rest args)
  "Invoke `stg undo ARGS...'."
  (interactive (transient-args 'magit-stgit-undo))
  (magit-run-stgit "undo" args))

(transient-define-prefix magit-stgit-redo ()
  "Undo a previous undo operation."
  :man-page "stg-redo"
  ["Arguments"
   ("-n" "Undo the last N undos" "--number="
    :reader (lambda (prompt _initial-input history)
              (number-to-string (read-number prompt nil history))))
   ("-h" "Discard changes in index/worktree" "--hard")]
  ["Actions"
   ("Z" "Redo" magit-stgit--redo)])

;;;###autoload
(defun magit-stgit--redo (&rest args)
  "Invoke `stg redo ARGS...'."
  (interactive (transient-args 'magit-stgit-redo))
  (magit-run-stgit "redo" args))

;;;; magit-stgit-mail

(transient-define-prefix magit-stgit-mail ()
  "Send a set of patches by e-mail."
  :value '("--git" "--auto-recipients")
  :man-page "stg-mail"
  ["Arguments"
   ("-m" "Generate an mbox file instead of sending" "--mbox")
   ("-g" "Use git send-email" "--git")
   ("-e" "Edit cover letter before sending" "--edit-cover")
   ("-a" "Automatically Cc the patch signers" "--auto")
   ("-A" "Auto-detect To, Cc and Bcc for all patches from cover"
    "--auto-recipients")
   ""
   ("-o" "Set file as cover message" "--cover="
    :reader (lambda (_prompt initial-input _history)
              (read-file-name "Find file: " nil nil nil initial-input)))
   ("-v" "Add version to [PATCH ...]" "--version=")
   ("-p" "Add prefix to [... PATCH ...]" "--prefix=")
   ("-t" "Mail To" "--to=")
   ("-c" "Mail Cc" "--cc=")
   ("-b" "Mail Bcc" "--bcc=")]
  ["Actions"
   ("m" "Send" magit-stgit--mail)])

(defun magit-stgit--mail-recipients (&optional cover)
  "Return a list of zero or one of each of `--to', `--cc' and
`--bcc' arguments for `stg mail', with the values determined from
the cover letter file COVER, or the current buffer if COVER is
nil."
  (let ((buffer (current-buffer))
        (fields '()))
    (with-temp-buffer
      (if cover
          (insert-file-contents cover)
        (insert-buffer-substring buffer))
      (goto-char (point-min))
      (while (re-search-forward (rx (group (or "To" "Cc" "Bcc")) ":" (+ blank)
                                    (group (* nonl)) (* blank) eol)
                                nil t)
        (let ((field (match-string 1))
              (recipient (match-string 2)))
          (when (string-match "<" recipient)
            (setf (alist-get field fields nil nil #'equal) recipient)))))
    (cl-loop for (field . recipient) in fields
             collect (format "--%s=\"%s\"" (downcase field) recipient))))

;;;###autoload
(defun magit-stgit--mail (patches &rest args)
  "Invoke `stg mail ARGS... -- PATCHES...'.

ARGS can contain the fake argument `--auto-recipients' which is
not passed to `stg mail'. If the argument is specified and
`--cover' is present in ARGS, the cover letter will be searched
to automatically set the values of `--to', `--cc' and `--bcc'.

If called interactively, mail the patches around point or read
one from the minibuffer."
  (interactive (cons (magit-stgit-read-patches t t t t "Send patch")
                     (transient-args 'magit-stgit-mail)))
  (let* ((autop (member "--auto-recipients" args))
         (args (remove "--auto-recipients" args))
         (cover (transient-arg-value "--cover=" args)))
    (magit-run-stgit-async
     "mail" args (and autop cover (magit-stgit--mail-recipients cover))
     "--" patches)))

;;; Mode

;;;###autoload
(eval-after-load 'magit
  '(progn
    (define-key magit-mode-map "/" 'magit-stgit-dispatch)
    (transient-append-suffix 'magit-dispatch '(0 -1 -1)
      '("/" "StGit" magit-stgit-dispatch))))

;;;###autoload
(define-minor-mode magit-stgit-mode
  "StGit support for Magit."
  :lighter magit-stgit-mode-lighter
  (unless (derived-mode-p 'magit-mode)
    (user-error "This mode only makes sense with Magit"))
  (if magit-stgit-mode
      (progn
        (magit-add-section-hook 'magit-status-sections-hook
                                'magit-insert-stgit-series
                                'magit-insert-stashes t t)
        (add-hook 'find-file-hook #'magit-stgit-new-check-buffer))
    (remove-hook 'magit-status-sections-hook
                 'magit-insert-stgit-series t)
    (remove-hook 'find-file-hook #'magit-stgit-new-check-buffer))
  (when (called-interactively-p 'any)
    (magit-refresh)))

;;;###autoload
(custom-add-option 'magit-mode-hook #'magit-stgit-mode)

(easy-menu-define magit-stgit-mode-menu nil "Magit-Stgit mode menu"
  '("StGit" :visible magit-stgit-mode
    ["Initialize" magit-stgit-init
     :help "Initialize StGit support for the current branch"]
    "---"
    ["Create new patch" magit-stgit-new
     :help "Create a new StGit patch"]
    ["Rename patch" magit-stgit-rename
     :help "Rename a patch"]
    ["Edit patch" magit-stgit-edit
     :help "Edit a patch"]
    ["Commit patch" magit-stgit-commit
     :help "Permanently store the base patch into the stack base"]
    ["Uncommit patch" magit-stgit-uncommit
     :help "Turn a regular commit into an StGit patch"]
    ["Delete patch" magit-stgit-delete
     :help "Delete an StGit patch"]
    "---"
    ["Float patch" magit-stgit-float
     :help "Float StGit patch to the top"]
    ["Sink patch" magit-stgit-sink
     :help "Sink StGit patch deeper down the stack"]
    "---"
    ["Refresh patch" magit-stgit-refresh
     :help "Refresh the contents of a patch in an StGit series"]
    ["Repair" magit-stgit-repair
     :help "Repair StGit metadata if branch was modified with git commands"]
    ["Rebase series" magit-stgit-rebase
     :help "Rebase an StGit patch series"]
    "---"
    ["Undo stack operation" magit-stgit-undo
     :help "Undo a previous stack operation"]
    ["Redo stack operation" magit-stgit-redo
     :help "Undo a previous undo operation"]))

(easy-menu-add-item 'magit-mode-menu '("Extensions") magit-stgit-mode-menu)

;;; Sections

(defconst magit-stgit-patch-re
  "^\\(.\\)\\([-+>!]\\) \\([^ ]+\\) +# \\(.*\\)$")

(defvar magit-stgit-patch-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map "k" #'magit-stgit--delete)
    (define-key map "a" #'magit-stgit--goto)
    (define-key map (kbd "RET") #'magit-stgit-show)
    (define-key map "#" #'magit-stgit-mark-toggle)
    map))

(defun magit-insert-stgit-series ()
  (when magit-stgit-mode
    (magit-insert-section (stgit-series)
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
      (delete-region (point) (line-end-position))
      (magit-insert-section (stgit-patch patch)
        (insert (if (magit-stgit-mark-contains patch) "#" " "))
        (insert (propertize state 'face
                            (cond ((equal state ">") 'magit-stgit-current)
                                  ((equal state "+") 'magit-stgit-applied)
                                  ((equal state "-") 'magit-stgit-unapplied)
                                  ((equal state "!") 'magit-stgit-hidden)
                                  (t (user-error "Unknown stgit patch state: %s"
                                                 state)))))
        (insert (propertize empty 'face 'magit-stgit-empty) ?\s)
        (when magit-stgit-show-patch-name
          (insert (propertize patch 'face 'magit-stgit-patch) ?\s))
        (insert msg)
        (put-text-property (line-beginning-position) (1+ (line-end-position))
                           'keymap 'magit-stgit-patch-map)
        (forward-line)))))

;;; magit-stgit.el ends soon

(define-obsolete-function-alias 'turn-on-magit-stgit 'magit-stgit-mode "2014-08-31")

(provide 'magit-stgit)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-stgit.el ends here
