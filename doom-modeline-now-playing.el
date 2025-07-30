;;; doom-modeline-now-playing.el --- Segment for Doom Modeline to show media player information -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021-2025 Ellis Kenyő
;;
;; Author: Ellis Kenyő <me@elken.dev>
;; Maintainer: Ellis Kenyő <me@elken.dev>
;; Created: January 23, 2021
;; Modified: July 7, 2025
;; Version: 1.0.1
;; Homepage: https://github.com/elken/doom-modeline-now-playing
;; Package-Requires: ((emacs "26.1") (doom-modeline "3.0.0"))
;; SPDX-License-Identifier: GPL3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Media player support for doom-modeline.
;;  Supports both playerctl (Linux) and AppleScript (macOS).
;;
;;  To enable:
;;  (doom-modeline-now-playing-timer)
;;
;;; Code:

(require 'cl-lib)
(require 'doom-modeline)
(require 'doom-modeline-now-playing-core)

(declare-function doom-modeline-now-playing-playerctl-create "doom-modeline-now-playing-playerctl")
(declare-function doom-modeline-now-playing-osascript-create "doom-modeline-now-playing-osascript")

;;
;; Custom variables
;;

(defcustom doom-modeline-now-playing t
  "Whether to display the now-playing segment."
  :type 'boolean
  :group 'doom-modeline-now-playing)

(defcustom doom-modeline-now-playing-interval 5
  "Default delay in seconds to update the status."
  :type 'integer
  :group 'doom-modeline-now-playing)

(defcustom doom-modeline-now-playing-max-length 40
  "Maximum length of output.
Truncates with `...' after."
  :type 'integer
  :group 'doom-modeline-now-playing)

(defface doom-modeline-now-playing-icon
  '((t (:inherit success)))
  "Face for the now-playing icon."
  :group 'doom-modeline-faces)

(defface doom-modeline-now-playing-text
  '((t (:inherit bold)))
  "Face for the now-playing text."
  :group 'doom-modeline-faces)

(defcustom doom-modeline-now-playing-current-provider nil
  "The current provider to use for running commands.
This will be automatically initialized when first accessed."
  :group 'doom-modeline-now-playing)

;;
;; Variables
;;

(defvar doom-modeline-now-playing-status nil
  "Current now-playing status object.")

;;
;; Provider initialization
;;

(defun doom-modeline-now-playing--get-provider ()
  "Get or initialize the current provider."
  (unless doom-modeline-now-playing-current-provider
    (setq doom-modeline-now-playing-current-provider
          (cond
           ((and (eq system-type 'gnu/linux)
                 (require 'doom-modeline-now-playing-playerctl nil t))
            (doom-modeline-now-playing-playerctl-create))
           ((eq system-type 'darwin)
            (if (and (executable-find "media-control")
                     (require 'doom-modeline-now-playing-media-control nil t))
                (doom-modeline-now-playing-media-control-create)
              (when (require 'doom-modeline-now-playing-osascript nil t)
                (doom-modeline-now-playing-osascript)))))))
  doom-modeline-now-playing-current-provider)

;;
;; Core functionality
;;

(defun doom-modeline-now-playing--update ()
 "Update the current playback status."
 (when (and doom-modeline-now-playing
            (> doom-modeline-now-playing-interval 0))
   (when-let ((provider (doom-modeline-now-playing--get-provider)))
     (let ((supported (oref provider supported-p)))
       (when (if (functionp supported)
                 (funcall supported)
               supported)
         (setq doom-modeline-now-playing-status
               (doom-modeline-now-playing-provider-get-info provider))
         (force-mode-line-update))))))

(defun doom-modeline-now-playing-toggle-status ()
 "Toggle play/pause for the current player."
 (interactive)
 (when-let* ((provider (doom-modeline-now-playing--get-provider))
             (status doom-modeline-now-playing-status)
             (player (oref status player)))
   (doom-modeline-now-playing-provider-play-pause provider player)
   (doom-modeline-now-playing--update)))

;;
;; Timer management
;;

(defvar doom-modeline-now-playing--timer nil
  "Timer for updating the playback status.")

(defun doom-modeline-now-playing-timer ()
  "Start/Stop the timer for now-playing update."
  (when (timerp doom-modeline-now-playing--timer)
    (cancel-timer doom-modeline-now-playing--timer))
  (setq doom-modeline-now-playing--timer
        (when doom-modeline-now-playing
          (run-with-timer 1 doom-modeline-now-playing-interval
                         #'doom-modeline-now-playing--update))))

(doom-modeline-add-variable-watcher
 'doom-modeline-now-playing
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-modeline-now-playing val)
     (doom-modeline-now-playing-timer))))

;;
;; Modeline segment
;;

(defun doom-modeline-now-playing--icon ()
  "Return the icon for the now-playing status."
  (let ((player (oref doom-modeline-now-playing-status player)))
    (when player
      (if (string= "spotify" (downcase player))
          (doom-modeline-icon 'faicon "nf-fa-spotify" "" "#"
                              :face 'doom-modeline-now-playing-icon
                              :v-adjust -0.0575)
        (doom-modeline-icon 'faicon "nf-fa-music" "" "#"
                            :face 'doom-modeline-now-playing-icon
                            :v-adjust -0.0575)))))

(defun doom-modeline-now-playing--playing ()
  "Return the playing status."
  (let ((status (oref doom-modeline-now-playing-status status)))
    (when status
      (if (equal status "playing")
          (doom-modeline-icon 'faicon "nf-fa-play" "" ">"
                              :v-adjust -0.0575)
        (doom-modeline-icon 'faicon "nf-fa-pause" "" "||"
                            :v-adjust -0.0575)))))

(defun doom-modeline-now-playing--text ()
  "Return the text from the now-playing status."
  (let ((text   (oref doom-modeline-now-playing-status text)))
    (when text
      (propertize
       (truncate-string-to-width text doom-modeline-now-playing-max-length nil nil "...")
       'face 'doom-modeline-now-playing-text))))

(doom-modeline-def-segment now-playing
  "Display current media playback status."
  (when (and doom-modeline-now-playing
             doom-modeline-now-playing-status
             (eieio-object-p doom-modeline-now-playing-status))
    (concat
     (doom-modeline-spc)
     (doom-modeline-now-playing--icon)
     (doom-modeline-spc)
     (propertize (doom-modeline-now-playing--playing)
                 'mouse-face 'mode-line-highlight
                 'help-echo "mouse-1: Toggle player status"
                 'local-map (let ((map (make-sparse-keymap)))
                              (define-key map [mode-line mouse-1]
                                          'doom-modeline-now-playing-toggle-status)))
     (doom-modeline-spc)
     (doom-modeline-now-playing--text))))

(provide 'doom-modeline-now-playing)
;;; doom-modeline-now-playing.el ends here
