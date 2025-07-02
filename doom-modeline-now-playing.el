;;; doom-modeline-now-playing.el --- Segment for Doom Modeline to show media player information -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021-2024 Ellis Kenyő
;;
;; Author: Ellis Kenyő <me@elken.dev>
;; Maintainer: Ellis Kenyő <me@elken.dev>
;; Created: January 23, 2021
;; Modified: July 2, 2025
;; Version: 1.0.0
;; Homepage: https://github.com/elken/doom-modeline-now-playing
;; Package-Requires: ((emacs "26.1"))
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
(require 'eieio)

;;
;; Custom variables
;;

(defgroup doom-modeline-now-playing nil
  "Settings related to `doom-modeline-now-playing'."
  :group 'doom-modeline)

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

(defcustom doom-modeline-now-playing-current-provider
  (cond
   ((and (eq system-type 'gnu/linux)
         (require 'doom-modeline-now-playing-playerctl nil t))
    (doom-modeline-now-playing-playerctl-create))
   ((and (eq system-type 'darwin)
         (require 'doom-modeline-now-playing-osascript nil t))
    (doom-modeline-now-playing-osascript-create)))
  "The current provider to use for running commands."
  :group 'doom-modeline-now-playing)

;;
;; Core classes
;;

(defclass doom-modeline-now-playing-status ()
  ((status :initarg :status
           :type string
           :documentation "Playback status (playing, paused, etc.)")
   (player :initarg :player
           :type string
           :documentation "Player name (spotify, music, etc.)")
   (text :initarg :text
         :type string
         :documentation "Track information text"))
  "Class holding the current playback status.")

(defclass doom-modeline-now-playing-provider ()
  ((name :initarg :name
         :type string
         :documentation "Name of the provider")
   (supported-p :initarg :supported-p
                :documentation "Bool or predicate to determine if the provider should run on the current setup.")
   (current-info :initform nil
                 :documentation "Current player status or nil if not set"))
  "Base class for media player providers."
  :abstract t)

(cl-defmethod doom-modeline-now-playing-provider-get-info ((provider doom-modeline-now-playing-provider))
  "Get current playback information from PROVIDER.
Should return a now-playing-status object or nil if no player is active."
  (error "Method must be implemented by subclass"))

(cl-defmethod doom-modeline-now-playing-provider-play-pause ((provider doom-modeline-now-playing-provider) player)
  "Toggle playback for PLAYER using PROVIDER."
  (error "Method must be implemented by subclass"))

;;
;; Variables
;;

(defvar doom-modeline-now-playing-status nil
  "Current now-playing status object.")

;;
;; Core functionality
;;

(defun doom-modeline-now-playing--update ()
 "Update the current playback status."
 (when (and doom-modeline-now-playing
            (> doom-modeline-now-playing-interval 0)
            doom-modeline-now-playing-current-provider)
   (let ((supported (oref doom-modeline-now-playing-current-provider supported-p)))
     (when (if (functionp supported)
               (funcall supported)
             supported)
       (setq doom-modeline-now-playing-status
             (doom-modeline-now-playing-provider-get-info
              doom-modeline-now-playing-current-provider))
       (force-mode-line-update)))))

(defun doom-modeline-now-playing-toggle-status ()
 "Toggle play/pause for the current player."
 (interactive)
 (when-let* ((provider doom-modeline-now-playing-current-provider)
             (player (oref doom-modeline-now-playing-status player)))
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

(doom-modeline-def-segment now-playing
  "Display current media playback status."
  (when (and doom-modeline-now-playing
             doom-modeline-now-playing-status)
    (let ((player (oref doom-modeline-now-playing-status player))
          (status (oref doom-modeline-now-playing-status status))
          (text   (oref doom-modeline-now-playing-status text)))
      (when (and player status text
                 (not (string= player "No players found")))
        (concat
         (doom-modeline-spc)
         (if (string= "spotify" (downcase player))
             (doom-modeline-icon 'faicon "nf-fa-spotify" "" "#"
                                :face 'doom-modeline-now-playing-icon
                                :v-adjust -0.0575)
           (doom-modeline-icon 'faicon "nf-fa-music" "" "#"
                              :face 'doom-modeline-now-playing-icon
                              :v-adjust -0.0575))
         (doom-modeline-spc)
         (propertize (if (equal status "playing")
                        (doom-modeline-icon 'faicon "nf-fa-play" "" ">"
                                          :v-adjust -0.0575)
                      (doom-modeline-icon 'faicon "nf-fa-pause" "" "||"
                                        :v-adjust -0.0575))
                    'mouse-face 'mode-line-highlight
                    'help-echo "mouse-1: Toggle player status"
                    'local-map (let ((map (make-sparse-keymap)))
                               (define-key map [mode-line mouse-1]
                                         'doom-modeline-now-playing-toggle-status)
                               map))
         (doom-modeline-spc)
         (propertize
          (truncate-string-to-width text doom-modeline-now-playing-max-length nil nil "...")
          'face 'doom-modeline-now-playing-text))))))

(provide 'doom-modeline-now-playing)
;;; doom-modeline-now-playing.el ends here
