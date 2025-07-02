;;; doom-modeline-now-playing-osascript.el --- AppleScript provider for doom-modeline-now-playing -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021-2024 Ellis Kenyő
;;
;; Author: Ellis Kenyő <me@elken.dev>
;; Maintainer: Ellis Kenyő <me@elken.dev>
;; Created: July 2, 2025
;; Modified: July 2, 2025
;; Homepage: https://github.com/elken/doom-modeline-now-playing
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1"))
;; SPDX-License-Identifier: GPL3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  macOS AppleScript provider for doom-modeline-now-playing.
;;  Supports Spotify, Music, and other macOS media players.
;;
;;; Code:

(require 'cl-lib)
(require 'doom-modeline-now-playing)

;;
;; Custom variables
;;

(defcustom doom-modeline-now-playing-osascript-players '("Spotify" "Music")
  "A list of all the players to check status for on macOS."
  :type '(repeat string)
  :group 'doom-modeline-now-playing)

;;
;; Provider implementation
;;

(defclass doom-modeline-now-playing-osascript (doom-modeline-now-playing-provider eieio-default-superclass)
  ((name :initform "osascript")
   (supported-p :initform (lambda ()
                           (and (eq system-type 'darwin)
                                (executable-find "osascript")))))
  "AppleScript provider implementation.")

(cl-defmethod doom-modeline-now-playing-provider-get-info ((provider doom-modeline-now-playing-osascript))
  "Get current playback information using AppleScript."
  (cl-loop for player in doom-modeline-now-playing-osascript-players
           for script = (format "
tell application \"%s\"
    if running then
        set playerState to (player state as string)
        set currentArtist to (artist of current track as string)
        set currentTrack to (name of current track as string)
        return \"%s|\" & playerState & \"|\" & currentArtist & \" - \" & currentTrack
    end if
end tell" player (downcase player))
           for result = (condition-case nil
                          (string-trim
                           (shell-command-to-string
                            (format "osascript -e '%s'" script)))
                        (error nil))
           when (and result (not (string-empty-p result)))
           return (pcase-let ((`(,player ,status ,text)
                              (split-string result "|")))
                   (make-instance 'doom-modeline-now-playing-status
                                  :status status
                                  :player player
                                  :text text))))

(cl-defmethod doom-modeline-now-playing-provider-play-pause ((provider doom-modeline-now-playing-osascript) player)
  "Toggle playback for PLAYER using AppleScript."
  (let ((script (format "tell application \"%s\"
    if player state is playing then
        pause
    else
        play
    end if
end tell" player)))
    (shell-command-to-string (format "osascript -e '%s'" script))))

;;
;; Constructor function
;;

(defun doom-modeline-now-playing-osascript-create ()
  "Create a new osascript provider instance."
  (make-instance 'doom-modeline-now-playing-osascript))

(provide 'doom-modeline-now-playing-osascript)
;;; doom-modeline-now-playing-osascript.el ends here
