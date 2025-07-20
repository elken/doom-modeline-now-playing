;;; doom-modeline-now-playing-playerctl.el --- Provider for playerctl for doom-modeline-now-playing -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021-2025 Ellis Kenyő
;;
;; Author: Ellis Kenyő <me@elken.dev>
;; Maintainer: Ellis Kenyő <me@elken.dev>
;; Created: July 2, 2025
;; Modified: July 7, 2025
;; Homepage: https://github.com/elken/doom-modeline-now-playing
;; Version: 1.0.1
;; Package-Requires: ((emacs "26.1"))
;; SPDX-License-Identifier: GPL3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Provider for playerctl for doom-modeline-now-playing.
;;  Supports any MPRIS-compatible media player on Linux.
;;
;;; Code:

(require 'cl-lib)
(require 'doom-modeline-now-playing-core)

;;
;; Custom variables
;;

(make-obsolete-variable 'doom-modeline-now-playing-format 'doom-modeline-now-playing-playerctl-format "v1.0.0")
(make-obsolete-variable 'doom-modeline-now-playing-ignored-players 'doom-modeline-now-playing-playerctl-ignored-players "v1.0.0")

(defcustom doom-modeline-now-playing-playerctl-format "{{artist}} - {{title}}"
  "Default output format for playerctl.
Supports a number of options (which must be wrapped in handlebars)
- `playerName': The name of the current player 'string'
- `position': Position of the current track in microseconds 'integer'
- `status': Playback status of the current player 'string'
- `volume': Volume level 'float'
- `album': Album of the current track 'string'
- `artist': Artist of the current track 'string'
- `title': Title of the current track 'string'

As well as a number of functions:
- `lc('string)': Convert string to lowercase
- `uc('string)': Convert string to uppercase
- `duration('integer)': Convert int to hh:mm:ss
- `markup_escape('string)': Escape XML markup
- `default('any,'any)': Return 1st arg unless null, then return 2nd
- `emoji(status|volume)': Convert either status or volume to emoji"
  :type 'string
  :group 'doom-modeline-now-playing)

(defcustom doom-modeline-now-playing-playerctl-ignored-players '("firefox" "plasma-browser-integration")
  "List of players to exclude from playerctl output."
  :type '(repeat string)
  :group 'doom-modeline-now-playing)

;;
;; Provider implementation
;;

(defclass doom-modeline-now-playing-playerctl (doom-modeline-now-playing-provider)
  ((name :initform "playerctl")
   (supported-p :initform (lambda ()
                           (and (eq system-type 'gnu/linux)
                                (executable-find "playerctl")))))
  "Linux playerctl provider implementation.")

(cl-defmethod doom-modeline-now-playing-provider-get-info ((provider doom-modeline-now-playing-playerctl))
  "Get playerctl information for PROVIDER."
  (let* ((ignore-arg (if doom-modeline-now-playing-playerctl-ignored-players
                         (format "--ignore-player=%s"
                                 (mapconcat #'identity doom-modeline-now-playing-playerctl-ignored-players ","))
                       ""))
         (command (format "playerctl %s metadata --format '{{playerName}}|{{lc(status)}}|%s'"
                          ignore-arg
                          doom-modeline-now-playing-playerctl-format))
         (result (string-trim (shell-command-to-string command))))
    (when (and result
               (not (string-empty-p result))
               (not (string-match-p "No players found" result)))
      (pcase-let ((`(,player ,status ,text) (split-string result "|")))
        (make-instance 'doom-modeline-now-playing-status
                       :status status
                       :player player
                       :text text)))))

(cl-defmethod doom-modeline-now-playing-provider-play-pause ((provider doom-modeline-now-playing-playerctl) _player)
  "Toggle playerctl playback for PROVIDER."
  (let* ((ignore-arg (if doom-modeline-now-playing-playerctl-ignored-players
                             (format "--ignore-player=%s"
                                     (mapconcat #'identity doom-modeline-now-playing-playerctl-ignored-players ","))
                           ""))
         (command (format "playerctl %s play-pause" ignore-arg)))
    (start-process-shell-command "playerctl" nil command)))

;;
;; Constructor function
;;

;;;###autoload
(defun doom-modeline-now-playing-playerctl-create ()
  "Create a new playerctl provider instance."
  (make-instance 'doom-modeline-now-playing-playerctl))

(provide 'doom-modeline-now-playing-playerctl)
;;; doom-modeline-now-playing-playerctl.el ends here
