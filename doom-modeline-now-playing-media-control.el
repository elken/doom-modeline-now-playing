;;; doom-modeline-now-playing-media-control.el --- media-control provider for doom-modeline-now-playing -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021-2025 Ellis Kenyő
;;
;; Author: Ellis Kenyő <me@elken.dev>
;; Maintainer: Ellis Kenyő <me@elken.dev>
;; Created: July 30, 2025
;; Modified: July 30, 2025
;; Homepage: https://github.com/elken/doom-modeline-now-playing
;; Version: 1.0.1
;; Package-Requires: ((emacs "26.1"))
;; SPDX-License-Identifier: GPL3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  macOS media-control provider for doom-modeline-now-playing.
;;  Uses the media-control CLI tool to get media information from any
;;  supported media application (TIDAL, Spotify, Music, etc.).
;;
;;  Requires: https://github.com/autumncheckers/media-control
;;
;;; Code:
(require 'cl-lib)
(require 'json)
(require 'doom-modeline-now-playing-core)



;;
;; Provider implementation
;;

(defclass doom-modeline-now-playing-media-control (doom-modeline-now-playing-provider)
  ((name :initform "media-control")
   (supported-p :initform (lambda ()
                           (and (eq system-type 'darwin)
                                (executable-find "media-control")))))
  "Media-control provider implementation.")

(cl-defmethod doom-modeline-now-playing-provider-get-info ((provider doom-modeline-now-playing-media-control))
  "Get current playback information using media-control for PROVIDER."
  (let* ((json-output (condition-case nil
                          (shell-command-to-string "media-control get")
                        (error nil)))
         (json-data (when (and json-output (not (string-empty-p json-output)))
                      (condition-case nil
                          (json-read-from-string json-output)
                        (error nil)))))
    (when json-data
      (let* ((bundle-id (cdr (assoc 'bundleIdentifier json-data)))
             (playing (cdr (assoc 'playing json-data)))
             (artist (cdr (assoc 'artist json-data)))
             (title (cdr (assoc 'title json-data)))
             (status (if playing "playing" "paused"))
             (player (pcase bundle-id
                       ("com.tidal.desktop" "TIDAL")
                       ("com.spotify.client" "Spotify")
                       ("com.apple.Music" "Music")
                       ("com.apple.iTunes" "iTunes")
                       (_ (or bundle-id "Unknown"))))
             (text (format "%s - %s"
                          (or artist "Unknown Artist")
                          (or title "Unknown Track"))))
        (make-instance 'doom-modeline-now-playing-status
                       :status status
                       :player player
                       :text text)))))

(cl-defmethod doom-modeline-now-playing-provider-play-pause ((provider doom-modeline-now-playing-media-control) _player)
  "Toggle playback using media-control for PROVIDER.
PLAYER argument is ignored since media-control operates on the active media session."
  (condition-case nil
      (shell-command-to-string "media-control play-pause")
    (error nil)))

;;
;; Constructor function
;;

;;;###autoload
(defun doom-modeline-now-playing-media-control-create ()
  "Create a new media-control provider instance."
  (make-instance 'doom-modeline-now-playing-media-control))

(provide 'doom-modeline-now-playing-media-control)
;;; doom-modeline-now-playing-media-control.el ends here
