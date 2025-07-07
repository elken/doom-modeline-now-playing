;;; doom-modeline-now-playing-core.el --- Core classes and protocols for doom-modeline-now-playing -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021-2025 Ellis Kenyő
;;
;; Author: Ellis Kenyő <me@elken.dev>
;; Maintainer: Ellis Kenyő <me@elken.dev>
;; Created: July 7, 2025
;; Modified: July 7, 2025
;; Version: 1.0.1
;; Homepage: https://github.com/elken/doom-modeline-now-playing
;; Package-Requires: ((emacs "26.1"))
;; SPDX-License-Identifier: GPL3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Core classes and protocols for doom-modeline-now-playing.
;;  This file contains the base classes that other provider files depend on.
;;
;;; Code:

(require 'cl-lib)
(require 'eieio)

;;
;; Custom group
;;

(defgroup doom-modeline-now-playing nil
  "Settings related to `doom-modeline-now-playing'."
  :group 'doom-modeline)

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

;;
;; Protocol methods (must be implemented by subclasses)
;;

(cl-defmethod doom-modeline-now-playing-provider-get-info ((provider doom-modeline-now-playing-provider))
  "Get current playback information from PROVIDER.
Should return a now-playing-status object or nil if no player is active."
  (error "Method must be implemented by subclass"))

(cl-defmethod doom-modeline-now-playing-provider-play-pause ((provider doom-modeline-now-playing-provider) player)
  "Toggle playback for PLAYER using PROVIDER."
  (error "Method must be implemented by subclass"))

(provide 'doom-modeline-now-playing-core)
;;; doom-modeline-now-playing-core.el ends here
