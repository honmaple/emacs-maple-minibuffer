;;; maple-minibuffer.el --- Initialize file configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2015-2019 lin.jiang

;; Author: lin.jiang <mail@honmaple.com>
;; URL: https://github.com/honmaple/emacs-maple-minibuffer

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; file configurations.
;;

;;; Code:
(defgroup maple-minibuffer nil
  "Display minibuffer with another frame."
  :group 'maple)

(defcustom maple-minibuffer:name "*Minibuffer*"
  "Maple minibuffer name."
  :type 'string
  :group 'maple-minibuffer)

(defcustom maple-minibuffer:position-type 'window-bottom-left
  "Maple minibuffer position."
  :type '(choice (const frame-center)
                 (const frame-top-center)
                 (const frame-top-left)
                 (const frame-top-right)
                 (const frame-bottom-left)
                 (const frame-bottom-right)
                 (const window-center)
                 (const window-top-center)
                 (const window-top-left)
                 (const window-top-right)
                 (const window-bottom-left)
                 (const window-bottom-right))
  :group 'maple-minibuffer)

(defcustom maple-minibuffer:height nil
  "Maple minibuffer height."
  :type 'number
  :group 'maple-minibuffer)

(defcustom maple-minibuffer:width nil
  "Maple minibuffer width."
  :type 'number
  :group 'maple-minibuffer)

(defcustom maple-minibuffer:border-color "gray50"
  "Maple minibuffer border color."
  :type 'string
  :group 'maple-minibuffer)

(defcustom maple-minibuffer:action
  '(read-from-minibuffer read-string)
  "Maple minibuffer advice around function."
  :type 'list
  :group 'maple-minibuffer)

(defcustom maple-minibuffer:ignore-action
  '(evil-ex eval-expression)
  "Maple minibuffer ignore function."
  :type 'list
  :group 'maple-minibuffer)

(defcustom maple-minibuffer:ignore-regexp
  '("^helm-")
  "Maple minibuffer ignore regular expression."
  :type 'list
  :group 'maple-minibuffer)

(defcustom maple-minibuffer:cache t
  "Whether use frame cache."
  :type 'boolean
  :group 'maple-minibuffer)

(defvar maple-minibuffer:frame nil)

(defun maple-minibuffer:parameters ()
  "Maple minibuffer parameters."
  `((height . ,(or maple-minibuffer:height 10))
    (width . ,(or maple-minibuffer:width 1.0))
    (left-fringe . 5)
    (right-fringe . 5)))

(defmacro maple-minibuffer:with (&rest body)
  "Run BODY forms within maple-minibuffer."
  (declare (indent 0) (debug t))
  `(let* ((pframe (selected-frame))
          (frame (if (and maple-minibuffer:frame (frame-live-p maple-minibuffer:frame))
                     maple-minibuffer:frame
                   (maple-minibuffer:create-frame pframe)))
          (position (maple-minibuffer:position frame))
          result)
     (set-frame-position frame (car position) (cdr position))
     (select-frame-set-input-focus frame)
     (unless (frame-visible-p frame)
       (make-frame-visible frame))
     (unwind-protect (setq result ,@body)
       (select-frame-set-input-focus pframe)
       (if maple-minibuffer:cache
           (make-frame-invisible frame)
         (delete-frame frame)))
     result))

(defun maple-minibuffer:create-frame(&optional parent-frame)
  "Create maple minibuffer frame with PARENT-FRAME."
  (let* ((before-make-frame-hook nil)
         (window-configuration-change-hook nil)
         (after-make-frame-functions nil)
         (frame (make-frame `(,@(maple-minibuffer:parameters)
                              (maple-minibuffer . t)
                              (parent-frame . ,(or parent-frame (selected-frame)))
                              (name . ,maple-minibuffer:name)
                              (minibuffer . only)
                              (visibility . nil)
                              (internal-border-width . 1)
                              (left-fringe . 0)
                              (right-fringe . 0)
                              (skip-taskbar . t)
                              (undecorated . t)
                              (unsplittable . t)
                              (vertical-scroll-bars . nil)
                              (horizontal-scroll-bars . nil)))))
    (when maple-minibuffer:border-color
      (set-face-background 'internal-border maple-minibuffer:border-color frame))
    (when maple-minibuffer:cache
      (setq maple-minibuffer:frame frame))
    frame))

(defun maple-minibuffer:position(frame)
  "Get position with FRAME."
  (let* ((width (with-selected-frame frame (window-pixel-width)))
         (height (with-selected-frame frame (window-pixel-height)))
         (frame-center (/ (max 0 (- (frame-pixel-width) width)) 2))
         (window-center (+ (window-pixel-left) (/ (max 0 (- (window-pixel-width) width)) 2))))
    (pcase maple-minibuffer:position-type
      ('frame-center
       (cons frame-center
             (/ (- (frame-pixel-height) height) 2)))
      ('frame-top-center
       (cons frame-center 0))
      ('frame-top-left
       (cons 0 0))
      ('frame-top-right
       (cons -1 0))
      ('frame-bottom-left
       (cons 0
             (- 0 (window-mode-line-height) (window-pixel-height (minibuffer-window)))))
      ('frame-bottom-right
       (cons -1
             (- 0 (window-mode-line-height) (window-pixel-height (minibuffer-window)))))
      ('window-center
       (cons window-center
             (+ (window-pixel-top) (/ (- (window-pixel-height) height) 2))))
      ('window-top-center
       (cons window-center
             (window-pixel-top)))
      ('window-top-left
       (cons (window-pixel-left)
             (window-pixel-top)))
      ('window-top-right
       (cons (+ (window-pixel-left) (window-pixel-width) (- 0 width))
             (window-pixel-top)))
      ('window-bottom-left
       (cons (window-pixel-left)
             (+ (window-pixel-top) (window-pixel-height)
                (- 0 (window-mode-line-height) height))))
      ('window-bottom-right
       (cons (+ (window-pixel-left) (window-pixel-width) (- 0 width))
             (+ (window-pixel-top) (window-pixel-height)
                (- 0 (window-mode-line-height) height))))
      (_ (funcall maple-minibuffer:position-type frame)))))

(defun maple-minibuffer:read (oldfun &rest args)
  "Around minibuffer read OLDFUN with ARGS."
  (if (or (not (display-graphic-p))
          (minibufferp)
          (memq this-command maple-minibuffer:ignore-action)
          (catch 'ignored
            (dolist (r maple-minibuffer:ignore-regexp)
              (when (and (stringp r) (string-match r (symbol-name this-command)))
                (throw 'ignored t))))
          (frame-parameter (selected-frame) 'maple-minibuffer))
      (apply oldfun args)
    (maple-minibuffer:with (apply oldfun args))))

(defun maple-minibuffer-mode-on()
  "Maple minibuffer mode on."
  (interactive)
  (dolist (func maple-minibuffer:action)
    (advice-add func :around 'maple-minibuffer:read)))

(defun maple-minibuffer-mode-off()
  "Maple minibuffer mode off."
  (interactive)
  (dolist (func maple-minibuffer:action)
    (advice-remove func 'maple-minibuffer:read))
  (setq maple-minibuffer:frame nil))

;;;###autoload
(define-minor-mode maple-minibuffer-mode
  "maple minibuffer mode"
  :group      'maple-minibuffer
  :global     t
  (if maple-minibuffer-mode (maple-minibuffer-mode-on) (maple-minibuffer-mode-off)))

(provide 'maple-minibuffer)
;;; maple-minibuffer.el ends here
