;;; faceheight.el --- Configure frame face size automatically
;;
;; Copyright (C) 2018 James Ferguson
;;
;; Author: James Ferguson <james@faff.org>
;; Version: 1.11
;; Package-Requires: ((emacs "24.4"))
;; Keywords: convenience
;; URL: https://github.com/WJCFerguson/faceheight
;;
;; This file is not part of GNU Emacs.
;;
;;; License:
;;
;; Licensed under the same terms as Emacs.
;;
;;; Commentary:
;;
;; This package attempts to set the default face size (via height)
;; appropriately for the monitor.
;;
;; Setting sizes via height seems to be the easiest way, but some
;; heights look better than others.  Therefore use faceheight-inc-size
;; to select good sizes and populate faceheight-heights.
;;
;; Bind the commands `faceheight-fix-frame', `faceheight-fix-all',
;; `faceheight-inc-size'.
;;
;;; Code:


;; =============================================================================
(defgroup faceheight nil
  "Setting up a frame frame sizes by height."
  :group 'convenience)

(defcustom faceheight-heights [75 90 105 120 135 150 165 180 203 218]
  "Array of face heights indexed by `faceheight--height'.

Call `faceheight-inc-size' and note pleasing sizes to generate a
list of acceptable heights."
  :type '(vector integer))

(defcustom faceheight-start-index -6
  "Baseline `faceheight-heights' index.

Adjust this first to change overall face size."
  :type 'integer)

(defcustom faceheight-px-factor 1.42
  "Monitor px/mm factor for `faceheight-heights' index increment."
  :type 'float)

(defcustom faceheight-size-increment 150
  "Monitor mm size divisor `faceheight-heights' index incrememnt."
  :type 'integer)


;; =============================================================================
(defun faceheight-monitor-size-mm (&optional frame)
  "Return the max mm dimension for monitor showing FRAME."
  (apply 'max (cdr (assoc 'mm-size (frame-monitor-attributes frame)))))


(defun faceheight-monitor-size-px (&optional frame)
  "Return the max px dimension for monitor showing FRAME."
  (apply 'max (cdddr (assoc 'geometry
                            (frame-monitor-attributes frame)))))


(defun faceheight-pixel-pitch (&optional frame)
  "Calculate the pixel pitch for FRAME (default current frame)."
  ;; On a rotated monitor, px geom is rotated but size is not, so use
  ;; max dimension of each.
  (let* ((monitor-attrs (frame-monitor-attributes frame)))
    (/ (float (faceheight-monitor-size-mm frame))
       (faceheight-monitor-size-px frame))))


(defun faceheight--height (arg &optional frame)
  "Return face height choice from `faceheight-heights'.

Numeric Prefix ARG may be used to shift the index, applies to
current or supplied FRAME."
  (interactive "P")
  (let ((idx
         (floor
          (+ faceheight-start-index
             (if (numberp arg) arg 0)
             (floor (/ (faceheight-monitor-size-mm frame)
                       faceheight-size-increment))
             (* faceheight-px-factor
                (/ 1 (faceheight-pixel-pitch frame)))))))
    (elt faceheight-heights
         (max 0 (min (- (length faceheight-heights) 1) idx)))))


;;;###autoload
(defun faceheight-fix-frame (arg &optional frame)
  "Set the default face height appropriately for the window size.

Positive/negative prefix ARG selects size up/down.  FRAME selects
frame to act upon (default current frame)"
  (interactive "P")
  (when (display-graphic-p)
    (let ((frame (or frame (selected-frame))))
      (set-face-attribute 'default
                          frame
                          :height (faceheight--height arg frame)))))

;;;###autoload
(defun faceheight-fix-all (&optional arg)
  "Set up all frames' face heights with faceheight-fix-frame.

Positive/negative prefix ARG selects size up/down."
  (interactive "P")
  (dolist (this-frame (frame-list))
    (faceheight-fix-frame arg this-frame)))


;;;###autoload
(defun faceheight-inc-size (&optional arg)
  "Increase the face one step, prefix ARG means step down."
  (interactive "P")
  ;; find current size in faceheight-heights
  (let* ((step (if arg -1 1))
         (start-size (face-attribute 'default :height))
         (tried-size (face-attribute 'default :height)))
    (while (= start-size (face-attribute 'default :height))
      (setq tried-size (+ step tried-size))
      (set-face-attribute 'default (selected-frame) :height tried-size)
      (redisplay))
    (message "default text height now: %s" (face-attribute 'default :height))))


(defun faceheight-inc-start-index (&optional arg)
  "Increment the start index, or adjust it with prefix ARG."
  (interactive "P")
  (setq faceheight-start-index (+ faceheight-start-index (if arg arg 1))))


(provide 'faceheight)
;;; faceheight.el ends here
