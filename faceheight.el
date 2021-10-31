;;; faceheight.el --- Configure frame face size automatically
;;
;; Copyright (C) James Ferguson
;;
;; Author: James Ferguson <james@faff.org>
;; Version: 2.0
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
;; This package attempts to set the default face's point size based on monitor
;; resolution and/or pixel pitch.  See customization items.
;;
;; Bind the commands `faceheight-fix-frame', `faceheight-adjust', and/or add
;; `faceheight-window-size-change' to `window-size-change-functions'.  e.g.:
;;
;; (use-package faceheight
;;   :commands (faceheight-fix-frame faceheight-window-size-change)
;;   :init
;;   (add-to-list #'window-size-change-functions #'faceheight-window-size-change))`
;;
;; If you find it lacking, let me know how you think it should be improved
;; (github issues).  Need more criteria for adjustment?  Frame width?
;;
;;; Code:


;; =============================================================================
(defgroup faceheight nil
  "Setting up a frame font sizes to suit the current display."
  :group 'convenience)

(defcustom faceheight-default-points 15
  "The baseline font point size, to then be adjusted with -thresholds values.

Some fonts at least appear to render better at point sizes that
are multiples of 3."
  :type 'string)

(defcustom faceheight-px-count-thresholds '((2000 . 3))
  "Point size offsets from the maximum monitor dimension in pixels.

List of pairs of (monitor-size-in-pixels . font-point-offset).

The 2nd value (cdr) of the final cell encountered where the 1st
value (car) is <= the monitor size in px, will be used as a
font point offset.  Thresholds should therefore be sorted in
rising order.

E.g.: if set to `(list (2000 . 3))' then a screen >= 2000px will
gain 3 font points.
")

(defcustom faceheight-pixel-pitch-thresholds '((0 . 3) (0.12 . 0) (0.17 . -3))
  "List of (px-pitch-threshold . font-point-offset).

As with `faceheight-px-count-thresholds', an offset will be
selected from the monitor's pixel pitch.
")

;; =============================================================================
(defun faceheight--monitor-size-mm (frame)
  "Return the max dimension of FRAME's monitor in mm."
  (apply 'max (frame-monitor-attribute 'mm-size frame)))


(defun faceheight--monitor-size-px (frame)
  "Return the max dimension of FRAME's monitor in pixels."
  (apply 'max (cddr (frame-monitor-attribute 'geometry frame))))


(defun faceheight--pixel-pitch (frame)
  "Calculate the pixel pitch for FRAME in mm."
  ;; On a rotated monitor, px geom is rotated but size is not, so use
  ;; max dimension of each.
  (/ (float (faceheight--monitor-size-mm frame))
     (faceheight--monitor-size-px frame)))


(defun faceheight--threshold-offset (threshold-list val)
  "Find the offset for VAL in THRESHOLD-LIST."
  (let ((result 0))
    (dolist (width-threshold threshold-list)
      (when (>= val (car width-threshold))
        (setq result (cdr width-threshold))))
    result))


(defun faceheight--point-size (frame)
  "Return the point size to use for this frame."
  (+ faceheight-default-points
     ;; manual adjustment:
     (or (frame-parameter frame 'faceheight-fixed-adjustment) 0)
     ;; pixel pitch adjustment:
     (faceheight--threshold-offset faceheight-pixel-pitch-thresholds
                                   (faceheight--pixel-pitch frame))
     ;; monitor size in px adjustment:
     (faceheight--threshold-offset faceheight-px-count-thresholds
                                   (faceheight--monitor-size-px frame))))

;;;###autoload
(defun faceheight-adjust (frame offset)
  "Adjust FRAME's font-point adjustment by OFFSET persistently.

Add a custom fixed offset to the faceheight point size calculation.

If OFFSET is nil, reset adjustment to zero."
  (set-frame-parameter
   frame
   'faceheight-fixed-adjustment
   (if offset
       (+ offset (or (frame-parameter frame 'faceheight-fixed-adjustment) 0))
     0))
  (message "Setting default font size to %s points" (faceheight--point-size frame))
  (faceheight-fix-frame frame))

;;;###autoload
(defun faceheight-fix-frame (&optional frame)
  "Set the default text size appropriately for the window display."
  (interactive)
  (when (display-graphic-p)
    (let* ((frame (or frame (selected-frame)))
           (monitor-size-px (faceheight--monitor-size-px frame)))
      (set-frame-parameter frame
                           'font
                           (format "%s-%d"
                                   (face-attribute 'default :family)
                                   (faceheight--point-size frame))))))

;;;###autoload
(defun faceheight-window-size-change (window-or-frame)
  "Function for `window-size-change-functions' to fix the frame text size."
  (when (and (framep window-or-frame) (frame-size-changed-p window-or-frame))
    (faceheight-fix-frame window-or-frame)))

(provide 'faceheight)
;;; faceheight.el ends here
