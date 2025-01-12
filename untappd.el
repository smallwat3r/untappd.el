;;; untappd.el --- Display your latest Untappd feed -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Matthieu Petiteau

;; Author: Matthieu Petiteau <matt@smallwat3r.com>
;; URL: https://github.com/smallwat3r/untappd.el
;; Package-Requires: ((emacs "26.1") (request "0.3.2") (emojify "1.2.1"))
;; Version: 0.0.2

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This package allows you to fetch your latest Untappd feed and
;; display it in a new buffer.

;;; Code:

(require 'cl-lib)
(require 'emojify)
(require 'request)

(defgroup untappd nil
  "Integration with the Untappd platform."
  :group 'applications)

;;; Customizable Variables
(defcustom untappd-access-token nil
  "Access token for Untappd API authentication."
  :group 'untappd
  :type 'string)

(defcustom untappd-feed-api-url "https://api.untappd.com/v4/checkin/recent"
  "API endpoint for fetching recent check-ins."
  :group 'untappd
  :type 'string)

;;; Faces
(defface untappd-rating-icon-face
  '((t :foreground "gold"))
  "Face for displaying the rating icons."
  :group 'untappd)

(defface untappd-main-element-face
  '((t :weight bold))
  "Face for highlighting main elements like beer and brewery names."
  :group 'untappd)

;;; Utility Functions
(defun untappd--get-from-object (key object)
  "Extract values for KEY from a list of OBJECTs."
  (mapcar (lambda (obj) (assoc-default key obj)) object))

(defun untappd--format-rating (rating)
  "Format RATING as a string of stars."
  (propertize
   (cond ((>= rating 4.5) "★★★★★")
         ((>= rating 4)   "★★★★ ")
         ((>= rating 3)   "★★★  ")
         ((>= rating 2)   "★★   ")
         (t               "★    "))
   'face 'untappd-rating-icon-face))

;;; Formatting Functions
(defun untappd--format-checkin-header (rating beer brewery)
  "Format the header line for a checkin with RATING, BEER, and BREWERY."
  (format "%s (%s/5) 🍺 %s (%s, %s%%) from %s (%s)"
          (untappd--format-rating rating)
          rating
          (propertize (assoc-default 'beer_name beer) 'face 'untappd-main-element-face)
          (assoc-default 'beer_style beer)
          (assoc-default 'beer_abv beer)
          (propertize (assoc-default 'brewery_name brewery) 'face 'untappd-main-element-face)
          (assoc-default 'country_name brewery)))

(defun untappd--format-checkin-details (date venue)
  "Format the details line for a checkin with DATE and VENUE."
  (concat date
          (if (eq (length venue) 0) ""
            (concat " @ " (assoc-default 'venue_name venue)))))

(defun untappd--format-checkin-description (comment user toasts)
  "Format the description for a checkin with COMMENT, USER, and TOASTS."
  (format "(%s) %s %s (%s): %s"
          (concat (number-to-string (assoc-default 'total_count toasts)) "🍻")
          (assoc-default 'first_name user)
          (assoc-default 'last_name user)
          (propertize (assoc-default 'user_name user) 'face 'untappd-main-element-face)
          (if (or (not comment) (equal comment "")) "-" comment)))

(defun untappd--format-checkin-external-link (user id)
  "Format the external link for a USER's checkin with ID."
  (propertize (format "https://untappd.com/user/%s/checkin/%s"
                      (assoc-default 'user_name user) id)
              'face 'link))

;;; Rendering Functions
(defun untappd--render-checkin (data)
  "Render the DATA in the current buffer."
  (let* ((items   (assoc-default 'items (assoc-default 'checkins (assoc-default 'response data))))
         (id      (untappd--get-from-object 'checkin_id items))
         (rating  (untappd--get-from-object 'rating_score items))
         (comment (untappd--get-from-object 'checkin_comment items))
         (date    (untappd--get-from-object 'created_at items))
         (user    (untappd--get-from-object 'user items))
         (beer    (untappd--get-from-object 'beer items))
         (brewery (untappd--get-from-object 'brewery items))
         (venue   (untappd--get-from-object 'venue items))
         (toasts  (untappd--get-from-object 'toasts items)))
    (cl-mapcar (lambda (id rating comment date user beer brewery venue toasts)
                 (insert (untappd--format-checkin-header rating beer brewery) "\n"
                         (untappd--format-checkin-details date venue) "\n"
                         (untappd--format-checkin-description comment user toasts) "\n"
                         (untappd--format-checkin-external-link user id) "\n\n"))
               id rating comment date user beer brewery venue toasts)))

(defun untappd--render-feed (data buffer)
  "Render the Untappd feed DATA into BUFFER."
  (with-current-buffer (get-buffer-create buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (untappd--render-checkin data)
      (goto-char (point-min))
      (setq buffer-read-only t)))
  (switch-to-buffer-other-window buffer))

;;; API Query
(defun untappd--query-feed ()
  "Query the Untappd API for the recent activity feed."
  (request untappd-feed-api-url
    :params `(("limit" . "50")
              ("access_token" . ,untappd-access-token))
    :parser 'json-read
    :success
    (cl-function
     (lambda (&key data &allow-other-keys)
       (untappd--render-feed data "*untappd*")))
    :error
    (cl-function
     (lambda (&key error-thrown &allow-other-keys)
       (message "Error querying the Untappd API: %s" error-thrown)))))

;;; User Command
;;;###autoload
(defun untappd-feed ()
  "Fetch and display the Untappd feed."
  (interactive)
  (unless untappd-access-token
    (user-error "Untappd access token is not set"))
  (untappd--query-feed))

(provide 'untappd)

;;; untappd.el ends here
