;;; yon-chan.el --- A 4chan browser for Emacs

;; Copyright (C) 2013  David Thompson
;;           (C) 2013  Mateusz Kowalczyk

;; Author: David Thompson <dthompson@member.fsf.org>
;; Keywords: hypermedia, games

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This mode uses the 4chan API to provide a board and thread browser
;; from within Emacs

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'dash)
(require 'yon-rendering)
(require 'yon-formatting)
(require 'yon-navigation)
(require 'yon-structures)
(require 'yon-utils)
(require 'yon-communication)

;;; Yon-chan mode
(defgroup yon-chan nil
  "Group used to store various yon-chan properties.")

(defcustom yon-chan-mode-default-board nil
  "The default board to visit when starting yon-chan"
  :type 'string
  :group 'yon-chan)

(defcustom yon-api-url "http://api.4chan.org/"
  "The 4chan API URL."
  :type 'string
  :group 'yon-chan)

(define-derived-mode yon-chan-mode
  special-mode "Yon-chan"
  "4chan browser."
  (define-key yon-chan-mode-map (kbd "n") 'yon-jump-post-forward)
  (define-key yon-chan-mode-map (kbd "p") 'yon-jump-post-backward)
  (define-key yon-chan-mode-map (kbd "P") 'yon-jump-post-first)
  (define-key yon-chan-mode-map (kbd "N") 'yon-jump-post-last)
  (define-key yon-chan-mode-map (kbd "g") 'yon-refresh-buffer))

(defvar yon-chan-mode-map (make-sparse-keymap)
  "yon-chan-mode keymap")

(defcustom yon-chan-boards
  '("/a/" "/b/" "/c/" "/d/" "/e/" "/f/" "/g/" "/gif/" "/h/" "/hr/"
    "/k/" "/m/" "/o/" "/p/" "/r/" "/s/" "/t/" "/u/" "/v/" "/vg/"
    "/vr/" "/w/" "/wg/" "/i/" "/ic/" "/r9k/" "/s4s/" "/cm/" "/hm/"
    "/lgbt/" "/y/" "/3/" "/adv/" "/an/" "/asp/" "/cgl/" "/ck/" "/co/"
    "/diy/" "/fa/" "/fit/" "/gd/" "/hc/" "/int/" "/jp/" "/lit/"
    "/mlp/" "/mu/" "/n/" "/out/" "/po/" "/pol/" "/sci/" "/soc/" "/sp/"
    "/tg/" "/toy/" "/trv/" "/tv/" "/vp/" "/wsg/" "/x/" "/q/")
  "The list of available 4chan boards"
  :type 'list
  :group 'yon-chan)

;;;###autoload
(defun yon-chan ()
  "Fetch and display 4chan boards."
  (interactive)
  (if yon-chan-mode-default-board
      (with-current-buffer (switch-to-buffer-other-window
                            (generate-new-buffer
                             (concat "*yon-chan-/"
                                     yon-chan-mode-default-board "/*")))
        (yon-chan-mode)
        (yon-browse-board-catalog (current-buffer) yon-chan-mode-default-board))
    (call-interactively 'yon-chan-browse-board)))


(provide 'yon-chan)
;;; yon-chan.el ends here
