;;; adaptive-theme.el --- Adaptive theme to protect your eyesight --- -*- lexical-binding: t -*-

;; Copyright (C) 2020  Álvaro Cortés Sánchez-Migallón

;; Author: Álvaro Cortés Sánchez-Migallón <alvarocsm.91@gmail.com>
;; Keywords: internals, tools, unix, local, terminals
;; Url: https://github.com/alvarocsm91/adaptive-theme
;; Package-requires: ((emacs "25.1"))
;; Version: 1.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Adaptive Theme package has been created to protect your eyesight while working near
;; windows or outside buildings. Its functions change your theme from light to dark
;; and backwards, depending on the hour.

;; This package provides three functions, explained from less to more resource usage.
;; The "adaptive-theme" function allows to load different themes automatically based
;; on the hour set up by the user or by default sunset and sunrise hours.
;; The "adaptive-theme-location" function allows to load different themes
;; automatically based on the city set up by the user or by default sunset and sunrise city
;; hours.
;; The "adaptive-theme-autolocation" function allows to load different themes
;; automatically based on the nearest city detected.

;;; Code:

;;; Adaptive theme function

(defun adaptive-theme (light-theme dark-theme &optional am-hour pm-hour am-min pm-min am-sec pm-sec)
  "Adaptive theme function:

    @Brief:   This function allow to configure different themes depending on the
              initialization time.

    @Author:  acsm

    @Version: A/1

    @Args:    LIGHT-THEME: Theme loaded in sun hours.
              DARK-THEME:  Theme loaded in dark hours.
              Optional arguments:
              AM-HOUR: Custom dawn hour (0-23) (optional, default 07)
              PM-HOUR: Custom sunset hour (0-23) (optional, default 20)
              AM-MIN:  Custom dawn min (0-59) (optional, default 00)
              PM-MIN:  Custom sunset min (0-59) (optional, default 00)
              AM-SEC:  Custom dawn sec (0-59) (optional, default 00)
              PM-SEC:  Custom sunset sec (0-59) (optional, default 00)

    @Links:"

  ;;;; Set default variables

  (defvar at--init-time)
  (defvar at--init-hour-str)
  (defvar at--init-min-str)
  (defvar at--init-sec-str)
  (defvar at--init-hour-int)
  (defvar at--init-min-int)
  (defvar at--init-sec-int)
  (defvar at--is-day)
  (defvar at--is-morning)
  (defvar at--is-night)
  (defvar at--pm-hour-str)
  (defvar at--pm-min-str)
  (defvar at--pm-str)
  (defvar at--am-hour-str)
  (defvar at--am-min-str)
  (defvar at--am-str)
  (defvar at--am-str-int)
  (defvar at--am-hour-init)
  (defvar at--am-min-init)
  (defvar at--am-sec-init)
  (defvar at--pm-hour-init)
  (defvar at--pm-min-init)
  (defvar at--pm-sec-init)
  (defvar at--am-timer)
  (defvar at--pm-timer)

  ;;;; Set default values

  ;; Set dawn time

  (unless (eval am-hour) (setq am-hour 7))
  (unless (eval am-min) (setq am-min 00))
  (unless (eval am-sec) (setq am-sec 00))

  ;; Set sundown time

  (unless (eval pm-hour) (setq pm-hour 20))
  (unless (eval pm-min) (setq pm-min 00))
  (unless (eval pm-sec) (setq pm-sec 00))

  ;; Set dawn time aux

  (setq at--am-hour-init am-hour)
  (setq at--am-min-init am-min)
  (setq at--am-sec-init am-sec)

  ;; Set sundown time

  (setq at--pm-hour-init pm-hour)
  (setq at--pm-min-init pm-min)
  (setq at--pm-sec-init pm-sec)

  ;;;; Get time

  (setq at--init-time (current-time-string))

  ;;;; Get integer hour

  ;; Get hour

  (setq at--init-hour-str (substring at--init-time 11 13))
  (setq at--init-hour-int (string-to-number at--init-hour-str 10))

  ;; Get minute

  (setq at--init-min-str (substring at--init-time 14 16))
  (setq at--init-min-int (string-to-number at--init-min-str 10))

  ;; Get Second

  (setq at--init-sec-str (substring at--init-time 17 19))
  (setq at--init-sec-int (string-to-number at--init-sec-str 10))

  ;;;; Detect if is day

  (setq at--is-day t)
  (setq at--is-morning nil)
  (setq at--is-night nil)

  ;;;;; Compare with am

  ;;;;;; Hour

  (if (< at--init-hour-int am-hour)

      ;; true if init hour <  dawn hour is night

      (setq at--is-day nil)

    ;; Evaluate minutes if is the same hour

    (if (= at--init-hour-int am-hour)

        ;; evaluate minutes

        (if (< at--init-min-int am-min)

            ;; true if init min <  dawn min is night

            (setq at--is-day nil)

          ;; Evaluate seconds if is the same minute

          (if (= at--init-sec-int am-sec)

              ;; evaluate seconds

              (if (< at--init-sec-int am-sec)

                  ;; true if init sec <  dawn sec is night

                  (setq at--is-day nil))))))

  ;; Is is before day is morning

  (if (null at--is-day)
      (setq at--is-morning t))

  ;;;;; Compare with pm

  ;;;;;; Hour

  (if (> at--init-hour-int pm-hour)

      ;; true if init hour >  sunset hour is night

      (setq at--is-day nil)

    ;; Evaluate minutes if is the same hour

    (if (= at--init-hour-int pm-hour)

        ;; evaluate minutes

        (if (> at--init-min-int pm-min)

            ;; true if init min >  sunset min is night

            (setq at--is-day nil)

          ;; Evaluate seconds if is the same minute

          (if (= at--init-sec-int pm-sec)

              ;; evaluate seconds

              (if (> at--init-sec-int pm-sec)

                  ;; true if init sec >  sunset sec is night

                  (setq at--is-day nil))))))

  ;; If is not day and no morning is night

  (if (null at--is-day)
      (if (null at--is-morning)
          (setq at--is-night t)))

  ;;;; Load theme

  (if at--is-day

      ;; Load ligth theme if is day

      (load-theme light-theme t)

    ;; Load dark theme if is not day

    (load-theme dark-theme t))

  ;;;; Program next theme change

  ;;;;; Calculate time before change day - night

  (if (eval at--is-day)
      (if (> pm-min 58)
          (lambda ()
            (if (equal pm-hour 23)
                (setq pm-hour 0)
              (setq pm-hour (+ pm-hour 1)))
            (setq pm-hour (+ pm-hour 1))
            (setq pm-min 0))
        (setq pm-min (+ pm-min 1)))

    ;; If is not day

    (if (equal am-min 59)
        (lambda ()
          (if (equal am-hour 23)
              (setq am-hour 0)
            (setq am-hour (+ am-hour 1)))
          (setq am-min 0))
      (setq am-min (+ am-min 1))))

  ;; Calculate next hour as str

  (if (< pm-hour 10)
      (setq at--pm-hour-str (concat "0" (number-to-string pm-hour)))
    (setq at--pm-hour-str (number-to-string pm-hour)))

  ;; Calculate next min asl str

  (if (< pm-min 10)
      (setq at--pm-min-str (concat "0" (number-to-string pm-min)))
    (setq at--pm-min-str (number-to-string pm-min)))

  ;; Define pm hour

  (setq at--pm-str (concat at--pm-hour-str ":" at--pm-min-str))

  ;; Calculate next hour as str

  (if (< am-hour 10)
      (setq at--am-hour-str (concat "0" (number-to-string am-hour)))
    (setq at--am-hour-str (number-to-string am-hour)))

  ;; Calculate next min asl str

  (if (< am-min 10)
      (setq at--am-min-str (concat "0" (number-to-string am-min)))
    (setq at--am-min-str (number-to-string am-min)))

  ;; Define am hour

  (setq at--am-str (concat at--am-hour-str ":" at--am-min-str))

  ;; If is night use timer in seconds

  (if (eval at--is-night)
      (lambda ()
        (setq at--am-str-int (+ (* (- 23 at--init-hour-int) 3600)
                                (* (- 59 at--init-min-int) 60)
                                (- 59 at--init-sec-int)))
        (setq at--am-str (concat (number-to-string at--am-str-int) " sec"))))

  ;;;;; Program

  ;; Cancel timer if exist

  (if (eval at--is-day)
      (if (boundp 'at--am-timer)
          (cancel-timer at--am-timer))
    (if (boundp 'at--pm-timer)
        (cancel-timer at--pm-timer)))

  ;; Reset timer

  (if (eval at--is-day)
      (setq at--pm-timer (run-at-time at--pm-str nil #'adaptive-theme 'gruvbox-light-soft 'gruvbox-dark-hard (eval at--am-hour-init) (eval at--pm-hour-init) (eval at--am-min-init) (eval at--pm-min-init) (eval at--am-sec-init) (eval at--pm-sec-init))))

  (if (eval at--is-morning)
      (setq at--am-timer (run-at-time at--am-str nil #'adaptive-theme 'gruvbox-light-soft 'gruvbox-dark-hard (eval at--am-hour-init) (eval at--pm-hour-init) (eval at--am-min-init) (eval at--pm-min-init) (eval at--am-sec-init) (eval at--pm-sec-init)))))

;;; Adaptive theme location

(defun adaptive-theme-location (light-theme dark-theme &optional country city)
  "Adaptive theme location function:

   @Brief:   This function allow to configure different themes depending on your
             location when work.

   @Author:  acsm

   @Version: A/1

   @Args:    LIGHT-THEME: Theme loaded in sun hours.
             DARK-THEME:  Theme loaded in dark hours.
             Optional arguments:
             COUNTRY:    Custom Country location (str) (optional, default spain)
             CITY:       Custom City or capital location (str) (optional, default madrid)

   @Links:   https://www.timeanddate.com/sun where look for your country and city names."


  ;;;; Setup variables

  ;; atl-- include all adaptive-theme-location variables

  (defvar atl--internet-external-host)
  (defvar atl--is-internet-up)
  (defvar atl--url)
  (defvar atl--web_to_scrap)
  (defvar atl--webDataHtml)
  (defvar atl--webRegexModel)
  (defvar atl--daylight-regex)
  (defvar atl--subWebStr)
  (defvar atl--timeGroupRegex)
  (defvar atl--timeStr)
  (defvar atl--amTime)
  (defvar atl--amSeparator)
  (defvar atl--amHourStr)
  (defvar atl--amHourInt)
  (defvar atl--amMinStr)
  (defvar atl--amMinInt)
  (defvar atl--pmSeparator)
  (defvar atl--pmHourStr)
  (defvar atl--pmHourInt)
  (defvar atl--pmMinStr)
  (defvar atl--pmMinInt)


  ;;;; Detect internet connection

  ;; Change "www.google.es" with your proxy server if you need it. "my_proxy.es"

  (if (null (boundp 'host))
      (setq atl--internet-external-host "www.google.com"))
  (setq atl--is-internet-up (call-process "ping" nil nil nil "-c" "1" "-w" "1" atl--internet-external-host))

  (if (/= atl--is-internet-up 0)

      ;; If internet is not connected

      (lambda ()
        (adaptive-theme (light-theme dark-theme))
        (progn (message "No network detected") nil)
        (return)))

  ;;;; Load basic requieres

  (require 'org-web-tools)

  ;;;; Web scraping

  ;; URL base to get am and pm data

  (setq atl--url "https://www.timeanddate.com/sun")

  ;; Set default county

  (unless (eval country)
    (setq country "spain"))

  ;; Set default city

  (unless (eval city)
    (setq city "madrid"))

  ;; Compose url

  (setq atl--web_to_scrap (concat atl--url "/" country "/" city))
  (setq atl--webDataHtml (org-web-tools--get-url atl--web_to_scrap))

  ;;;; Web regex model

  (setq atl--webRegexModel "\\s_Daylight\\s_\\{2\\}div\\s_\\{2\\}p\\s-class\\s_dn\\s_mob\\s_[0-9]\\{1,2\\}:[0-9]\\{1,2\\}\\s-\\s_\\{1,\\}")

  ;;;; Extract regex value

  (setq atl--daylight-regex
        (string-match atl--webRegexModel atl--webDataHtml))

  ;;;; Extract substring

  (setq atl--subWebStr
        (substring atl--webDataHtml atl--daylight-regex (+ atl--daylight-regex 100)))

  ;;;; Create regex group time

  (setq atl--timeGroupRegex "[0-9]\\{1,2\\}:[0-9]\\{1,2\\}")

  ;;;; Extract time value as string

  (setq atl--timeStr (string-match atl--timeGroupRegex atl--subWebStr))

  ;;;; Extract AM hour

  (setq atl--amTime (substring atl--subWebStr atl--timeStr (+ atl--timeStr 5)))

  ;;;; Get substring pm time
  (setq atl--timeStr (string-match atl--timeGroupRegex atl--subWebStr (+ atl--timeStr 6)))

  ;;;; Extract PM time
  (defvar atl--pmTime)
  (setq atl--pmTime (substring atl--subWebStr atl--timeStr (+ atl--timeStr 5)))

  ;;;; Regenerate time regext to get hour and minutes
  (defvar atl--timeGroupRegex)
  (setq atl--timeGroupRegex ":")

  ;;;; Get AM Hour

  (setq atl--amSeparator (string-match atl--timeGroupRegex atl--amTime))
  (setq atl--amHourStr (substring atl--amTime 0 atl--amSeparator))
  (setq atl--amHourInt (string-to-number atl--amHourStr))

  ;;;; Get AM Min

  (setq atl--amSeparator (string-match atl--timeGroupRegex atl--amTime))
  (setq atl--amMinStr (substring atl--amTime (+ atl--amSeparator 1) (+ atl--amSeparator 3)))
  (setq atl--amMinInt (string-to-number atl--amMinStr))

  ;;;; Get PM Hour

  (setq atl--pmSeparator (string-match atl--timeGroupRegex atl--pmTime))
  (setq atl--pmHourStr (substring atl--pmTime 0 atl--pmSeparator))
  (setq atl--pmHourInt (string-to-number atl--pmHourStr))

  ;;;; Get PM Min

  (setq atl--pmSeparator (string-match atl--timeGroupRegex atl--pmTime))
  (setq atl--pmMinStr (substring atl--pmTime (+ atl--pmSeparator 1) (+ atl--pmSeparator 3)))
  (setq atl--pmMinInt (string-to-number atl--pmMinStr))

  ;;;; Execute adaptive theme function

  (adaptive-theme light-theme dark-theme atl--amHourInt atl--pmHourInt atl--amMinInt atl--pmMinInt))

;;; Auto location adaptive theme

(defun adaptive-theme-autolocation (light-theme dark-theme)
  "Adaptive theme auto-location function:

   @Brief:   This function allow to configure different themes depending on your
             location when work, it get your location from internet.

   @Author:  acsm

   @Version: A/1

   @Args:    LIGHT-THEME: Theme loaded in sun hours.
             DARK-THEME:  Theme loaded in dark hours.

   @Links:   https://www.timeanddate.com where look for your country and city names."

  ;;;; Setup variables

  ;; ata-- include all adaptive-theme-autolocation variables

  (defvar ata--internet-external-host)
  (defvar ata--is-internet-up)
  (defvar ata--url-location)
  (defvar ata--webDataHtml)
  (defvar ata--location-regex)
  (defvar ata--subLocStr)
  (defvar ata--init-regex)
  (defvar ata--end-regex)
  (defvar ata--myCountryLoc)
  (defvar ata--myCityLoc)

  ;;;; Detect internet connection

  ;; Change "www.google.es" with your proxy server if you need it. "my_proxy.es"

  (if (null (boundp 'host))
      (setq ata--internet-external-host "www.google.com"))
  (setq ata--is-internet-up (call-process "ping" nil nil nil "-c" "1" "-w" "1" ata--internet-external-host))

  (if (/= ata--is-internet-up 0)

      ;; If internet is not connected

      (lambda ()
        (adaptive-theme (dark-theme light-theme))
        (progn (message "No network detected") nil) (return)))

  ;;;; Load basic requieres

  (require 'org-web-tools)

  ;;;; Web scraping

  ;; URL base to get am and pm data

  (setq ata--url-location "https://www.timeanddate.com")

  ;; Get main web where display your location

  (setq ata--webDataHtml (org-web-tools--get-url ata--url-location))

  ;;;; Extract regex value

  (setq ata--location-regex
        (string-match "The World Clock" ata--webDataHtml))

  ;;;; Extract substring

  (setq ata--subLocStr
        (substring ata--webDataHtml ata--location-regex (+ ata--location-regex 120)))

  ;;;; Extract regex value

  (setq ata--init-regex
        (string-match "worldclock/" ata--subLocStr))
  (setq ata--end-regex
        (string-match "id=" ata--subLocStr))

  ;;;; Extract substring

  (setq ata--subLocStr
        (substring ata--subLocStr (+ ata--init-regex 11) (+ ata--end-regex 3)))

  ;;;; Extract country

  (setq ata--end-regex
        (string-match "/" ata--subLocStr))
  (setq ata--myCountryLoc
        (substring ata--subLocStr 0 ata--end-regex))

  ;;;; Extract city

  (setq ata--init-regex
        (string-match "/" ata--subLocStr))

  (setq ata--end-regex
        (string-match "id=" ata--subLocStr))

  (setq ata--myCityLoc
        (substring ata--subLocStr (+ ata--init-regex 1) (- ata--end-regex 2)))

  ;; Execute location function with data extracted.

  (adaptive-theme-location light-theme dark-theme ata--myCountryLoc ata--myCityLoc))

;;;(provide 'adaptive-theme)

(provide 'adaptive-theme)

;;; adaptive-theme.el ends here
