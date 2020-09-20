;;; adaptive-theme.el --- Adaptive theme to protect your eyesight --- -*- lexical-binding: t -*-

;; Copyright (C) 2020  Álvaro Cortés Sánchez-Migallón

;; Author: Álvaro Cortés Sánchez-Migallón <alvarocsm.91@gmail.com>
;; Keywords: internals, tools, unix, local, terminals
;; Url: https://github.com/alvarocsm91/adaptive-theme
;; Package-requires: ((emacs "25.1") (org-web-tools "1.1.1"))
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

  (defvar adaptive-theme--init-time nil
    "Store current time. adaptive-theme variable")
  (defvar adaptive-theme--init-hour-str nil
    "Store hour value with string format. adaptive-theme variable")
  (defvar adaptive-theme--init-min-str nil
    "Store minute value with string format. adaptive-theme variable")
  (defvar adaptive-theme--init-sec-str nil
    "Store seconds value with string format. adaptive-theme variable")
  (defvar adaptive-theme--init-hour-int nil
    "Store hour value with integer format. adaptive-theme variable")
  (defvar adaptive-theme--init-min-int nil
    "Store minute value with integer format. adaptive-theme variable")
  (defvar adaptive-theme--init-sec-int nil
    "Store seconds value with integer format. adaptive-theme variable")
  (defvar adaptive-theme--is-day nil
    "True if is day, else nil. adaptive-theme variable")
  (defvar adaptive-theme--is-morning nil
    "True if is morning, else nil. adaptive-theme variable")
  (defvar adaptive-theme--is-night nil
    "True if is night, else nil. adaptive-theme variable")
  (defvar adaptive-theme--pm-hour-str nil
    "Store pm hour with string format. adaptive-theme variable")
  (defvar adaptive-theme--pm-min-str nil
    "Store pm minutes with string format. adaptive-theme variable")
  (defvar adaptive-theme--pm-str nil
    "Store pm time with string format. adaptive-theme variable")
  (defvar adaptive-theme--am-hour-str nil
    "Store am hour with string format. adaptive-theme variable")
  (defvar adaptive-theme--am-min-str nil
    "Store am minutes with string format. adaptive-theme variable")
  (defvar adaptive-theme--am-str nil
    "Store am time with string format. adaptive-theme variable")
  (defvar adaptive-theme--am-str-int nil
    "Store am time with integer format. adaptive-theme variable")
  (defvar adaptive-theme--pm-hour-init nil
    "Store pm hour with integer format. adaptive-theme variable")
  (defvar adaptive-theme--pm-min-init nil
    "Store pm minutes with integer format. adaptive-theme variable")
  (defvar adaptive-theme--pm-sec-init nil
    "Store pm seconds with integer format. adaptive-theme variable")
  (defvar adaptive-theme--am-hour-init nil
    "Store am hour with integer format. adaptive-theme variable")
  (defvar adaptive-theme--am-min-init nil
    "Store am minutes with integer format. adaptive-theme variable")
  (defvar adaptive-theme--am-sec-init nil
    "Store am seconds with integer format. adaptive-theme variable")
  (defvar adaptive-theme--pm-timer nil
    "Store pm timer value. adaptive-theme variable")
  (defvar adaptive-theme--am-timer nil
    "Store am timer value. adaptive-theme variable")

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

  (setq adaptive-theme--am-hour-init am-hour)
  (setq adaptive-theme--am-min-init am-min)
  (setq adaptive-theme--am-sec-init am-sec)

  ;; Set sundown time

  (setq adaptive-theme--pm-hour-init pm-hour)
  (setq adaptive-theme--pm-min-init pm-min)
  (setq adaptive-theme--pm-sec-init pm-sec)

  ;;;; Get time

  (setq adaptive-theme--init-time (current-time-string))

  ;;;; Get integer hour

  ;; Get hour

  (setq adaptive-theme--init-hour-str (substring adaptive-theme--init-time 11 13))
  (setq adaptive-theme--init-hour-int (string-to-number adaptive-theme--init-hour-str 10))

  ;; Get minute

  (setq adaptive-theme--init-min-str (substring adaptive-theme--init-time 14 16))
  (setq adaptive-theme--init-min-int (string-to-number adaptive-theme--init-min-str 10))

  ;; Get Second

  (setq adaptive-theme--init-sec-str (substring adaptive-theme--init-time 17 19))
  (setq adaptive-theme--init-sec-int (string-to-number adaptive-theme--init-sec-str 10))

  ;;;; Detect if is day

  (setq adaptive-theme--is-day t)
  (setq adaptive-theme--is-morning nil)
  (setq adaptive-theme--is-night nil)

  ;;;;; Compare with am

  ;;;;;; Hour

  (if (< adaptive-theme--init-hour-int am-hour)

      ;; true if init hour <  dawn hour is night

      (setq adaptive-theme--is-day nil)

    ;; Evaluate minutes if is the same hour

    (if (= adaptive-theme--init-hour-int am-hour)

        ;; evaluate minutes

        (if (< adaptive-theme--init-min-int am-min)

            ;; true if init min <  dawn min is night

            (setq adaptive-theme--is-day nil)

          ;; Evaluate seconds if is the same minute

          (if (= adaptive-theme--init-sec-int am-sec)

              ;; evaluate seconds

              (if (< adaptive-theme--init-sec-int am-sec)

                  ;; true if init sec <  dawn sec is night

                  (setq adaptive-theme--is-day nil))))))

  ;; Is is before day is morning

  (if (null adaptive-theme--is-day)
      (setq adaptive-theme--is-morning t))

  ;;;;; Compare with pm

  ;;;;;; Hour

  (if (> adaptive-theme--init-hour-int pm-hour)

      ;; true if init hour >  sunset hour is night

      (setq adaptive-theme--is-day nil)

    ;; Evaluate minutes if is the same hour

    (if (= adaptive-theme--init-hour-int pm-hour)

        ;; evaluate minutes

        (if (> adaptive-theme--init-min-int pm-min)

            ;; true if init min >  sunset min is night

            (setq adaptive-theme--is-day nil)

          ;; Evaluate seconds if is the same minute

          (if (= adaptive-theme--init-sec-int pm-sec)

              ;; evaluate seconds

              (if (> adaptive-theme--init-sec-int pm-sec)

                  ;; true if init sec >  sunset sec is night

                  (setq adaptive-theme--is-day nil))))))

  ;; If is not day and no morning is night

  (if (null adaptive-theme--is-day)
      (if (null adaptive-theme--is-morning)
          (setq adaptive-theme--is-night t)))

  ;;;; Load theme

  (if adaptive-theme--is-day

      ;; Load ligth theme if is day

      (load-theme light-theme t)

    ;; Load dark theme if is not day

    (load-theme dark-theme t))

  ;;;; Program next theme change

  ;;;;; Calculate time before change day - night

  (if (eval adaptive-theme--is-day)
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
      (setq adaptive-theme--pm-hour-str (concat "0" (number-to-string pm-hour)))
    (setq adaptive-theme--pm-hour-str (number-to-string pm-hour)))

  ;; Calculate next min asl str

  (if (< pm-min 10)
      (setq adaptive-theme--pm-min-str (concat "0" (number-to-string pm-min)))
    (setq adaptive-theme--pm-min-str (number-to-string pm-min)))

  ;; Define pm hour

  (setq adaptive-theme--pm-str (concat adaptive-theme--pm-hour-str ":" adaptive-theme--pm-min-str))

  ;; Calculate next hour as str

  (if (< am-hour 10)
      (setq adaptive-theme--am-hour-str (concat "0" (number-to-string am-hour)))
    (setq adaptive-theme--am-hour-str (number-to-string am-hour)))

  ;; Calculate next min asl str

  (if (< am-min 10)
      (setq adaptive-theme--am-min-str (concat "0" (number-to-string am-min)))
    (setq adaptive-theme--am-min-str (number-to-string am-min)))

  ;; Define am hour

  (setq adaptive-theme--am-str (concat adaptive-theme--am-hour-str ":" adaptive-theme--am-min-str))

  ;; If is night use timer in seconds

  (if (eval adaptive-theme--is-night)
      (lambda ()
        (setq adaptive-theme--am-str-int (+ (* (- 23 adaptive-theme--init-hour-int) 3600)
                                            (* (- 59 adaptive-theme--init-min-int) 60)
                                            (- 59 adaptive-theme--init-sec-int)))
        (setq adaptive-theme--am-str (concat (number-to-string adaptive-theme--am-str-int) " sec"))))

  ;;;;; Program

  ;; Cancel timer if exist

  (if (eval adaptive-theme--is-day)
      (if (boundp 'adaptive-theme--am-timer)
          (cancel-timer adaptive-theme--am-timer))
    (if (boundp 'adaptive-theme--pm-timer)
        (cancel-timer adaptive-theme--pm-timer)))

  ;; Reset timer

  (if (eval adaptive-theme--is-day)
      (setq adaptive-theme--pm-timer (run-at-time adaptive-theme--pm-str nil #'adaptive-theme 'gruvbox-light-soft 'gruvbox-dark-hard (eval adaptive-theme--am-hour-init) (eval adaptive-theme--pm-hour-init) (eval adaptive-theme--am-min-init) (eval adaptive-theme--pm-min-init) (eval adaptive-theme--am-sec-init) (eval adaptive-theme--pm-sec-init))))

  (if (eval adaptive-theme--is-morning)
      (setq adaptive-theme--am-timer (run-at-time adaptive-theme--am-str nil #'adaptive-theme 'gruvbox-light-soft 'gruvbox-dark-hard (eval adaptive-theme--am-hour-init) (eval adaptive-theme--pm-hour-init) (eval adaptive-theme--am-min-init) (eval adaptive-theme--pm-min-init) (eval adaptive-theme--am-sec-init) (eval adaptive-theme--pm-sec-init)))))

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

  ;; adaptive-theme-- include all adaptive-theme-location variables

  (defvar adaptive-theme--internet-external-host nil
    "Store if can connect with internet. adaptive-theme variable")
  (defvar adaptive-theme--is-internet-up nil
    "Store if can connect with internet. adaptive-theme variable")
  (defvar adaptive-theme--url nil
    "Store url string to connect. adaptive-theme variable")
  (defvar adaptive-theme--web_to_scrap nil
    "Store wev to scrap with time information. adaptive-theme variable")
  (defvar adaptive-theme--webDataHtml nil
    "Store HTML from with information. adaptive-theme variable")
  (defvar adaptive-theme--webRegexModel nil
    "Store regex string model. adaptive-theme variable")
  (defvar adaptive-theme--daylight-regex nil
    "Store sunset regex store. adaptive-theme variable")
  (defvar adaptive-theme--subWebStr nil
    "Store HTML subtring. adaptive-theme variable")
  (defvar adaptive-theme--timeGroupRegex nil
    "Store time group regex. adaptive-theme variable")
  (defvar adaptive-theme--timeStr nil
    "Store time string from web. adaptive-theme variable")
  (defvar adaptive-theme--amTime nil
    "Store am time string. adaptive-theme variable")
  (defvar adaptive-theme--amSeparator nil
    "Store am separator (:). adaptive-theme variable")
  (defvar adaptive-theme--amHourStr nil
    "Store am hour with string format. adaptive-theme variable")
  (defvar adaptive-theme--amHourInt nil
    "Store am hour with integer format. adaptive-theme variable")
  (defvar adaptive-theme--amMinStr nil
    "Store am minutes with string format. adaptive-theme variable")
  (defvar adaptive-theme--amMinInt nil
    "Store am minutes with integer format. adaptive-theme variable")
  (defvar adaptive-theme--pmSeparator nil
    "Store pm separator (:). adaptive-theme variable")
  (defvar adaptive-theme--pmHourStr nil
    "Store pm hour with string format. adaptive-theme variable")
  (defvar adaptive-theme--pmHourInt nil
    "Store pm hour with integer format. adaptive-theme variable")
  (defvar adaptive-theme--pmMinStr nil
    "Store pm minutes with string format. adaptive-theme variable")
  (defvar adaptive-theme--pmMinInt nil
    "Store pm minutes with integer format. adaptive-theme variable")
  (defvar adaptive-theme--pmTime nil
    "Store pm time. adaptive-theme variable")
  (defvar adaptive-theme--timeGroupRegex nil
    "Store time group regex. adaptive-theme variable")

  ;;;; Detect internet connection

  ;; Change "www.google.es" with your proxy server if you need it. "my_proxy.es"

  (if (null (boundp 'host))
      (setq adaptive-theme--internet-external-host "www.google.com"))
  (setq adaptive-theme--is-internet-up (call-process "ping" nil nil nil "-c" "1" "-w" "1" adaptive-theme--internet-external-host))

  (if (/= adaptive-theme--is-internet-up 0)

      ;; If internet is not connected

      (lambda ()
        (adaptive-theme (light-theme dark-theme))
        (progn (message "No network detected") nil)
        (return)))

  ;;;; Load basic requieres

  (require 'org-web-tools)

  ;;;; Web scraping

  ;; URL base to get am and pm data

  (setq adaptive-theme--url "https://www.timeanddate.com/sun")

  ;; Set default county

  (unless (eval country)
    (setq country "spain"))

  ;; Set default city

  (unless (eval city)
    (setq city "madrid"))

  ;; Compose url

  (setq adaptive-theme--web_to_scrap (concat adaptive-theme--url "/" country "/" city))
  (setq adaptive-theme--webDataHtml (org-web-tools--get-url adaptive-theme--web_to_scrap))

  ;;;; Web regex model

  (setq adaptive-theme--webRegexModel "\\s_Daylight\\s_\\{2\\}div\\s_\\{2\\}p\\s-class\\s_dn\\s_mob\\s_[0-9]\\{1,2\\}:[0-9]\\{1,2\\}\\s-\\s_\\{1,\\}")

  ;;;; Extract regex value

  (setq adaptive-theme--daylight-regex
        (string-match adaptive-theme--webRegexModel adaptive-theme--webDataHtml))

  ;;;; Extract substring

  (setq adaptive-theme--subWebStr
        (substring adaptive-theme--webDataHtml adaptive-theme--daylight-regex (+ adaptive-theme--daylight-regex 100)))

  ;;;; Create regex group time

  (setq adaptive-theme--timeGroupRegex "[0-9]\\{1,2\\}:[0-9]\\{1,2\\}")

  ;;;; Extract time value as string

  (setq adaptive-theme--timeStr (string-match adaptive-theme--timeGroupRegex adaptive-theme--subWebStr))

  ;;;; Extract AM hour

  (setq adaptive-theme--amTime (substring adaptive-theme--subWebStr adaptive-theme--timeStr (+ adaptive-theme--timeStr 5)))

  ;;;; Get substring pm time

  (setq adaptive-theme--timeStr (string-match adaptive-theme--timeGroupRegex adaptive-theme--subWebStr (+ adaptive-theme--timeStr 6)))

  ;;;; Extract PM time

  (setq adaptive-theme--pmTime (substring adaptive-theme--subWebStr adaptive-theme--timeStr (+ adaptive-theme--timeStr 5)))

  ;;;; Regenerate time regext to get hour and minutes

  (setq adaptive-theme--timeGroupRegex ":")

  ;;;; Get AM Hour

  (setq adaptive-theme--amSeparator (string-match adaptive-theme--timeGroupRegex adaptive-theme--amTime))
  (setq adaptive-theme--amHourStr (substring adaptive-theme--amTime 0 adaptive-theme--amSeparator))
  (setq adaptive-theme--amHourInt (string-to-number adaptive-theme--amHourStr))

  ;;;; Get AM Min

  (setq adaptive-theme--amSeparator (string-match adaptive-theme--timeGroupRegex adaptive-theme--amTime))
  (setq adaptive-theme--amMinStr (substring adaptive-theme--amTime (+ adaptive-theme--amSeparator 1) (+ adaptive-theme--amSeparator 3)))
  (setq adaptive-theme--amMinInt (string-to-number adaptive-theme--amMinStr))

  ;;;; Get PM Hour

  (setq adaptive-theme--pmSeparator (string-match adaptive-theme--timeGroupRegex adaptive-theme--pmTime))
  (setq adaptive-theme--pmHourStr (substring adaptive-theme--pmTime 0 adaptive-theme--pmSeparator))
  (setq adaptive-theme--pmHourInt (string-to-number adaptive-theme--pmHourStr))

  ;;;; Get PM Min

  (setq adaptive-theme--pmSeparator (string-match adaptive-theme--timeGroupRegex adaptive-theme--pmTime))
  (setq adaptive-theme--pmMinStr (substring adaptive-theme--pmTime (+ adaptive-theme--pmSeparator 1) (+ adaptive-theme--pmSeparator 3)))
  (setq adaptive-theme--pmMinInt (string-to-number adaptive-theme--pmMinStr))

  ;;;; Execute adaptive theme function

  (adaptive-theme light-theme dark-theme adaptive-theme--amHourInt adaptive-theme--pmHourInt adaptive-theme--amMinInt adaptive-theme--pmMinInt))

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

  ;; adaptive-theme-- include all adaptive-theme-autolocation variables

  (defvar adaptive-theme--internet-external-host nil
    "Store external host to detect internet connection. adaptive-theme variable")
  (defvar adaptive-theme--is-internet-up nil
    "Store if internet connection exist. adaptive-theme variable")
  (defvar adaptive-theme--url-location nil
    "Store url from web location. adaptive-theme variable")
  (defvar adaptive-theme--webDataHtml nil
    "Store HTML web page. adaptive-theme variable")
  (defvar adaptive-theme--location-regex nil
    "Store location regex pattern. adaptive-theme variable")
  (defvar adaptive-theme--subLocStr nil
    "Store HTML substring first filter. adaptive-theme variable")
  (defvar adaptive-theme--init-regex nil
    "Store previous substring pattern. adaptive-theme variable")
  (defvar adaptive-theme--end-regex nil
    "Store subsequent regex substring pattern. adaptive-theme variable")
  (defvar adaptive-theme--myCountryLoc nil
    "Store substring country location. adaptive-theme variable")
  (defvar adaptive-theme--myCityLoc nil
    "Store substring city location. adaptive-theme variable")

  ;;;; Detect internet connection

  ;; Change "www.google.es" with your proxy server if you need it. "my_proxy.es"

  (if (null (boundp 'host))
      (setq adaptive-theme--internet-external-host "www.google.com"))
  (setq adaptive-theme--is-internet-up (call-process "ping" nil nil nil "-c" "1" "-w" "1" adaptive-theme--internet-external-host))

  (if (/= adaptive-theme--is-internet-up 0)

      ;; If internet is not connected

      (lambda ()
        (adaptive-theme (dark-theme light-theme))
        (progn (message "No network detected") nil) (return)))

  ;;;; Load basic requieres

  (require 'org-web-tools)

  ;;;; Web scraping

  ;; URL base to get am and pm data

  (setq adaptive-theme--url-location "https://www.timeanddate.com")

  ;; Get main web where display your location

  (setq adaptive-theme--webDataHtml (org-web-tools--get-url adaptive-theme--url-location))

  ;;;; Extract regex value

  (setq adaptive-theme--location-regex
        (string-match "The World Clock" adaptive-theme--webDataHtml))

  ;;;; Extract substring

  (setq adaptive-theme--subLocStr
        (substring adaptive-theme--webDataHtml adaptive-theme--location-regex (+ adaptive-theme--location-regex 120)))

  ;;;; Extract regex value

  (setq adaptive-theme--init-regex
        (string-match "worldclock/" adaptive-theme--subLocStr))
  (setq adaptive-theme--end-regex
        (string-match "id=" adaptive-theme--subLocStr))

  ;;;; Extract substring

  (setq adaptive-theme--subLocStr
        (substring adaptive-theme--subLocStr (+ adaptive-theme--init-regex 11) (+ adaptive-theme--end-regex 3)))

  ;;;; Extract country

  (setq adaptive-theme--end-regex
        (string-match "/" adaptive-theme--subLocStr))
  (setq adaptive-theme--myCountryLoc
        (substring adaptive-theme--subLocStr 0 adaptive-theme--end-regex))

  ;;;; Extract city

  (setq adaptive-theme--init-regex
        (string-match "/" adaptive-theme--subLocStr))

  (setq adaptive-theme--end-regex
        (string-match "id=" adaptive-theme--subLocStr))

  (setq adaptive-theme--myCityLoc
        (substring adaptive-theme--subLocStr (+ adaptive-theme--init-regex 1) (- adaptive-theme--end-regex 2)))

  ;; Execute location function with data extracted.

  (adaptive-theme-location light-theme dark-theme adaptive-theme--myCountryLoc adaptive-theme--myCityLoc))

;;;(provide 'adaptive-theme)

(provide 'adaptive-theme)

;;; adaptive-theme.el ends here
