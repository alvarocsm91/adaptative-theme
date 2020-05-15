;;; Adaptative theme function
(defun adaptative-theme
  (light-theme dark-theme &optional am-hour pm-hour am-min pm-min am-sec pm-sec)
  " Adaptative theme function:
@Brief:   This function allow to configure different themes depending on the
          emacs initialization time.

@Author:  acsm

@Version: A/1

@Args:    light-theme: Theme loaded in sun hours.
          dark-theme:  Theme loaded in dark hours.
          &-am-hour: Custom dawn hour (0-23) (optional, default 07)
          &-pm-hour: Custom sunset hour (0-23) (optional, default 20)
          &-am-min:  Custom dawn min (0-59) (optional, default 00)
          &-pm-min:  Custom sunset min (0-59) (optional, default 00)
          &-am-sec:  Custom dawn sec (0-59) (optional, default 00)
          &-pm-sec:  Custom sunset sec (0-59) (optional, default 00)

@Links:
"

;;;; Set default values
  ;; Set dawn time
  (unless (eval am-hour)
    (set 'am-hour 7))
  (unless (eval am-min)
    (set 'am-min 00))
  (unless (eval am-sec)
    (set 'am-sec 00))

  ;; Set sundown time
  (unless (eval pm-hour)
    (set 'pm-hour 20))
  (unless (eval pm-min)
    (set 'pm-min 00))
  (unless (eval pm-sec)
    (set 'pm-sec 00))

  ;; Set dawn time aux
  (setq am-hour-init am-hour)
  (setq am-min-init am-min)
  (setq am-sec-init am-sec)
  ;; Set sundown time
  (setq pm-hour-init pm-hour)
  (setq pm-min-init pm-min)
  (setq pm-sec-init pm-sec)

;;;; Get time
  (set 'init-time (current-time-string))

;;;; Get integer hour
  ;; Get hour
  (set 'init-hour-str (substring init-time 11 13))
  (set 'init-hour-int (string-to-number init-hour-str 10))
  ;; Get minute
  (set 'init-min-str (substring init-time 14 16))
  (set 'init-min-int (string-to-number init-min-str 10))
  ;; Get Second
  (set 'init-sec-str (substring init-time 17 19))
  (set 'init-sec-int (string-to-number init-sec-str 10))

;;;; Detect if is day
  (setq is-day t)
  (setq is-morning nil)
  (setq is-night nil)

;;;;; Compare with am
;;;;;; Hour
  (if (< init-hour-int am-hour)
      ;; true if init hour <  dawn hour is night
      (set 'is-day nil)
    ;; Evaluate minutes if is the same hour
    (if (= init-hour-int am-hour)
        ;; evaluate minutes
        (if (< init-min-int am-min)
            ;; true if init min <  dawn min is night
            (set 'is-day nil)
          ;; Evaluate seconds if is the same minute
          (if (= init-sec-int am-sec)
              ;; evaluate seconds
              (if (< init-sec-int am-sec)
                  ;; true if init sec <  dawn sec is night
                  (set 'is-day nil)
                  )))
          )
      )

  ;; Is is before day is morning
  (if (null is-day)
      (setq is-morning t))

;;;;; Compare with pm
;;;;;; Hour
  (if (> init-hour-int pm-hour)
      ;; true if init hour >  sunset hour is night
      (set 'is-day nil)
    ;; Evaluate minutes if is the same hour
    (if (= init-hour-int pm-hour)
        ;; evaluate minutes
        (if (> init-min-int pm-min)
            ;; true if init min >  sunset min is night
            (set 'is-day nil)
          ;; Evaluate seconds if is the same minute
          (if (= init-sec-int pm-sec)
              ;; evaluate seconds
              (if (> init-sec-int pm-sec)
                  ;; true if init sec >  sunset sec is night
                  (set 'is-day nil)))
          )
      )
    )

  ;; If is not day and no morning is night
  (if (null is-day)
      (if (null is-morning)
          (setq is-night t)))

;;;; Load theme
  (if is-day
      ;; Load ligth theme if is day
      (load-theme light-theme t)
    ;;(load-theme 'gruvbox-light-soft t)
    ;; Load dark theme if is not day
    (load-theme dark-theme t))
  ;;(load-theme 'gruvbox-dark-hard t))

;;;; Program nex theme change
  ;; Timer example
  ;;(run-at-time "5 sec" nil #'message "Prueba timer")
  ;;(run-at-time "20:30" nil #'kill-emacs)
  ;;(run-at-time "5 sec" nil #'adaptative-theme 'gruvbox-light-soft 'gruvbox-dark-hard)

  ;;;;; Calculate time before change day - night
  (if (eval is-day)
      (if (> pm-min 58)
          (lambda ()
            (if (equal pm-hour 23)
              (setq pm-hour 0)
              (setq pm-hour (+ pm-hour 1))
              )
            (setq pm-hour (+ pm-hour 1))
            (setq pm-min 0))
        (setq pm-min (+ pm-min 1))
        )
    ;; If is not day
    (if (equal am-min 59)
        (lambda ()
          (if (equal am-hour 23)
            (setq am-hour 0)
            (setq am-hour (+ am-hour 1))
            )
          (setq am-min 0))
      (setq am-min (+ am-min 1))
      )
    )

  ;; Calculate next hour as str
  (if (< pm-hour 10)
      (setq pm-hour-str (concat "0" (number-to-string pm-hour)))
    (setq pm-hour-str (number-to-string pm-hour))
    )

  ;; Calculate next min asl str
  (if (< pm-min 10)
      (setq pm-min-str (concat "0" (number-to-string pm-min)))
    (setq pm-min-str (number-to-string pm-min))
    )

  ;; Define pm hour
  (setq pm-str (concat pm-hour-str ":" pm-min-str))

  ;; Calculate next hour as str
  (if (< am-hour 10)
      (setq am-hour-str (concat "0" (number-to-string am-hour)))
    (setq am-hour-str (number-to-string am-hour))
    )

  ;; Calculate next min asl str
  (if (< am-min 10)
      (setq am-min-str (concat "0" (number-to-string am-min)))
    (setq am-min-str (number-to-string am-min))
    )

  ;; Define am hour
  (setq am-str (concat am-hour-str ":" am-min-str))

  ;; If is night use timer in seconds
  (if (eval is-night)
      (lambda ()
        (setq am-str-int (+ (* (- 23 init-hour-int) 3600)
                            (* (- 59 init-min-int) 60)
                            (- 59 init-sec-int)))
        (setq am-str (concat (number-to-string am-str-int) " sec")))
    )

;;;;; Program
;; Cancel timer if exist
(if (eval is-day)
    (if (boundp 'am-timer)
        (cancel-timer am-timer)
      )
  (if (boundp 'pm-timer)
      (cancel-timer pm-timer)
    )
  )

;; Reset timer
(if (eval is-day)
    (setq pm-timer (run-at-time pm-str nil #'adaptative-theme 'gruvbox-light-soft 'gruvbox-dark-hard (eval am-hour-init) (eval pm-hour-init) (eval am-min-init) (eval pm-min-init) (eval am-sec-init) (eval pm-sec-init))))

(if (eval is-morning)
    (setq am-timer (run-at-time am-str nil #'adaptative-theme 'gruvbox-light-soft 'gruvbox-dark-hard (eval am-hour-init) (eval pm-hour-init) (eval am-min-init) (eval pm-min-init) (eval am-sec-init) (eval pm-sec-init))))

;;(if (eval is-night)
;;    (setq am-timer (run-at-time am-str nil #'adaptative-theme 'gruvbox-light-soft 'gruvbox-dark-hard (eval am-hour-init) (eval pm-hour-init) (eval am-min-init) (eval pm-min-init) (eval am-sec-init) (eval pm-sec-init))))

    )

;;; Adaptative theme location
(defun adaptative-theme-location (ligth-theme dark-theme &optional country city)
  "  Adaptative theme location function:
@Brief:   This function allow to configure different themes depending on your
          location when work emacs.

@Author:  acsm

@Version: A/1

@Args:    light-theme: Theme loaded in sun hours.
          dark-theme:  Theme loaded in dark hours.
          &country:    Custom Country location (str) (optional, default spain)
          &city:       Custom City or capital location (str) (optional, default madrid)

@Links:   https://www.timeanddate.com/sun where look for your country and city names.
"

;;;; Detect internet connection
  ;; Change "www.google.es" with your proxy server if you need it. "my_proxy.es"
  (if (null (boundp 'host))
      (setq internet-external-host "www.google.com"))
  (setq is-internet-up (call-process "ping" nil nil nil "-c" "1" "-w" "1" internet-external-host))

  (if (/= is-internet-up 0)
      ;; If internet is not connected
      (lambda ()
        (adaptative-theme(dark-theme light-theme))
        (progn (message "No network detected") nil)
        (return))
    )

;;;; Load basic requieres
(require 'org-web-tools)

;;;; Web scraping
;; URL base to get am and pm data
(setq url "https://www.timeanddate.com/sun")
;; Set default county
(unless (eval country)
  (setq country "spain"))

;; Set default city
(unless (eval city)
  (setq city "madrid"))

;; Compose url
(setq web_to_scrap (concat url "/" country "/" city))
(setq webDataHtml (org-web-tools--get-url web_to_scrap))
;; web string to search
;; <div class=\"h1 dn-mob\">Daylight</div><p class=dn-mob>7:18 &#8211; 21:06<br>13 hours, 48 minutes</p></div>
;; first regex model
;;>Daylight<\/div><p class=dn-mob>[0-9]{1,2}:[0-9]{1,2} &#8211; [0-9]{1,2}:[0-9]{1,2}<br>[0-9]{1,2} hours, [0-9]{1,2} minutes<\/p><\/div>
;; second regex model
;;\WDaylight\W{1,}div\W{1,}p\sclass\Wdn\Wmob\W[0-9]{1,2}\W[0-9]{1,2} \W{1,}[0-9]{1,}\W [0-9]{1,2}\W[0-9]{1,2}\Wbr\W[0-9]{1,2}\shours\W\s[0-9]{1,2}\sminutes\W{1,}p\W{1,}div\W
;; Helm regex model
;;\\s_Daylight\\s_\\{2\\}div\\s_\\{2\\}p\\s-class\\s_dn\\s_mob\\s_[0-9]\\{1,2\\}:[0-9]\\{1,2\\}\\s-\\s_\\{1,\\}
;;;; Web regex model
(setq webRegexModel "\\s_Daylight\\s_\\{2\\}div\\s_\\{2\\}p\\s-class\\s_dn\\s_mob\\s_[0-9]\\{1,2\\}:[0-9]\\{1,2\\}\\s-\\s_\\{1,\\}")

;;;; Extract regex value
(setq daylight-regex
      (string-match webRegexModel webDataHtml))

;;;; Extract substring
(setq subWebStr
      (substring webDataHtml daylight-regex (+ daylight-regex 100)))

;;;; Create regex group time
(setq timeGroupRegex "[0-9]\\{1,2\\}:[0-9]\\{1,2\\}")

;;;; Extract time value as string
(setq timeStr (string-match timeGroupRegex subWebStr))

;;;; Extract AM hour
(setq amTime (substring subWebStr timeStr (+ timeStr 5)))

;;;; Get substring pm time
(setq timeStr (string-match timeGroupRegex subWebStr (+ timeStr 6)))

;;;; Extract PM time
(setq pmTime (substring subWebStr timeStr (+ timeStr 5)))

;;;; Regenerate time regext to get hour and minutes
(setq timeGroupRegex ":")

;;;; Get AM Hour
(setq amSeparator (string-match timeGroupRegex amTime))
(setq amHourStr (substring amTime 0 amSeparator))
(setq amHourInt (string-to-number amHourStr))

;;;; Get AM Min
(setq amSeparator (string-match timeGroupRegex amTime))
(setq amMinStr (substring amTime (+ amSeparator 1) (+ amSeparator 3)))
(setq amMinInt (string-to-number amMinStr))

;;;; Get PM Hour
(setq pmSeparator (string-match timeGroupRegex pmTime))
(setq pmHourStr (substring pmTime 0 pmSeparator))
(setq pmHourInt (string-to-number pmHourStr))

;;;; Get PM Min
(setq pmSeparator (string-match timeGroupRegex pmTime))
(setq pmMinStr (substring pmTime (+ pmSeparator 1) (+ pmSeparator 3)))
(setq pmMinInt (string-to-number pmMinStr))

;;;; Execute adaptative theme function
(adaptative-theme ligth-theme dark-theme amHourInt pmHourInt amMinInt pmMinInt))

;;; Auto location adaptative theme
(defun adaptative-theme-autolocation (light-theme dark-theme)
  "  Adaptative theme auto-location function:
     @Brief:   This function allow to configure different themes depending on your
               location when work emacs, it get your location from internet.

     @Author:  acsm

     @Version: A/1

     @Args:    light-theme: Theme loaded in sun hours.
               dark-theme:  Theme loaded in dark hours.

     @Links:   https://www.timeanddate.com where look for your country and city names.
     "

;;;; Detect internet connection
  ;; Change "www.google.es" with your proxy server if you need it. "my_proxy.es"
  (if (null (boundp 'host))
      (setq internet-external-host "www.google.com"))
  (setq is-internet-up (call-process "ping" nil nil nil "-c" "1" "-w" "1" internet-external-host))

  (if (/= is-internet-up 0)
      ;; If internet is not connected
      (lambda ()
        (adaptative-theme(dark-theme light-theme))
        (progn (message "No network detected") nil)
        (return))
    )

;;;; Load basic requieres
  (require 'org-web-tools)

     ;;;; Web scraping
  ;; URL base to get am and pm data
  (setq url-location "https://www.timeanddate.com")

  ;; Get main web where display your location
  (setq webDataHtml (org-web-tools--get-url url-location))
  ;; web string to search
  ;;title=\"The World Clock / Time Zones\">Current Time</a></h2><a href=\"/worldclock/spain/madrid\" id=clk_box
  ;; Fist regex model
  ;;title=\W{2}The World Clock \W Time Zones\W{2}>Current Time<\Wa><\Wh2><a href=\W{3}worldclock\Wspain\Wmadrid\W{2} id=clk_box
  ;; Helm regex with regex-builder
  ;;"the world clock"

;;;; Extract regex value
  (setq location-regex
        (string-match "The World Clock" webDataHtml))
     ;;;; Extract substring
  (setq subLocStr
        (substring webDataHtml location-regex (+ location-regex 120)))

;;;; Extract regex value
  (setq init-regex
        (string-match "worldclock/" subLocStr))

  (setq end-regex
        (string-match "id=" subLocStr))

  ;;;; Extract substring
  (setq subLocStr
        (substring subLocStr (+ init-regex 11) (+ end-regex 3)))

  ;;;; Extract country
  (setq end-regex
        (string-match "/" subLocStr))
  (setq myCountryLoc
        (substring subLocStr 0 end-regex))

  ;;;; Extract city
  (setq init-regex
        (string-match "/" subLocStr))

  (setq end-regex
        (string-match "id=" subLocStr))

  (setq myCityLoc
        (substring subLocStr (+ init-regex 1) (- end-regex 2)))

;; Execute location function with data extracted.
(adaptative-theme-location light-theme dark-theme myCountryLoc myCityLoc)
)
