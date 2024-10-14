; G30 dashboard compatibility lisp script v1.0 by AKA13 and 1zuna and edited by sunni --- BETA
; UART Wiring: red=5V black=GND yellow=COM-TX (UART-HDX) green=COM-RX (button)+3.3V with 1K Resistor
; Guide (German): https://rollerplausch.com/threads/vesc-controller-einbau-1s-pro2-g30.6032/
; Tested on VESC 6.05 on G30D w/ MKS 84100HP and MP2 300A VESC

; -> User parameters (change these to your needs)

(def software-adc 1) ;set to 1 to use ble throttle control set to 0 for direct wired throttle to vesc
(def min-adc-thr 0.1) ; throttle min value (adjusts min deadzone)
(def min-adc-brake 0.1) ;brake min value (same here)
(def brake-threshold 0.54) ; Brake threshold value for when brakes override throttle (after this voltage the brakes will override gas) may need adjusting when throttle doesn't work)

; button timing adjustment 
(def button-debounce-time 0.03)
(def single-press-delay 0.2)
(def long-press-time 6000)
(def double-press-time 2500)

; dash setup
(def startup-mode 1) ; 1 = drive, 2 = eco, 4 = sport
(def bShowMPH 1) ; Set to 1 for mph, 0 for km/h
(def conv (if (= bShowMPH 1) 2.237 3.6)) ; (don't touch!!) meters per sec to kmh 3.6 or mph 2.23
(def show-batt-in-idle 1) ; shows batter while idling, set to 0 for off, 1 for on

; safety speed
(def min-speed -1) ; set minimum speed to start motor
(def button-safety-speed (/ 20 conv)) ; if you want to disable the button above a certain speed. 

; Takeoff boost
(def boost 1.2) ; set boost factor for under speed of 10 20% increases wattage 

; basic battery level detection (most batteys are not linear. set it to your liking.) 
(def max-voltage 42.0) ; max battery voltage
(def min-voltage 30.0) ; min battery voltage

;shut down timer
(def tout 7) ;set to however many minutes you want till it automatically turns off (basic timmer starts only when speed is 0)

; Speed modes (set speed limit kmh/mph,  watts, current scale)
(def eco-speed (/ 10 conv)) ;defines speed limit
(def eco-current 0.6) ;defines eco current scale
(def eco-watts 400) ;defines eco watt limit
(def eco-fw 0) ; defines eco feild weakening amps

(def drive-speed (/ 20 conv)) ;speed limit
(def drive-current 0.7) ;current scale
(def drive-watts 700) ;watt
(def drive-fw 0) ;feild weakening

(def sport-speed (/ 30 conv)) ;SL
(def sport-current 1.0) ;CS
(def sport-watts 900) ;watt
(def sport-fw 25) ;fW

; -> Initialization (code begins here (touch if u dare)

; Load VESC CAN code server
(import "pkg@://vesc_packages/lib_code_server/code_server.vescpkg" 'code-server)
(read-eval-program code-server)

; Packet handling
(uart-start 115200 'half-duplex)
(gpio-configure 'pin-rx 'pin-mode-in-pu)

(define tx-frame (array-create 15))
(bufset-u16 tx-frame 0 0x5AA5) ;Ninebot protocol
(bufset-u8 tx-frame 2 0x06) ;Payload length is 5 bytes
(bufset-u16 tx-frame 3 0x2021) ; Packet is from ESC to BLE
(bufset-u16 tx-frame 5 0x6400) ; Packet is from ESC to BLE
(def uart-buf (array-create 64))


; Button handling
(def presstime (systime))
(def presses 0)

; Mode states
(def off 0)
(def lock 0)
(def speedmode 4)
(def light 0)
(def unlock 0)

; Sound feedback
(def feedback 0)
(def beep-time 1)

(if (= software-adc 1)
    (app-adc-detach 3 1)
    (app-adc-detach 3 0)
)

(defun beep(time count)
    {
        (set 'beep-time time)
        (set 'feedback count)
    }
)

(define secs-left 0)
(define last-action-time (systime))


; timeout
(loopwhile-thd 100 t {
       
        (if (> secs-left (* tout 60));timeout
            (if (= off 0)
                (shut-down-ble)
            )
        )
        ;(print "Timer: " secs-left)
        (sleep 0.05)
})


;adc code

(defun adc-input(buffer) ; Frame 0x65
    {
        (let ((current-speed (* (get-speed) conv))
            (throttle (/(bufget-u8 uart-buf 5) 77.2)) ; 255/3.3 = 77.2
            (brake (/(bufget-u8 uart-buf 6) 77.2)))
            {
               
                (if (< throttle 0)
                    (setf throttle 0))
                (if (> throttle 3.3)
                    (setf throttle 3.3))
                (if (< brake 0)
                    (setf brake 0))
                (if (> brake 3.3)
                    (setf brake 3.3))
                
                ; Pass through throttle and brake to VESC
               
                (app-adc-override 1 brake)
                
                ; Only pass through throttle if brake is not applied
                 ; Set throttle to 0 if brake is above threshold
                (if (>= brake brake-threshold)
                    (app-adc-override 0 0)
                    (app-adc-override 0 throttle))
                    
                    ;Timeout timmer
                    
                    (if (= off 0)
            (if (> current-speed 1)
                (setvar 'last-action-time (systime))
            
        
               (if (= off 0)
            (setvar 'secs-left (secs-since last-action-time)))
            
                 )
              )      
           }
        )
    }
)

   
   

(defun handle-features() ;on off code
    {
        (if (or (or (= off 1) (= lock 1) (< (* (get-speed) conv) min-speed)))
            (if (not (app-is-output-disabled)) ; Disable output when scooter is turned off
                {
                    (app-adc-override 0 0)
                    (app-adc-override 1 0)
                    (app-disable-output -1)
                    (set-current 0)
                    ;(loopforeach i (can-list-devs)
                    ;    (canset-current i 0)
                    ;)
                }
                
            )
            (if (app-is-output-disabled) ; Enable output when the scooter is turned on
                (app-disable-output 0)
            )
        )
        
        (if (= lock 1)
            {
                (set-current-rel 0) ; No current input when locked
                (if (> (* (get-speed) conv) min-speed)
                    (set-brake-rel 1) ; Full power brake
                    (set-brake-rel 0) ; No brake
                )
            }
        )
    }
)

(defun update-dash(buffer) ; Frame 0x64 dash code section
    {
        (var current-speed (* (l-speed) conv))
        (var battery (get-battery-level))

        ; mode field (1=drive, 2=eco, 4=sport, 8=charge, 16=off, 32=lock)
        (if (= off 1)
            (bufset-u8 tx-frame 7 16)
            (if (= lock 1)
                (bufset-u8 tx-frame 7 32) ; lock display
                (if (or (> (get-temp-fet) 60) (> (get-temp-mot) 60)) ; temp icon will show up above 60 degree
                    (bufset-u8 tx-frame 7 (+ 128 speedmode))
                    (bufset-u8 tx-frame 7 speedmode)
                )            
            )
        )
                
        ; batt field
        (bufset-u8 tx-frame 8 battery)

        ; light field 
        (if (= off 0)
            (bufset-u8 tx-frame 9 light)
            (bufset-u8 tx-frame 9 0)
        )   

        ; beep field
        (if (= lock 1)
            (if (> current-speed min-speed)
                (bufset-u8 tx-frame 10 1) ; beep lock
                (bufset-u8 tx-frame 10 0))
            (if (> feedback 0)
                {
                    (bufset-u8 tx-frame 10 1)
                    (set 'feedback (- feedback 1))
                }
                (bufset-u8 tx-frame 10 0)
            )
        )
        

        ; speed field
        (if (= (+ show-batt-in-idle unlock) 1)
            (if (> current-speed 1)
            (bufset-u8 tx-frame 11 current-speed)
                (bufset-u8 tx-frame 11 battery))
            (bufset-u8 tx-frame 11 current-speed)
        )

        ;mph/kmh feild
        ; Set bFlags field to 64 when bShowMPH is 1
        (if (= bShowMPH 1)
        (bufset-u8 tx-frame 7 (bitwise-or (bufget-u8 tx-frame 7) 64)) ; mph
        (bufset-u8 tx-frame 7 (bufget-u8 tx-frame 7)) ; km/h
        )
                
        ; error field
        (bufset-u8 tx-frame 12 (get-fault))

        ; calc crc
        (var crcout 0)
        (looprange i 2 13
        (set 'crcout (+ crcout (bufget-u8 tx-frame i))))
        (set 'crcout (bitwise-xor crcout 0xFFFF))
        (bufset-u8 tx-frame 13 crcout)
        (bufset-u8 tx-frame 14 (shr crcout 8))

        ; write
        (uart-write tx-frame)
    }
)

(defun read-frames()
    (loopwhile t
        {
            (uart-read-bytes uart-buf 3 0)
            (if (= (bufget-u16 uart-buf 0) 0x5aa5)
                {
                    (var len (bufget-u8 uart-buf 2))
                    (var crc len)
                    (if (and (> len 0) (< len 60)) ; max 64 bytes
                        {
                            (uart-read-bytes uart-buf (+ len 6) 0) ;read remaining 6 bytes + payload, overwrite buffer

                            (let ((code (bufget-u8 uart-buf 2)) (checksum (bufget-u16 uart-buf (+ len 4))))
                                {
                                    (looprange i 0 (+ len 4) (set 'crc (+ crc (bufget-u8 uart-buf i))))

                                    (if (= checksum (bitwise-and (+ (shr (bitwise-xor crc 0xFFFF) 8) (shl (bitwise-xor crc 0xFFFF) 8)) 65535)) ;If the calculated checksum matches with sent checksum, forward comman
                                        (handle-frame code)
                                    )
                                }
                            )
                        }
                    )
                }
            )
        }
    )
)

(defun handle-frame(code)
    {
        (if (and (= code 0x65) (= software-adc 1))
            (adc-input uart-buf)
        )
        
        (if(= code 0x64)
            (update-dash uart-buf)
        )
    }
)

(defun turn-on-ble()
    {
        (set 'speedmode startup-mode)
        (apply-mode) ; Apply mode on start-up
        (set 'last-action-time (systime))
        (set 'feedback 1) ; beep feedback
        (set 'unlock 0) ; Disable unlock on turn off
        (set 'off 0) ; turn on
       
    }
)

(defun handle-button() 
  (if (= presses 1) ; single press
    (if (= off 1) ; is it off? turn on the scooter again
            {
                (turn-on-ble)
            }
        (progn
            (set 'light (bitwise-xor light 1)) ; toggle light
            (sleep 0.1) ;add a small delay
            (set 'presses 0) ; reset presses
        )
    )
        (if (>= presses 2) ; double press
            {
                (if (> (get-adc-decoded 1) min-adc-brake) ; if brake is pressed
                    (if (and (> (get-adc-decoded 0) min-adc-thr))
                        {
                            (set 'feedback 2) ; beep 2x
                            (apply-mode)
                        }
                        {
                            (set 'unlock 0)
                            (apply-mode)
                            (set 'lock (bitwise-xor lock 1)) ; lock on or off
                            (set 'feedback 1) ; beep feedback
                        }
                    )
                    {
                        (if (= lock 0)
                            {
                                (cond
                                    ((= speedmode 1) (set 'speedmode 4))
                                    ((= speedmode 2) (set 'speedmode 1))
                                    ((= speedmode 4) (set 'speedmode 2))
                                )
                                (apply-mode)
                            }
                        )
                    }
                )
            }
        )
    )
)

(defun shut-down-ble()
    {
        (if (= (+ lock off) 0) ; it is locked and off?
            {
                
                (set 'unlock 0) ; Disable unlock on turn off
                (apply-mode)
                (set 'light 0) ; turn off light
                (set 'feedback 1) ; beep feedback
                (set 'secs-left 0)
                (set 'off 1) ; turn off
            }
        )
    }
)

; (defun handle-holding-button()
 

(defun reset-button()
    {
        (set 'presstime (systime)) ; reset press time again
        (set 'presses 0)
    }
)

; Speed mode implementation

(defun apply-mode()
    (let ((speed (cond
                    ((= speedmode 1) drive-speed)
                    ((= speedmode 2) eco-speed)
                    ((= speedmode 4) sport-speed)))
          (watts (cond
                    ((= speedmode 1) drive-watts)
                    ((= speedmode 2) eco-watts)
                    ((= speedmode 4) sport-watts)))
          (current (cond
                    ((= speedmode 1) drive-current)
                    ((= speedmode 2) eco-current)
                    ((= speedmode 4) sport-current)))
          (fw (cond
                ((= speedmode 1) drive-fw)
                ((= speedmode 2) eco-fw)
                ((= speedmode 4) sport-fw))))
        (configure-speed speed watts current fw)))

(defun configure-speed(speed watts current fw) ;configures power
    {
        (if (< (* (get-speed) conv) 10)
            (set-param 'l-watt-max (* watts boost)) ; Boost wattage by 20% if speed is under 5
            (set-param 'l-watt-max watts) ; Revert to original wattage if speed is 5 or above
        )
        (set-param 'max-speed speed)
        (set-param 'l-current-max-scale current)
        (set-param 'foc-fw-current-max fw)
    }
)

(defun set-param (param value)
    {
        (conf-set param value)
        (loopforeach id (can-list-devs)
            (looprange i 0 5 {
                (if (eq (rcode-run id 0.1 `(conf-set (quote ,param) ,value)) t) (break t))
                false
            })
        )
    }
)

(defun l-speed() ;gets speed
    {
        (var l-speed (get-speed))
        (loopforeach i (can-list-devs)
            {
                (var l-can-speed (canget-speed i))
                (if (< l-can-speed l-speed)
                    (setf l-speed l-can-speed)
                )
            }
        )

        l-speed
    }
)

(defun button-logic() ;revamped button pressing stuff
    {
        ; Assume the button is not pressed by default
        (var buttonold 0)
        (var last_click_time 0)
        (loopwhile t
            {
                (var button (gpio-read 'pin-rx))
                (sleep 0.03) ; wait 50 ms to debounce
                (var buttonconfirm (gpio-read 'pin-rx))
                (if (not (= button buttonconfirm))
                    (set 'button 0)
                )

                (if (> buttonold button)
                    {
                        (set 'presses (+ presses 1))
                        (set 'presstime (systime))
                        (set 'last_click_time (systime))
                    }
                    (button-apply button)
                )

                (if (and (= presses 2) (< (- (systime) last_click_time) double-press-time))
                    {
                        (handle-double-press)
                        (set 'presses 0)
                    }
                )

                (set 'buttonold button)
                (handle-features)
            }
        )
    }
)

(defun button-apply(button)
    {
        ; Calculate the time passed since the last button press
        (var time-passed (- (systime) presstime))
        
        ; Check if the button is active (i.e., not off and speed is within safety limits)
        (var is-active (or (= off 1) (<= (get-speed) button-safety-speed)))

        ; Check if the button has been pressed for more than 2500 ms
        (if (> time-passed 2500) 
            ; Check if the button is still pressed
            (if (= button 0) 
                ; Check if the button has been pressed for more than 6000 ms (long press)
                (if (> time-passed 6000) 
                    {
                        ; Handle long press
                        (if is-active
                            (shut-down-ble)
                        )
                        (reset-button) ; reset button
                    }
                )
                ; Handle single press
                (if (> presses 0) 
                    {
                        ; Add a delay to single button press handling
                        (if is-active
                            (progn
                                (sleep 0.03) 
                                (handle-button) ; handle single-press
                            )
                        )
                        (reset-button) ; reset button
                    }
                )
            )
        )
    }
)
(defun handle-double-press()
  {
    (var is-active (or (= off 1) (<= (get-speed) button-safety-speed)))
    (if is-active
      {
        ; handle double press event
        (cond
          ((= speedmode 1) (set 'speedmode 4))
          ((= speedmode 2) (set 'speedmode 1))
          ((= speedmode 4) (set 'speedmode 2))
        )
        (apply-mode)
      }
    )
  }
)

(defun get-battery-level() ;battery level linear calc 
    {
        (var vin (get-vin))
        (var battery-level (* (/ (- vin min-voltage) (- max-voltage min-voltage)) 100))
        (if (< battery-level 0) (set 'battery-level 0))
        (if (> battery-level 100) (set 'battery-level 100))
        battery-level
    }
)



; Apply mode on start-up
(set 'speedmode startup-mode)
(apply-mode)



; Spawn UART reading frames thread
(spawn 150 read-frames)
(button-logic) ; Start button logic in the main thread - this will block the main thread
