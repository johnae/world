(var main-ratio 0.65)
(var gaps 10)
(var smart-gaps false)
(var layout :main-side-stack)
(var tag-layouts {})

; The argument is a table with:
;  * Focused tags (`args.tags`)
;  * Window count (`args.count`)
;  * Output width (`args.width`)
;  * Output height (`args.height`)
;  * Output name (`args.output`)
;
; The return value must be a table with exactly `count` entries. Each entry is a table with four
; numbers:
;  * X coordinate
;  * Y coordinate
;  * Window width
;  * Window height

(lambda main-side-stack [args]
  (var retval [])
  (case args.count
    1 (if smart-gaps (table.insert retval [0 0 args.width args.height])
          (table.insert retval
                        [gaps
                         gaps
                         (- args.width (* gaps 2))
                         (- args.height (* gaps 2))]))
    2 (let [main-w (* (- args.width (* gaps 3)) main-ratio)
            side-w (- (- args.width (* gaps 3)) main-w)
            main-h (- args.height (* gaps 2))]
        (table.insert retval [gaps gaps main-w main-h])
        (table.insert retval [(+ main-w (* gaps 2)) gaps main-w main-h]))
    _ (let [main-w (* (- args.width (* gaps 3)) main-ratio)
            side-w (- (- args.width (* gaps 3)) main-w)
            main-h (- args.height (* gaps 2))]
        (table.insert retval [gaps gaps main-w main-h])
        (table.insert retval
                      [(+ main-w (* gaps 2)) gaps side-w (* main-h main-ratio)])
        (for [i 0 (- args.count 3)]
          (let [x (+ (+ main-w (* gaps 2)) (* i gaps))
                y (+ (* main-h main-ratio) (* gaps 2) (* i gaps))
                width (- side-w (* i gaps) (* (- args.count 3 i) gaps))
                height (- main-h (* main-h main-ratio) gaps (* i gaps))]
            (table.insert retval [x y width height])))))
  retval)

(lambda main-bottom-stack [args]
  (var retval [])
  (case args.count
    1 (if smart-gaps (table.insert retval [0 0 args.width args.height])
          (table.insert retval
                        [gaps
                         gaps
                         (- args.width (* gaps 2))
                         (- args.height (* gaps 2))]))
    2 (let [main-h (* (- args.height (* gaps 3)) main-ratio)
            side-h (- (- args.height (* gaps 3)) main-h)
            main-w (- args.width (* gaps 2))]
        (table.insert retval [gaps gaps main-w main-h])
        (table.insert retval [gaps (+ main-h (* gaps 2)) main-w side-h]))
    _ (let [main-h (* (- args.height (* gaps 3)) main-ratio)
            side-h (- (- args.height (* gaps 3)) main-h)
            main-w (- args.width (* gaps 2))]
        (table.insert retval [gaps gaps main-w main-h])
        (table.insert retval [gaps
                              (+ main-h (* gaps 2))
                              (* main-w main-ratio)
                              side-h])
        (for [i 0 (- args.count 3)]
          (let [y (+ (+ main-h (* gaps 2)) (* i gaps))
                x (+ (* main-w main-ratio) (* gaps 2) (* i gaps))
                height (- side-h (* i gaps) (* (- args.count 3 i) gaps))
                width (- main-w (* main-w main-ratio) gaps (* i gaps))]
            (table.insert retval [x y width height])))))
  retval)

(lambda monocle [args]
  (var retval [])
  (for [i 0 (- args.count 1)]
    (table.insert retval [0 0 args.width args.height]))
  retval)

(set _G.monocle monocle)

; This layout function emulates the default rivertile layout
(lambda main-stack [args]
  (var retval [])
  (if (= args.count 1)
      (if smart-gaps (table.insert retval [0 0 args.width args.height])
          (table.insert retval
                        [gaps
                         gaps
                         (- args.width (* gaps 2))
                         (- args.height (* gaps 2))]))
      (let [main-w (* (- args.width (* gaps 3)) main-ratio)
            side-w (- args.width (* gaps 3) main-w)
            main-h (- args.height (* gaps 2))
            side-h (- (/ (- args.height gaps) (- args.count 1)) gaps)]
        (table.insert retval [gaps gaps main-w main-h])
        (for [i 0 (- args.count 2)]
          (table.insert retval [(+ main-w (* gaps 2))
                                (+ gaps (* i (+ side-h gaps)))
                                side-w
                                side-h]))))
  retval)

(lambda handle-layout [args]
  (let [resolved-layout (or (. tag-layouts args.tags) layout)]
    (case resolved-layout
      :main-side-stack (main-side-stack args)
      :main-bottom-stack (main-bottom-stack args)
      :monocle (monocle args)
      _ (main-stack args))))

(set _G.handle_layout handle-layout)

(lambda handle-metadata [args]
  {:name layout})

(set _G.handle_metadata handle-metadata)

(var gaps-alt 0)
(lambda toggle-gaps []
  (let [tmp gaps]
    (set gaps gaps-alt)
    (set gaps-alt tmp)))

(set _G.toggle_gaps toggle-gaps)

(lambda set-main-ratio [ratio]
  (set main-ratio ratio))

(set _G.set_main_ratio set-main-ratio)

(lambda adjust-main-ratio-by [val]
  (set main-ratio (+ main-ratio val)))

(set _G.adjust_main_ratio_by adjust-main-ratio-by)

(lambda set-gaps [gap]
  (set gaps gap))

(set _G.set_gaps set-gaps)

(lambda set-smart-gaps [smart]
  (set smart-gaps smart))

(set _G.set_smart_gaps set-smart-gaps)

(lambda set-layout [name]
  (set layout name))

(set _G.set_layout set-layout)

(lambda set-tag-layout [name]
  (set (. tag-layouts _G.CMD_TAGS) name))

(set _G.set_tag_layout set-tag-layout)

(lambda next-layout []
  (case layout
    :main-side-stack (set-layout :monocle)
    :monocle (set-layout :main-stack)
    :main-stack (set-layout :main-bottom-stack)
    _ (set-layout :main-side-stack)))

(set _G.next_layout next-layout)

(lambda next-tag-layout []
  (let [resolved-layout (or (. tag-layouts _G.CMD_TAGS) layout)]
    (case resolved-layout
      :main-side-stack (set-tag-layout :monocle)
      :monocle (set-tag-layout :main-stack)
      :main-stack (set-tag-layout :main-bottom-stack)
      _ (set-tag-layout :main-side-stack))))

(set _G.next_tag_layout next-tag-layout)

