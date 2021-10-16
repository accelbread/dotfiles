;;; emoji-zwj-sequences.el --- Display emoji ZWJ sequences -*- lexical-binding: t -*-
(provide 'emoji-zwj-sequences)

;; setting up composition functions for emoji modifiers
;; https://emacs.stackexchange.com/questions/62219/how-do-i-get-colour-emoji-to-display-in-emacs/62220#62220
(dolist (items `(((?🇦 . ?🇿) [".[🇦-🇿]+" 0 font-shape-gstring])
                 ((?🏳 . ?🏴) [".[️‍🌈⚧☠󠀠-󠁿]*" 0 font-shape-gstring])
                 (?⃣ ["[#*0-9]️⃣" 2 font-shape-gstring])
                 ,@(mapcar (lambda (range) (list range [".‍?[🏻-🏿]?[‍️♂♀]*️?" 0 font-shape-gstring]))
                           (cl-concatenate 'list "☝🎅🏇👂👃👦👧👼💏💑💪🕴🕵🕺🖐🖕🖖🙇🚣🛀🛌🤏🤞🤟🤦🤽🤾🥷🦻👯❤"
                                        '((?⛹ . ?✍) (?🏂 . ?🏄) (?🏊 . ?🏌) (?👆 . ?👐)
                                          (?👫 . ?👮) (?👰 . ?👸) (?💁 . ?💇) (?🙅 . ?🙇) (?🙋 . ?🙏)
                                          (?🚴 . ?🚶) (?🤘 . ?🤜) (?🤰 . ?🤹) (?🤼 . ?🤾) (?🦵 . ?🦹)
                                          (?🧍 . ?🧏) (?🧒 . ?🧟))) )
                 (?🧑 [".‍?[🏻-🏿]?[‍⚕⚖✈❤️🌾🍳🍼🎄🎓🎤🎨🏫🏭👦-👩💋💻💼🔧🔬🚀🚒🤝🦯🦰-🦳🦼🦽🧑]*" 0 font-shape-gstring])
                 ((?👨 . ?👩) [".‍?[🏻-🏿]?[‍⚕⚖✈❤️🌾🍳🍼🎄🎓🎤🎨🏫🏭👦-👩💋💻💼🔧🔬🚀🚒🤝🦯🦰-🦳🦼🦽🧑]*" 0 font-shape-gstring])
                 ,@(mapcar (lambda (str) (list (elt str 0) (vector str 0 'font-shape-gstring)))
                           '("😶‍🌫️" "🐈‍⬛" "🐕‍🦺" "🐻‍❄️" "👁️‍🗨️" "😮‍💨" "😵‍💫"))))
  (set-char-table-range
   composition-function-table
   (car items)
   (list (cadr items))))
