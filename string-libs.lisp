(defparameter *excuses* (list
                         "left on a journey of spritual enlightenment."
                         "was eaten by a grue."
                         "is canvassing for Joe Biden."
                         "has more important things to do."
                         "is sleeping at home."
                         "went traveling through space and time."
                         "hates statistics."
                         "despises school."
                         "is getting vaccinated against rabies."
                         "is grinding loot on MMOs."
                         "joined a cult."
                         "went out dating with their lover."
                         "is busy failing the driver's test again."
                         "has been subsumed by a malevolent AI."
                         "is taking up a hobby in arms trafficking."
                         "is having their nth birthday party this year."
                         "dropped out and became a farmer."
                         "has paralyzing back pain."
                         "was trapped in their bed."
                         "is rebooting their brain's internal firmware."
                         "is investing in bitcoin."
                         "was murdered by the baoan."
                         "wants their head teacher to suffer."
                         "is fighting to save a rare form of algae."
                         "commiting thoughtcrime."
                         "is living life like a dog."
                         "was isekai'd."
                         "is here, but invisible."
                         "went skinny dipping and is now in jail."
                         "is among the stars."
                         "died.  RIP."
                         "is too hungover for math."
                         "was failed by their own alarm clock."
                         "was kidnapped by eldritch beings from another dimension."
                         "has a pregnancy scare."
                         "went to enjoy the lovely weather in the park."
                         "is hiding in the library."
                         "is training to become a pokemon master."
                         "is locked in their own home."
                         "is trapped in a dream and can't wake up."
                         "had their 5th appendectomy this year."
                         "is interviewing with a college they won't get into."
                         "is fighting with ghosts."
                         "had a mechanical failure."
                         "forgot their way to school."
                         "thought the school was destroyed."))

(defun whitespace-char-p (c)
  "a test for whether or not a character is whitespace"
  (or (char= #\space c)
      (not (graphic-char-p c))))

(defun until-first-space (text)
  "creates a substring out of text until whitespace appears"
  (concatenate 'string (loop for char across text
        while (not (whitespace-char-p char))
        collect char)))

(defun until-next-word (text)
  "creates a substring out of text until whitespace appears"
  (concatenate 'string (loop for char across text
        while (not (whitespace-char-p char))
        collect char)))

(defun until-first-space (text)
  "creates a substring out of text until whitespace appears"
  (concatenate 'string (loop for char across text
        while (not (whitespace-char-p char))
        collect char)))

(defun split-by-one-space (string)
  "Returns a list of substrings of string divided by ONE space each. Note: Two consecutive spaces will be seen as if there were an empty string between them."
  (loop for i = 0 then (1+ j)
        as j = (position #\Space string :start i)
        collect (subseq string i j)
        while j))

(defun join-string-list (string-list)
  "Concatenates a list of strings and puts spaces between the elements."
  (format nil "~{~A~^ ~}" string-list))

(defmacro flip-name (name)
  `(join-string-list (nreverse (split-by-one-space ,name))))
