;;; Copyright 2024 Indigo BioAutomation, Inc.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

#!chezscheme
(import
 (chezscheme)
 (swish imports)
 (swish mat)
 )

(mat path-absolute ()
  (assert (string=? (current-directory) (path-absolute ".")))
  (assert (string=? (current-directory) (path-absolute "." ".")))
  (if windows?
      (match-let*
       (["C:\\Temp" (path-absolute "C:\\Temp\\")]
        ["\\Temp" (path-absolute "\\Temp\\")]
        ["\\\\Server\\Temp" (path-absolute "\\\\Server\\Temp\\")]
        ["C:\\Program Files\\Swish\\Mats" (path-absolute "Mats" "C:\\Program Files\\Swish")]
        ["C:\\Program Files\\Swish" (path-absolute "." "C:\\Program Files\\Swish")]
        ["C:\\Program Files\\Emacs" (path-absolute "..\\Emacs" "C:\\Program Files\\Swish")]
        )
       'ok)
      (match-let*
       (["/tmp" (path-absolute "/tmp/")]
        ["/opt/swish/mats" (path-absolute "mats" "/opt/swish")]
        ["/opt/swish" (path-absolute "." "/opt/swish")]
        ["/opt/emacs" (path-absolute "../emacs" "/opt/swish")]
        )
       'ok)))

(mat path-combine ()
  (if windows?
      (match-let*
       (["x" (path-combine "x")]
        ["x" (path-combine "" "x")]
        ["x\\y" (path-combine "x\\" "y")]
        ["x/y" (path-combine "x/" "y")]
        ["x\\y" (path-combine "x" "y")]
        ["x\\y\\" (path-combine "x" "y\\")]
        ["x\\y/" (path-combine "x" "y/")]
        ["x\\y\\z" (path-combine "x" "y" "z")]
        ["w\\x\\y\\z" (path-combine "w" "x" "y" "z")]
        )
       'ok)
      (match-let*
       (["x" (path-combine "x")]
        ["x" (path-combine "" "x")]
        ["x/y" (path-combine "x/" "y")]
        ["x\\/y" (path-combine "x\\" "y")]
        ["x/y" (path-combine "x" "y")]
        ["x/y\\" (path-combine "x" "y\\")]
        ["x/y/" (path-combine "x" "y/")]
        ["x/y/z" (path-combine "x" "y" "z")]
        ["w/x/y/z" (path-combine "w" "x" "y" "z")]
        )
       'ok)))

(mat path-normalize ()
  (if windows?
      (match-let*
       (["." (path-normalize "")]
        ["." (path-normalize ".")]
        ["." (path-normalize ".\\")]
        [".." (path-normalize "..")]
        [".." (path-normalize "..\\")]
        [".." (path-normalize ".\\..")]
        [".." (path-normalize "..\\.")]
        [".." (path-normalize "..\\.\\.\\")]
        ["..\\c" (path-normalize "a\\..\\..\\b\\..\\c\\")]
        ["..\\b\\c" (path-normalize "a\\..\\..\\b\\c\\")]
        ["C:\\Users" (path-normalize "C:\\..\\..\\Users\\")]
        ["\\Users" (path-normalize "\\..\\..\\Users\\")]
        ["\\\\Server\\Users" (path-normalize "\\\\Server\\..\\..\\Users\\")]
        ["C:\\Data" (path-normalize "C:\\Program Files\\Swish\\..\\..\\Data")]
        )
       'ok)
      (match-let*
       (["." (path-normalize "")]
        ["." (path-normalize ".")]
        ["." (path-normalize "./")]
        [".." (path-normalize "..")]
        [".." (path-normalize "../")]
        [".." (path-normalize "./..")]
        [".." (path-normalize "../.")]
        [".." (path-normalize "../././")]
        ["../c" (path-normalize "a/../../b/../c/")]
        ["../b/c" (path-normalize "a/../../b/c/")]
        ["/Users" (path-normalize "/../../Users/")]
        ["/tmp" (path-normalize "/opt/swish/../../tmp")]
        )
       'ok)))
