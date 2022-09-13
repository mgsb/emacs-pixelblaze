(require 'ert)
(require 'pixelblaze)
(require 'websocket)

;; these will need to be updated for different environments/boards
(defconst ip-address "192.168.10.113")
(defconst num-patterns 37)
(defconst dev-name "pixelblaze_mark")

(defconst pattern-name "Line Dancer 2D")
(defconst pattern-id "yK2F8n4qFnsBDsPGD")

(defmacro with-pixelblaze (&rest forms)
  `(let ((pb (pixelblaze-open ip-address)))
     (progn ,@forms)
     (pixelblaze-close pb)))

(ert-deftest pixelblaze-test-open ()
  (should (let* ((pb (pixelblaze-open ip-address))
                 (result (websocket-openp pb)))
            (pixelblaze-close pb)
            result)))
;; TODO: does not work
;; (should (not (pixelblaze-open "1.2.3.4"))))

(ert-deftest pixelblaze-test-close ()
  (should (not (pixelblaze-close (pixelblaze-open ip-address)))))

(ert-deftest pixelblaze-test-get-config ()
  (with-pixelblaze
   (should (string= dev-name (gethash "name" (pixelblaze-get-config pb))))))

(ert-deftest pixelblaze-test-set-brightness ()
  (with-pixelblaze
   (should (not (pixelblaze-set-brightness pb 0.125)))
   (should (= 0.125 (gethash "brightness" (pixelblaze-get-config pb))))))

(ert-deftest pixelblaze-test-set-pattern-id ()
  (with-pixelblaze
   (should (string= pattern-id (gethash "activeProgramId"
                                        (pixelblaze-set-pattern-id pb pattern-id))))))

(ert-deftest pixelblaze-test-get-patterns ()
  (with-pixelblaze (should (equal (hash-table-count
                                   (pixelblaze-get-patterns pb))
                                  num-patterns))))

(ert-deftest pixelblaze-test-get-set-vars ()
  (with-pixelblaze
   (let* ((controls (pixelblaze-set-pattern pb pattern-name)) (speed 3)
          (vars (pixelblaze-get-vars pb)))

     (should (>= (gethash "speed" vars) 0.0))
     (puthash "speed" speed vars)
     (should (not (pixelblaze-set-vars pb vars)))
     (should (= (gethash "speed" (pixelblaze-get-vars pb)) speed)))))

(defun hash-equal (hash1 hash2)
  "Compare two hash tables to see whether they are equal."
  (and (= (hash-table-count hash1)
          (hash-table-count hash2))
       (catch 'flag (maphash (lambda (x y)
                               (or (equal (gethash x hash2) y)
                                   (throw 'flag nil)))
                             hash1)
              (throw 'flag t))))

(ert-deftest pixelblaze-test-get-set-controls ()
  (with-pixelblaze
   (let* ((def-controls (gethash "controls" (pixelblaze-set-pattern pb pattern-name)))
          (got-controls (pixelblaze-get-controls pb pattern-id)) (speed 5))

     (should (hash-equal def-controls got-controls))
     (message "%s" def-controls)
     (puthash "sliderSpeed" speed got-controls)
     (message "%s" (gethash "sliderSpeed" got-controls))
     (should (not (pixelblaze-set-controls pb got-controls)))
     (should (not (pixelblaze-set-controls pb got-controls t))))))
;; NOTE: from forum discussion, it appears "set" controls cannot be read back
;; (should (= (gethash "sliderSpeed" (pixelblaze-get-controls pb pattern-id)) speed)))))

(ert-deftest pixelblaze-test-set-brightness ()
  (with-pixelblaze
   (should (equal (pixelblaze-set-brightness pb 0.5) nil))))
