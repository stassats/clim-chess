(in-package #:clim-chess)

#+sbcl
(defun run-program% (program arguments)
  (let ((process (sb-ext:run-program program arguments
                                     :wait nil :search t
                                     :output :stream
                                     :input :stream)))
    (values process
            (make-two-way-stream (sb-ext:process-output process)
                                 (sb-ext:process-input process)))))
#+ccl
(defun run-program% (program arguments)
  (let ((process (ccl:run-program program arguments
                                  :wait nil
                                  :output :stream
                                  :input :stream
                                  :sharing :lock)))
    (values process
            (make-two-way-stream
             (ccl:external-process-output-stream process)
             (ccl:external-process-input-stream process)))))

(defun run-program (program arguments)
  #+(or sbcl ccl) (run-program% program arguments)
  #-(or sbcl ccl) (error "Don't know how to run external processes."))
