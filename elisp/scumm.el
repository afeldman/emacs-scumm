;;; scumm.el --- SCUMM-like adventure game engine (core loader) -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Anton Feldmann

;; Author: Anton Feldmann <your.email@example.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: games
;; URL: https://github.com/afeldman/emacs-scumm
;; License: GPL-3.0-or-later

;;; Commentary:

;; Core loader for the SCUMM game engine modules.
;; This file is loaded by the root scumm.el and should not be required directly.

;;; Code:

;; Load all core modules from this directory
(require 'scumm-core)
(require 'scumm-world)
(require 'scumm-script)
(require 'scumm-render)
(require 'scumm-input)
(require 'scumm-dialog)
(require 'scumm-inventory)
(require 'scumm-save)

(provide 'scumm)
;;; scumm.el ends here
