((nil . ((eval . (defun my/project-setup ()		   
                   "Project-specific window layout."
		   (jae/layout-main-side-term-bottom-term "direnv exec . direnv exec . watchexec -W . -w devenv -w files -w flake -w hosts -w microvms -w misc -w modules -w packages -w profiles -w secrets -w tofu -w users -w utils --stdin-quit -- 'world lint; world dead; world dscheck'")
		   )))))
