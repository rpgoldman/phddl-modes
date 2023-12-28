# PDDL and HDDL modes for Emacs
Emacs modes for editing [PDDL](https://planning.wiki/guide/whatis/pddl), the Planning Domain Definition Language, and [HDDL](https://arxiv.org/abs/1911.05499) files.

# PDDL Mode

PDDL mode was originally developed by Surendra K Singhi, and later extended by me.  My extensions have never been properly cleaned up. I hope by putting them here, that process will be accelerated.

# HDDL Mode

HDDL is a language -- still in its infancy compared to PDDL -- that extends PDDL to cover Hierarchical Task Network (HTN) planning.

# Using these modes

1. Put pddl-mode.el and hddl-mode.el somewhere in your Emacs load-path.

2. Associate these modes with the relevant file types:

   ```

      (add-to-list 'auto-mode-alist
                   (cons "\\.PDDL$"
                         'pddl-mode))

      (add-to-list 'auto-mode-alist
                   (cons "\\.pddl$"))

      (add-to-list 'auto-mode-alist
                   (cons "\\.HDDL$"
                         'hddl-mode))

      (add-to-list 'auto-mode-alist
                   (cons "\\.hddl$"))

   ```

3. To make these modes easier to learn, I have added PDDL and HDDL menus.

