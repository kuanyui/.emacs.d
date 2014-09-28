Add following block to your Emacs configuration

    (pyenv-mode)

Now you are available to specify pyenv python installation

    M-x pyenv-mode-set

So now when you run inferior python with

    M-x run-python

process will start inside specified python installation.  You can
unset current version with

    M-x pyenv-mode-unset
