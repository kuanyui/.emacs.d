Minor mode for org-mode to sync org-mode and trello

1) Add the following to your emacs init file
(require 'org-trello)
(add-hook 'org-mode-hook 'org-trello-mode)

2) Once - Install the consumer-key and the read-write token for org-trello to be able to work in your name with your trello boards (C-c o i)
M-x org-trello/install-key-and-token

3) Once per org-mode file/board you want to connect to (C-c o I)
M-x org-trello/install-board-and-lists-ids

4) You can also create a board directly from a org-mode buffer (C-c o b)
M-x org-trello/create-board

5) Check your setup (C-c o d)
M-x org-trello/check-setup

6) Help (C-c o h)
M-x org-trello/help-describing-setup
