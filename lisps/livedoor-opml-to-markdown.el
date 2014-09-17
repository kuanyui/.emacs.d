;;; opml-to-md.el ---                                -*- lexical-binding: t; -*-

;; Convert Livedoor Reader's OPML file into Markdown.
;; This tools is just for personal usage, so without any warranty.

(require 'html-entities-convert)

(defun xml-getdata (data &rest args)
  (do ((i 0 (1+ i)))
      ((> i (1- (length args))) data)
    (cond ((symbolp (elt args i))
	   (setq data (cdr (assq (elt args i) data))))
	  ((numberp (elt args i))
	   (setq data (elt data (elt args i)))))))

(defun livedoor--convert-body (xml-file-path)
  (let ((data (xml-parse-file xml-file-path))
	(time (xml-getdata data 0 'head 'dateCreated 1))
	(body (cdr (xml-getdata data 0 'body 'outline)))
	(FIN "")
	)
      (loop for directory in body
	when (not (eq 'string (type-of directory))) do
	(setq FIN (concat FIN (format "\n###%s###\n" (xml-getdata directory 1 'title))))
	(loop for item in directory do
	      (if (and (eq 'cons (type-of item))
		       (> (length item) 1))
		  (setq FIN (concat FIN (format "- [%s](%s)\n"
						(html-entities-convert
						 (replace-regexp-in-string "\n" "" (xml-getdata (elt item 1) 'title)))
						(xml-getdata (elt item 1) 'htmlUrl)
						))))))
    FIN))

(defun livedoor-convert-opml-to-markdown ()
  (interactive)
  (let ((output-markdown "~/Dropbox/Blog/kuanyui.github.io/source/blogrolls/index.md")
	(opml-copy-to "~/Dropbox/Blog/kuanyui.github.io/source/blogrolls/"); "Directory" and remember to append slash at the end.
	(input-file (read-file-name "Livedoor Reader's OPML file location: " "~/download/" nil t "export.xml")))
    (if (not (string-match "\\(\.opml\\|\.xml\\)$" input-file))
        (progn (message "It's not an OPML file.")
	       (sleep-for 1)
               (livedoor-convert-opml-to-markdown))
      (progn
        (copy-file input-file (format "%sfeed-from-rss-reader.xml" opml-copy-to) 'overwrite)
        (find-file output-markdown)
	(delete-region (point-min) (point-max))
	(insert (concat
		 (format-time-string "title: Subscribed Feeds
date: %Y-%m-%d %H:%M:%S
---\n" (current-time))
		 (format-time-string "<blockquote class=\"pullquote\">我有每天讀RSS reader的習慣，這個頁面就是我所訂閱的完整RSS feeds列表。嗯...或許可以把這個頁面視為blog聯播？<br>
部份飼料的分類標準不明不白為正常現象，敬請安心食用。<br>
原始的OPML檔可以在<a href=\"feed-from-rss-reader.xml\">這裡</a>取得。<br>
<span style='font-style:italic;color:#999;font-size:0.8em;'>此頁面於%Y/%m/%d  %H:%M:%S產生</span></blockquote>" (current-time))
		 (livedoor--convert-body input-file)
	 ))
	(save-buffer)))))

(provide 'livedoor-opml-to-markdown)
