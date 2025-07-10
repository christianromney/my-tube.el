;;; my-tube.el --- YouTube playlist management for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Christian Romney <christian.a.romney@gmail.com>
;; Keywords: multimedia, youtube, api
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
;; URL: https://github.com/your-username/my-tube.el

;;; Commentary:

;; This package provides a simple minor mode for Emacs to interact with the
;; YouTube Data API v3. It allows users to perform basic YouTube playlist
;; management from within Emacs.
;;
;; Features:
;; - List user's playlists
;; - Create new playlists
;; - Delete playlists
;; - List playlist items
;; - Add items to playlists
;; - Remove items from playlists
;;
;; Setup:
;; 1. Create OAuth 2.0 credentials at https://console.developers.google.com/
;; 2. Store credentials securely using auth-source
;; 3. Enable my-tube-mode and start managing your playlists

;;; Code:

(require 'url)
(require 'json)
(require 'auth-source)
(require 'browse-url)

;;; Customization

(defgroup my-tube nil
  "YouTube playlist management for Emacs."
  :group 'multimedia
  :prefix "my-tube-")

(defcustom my-tube-client-id nil
  "OAuth 2.0 client ID for YouTube API access.
This should be stored securely using auth-source."
  :type '(choice (const :tag "Use auth-source" nil)
                 (string :tag "Client ID"))
  :group 'my-tube)

(defcustom my-tube-client-secret nil
  "OAuth 2.0 client secret for YouTube API access.
This should be stored securely using auth-source."
  :type '(choice (const :tag "Use auth-source" nil)
                 (string :tag "Client Secret"))
  :group 'my-tube)

(defcustom my-tube-redirect-uri "urn:ietf:wg:oauth:2.0:oob"
  "OAuth 2.0 redirect URI for installed applications."
  :type 'string
  :group 'my-tube)

(defcustom my-tube-auth-source-host "youtube-api"
  "Host name used for auth-source credential lookup."
  :type 'string
  :group 'my-tube)

;;; Constants

(defconst my-tube--api-base-url "https://www.googleapis.com/youtube/v3"
  "Base URL for YouTube Data API v3.")

(defconst my-tube--oauth-auth-url "https://accounts.google.com/o/oauth2/v2/auth"
  "OAuth 2.0 authorization endpoint.")

(defconst my-tube--oauth-token-url "https://oauth2.googleapis.com/token"
  "OAuth 2.0 token endpoint.")

(defconst my-tube--oauth-scope "https://www.googleapis.com/auth/youtube"
  "OAuth 2.0 scope for YouTube API access.")

;;; Variables

(defvar my-tube--access-token nil
  "Current OAuth 2.0 access token.")

(defvar my-tube--refresh-token nil
  "Current OAuth 2.0 refresh token.")

(defvar my-tube--token-expiry nil
  "Expiry time for current access token.")

;;; Authentication

(defun my-tube--get-credentials ()
  "Get OAuth 2.0 credentials from auth-source or custom variables."
  (let ((auth-info (auth-source-search :host my-tube-auth-source-host
                                       :max 1)))
    (if auth-info
        (let ((entry (car auth-info)))
          (cons (plist-get entry :user)
                (let ((secret (plist-get entry :secret)))
                  (if (functionp secret)
                      (funcall secret)
                    secret))))
      (cons my-tube-client-id my-tube-client-secret))))

(defun my-tube--build-auth-url (client-id)
  "Build OAuth 2.0 authorization URL with CLIENT-ID."
  (format "%s?client_id=%s&redirect_uri=%s&scope=%s&response_type=code&access_type=offline"
          my-tube--oauth-auth-url
          (url-hexify-string client-id)
          (url-hexify-string my-tube-redirect-uri)
          (url-hexify-string my-tube--oauth-scope)))

(defun my-tube--exchange-code-for-token (code)
  "Exchange authorization CODE for access token."
  (let* ((credentials (my-tube--get-credentials))
         (client-id (car credentials))
         (client-secret (cdr credentials)))
    (unless (and client-id client-secret)
      (error "Missing OAuth 2.0 credentials"))
    
    (let ((url-request-method "POST")
          (url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded")))
          (url-request-data
           (mapconcat 'identity
                      (list
                       (format "code=%s" (url-hexify-string code))
                       (format "client_id=%s" (url-hexify-string client-id))
                       (format "client_secret=%s" (url-hexify-string client-secret))
                       (format "redirect_uri=%s" (url-hexify-string my-tube-redirect-uri))
                       "grant_type=authorization_code")
                      "&")))
      (with-current-buffer (url-retrieve-synchronously my-tube--oauth-token-url)
        (goto-char (point-min))
        (search-forward "\n\n")
        (let* ((json-object-type 'plist)
               (json-key-type 'symbol)
               (json-array-type 'vector)
               (response (json-read)))
          (kill-buffer)
          (if (plist-get response :error)
              (error "OAuth error: %s" (plist-get response :error_description))
            (setq my-tube--access-token (plist-get response :access_token))
            (setq my-tube--refresh-token (plist-get response :refresh_token))
            (setq my-tube--token-expiry
                  (time-add (current-time) (seconds-to-time (plist-get response :expires_in))))
            t))))))

(defun my-tube--refresh-access-token ()
  "Refresh the access token using the refresh token."
  (unless my-tube--refresh-token
    (error "No refresh token available"))
  
  (let* ((credentials (my-tube--get-credentials))
         (client-id (car credentials))
         (client-secret (cdr credentials)))
    (unless (and client-id client-secret)
      (error "Missing OAuth 2.0 credentials"))
    
    (let ((url-request-method "POST")
          (url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded")))
          (url-request-data
           (mapconcat 'identity
                      (list
                       (format "refresh_token=%s" (url-hexify-string my-tube--refresh-token))
                       (format "client_id=%s" (url-hexify-string client-id))
                       (format "client_secret=%s" (url-hexify-string client-secret))
                       "grant_type=refresh_token")
                      "&")))
      (with-current-buffer (url-retrieve-synchronously my-tube--oauth-token-url)
        (goto-char (point-min))
        (search-forward "\n\n")
        (let* ((json-object-type 'plist)
               (json-key-type 'symbol)
               (json-array-type 'vector)
               (response (json-read)))
          (kill-buffer)
          (if (plist-get response :error)
              (error "Token refresh error: %s" (plist-get response :error_description))
            (setq my-tube--access-token (plist-get response :access_token))
            (setq my-tube--token-expiry
                  (time-add (current-time) (seconds-to-time (plist-get response :expires_in))))
            t))))))

(defun my-tube--ensure-valid-token ()
  "Ensure we have a valid access token."
  (when (or (not my-tube--access-token)
            (and my-tube--token-expiry (time-less-p my-tube--token-expiry (current-time))))
    (if my-tube--refresh-token
        (my-tube--refresh-access-token)
      (error "No valid token available. Please run `my-tube-authenticate`"))))

;;;###autoload
(defun my-tube-authenticate ()
  "Authenticate with YouTube API using OAuth 2.0."
  (interactive)
  (let* ((credentials (my-tube--get-credentials))
         (client-id (car credentials)))
    (unless client-id
      (error "Missing OAuth 2.0 client ID"))
    
    (let ((auth-url (my-tube--build-auth-url client-id)))
      (browse-url auth-url)
      (let ((code (read-string "Enter authorization code: ")))
        (if (my-tube--exchange-code-for-token code)
            (message "Successfully authenticated with YouTube API")
          (error "Authentication failed"))))))

;;; HTTP API Layer

(defun my-tube--make-request (method endpoint &optional params data)
  "Make HTTP request to YouTube API.
METHOD is the HTTP method, ENDPOINT is the API endpoint,
PARAMS is an alist of query parameters, DATA is request body for POST/PUT."
  (my-tube--ensure-valid-token)
  
  (let* ((url (concat my-tube--api-base-url endpoint))
         (query-string (when params
                         (mapconcat (lambda (param)
                                      (format "%s=%s"
                                              (url-hexify-string (symbol-name (car param)))
                                              (url-hexify-string (cdr param))))
                                    params "&")))
         (full-url (if query-string
                       (concat url "?" query-string)
                     url))
         (url-request-method method)
         (url-request-extra-headers
          `(("Authorization" . ,(format "Bearer %s" my-tube--access-token))
            ("Content-Type" . "application/json")))
         (url-request-data (when data (json-encode data))))
    
    (with-current-buffer (url-retrieve-synchronously full-url)
      (goto-char (point-min))
      (search-forward "\n\n")
      (let* ((json-object-type 'plist)
             (json-key-type 'symbol)
             (json-array-type 'vector)
             (response (json-read)))
        (kill-buffer)
        (if (plist-get response :error)
            (error "API error: %s" (plist-get (plist-get response :error) :message))
          response)))))

;;; YouTube API Wrapper Functions

(defun my-tube--api-playlists-list (&optional mine channel-id)
  "List playlists. If MINE is non-nil, list user's playlists.
If CHANNEL-ID is provided, list playlists for that channel."
  (let ((params '((part . "snippet,contentDetails"))))
    (cond
     (mine (push '(mine . "true") params))
     (channel-id (push (cons 'channelId channel-id) params)))
    (my-tube--make-request "GET" "/playlists" params)))

(defun my-tube--api-playlists-insert (title description)
  "Create a new playlist with TITLE and DESCRIPTION."
  (let ((data `((snippet . ((title . ,title)
                            (description . ,description)))
                (status . ((privacyStatus . "private"))))))
    (my-tube--make-request "POST" "/playlists" '((part . "snippet,status")) data)))

(defun my-tube--api-playlists-delete (playlist-id)
  "Delete playlist with PLAYLIST-ID."
  (my-tube--make-request "DELETE" "/playlists" `((id . ,playlist-id))))

(defun my-tube--api-playlist-items-list (playlist-id)
  "List items in playlist with PLAYLIST-ID."
  (my-tube--make-request "GET" "/playlistItems" 
                         `((part . "snippet,contentDetails")
                           (playlistId . ,playlist-id))))

(defun my-tube--api-playlist-items-insert (playlist-id video-id)
  "Add VIDEO-ID to playlist with PLAYLIST-ID."
  (let ((data `((snippet . ((playlistId . ,playlist-id)
                            (resourceId . ((kind . "youtube#video")
                                          (videoId . ,video-id))))))))
    (my-tube--make-request "POST" "/playlistItems" '((part . "snippet")) data)))

(defun my-tube--api-playlist-items-delete (playlist-item-id)
  "Remove playlist item with PLAYLIST-ITEM-ID."
  (my-tube--make-request "DELETE" "/playlistItems" `((id . ,playlist-item-id))))

;;; Utility Functions

(defun my-tube--extract-video-id (url)
  "Extract video ID from YouTube URL."
  (cond
   ((string-match "youtube\\.com/watch\\?v=\\([^&]+\\)" url)
    (match-string 1 url))
   ((string-match "youtu\\.be/\\([^?]+\\)" url)
    (match-string 1 url))
   (t nil)))

(defun my-tube--format-playlist (playlist)
  "Format playlist data for display."
  (let ((snippet (plist-get playlist :snippet)))
    (format "%s (%s items)"
            (plist-get snippet :title)
            (plist-get (plist-get playlist :contentDetails) :itemCount))))

(defun my-tube--format-playlist-item (item)
  "Format playlist item data for display."
  (let ((snippet (plist-get item :snippet)))
    (format "%s - %s"
            (plist-get snippet :title)
            (plist-get snippet :channelTitle))))

;;; Interactive Commands

;;;###autoload
(defun my-tube-list-playlists ()
  "List user's playlists."
  (interactive)
  (condition-case err
      (let* ((response (my-tube--api-playlists-list t))
             (playlists (plist-get response :items)))
        (if playlists
            (with-current-buffer (get-buffer-create "*My Tube Playlists*")
              (erase-buffer)
              (insert "YouTube Playlists:\n\n")
              (dolist (playlist playlists)
                (insert (format "- %s\n" (my-tube--format-playlist playlist))))
              (goto-char (point-min))
              (display-buffer (current-buffer)))
          (message "No playlists found")))
    (error (message "Error listing playlists: %s" (error-message-string err)))))

;;;###autoload
(defun my-tube-create-playlist (title description)
  "Create a new playlist with TITLE and DESCRIPTION."
  (interactive "sPlaylist title: \nsPlaylist description: ")
  (condition-case err
      (let ((response (my-tube--api-playlists-insert title description)))
        (message "Created playlist: %s" (plist-get (plist-get response :snippet) :title)))
    (error (message "Error creating playlist: %s" (error-message-string err)))))

;;;###autoload
(defun my-tube-delete-playlist ()
  "Delete a playlist."
  (interactive)
  (condition-case err
      (let* ((response (my-tube--api-playlists-list t))
             (playlists (plist-get response :items))
             (playlist-names (mapcar (lambda (p) 
                                       (cons (my-tube--format-playlist p) 
                                             (plist-get p :id))) 
                                     playlists))
             (selected (completing-read "Delete playlist: " playlist-names))
             (playlist-id (cdr (assoc selected playlist-names))))
        (when (yes-or-no-p (format "Really delete playlist '%s'? " selected))
          (my-tube--api-playlists-delete playlist-id)
          (message "Deleted playlist: %s" selected)))
    (error (message "Error deleting playlist: %s" (error-message-string err)))))

;;;###autoload
(defun my-tube-list-playlist-items ()
  "List items in a playlist."
  (interactive)
  (condition-case err
      (let* ((response (my-tube--api-playlists-list t))
             (playlists (plist-get response :items))
             (playlist-names (mapcar (lambda (p) 
                                       (cons (my-tube--format-playlist p) 
                                             (plist-get p :id))) 
                                     playlists))
             (selected (completing-read "Select playlist: " playlist-names))
             (playlist-id (cdr (assoc selected playlist-names)))
             (items-response (my-tube--api-playlist-items-list playlist-id))
             (items (plist-get items-response :items)))
        (if items
            (with-current-buffer (get-buffer-create "*My Tube Playlist Items*")
              (erase-buffer)
              (insert (format "Items in playlist '%s':\n\n" selected))
              (dolist (item items)
                (insert (format "- %s\n" (my-tube--format-playlist-item item))))
              (goto-char (point-min))
              (display-buffer (current-buffer)))
          (message "No items found in playlist")))
    (error (message "Error listing playlist items: %s" (error-message-string err)))))

;;;###autoload
(defun my-tube-add-item-to-playlist (url)
  "Add a YouTube video URL to a playlist."
  (interactive "sYouTube video URL: ")
  (condition-case err
      (let ((video-id (my-tube--extract-video-id url)))
        (unless video-id
          (error "Invalid YouTube URL"))
        
        (let* ((response (my-tube--api-playlists-list t))
               (playlists (plist-get response :items))
               (playlist-names (mapcar (lambda (p) 
                                         (cons (my-tube--format-playlist p) 
                                               (plist-get p :id))) 
                                       playlists))
               (selected (completing-read "Add to playlist: " playlist-names))
               (playlist-id (cdr (assoc selected playlist-names))))
          (my-tube--api-playlist-items-insert playlist-id video-id)
          (message "Added video to playlist: %s" selected)))
    (error (message "Error adding item to playlist: %s" (error-message-string err)))))

;;;###autoload
(defun my-tube-remove-item-from-playlist ()
  "Remove an item from a playlist."
  (interactive)
  (condition-case err
      (let* ((playlists-response (my-tube--api-playlists-list t))
             (playlists (plist-get playlists-response :items))
             (playlist-names (mapcar (lambda (p) 
                                       (cons (my-tube--format-playlist p) 
                                             (plist-get p :id))) 
                                     playlists))
             (selected-playlist (completing-read "Select playlist: " playlist-names))
             (playlist-id (cdr (assoc selected-playlist playlist-names)))
             (items-response (my-tube--api-playlist-items-list playlist-id))
             (items (plist-get items-response :items))
             (item-names (mapcar (lambda (i) 
                                   (cons (my-tube--format-playlist-item i) 
                                         (plist-get i :id))) 
                                 items))
             (selected-item (completing-read "Remove item: " item-names))
             (item-id (cdr (assoc selected-item item-names))))
        (when (yes-or-no-p (format "Really remove '%s' from playlist? " selected-item))
          (my-tube--api-playlist-items-delete item-id)
          (message "Removed item from playlist: %s" selected-item)))
    (error (message "Error removing item from playlist: %s" (error-message-string err)))))

;;; Minor Mode

(defvar my-tube-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c y l") 'my-tube-list-playlists)
    (define-key map (kbd "C-c y c") 'my-tube-create-playlist)
    (define-key map (kbd "C-c y d") 'my-tube-delete-playlist)
    (define-key map (kbd "C-c y i") 'my-tube-list-playlist-items)
    (define-key map (kbd "C-c y a") 'my-tube-add-item-to-playlist)
    (define-key map (kbd "C-c y r") 'my-tube-remove-item-from-playlist)
    (define-key map (kbd "C-c y A") 'my-tube-authenticate)
    map)
  "Keymap for my-tube minor mode.")

;;;###autoload
(define-minor-mode my-tube-mode
  "Minor mode for YouTube playlist management."
  :lighter " MyTube"
  :keymap my-tube-mode-map
  :global t
  :group 'my-tube)

(provide 'my-tube)

;;; my-tube.el ends here