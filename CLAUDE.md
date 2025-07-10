# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an Emacs Lisp package called `my-tube.el` that provides a minor mode for interacting with the YouTube Data API v3. The project is currently in early development stage with only specification documentation (readme.org) present.

## Project Structure

The repository currently contains:
- `readme.org` - Project specification and requirements
- `.claude/settings.local.json` - Claude Code configuration with bash permissions

## Core Functionality

The package will provide YouTube playlist management capabilities within Emacs:

### Primary Commands (Planned)
- `list-playlists` - List user's playlists
- `create-playlist` - Create new named playlist
- `delete-playlist` - Delete a playlist
- `list-playlist-items` - List items in a playlist
- `add-item-to-playlist` - Add item to playlist
- `remove-item-from-playlist` - Remove item from playlist

### Key Implementation Requirements
- Use idiomatic Emacs Lisp patterns and built-in libraries
- Always prefer simplicity. If multiple idiomatic ways exist to achieve some aim, prefer the simplest. 
- Secure credential storage (OAuth client IDs, API keys)
- Consider macOS compatibility for credential management
- Create simplified wrapper APIs for complex YouTube API operations
- Focus on Mac OS users when making implementation decisions

## Development Notes

This project is in the specification phase. The main implementation file `my-tube.el` has not been created yet. When implementing:

1. Follow Emacs Lisp conventions and use built-in libraries
2. Implement secure credential management (consider macOS Keychain integration)
3. Create user-friendly wrapper functions for YouTube API complexity
4. Focus on the core use cases outlined in readme.org

## Getting Started

Since this is an early-stage project, development will involve:
1. Creating the main `my-tube.el` file
2. Implementing OAuth flow for YouTube API authentication
3. Building the core playlist management functions
4. Testing with actual YouTube API integration
