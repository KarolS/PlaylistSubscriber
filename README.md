PlaylistSubscriber
==================

Program for checking if new videos were added to given Youtube playlists.

Compilation prerequisites
=========================

* GHC 7.6.* or higher

* Cabal 1.18.* or higher

* appropriate dependencies

Building
========

    cabal sandbox init
    cabal install

Usage
=====

* get an API key for Youtube API v3: <https://developers.google.com/youtube/registering_an_application>; pick a server key

* save the key in a file `.youtubeApiKey` in your home directory

* create a file `.playlistSubscriptions` in your home directory and put IDs or URLs of playlists you want to subscribe to in there

* run the program

* if you don't want to see the entries again, mark them as seen

License
=======

GPL v3. See the attached file.


