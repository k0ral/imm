# imm

*imm* is a tool to execute arbitrary actions for each new element from RSS/Atom feeds (e.g. sending a mail, or writing a file).

*imm* is written and configured in *Haskell*.

Technical documentation is available at [hackage][1].

To get started, please consult documentation of `Imm.Boot` module.


## Example use cases

### Online feed reader

For the sake of *I-want-the-mutt-of-feed-readers* zealots, it is possible to turn any mail reader into a feed reader, by having *imm* send an e-mail with unread elements to an arbitrary address.
You can then browse your feeds through your favourite mail reader, and leverage any mail-related tool on your feeds.
Bonus points if your mail reader is online as you can now access your feeds from any computer connected to the Internet.

Check out `Imm.Hooks.SendMail` module.

### Offline read-it-later

*imm* is able to store a local copy of unread elements, to read them later while offline for example. External links won't work offline though.

Check out `Imm.Hooks.WriteFile` module.


## Example usage

- Subscribe to a feed:

  ```
  imm subscribe http://your.feed.org
  ```
- Import feeds from an OPML file:

  ```
  cat feeds.opml | imm import
  ```
- List subscribed feeds:

  ```
  imm show
  ```
- Unsubscribe from a feed:

  ```
  imm unsubscribe http://your.feed.org
  ```
- Check for new elements without executing any action:

  ```
  imm check
  ```
- Execute configured actions for each new element from subscribed feeds:

  ```
  imm run
  ```
[1]: http://hackage.haskell.org/package/imm
