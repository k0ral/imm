# imm

*imm* is a program that executes arbitrary callbacks (e.g. sending a mail, or writing a file) for each element of RSS/Atom feeds.

*imm* is written in [Haskell][2], configured in [Dhall][3]. The project includes:

- a main executable `imm` that users directly run
- additional executables (`imm-writefile` and `imm-sendmail`) that `imm` can be configured to use as callbacks
- a [*Haskell*][2] library, that exports functions useful to both the main executable and callbacks; the API is documented [in Hackage][1].

## Callbacks

Callbacks are configured through the `$XDG_CONFIG_HOME/imm/callbacks.dhall` file. A commented template file is bundled with the project.

`imm` will call each callback once per feed element, and will fill its standard input (`stdin`) with a JSON object structured as follows:

```json
{
  "feed": "RSS document or Atom feed (XML)",
  "element": "RSS item or Atom entry (XML)"
}
```

`imm` expects callbacks to return `0` in case of success, or any other exit code in case of failure, in which case the standard error output (`stderr`) will be displayed.


## Example use cases

### Online feed reader

For the sake of *I-want-the-mutt-of-feed-readers* zealots, it is possible to turn any mail reader into a feed reader, by having *imm* send an e-mail with unread elements to an arbitrary address.
You can then browse your feeds through your favourite mail reader, and leverage any mail-related tool on your feeds.
Bonus points if your mail reader is online as you can now access your feeds from any computer connected to the Internet.

Here is an example configuration:
```dhall
let Callback : Type =
  { _executable : Text
  , _arguments : List Text
  }

let sendMail =
  { _executable = "imm-sendmail"
  , _arguments = ["--login", "-u", "user@domain.com", "-P", "password", "-s", "smtp.domain.com", "-p", "587", "--to", "foo.bar@domain.com"]
  }

let config : List Callback = [ sendMail ]
in config
```

### Offline read-it-later

*imm* is able to store a local copy of unread elements, to read them later while offline for example. External links won't work offline though.

```
let Callback : Type =
  { _executable : Text
  , _arguments : List Text
  }

let writeFile =
  { _executable = "imm-writefile"
  , _arguments = [ "-d", "/path/to/folder" ]
  }

let config : List Callback = [ writeFile ]
in config
```

## Example usage

- Subscribe to a feed:
  ```bash
  imm subscribe http://your.feed.org
  ```

- Import feeds from an OPML file:
  ```bash
  cat feeds.opml | imm import
  ```

- List subscribed feeds:
  ```bash
  imm list
  ```

- Show details about a feed:
  ```bash
  imm show http://your.feed.org
  ```

- Unsubscribe from a feed:
  ```bash
  imm unsubscribe http://your.feed.org
  ```

- Check for new elements without executing any action:
  ```bash
  imm --read-only run --no-callbacks
  ```

- Execute configured callbacks for each new element from subscribed feeds:
  ```bash
  imm run
  ```

[1]: http://hackage.haskell.org/package/imm
[2]: https://www.haskell.org
[3]: https://dhall-lang.org/
