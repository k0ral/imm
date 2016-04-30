# imm

## In a nutshell

*imm* is a tool that does only one thing: it retrieves a list of RSS/Atom feeds, and executes arbitrary actions for each of them (e.g. sending a mail).

Notably, *imm* makes it possible to use mail readers for feeds, for the sake of *I-want-the-mutt-of-feed-readers* zealots.

*imm* is written and configured in *Haskell*. To get started, please consult documentation of `Imm.Boot` module.

Informations about versions, dependencies, source repositories and contacts can be found in [hackage][1].


## Rationale

Following numerous RSS/Atom feeds needs organization and aggregation, which is usually accomplished through feed readers.
Although there are a lot of those, some people still feel unsatisfied with the existing implementations.

The expected features of a feed reader could be defined as follows:

- it retrieves a bunch of items that have some attributes: an author, a date/time, a (possibly enriched) body;
- items can be sorted, categorized, marked as read/unread, tagged, shared/forwarded;
- items must be available from anywhere on the internet.

Luckily, there's already a widespread solution that provides such features: mail readers.
Considering that, *imm* can project the RSS/Atom paradigm onto the mail one; this way, all the existing tools that work on mails can be leveraged to work on RSS/Atom feeds as well, no wheel reinventing.

[1]: http://hackage.haskell.org/package/imm
