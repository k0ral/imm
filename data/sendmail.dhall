-- This file must produce a `Command` expression,
-- that represents how the external SMTP client program is to be called.
let Command : Type =
  { _executable : Text
  , _arguments : List Text
  }

-- Below is an example using the msmtp program
--
-- Check out `man msmtp`.
let sendmail : Command =
  { _executable = "msmtp"
  , _arguments = ["-t", "-a", "<account>"]
  }
in sendmail

