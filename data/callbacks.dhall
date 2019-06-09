-- This file must produce a `List Callback` expression,
-- where `Callback` is defined as follows:
let Callback : Type =
  { _executable : Text
  , _arguments : List Text
  }

-- Below are 2 basic callbacks bundled with imm.
--
-- Check out `imm-writefile --help`
let writeFile =
  { _executable = "imm-writefile"
  , _arguments = [ "-d", "/path/to/folder" ]
  }

-- Check out `imm-sendmail --help`
let sendMail =
  { _executable = "imm-sendmail"
  , _arguments = ["--login", "-u", "user@domain.com", "-P", "password", "-s", "smtp.domain.com", "-p", "587", "--to", "foo.bar@domain.com"]
  }

let config : List Callback = [ writeFile ]
in config
