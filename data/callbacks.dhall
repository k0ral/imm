-- This file must produce a `List Callback` expression,
-- where `Callback` is defined as follows:
let Callback : Type =
  { _executable : Text
  , _arguments : List Text
  }

-- Below are 4 basic callbacks bundled with imm.
-- Check out `imm-monolith --help`
let downloadPage =
  { _executable = "imm-monolith"
  , _arguments = [ "-d", "/path/to/folder", "-F"]  -- Option `-F` is forwarded to monolith
  }

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

-- Check out `imm-wallabag --help`
let wallabag =
      { _executable = "imm-wallabag"
      , _arguments =
        [ "--host"
        , "INSERT_HOST"
        , "--port"
        , "INSERT_PORT"
        , "--client-id"
        , "INSERT_CLIENT_ID"
        , "--client-secret"
        , "INSERT_CLIENT_SECRET"
        , "--username"
        , "INSERT_USERNAME"
        , "--password"
        , "INSERT_PASSWORD"
        ]
      }

-- Check out `imm-shiori --help`
let shiori =
      { _executable = "imm-shiori"
      , _arguments =
        [ "--tags"
        , "TAG1,TAG2,TAG3"
        ]
      }

let config : List Callback = [ downloadPage ]
in config
