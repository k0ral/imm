{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Imm.Pretty (module Imm.Pretty, module X) where

-- {{{ Imports
import qualified Data.Text                                 as Text
import           Data.Text.Prettyprint.Doc                 as X hiding (list, width)
import           Data.Text.Prettyprint.Doc                 (list)
import           Data.Text.Prettyprint.Doc.Render.Terminal as X (AnsiStyle)
import           Data.Text.Prettyprint.Doc.Render.Terminal
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty
import           Data.Time
import           Data.Tree
import           Data.XML.Types                            as XML
import           Refined
import           Text.Atom.Types                           as Atom
import           Text.RSS.Types                            as RSS
import           URI.ByteString
-- }}}


-- | Newtype wrapper to prettyprint a key uniquely identifying an object
newtype PrettyKey a = PrettyKey a

prettyKey :: Pretty (PrettyKey a) => a -> Doc b
prettyKey = pretty . PrettyKey

-- | Newtype wrapper to prettyprint a user-friendly name referring to an object
newtype PrettyName a = PrettyName a

prettyName :: Pretty (PrettyName a) => a -> Doc b
prettyName = pretty . PrettyName

-- | Newtype wrapper to prettyprint a short description of an object
newtype PrettyShort a = PrettyShort a

prettyShort :: Pretty (PrettyShort a) => a -> Doc b
prettyShort = pretty . PrettyShort


-- | Infix operator for 'line'
(<++>) :: Doc a -> Doc a -> Doc a
x <++> y = x <> line <> y

prettyTree :: (Pretty a) => Tree a -> Doc b
prettyTree (Node n s) = pretty n <++> indent 2 (vsep $ prettyTree <$> s)

prettyTime :: UTCTime -> Doc a
prettyTime = pretty . formatTime defaultTimeLocale "%F %T"

prettyPerson :: AtomPerson -> Doc a
prettyPerson p = pretty (unrefine $ personName p) <> email where
  email = if Text.null $ personEmail p
    then mempty
    else space <> angles (pretty $ personEmail p)

prettyLink :: AtomLink -> Doc a
prettyLink l = withAtomURI prettyURI $ linkHref l

prettyAtomText :: AtomText -> Doc a
prettyAtomText (AtomPlainText _ t)     = pretty t
prettyAtomText (AtomXHTMLText element) = prettyElement element

prettyElement :: Element -> Doc a
prettyElement (Element _ _ nodes) = mconcat $ map prettyNode nodes

prettyNode :: Node -> Doc a
prettyNode (NodeElement element) = prettyElement element
prettyNode (NodeContent content) = prettyContent content
prettyNode _                     = mempty

prettyContent :: Content -> Doc a
prettyContent (ContentText t)      = pretty t
prettyContent (ContentEntity "lt") = "<"
prettyContent (ContentEntity "gt") = ">"
prettyContent _                    = "?"

prettyEntry :: AtomEntry -> Doc a
prettyEntry e = "Entry:" <+> prettyAtomText (entryTitle e) <++> indent 4
  (         "By" <+> equals <+> list (prettyPerson <$> entryAuthors e)
  <++> "Updated" <+> equals <+> prettyTime (entryUpdated e)
  <++> "Links"   <+> equals <+> list (prettyLink <$> entryLinks e)
  -- , "   Item Body:   " ++ (Imm.Mail.getItemContent item),
  )

prettyItem :: RssItem e -> Doc a
prettyItem i = "Item:" <+> pretty (itemTitle i) <++> indent 4
  (         "By" <+> equals <+> pretty (itemAuthor i)
  <++> "Updated" <+> equals <+> maybe "<empty>" prettyTime (itemPubDate i)
  <++> "Link"    <+> equals <+> maybe "<empty>" (withRssURI prettyURI) (itemLink i)
  )

prettyURI :: URIRef a -> Doc b
prettyURI uri = pretty @Text $ decodeUtf8 $ serializeURIRef' uri

prettyGuid :: RssGuid -> Doc a
prettyGuid (GuidText t)         = pretty t
prettyGuid (GuidUri (RssURI u)) = prettyURI u

prettyAtomContent :: AtomContent -> Doc a
prettyAtomContent (AtomContentInlineText _ t)      = pretty t
prettyAtomContent (AtomContentInlineXHTML element) = prettyElement element
prettyAtomContent (AtomContentInlineOther _ t)     = pretty t
prettyAtomContent (AtomContentOutOfLine _ u)       = withAtomURI prettyURI u

magenta :: Doc AnsiStyle -> Doc AnsiStyle
magenta = annotate $ color Magenta

yellow :: Doc AnsiStyle -> Doc AnsiStyle
yellow = annotate $ color Yellow

red :: Doc AnsiStyle -> Doc AnsiStyle
red = annotate $ color Red

green :: Doc AnsiStyle -> Doc AnsiStyle
green = annotate $ color Green

cyan :: Doc AnsiStyle -> Doc AnsiStyle
cyan = annotate $ color Cyan

bold :: Doc AnsiStyle -> Doc AnsiStyle
bold = annotate Pretty.bold
