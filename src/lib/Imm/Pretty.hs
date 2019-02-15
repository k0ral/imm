{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Imm.Pretty (module Imm.Pretty, module X) where

-- {{{ Imports
import           Data.Text                                 as Text
import           Data.Time
import           Data.Tree

import           Text.Atom.Types                           as Atom
-- import           Text.OPML.Types              as OPML hiding (text)
-- import qualified Text.OPML.Types              as OPML
import           Data.Text.Prettyprint.Doc                 as X hiding (list,
                                                                 width)
import           Data.Text.Prettyprint.Doc                 (list)
import           Data.Text.Prettyprint.Doc.Render.Terminal as X (AnsiStyle)
import           Data.Text.Prettyprint.Doc.Render.Terminal
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty
import           Refined
import           Text.RSS.Types                            as RSS
import           URI.ByteString
-- }}}

-- | Infix operator for 'line'
(<++>) :: Doc a -> Doc a -> Doc a
x <++> y = x <> line <> y

prettyTree :: (Pretty a) => Tree a -> Doc b
prettyTree (Node n s) = pretty n <++> indent 2 (vsep $ prettyTree <$> s)

prettyTime :: UTCTime -> Doc a
prettyTime = pretty . formatTime defaultTimeLocale rfc822DateFormat

-- instance Pretty OpmlHead where
--   pretty h = hsep $ catMaybes
--                [ pretty <$> fromNullable (opmlTitle h)
--                , (text "created at:" <+>) . pretty <$> opmlCreated h
--                , (text "modified at:" <+>) . pretty <$> modified h
--                , (text "by" <+>) . pretty <$> fromNullable (ownerName h)
--                , angles . pretty <$> fromNullable (ownerEmail h)
--                ]

-- instance Pretty OutlineBase where
--   pretty b = pretty $ OPML.text b

-- instance Pretty OutlineSubscription where
--   pretty b = angles $ pretty $ xmlUri b

-- instance Pretty OpmlOutline where
--   pretty (OpmlOutlineGeneric base otype) = hsep
--                                              [ text "type:" <+> pretty otype
--                                              , pretty base
--                                              ]
--   pretty (OpmlOutlineSubscription base s) = text "Subscription:" <+> pretty base <+> pretty s
--   pretty (OpmlOutlineLink base uri) = text "Link:" <+> pretty base <+> pretty uri

-- instance Pretty Opml where
--   pretty o = text "OPML" <+> pretty (opmlVersion o) <++> indent 2 (pretty (opmlHead o) <++> (vsep . map pretty $ opmlOutlines o))

prettyPerson :: AtomPerson -> Doc a
prettyPerson p = pretty (unrefine $ personName p) <> email where
  email = if Text.null $ personEmail p
    then mempty
    else space <> angles (pretty $ personEmail p)

prettyLink :: AtomLink -> Doc a
prettyLink l = withAtomURI prettyURI $ linkHref l

prettyAtomText :: AtomText -> Doc a
prettyAtomText (AtomPlainText _ t) = pretty t
prettyAtomText (AtomXHTMLText t)   = pretty t

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
prettyAtomContent (AtomContentInlineText _ t)  = pretty t
prettyAtomContent (AtomContentInlineXHTML t)   = pretty t
prettyAtomContent (AtomContentInlineOther _ t) = pretty t
prettyAtomContent (AtomContentOutOfLine _ u)   = withAtomURI prettyURI u

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
