{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Imm.Pretty (module Imm.Pretty, module X) where

-- {{{ Imports
import           Imm.Prelude

import           Data.Monoid.Textual
import           Data.NonNull
import           Data.Time
import           Data.Tree

import           Text.Atom.Types              as Atom
-- import           Text.OPML.Types              as OPML hiding (text)
-- import qualified Text.OPML.Types              as OPML
import           Text.PrettyPrint.ANSI.Leijen as X hiding (sep, width, (<$>),
                                                    (</>), (<>))
import           Text.RSS.Types               as RSS

import           URI.ByteString
-- }}}

-- | Generalized 'text'
textual :: TextualMonoid t => t -> Doc
textual = text . toString (const "?")

prettyTree :: (Pretty a) => Tree a -> Doc
prettyTree (Node n s) = pretty n <++> indent 2 (vsep $ prettyTree <$> s)

prettyTime :: UTCTime -> Doc
prettyTime = text . formatTime defaultTimeLocale rfc822DateFormat

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

prettyPerson :: AtomPerson -> Doc
prettyPerson p = text (fromText $ toNullable $ personName p) <> email where
  email = if null $ personEmail p
    then mempty
    else space <> angles (text $ fromText $ personEmail p)

prettyLink :: AtomLink -> Doc
prettyLink l = withAtomURI prettyURI $ linkHref l

prettyAtomText :: AtomText -> Doc
prettyAtomText (AtomPlainText _ t) = text $ fromText t
prettyAtomText (AtomXHTMLText t)   = text $ fromText t

prettyEntry :: AtomEntry -> Doc
prettyEntry e = "Entry:" <+> prettyAtomText (entryTitle e) <++> indent 4
  (         "By" <+> equals <+> list (prettyPerson <$> entryAuthors e)
  <++> "Updated" <+> equals <+> prettyTime (entryUpdated e)
  <++> "Links"   <+> equals <+> list (prettyLink <$> entryLinks e)
  -- , "   Item Body:   " ++ (Imm.Mail.getItemContent item),
  )

prettyItem :: RssItem e -> Doc
prettyItem i = "Item:" <+> text (fromText $ itemTitle i) <++> indent 4
  (         "By" <+> equals <+> text (fromText $ itemAuthor i)
  <++> "Updated" <+> equals <+> fromMaybe "<empty>" (prettyTime <$> itemPubDate i)
  <++> "Link"    <+> equals <+> fromMaybe "<empty>" (withRssURI prettyURI <$> itemLink i)
  )

prettyURI :: URIRef a -> Doc
prettyURI uri = text $ fromText $ decodeUtf8 $ serializeURIRef' uri

prettyGuid :: RssGuid -> Doc
prettyGuid (GuidText t)         = text $ fromText t
prettyGuid (GuidUri (RssURI u)) = prettyURI u

prettyAtomContent :: AtomContent -> Doc
prettyAtomContent (AtomContentInlineText _ t)  = text $ fromText t
prettyAtomContent (AtomContentInlineXHTML t)   = text $ fromText t
prettyAtomContent (AtomContentInlineOther _ t) = text $ fromText t
prettyAtomContent (AtomContentOutOfLine _ u)   = withAtomURI prettyURI u
