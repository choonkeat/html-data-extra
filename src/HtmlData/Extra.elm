module HtmlData.Extra exposing
    ( SanitizeConfig
    , defaultSanitizeConfig
    , escapeHtml
    , isBlockElement
    , isIndented
    , sanitize
    , texthtmlFromHtml
    , textplainFromHtml
    )

import ElmEscapeHtml
import Html.Parser
import HtmlData exposing (..)
import HtmlData.Attributes exposing (..)


{-| -}
isIndented : Html msg -> Bool
isIndented element =
    case element of
        Text _ ->
            False

        Element eleName _ _ ->
            List.member eleName
                [ "blockquote", "dd", "ol", "ul" ]


isBlockElement : Html msg -> Bool
isBlockElement element =
    case element of
        Text _ ->
            False

        Element eleName _ _ ->
            List.member eleName blockElements


{-| <https://developer.mozilla.org/en-US/docs/Web/HTML/Block-level_elements#elements>
-}
blockElements : List String
blockElements =
    [ "address"
    , "article"
    , "aside"
    , "blockquote"
    , "details"
    , "dialog"
    , "dd"
    , "div"
    , "dl"
    , "dt"
    , "fieldset"
    , "figcaption"
    , "figure"
    , "figcaption"
    , "footer"
    , "form"
    , "h1"
    , "h2"
    , "h3"
    , "h4"
    , "h5"
    , "h6"
    , "header"
    , "hgroup"
    , "hr"
    , "li"
    , "main"
    , "nav"
    , "ol"
    , "p"
    , "pre"
    , "section"
    , "table"
    , "ul"
    ]



--


{-|

    import HtmlData exposing (..)
    import HtmlData.Attributes exposing (..)

    div [ classList
            [ ("hello", True)
            , ("world", True )
            , ("there", False )
            ]
        ]
        [ button [ id "Decrement", name "buttonDecrement" ] [ text "-" ]
        , div [] [ text ("Hello " ++ String.fromInt 1999) ]
        , button [ id "Increment", name "buttonIncrement" ] [ text "+" ]
        ]
        |> texthtmlFromHtml defaultSanitizeConfig
    --> "<div class=\"hello&#32;world\"><button id=\"Decrement\" name=\"buttonDecrement\">-</button><div>Hello&#32;1999</div><button id=\"Increment\" name=\"buttonIncrement\">&#43;</button></div>"

    -- roughly copied from
    -- https://developer.mozilla.org/en-US/docs/Web/HTML/Block-level_elements
    div []
        [ h1 [] [ text "Block-level elements" ]
        , p []
            [ text "In this article, we'll examine HTML block-level elements and how they differ from "
            , a [ href "https://developer.mozilla.org/en-US/docs/Web/HTML/Inline_elements" ] [ text "inline-level elements" ]
            , text "."
            ]
        , p []
            [ text "HTML ("
            , b [] [ text "Hypertext Markup Language" ]
            , text ") elements ... by CSS in the "
            , a [ href "https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Flow_Layout" ] [ text "Flow Layout" ]
            , text ". A Block-level element occupies ... contents, thereby creating a \"block\"."
            , aside []
                [ strong [] [ text "Note:" ]
                , text " A block-level element always starts on a new line and ... as it can)."
                ]
            , h3 [] [ text "See also" ]
            , ol []
                [ li [] [ a [ href "" ] [ text "Inline elements" ] ]
                , li [] [ a [ href "" ] [ text "display" ] ]
                , li [] [ a [ href "" ] [ text "Block and Inline Layout in Normal Flow" ] ]
                ]
            ]
        ]
        |> texthtmlFromHtml defaultSanitizeConfig
    --> "<div><h1>Block-level&#32;elements</h1><p>In&#32;this&#32;article,&#32;we&#39;ll&#32;examine&#32;HTML&#32;block-level&#32;elements&#32;and&#32;how&#32;they&#32;differ&#32;from&#32;<a href=\"https://developer.mozilla.org/en-US/docs/Web/HTML/Inline_elements\">inline-level&#32;elements</a>.</p><p>HTML&#32;&#40;<b>Hypertext&#32;Markup&#32;Language</b>&#41;&#32;elements&#32;...&#32;by&#32;CSS&#32;in&#32;the&#32;<a href=\"https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Flow_Layout\">Flow&#32;Layout</a>.&#32;A&#32;Block-level&#32;element&#32;occupies&#32;...&#32;contents,&#32;thereby&#32;creating&#32;a&#32;&quot;block&quot;.<aside><strong>Note:</strong>&#32;A&#32;block-level&#32;element&#32;always&#32;starts&#32;on&#32;a&#32;new&#32;line&#32;and&#32;...&#32;as&#32;it&#32;can&#41;.</aside><h3>See&#32;also</h3><ol><li><a>Inline&#32;elements</a></li><li><a>display</a></li><li><a>Block&#32;and&#32;Inline&#32;Layout&#32;in&#32;Normal&#32;Flow</a></li></ol></p></div>"


    -- https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dd
    div []
        [ p []
            [ text "Cryptids of Cornwall:" ]
        , dl []
            [ dt []
                [ text "Beast of Bodmin" ]
            , dd []
                [ text "A large feline inhabiting Bodmin Moor." ]
            , dt []
                [ text "Morgawr" ]
            , dd []
                [ text "A sea serpent." ]
            , dt []
                [ text "Owlman" ]
            , dd []
                [ text "A giant owl-like creature." ]
            ]
        ]
        |> texthtmlFromHtml defaultSanitizeConfig
    --> "<div><p>Cryptids&#32;of&#32;Cornwall:</p><dl><dt>Beast&#32;of&#32;Bodmin</dt><dd>A&#32;large&#32;feline&#32;inhabiting&#32;Bodmin&#32;Moor.</dd><dt>Morgawr</dt><dd>A&#32;sea&#32;serpent.</dd><dt>Owlman</dt><dd>A&#32;giant&#32;owl-like&#32;creature.</dd></dl></div>"

-}
texthtmlFromHtml : SanitizeConfig -> Html msg -> String
texthtmlFromHtml config html =
    case html of
        Text string ->
            sanitize config string
                |> Maybe.withDefault ""

        Element name attrs children ->
            [ "<" :: name :: List.map (texthtmlFromAttr config) attrs ++ [ ">" ]
            , List.map (texthtmlFromHtml config) children
            , [ "</", name, ">" ]
            ]
                |> List.concat
                |> String.join ""


texthtmlFromAttr : SanitizeConfig -> Attribute msg -> String
texthtmlFromAttr config attr =
    case attr of
        NoAttribute ->
            ""

        Attribute rawk rawv ->
            Maybe.withDefault "" <|
                Maybe.map2 (\k v -> " " ++ k ++ "=\"" ++ v ++ "\"")
                    (sanitize config rawk)
                    (sanitize config rawv)


{-|

    import HtmlData exposing (..)
    import HtmlData.Attributes exposing (..)

    -- roughly copied from
    -- https://developer.mozilla.org/en-US/docs/Web/HTML/Block-level_elements
    div []
        [ h1 [] [ text "Block-level elements" ]
        , p []
            [ text "In this article, we'll examine HTML block-level elements and how they differ from "
            , a [ href "https://developer.mozilla.org/en-US/docs/Web/HTML/Inline_elements" ] [ text "inline-level elements" ]
            , text "."
            ]
        , p []
            [ text "HTML ("
            , b [] [ text "Hypertext Markup Language" ]
            , text ") elements ... by CSS in the "
            , a [ href "https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Flow_Layout" ] [ text "Flow Layout" ]
            , text ". A Block-level element occupies ... contents, thereby creating a \"block\"."
            , aside []
                [ strong [] [ text "Note:" ]
                , text " A block-level element always starts on a new line and ... as it can)."
                ]
            , h3 [] [ text "See also" ]
            , ol []
                [ li [] [ a [ href "" ] [ text "Inline elements" ] ]
                , li [] [ a [ href "" ] [ text "display" ] ]
                , li [] [ a [ href "" ] [ text "Block and Inline Layout in Normal Flow" ] ]
                ]
            ]
        ]
        |> textplainFromHtml
    --> String.join "\n"
    -->     [ "Block-level elements"
    -->     , ""
    -->     , "In this article, we'll examine HTML block-level elements and how they differ from inline-level elements https://developer.mozilla.org/en-US/docs/Web/HTML/Inline_elements ."
    -->     , ""
    -->     , "HTML (Hypertext Markup Language) elements ... by CSS in the Flow Layout https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Flow_Layout . A Block-level element occupies ... contents, thereby creating a \"block\"."
    -->     , ""
    -->     , "Note: A block-level element always starts on a new line and ... as it can)."
    -->     , ""
    -->     , "See also"
    -->     , ""
    -->     , "    1. Inline elements"
    -->     , ""
    -->     , "    2. display"
    -->     , ""
    -->     , "    3. Block and Inline Layout in Normal Flow"
    -->     ]


    -- https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dd
    div []
        [ p []
            [ text "Cryptids of Cornwall:" ]
        , dl []
            [ dt []
                [ text "Beast of Bodmin" ]
            , dd []
                [ text "A large feline inhabiting Bodmin Moor." ]
            , dt []
                [ text "Morgawr" ]
            , dd []
                [ text "A sea serpent." ]
            , dt []
                [ text "Owlman" ]
            , dd []
                [ text "A giant owl-like creature." ]
            ]
        ]
        |> textplainFromHtml
    --> String.join "\n"
    -->     [ "Cryptids of Cornwall:"
    -->     , ""
    -->     , "Beast of Bodmin"
    -->     , ""
    -->     , "    A large feline inhabiting Bodmin Moor."
    -->     , ""
    -->     , "Morgawr"
    -->     , ""
    -->     , "    A sea serpent."
    -->     , ""
    -->     , "Owlman"
    -->     , ""
    -->     , "    A giant owl-like creature."
    -->     ]

-}
textplainFromHtml : Html msg -> String
textplainFromHtml element =
    let
        textplainFromHtml_helper indent prefixEachChild htmlList =
            let
                prefix number =
                    String.padLeft number ' ' ""
            in
            htmlList
                |> List.foldl
                    (\curr ( acc, last ) ->
                        case curr of
                            Text string ->
                                ( acc, last ++ string )

                            Element "a" attrs children ->
                                let
                                    linkSuffix =
                                        List.filterMap
                                            (\attr ->
                                                case attr of
                                                    Attribute "href" string ->
                                                        -- a very slight awkward space after urls to prevent
                                                        -- punctuations from being confused as part of url
                                                        Just (string ++ " ")

                                                    Attribute _ _ ->
                                                        Nothing

                                                    NoAttribute ->
                                                        Nothing
                                            )
                                            attrs
                                            |> String.join ""

                                    linkContent =
                                        textplainFromHtml_helper indent (always "") children
                                            |> String.replace (String.trim linkSuffix) ""
                                in
                                ( acc
                                , last
                                    ++ ([ linkContent, linkSuffix ]
                                            |> List.filter (\string -> String.trim string /= "")
                                            |> String.join " "
                                       )
                                )

                            Element "ol" _ children ->
                                ( acc ++ [ last ]
                                , children
                                    |> textplainFromHtml_helper (indent + 2) (\number -> String.fromInt (number + 1) ++ ". ")
                                    |> String.append (prefix (indent + 2) ++ prefixEachChild (List.length acc))
                                )

                            Element "ul" _ children ->
                                ( acc ++ [ last ]
                                , children
                                    |> textplainFromHtml_helper (indent + 2) (always "- ")
                                    |> String.append (prefix (indent + 2) ++ prefixEachChild (List.length acc))
                                )

                            Element _ _ children ->
                                if isIndented curr then
                                    ( acc ++ [ last ]
                                    , children
                                        |> textplainFromHtml_helper (indent + 4) (always "")
                                        |> String.append (prefix (indent + 4) ++ prefixEachChild (List.length acc))
                                    )

                                else if isBlockElement curr then
                                    ( acc ++ [ last ], prefix indent ++ prefixEachChild (List.length acc) ++ textplainFromHtml_helper indent (always "") children )

                                else
                                    ( acc, last ++ textplainFromHtml_helper indent (always "") children )
                    )
                    ( [], "" )
                |> (\( acc, last ) ->
                        (acc ++ [ last ])
                            |> List.filter (\s -> String.trim s /= "")
                            |> String.join ("\n\n" ++ prefix indent)
                   )
    in
    textplainFromHtml_helper 0 (always "") [ element ]



--


type alias SanitizeConfig =
    { urlAttributes : List String
    , removedAttributes : List String
    , isAllowedUrl : String -> Bool
    }


defaultSanitizeConfig : SanitizeConfig
defaultSanitizeConfig =
    { urlAttributes =
        --  https://stackoverflow.com/a/2725168
        [ "action"
        , "archive"
        , "background"
        , "cite"
        , "classid"
        , "codebase"
        , "content"
        , "data"
        , "dynsrc"
        , "formaction"
        , "href"
        , "icon"
        , "longdesc"
        , "lowsrc"
        , "manifest"
        , "poster"
        , "profile"
        , "src"
        , "srcset"
        , "usemap"
        ]
    , removedAttributes =
        [ "style"

        -- and some from urlAttributes
        , "action"
        , "archive"
        , "background"
        , "cite"
        , "classid"
        , "codebase"
        , "content"
        , "data"
        , "dynsrc"
        , "formaction"
        , "icon"
        , "longdesc"
        , "lowsrc"
        , "manifest"
        , "poster"
        , "profile"
        , "srcset"
        , "usemap"
        ]
    , isAllowedUrl =
        \urlString ->
            String.startsWith "http://" urlString
                || String.startsWith "https://" urlString
    }


{-|

    sanitize defaultSanitizeConfig """<h1 class="javascript:yo"> hello </h1>"""
    --> Just "<h1 class=\"javascript:yo\">&#32;hello&#32;</h1>"

    sanitize defaultSanitizeConfig """<a onclick='yo' data-other='yo' href="javascript :alert('Hi')">Cli>ckMe</a><script>alert("hello");</script>"""
    --> Just "<a data-other=\"yo\">Cli&gt;ckMe</a>"

    sanitize defaultSanitizeConfig """<b onmouseover=alert('Wufff!')>click me!</b>"""
    --> Nothing

    sanitize defaultSanitizeConfig """blah"/><script>alert("hello");</script>"""
    --> Just "blah&quot;/&gt;"

    sanitize defaultSanitizeConfig """<b onmouseover=alert(‘XSS!‘)></b>"""
    --> Just "<b></b>"

    sanitize defaultSanitizeConfig """<body style="javascript:yo" onload=alert(‘something’)></body>"""
    --> Just "<body></body>"

    sanitize defaultSanitizeConfig """<script>alert("hello");</script>"""
    --> Nothing

    sanitize defaultSanitizeConfig """<scr<script>ipt>alert(‘XSS’)</script>"""
    --> Nothing

    sanitize defaultSanitizeConfig """<SCRIPT>yo</SCRIPT>"""
    --> Nothing

    sanitize defaultSanitizeConfig """<IMG SRC=j&#X41vascript:alert('test2')>"""
    --> Nothing

    sanitize defaultSanitizeConfig """<IMG SRC="j&#X41vascript:alert('test2')">"""
    --> Just "<img>"

    sanitize defaultSanitizeConfig """<a onclick='yo' href="javascript :alert('Hi')">ClickMe</a><scr<script>ipt>alert("hello");</script>"""
    --> Nothing

    sanitize defaultSanitizeConfig """<img src="data:text/html;base64,PHNjcmlwdD5hbGVydCgndGVzdDMnKTwvc2NyaXB0Pg">"""
    --> Just "<img>"

    sanitize defaultSanitizeConfig """< h1>strict</h1>"""
    --> Nothing

    sanitize defaultSanitizeConfig """<h1>strict</ h1>"""
    --> Nothing

    sanitize defaultSanitizeConfig ""
    --> Nothing

-}
sanitize : SanitizeConfig -> String -> Maybe String
sanitize config rawHtml =
    Html.Parser.run rawHtml
        |> Result.toMaybe
        |> Maybe.map (List.filterMap (sanitizeNode config))
        |> Maybe.map (List.map Html.Parser.nodeToString)
        |> Maybe.map String.concat
        |> Maybe.andThen
            (\s ->
                if String.trim s == "" then
                    Nothing

                else
                    Just s
            )


sanitizeNode : SanitizeConfig -> Html.Parser.Node -> Maybe Html.Parser.Node
sanitizeNode config node =
    case node of
        Html.Parser.Text s ->
            Just (Html.Parser.Text (escapeHtml s))

        Html.Parser.Comment _ ->
            Nothing

        Html.Parser.Element rawName attr children ->
            case String.filter (\c -> Char.isAlphaNum c || c == '-') (normalize rawName) of
                "" ->
                    Nothing

                "style" ->
                    Nothing

                "script" ->
                    Nothing

                name ->
                    Just (Html.Parser.Element name (List.filterMap (sanitizeAttribute config) attr) (List.filterMap (sanitizeNode config) children))


sanitizeAttribute : SanitizeConfig -> Html.Parser.Attribute -> Maybe Html.Parser.Attribute
sanitizeAttribute config ( rawName, rawValue ) =
    let
        name =
            normalize rawName
    in
    if String.startsWith "on" name then
        Nothing

    else if List.member name config.removedAttributes then
        Nothing

    else if List.member name config.urlAttributes && not (config.isAllowedUrl (normalize rawValue)) then
        Nothing

    else
        Just ( name, escapeHtml rawValue )


normalize : String -> String
normalize =
    String.trim << String.toLower


{-| <http://wonko.com/post/html-escaping>

    """
    <a href="/user/foo" onmouseover="alert(1)">foo" onmouseover="alert(1)</a>
    <a href='/user/foo' onmouseover='alert(1)'>foo' onmouseover='alert(1)</a>
    """
    |> String.trim
    |> escapeHtml
    --> "&lt;a&#32;href&#61;&quot;/user/foo&quot;&#32;onmouseover&#61;&quot;alert&#40;1&#41;&quot;&gt;foo&quot;&#32;onmouseover&#61;&quot;alert&#40;1&#41;&lt;/a&gt;\n&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&#32;&lt;a&#32;href&#61;&#39;/user/foo&#39;&#32;onmouseover&#61;&#39;alert&#40;1&#41;&#39;&gt;foo&#39;&#32;onmouseover&#61;&#39;alert&#40;1&#41;&lt;/a&gt;"

-}
escapeHtml : String -> String
escapeHtml rawText =
    ElmEscapeHtml.escape rawText
