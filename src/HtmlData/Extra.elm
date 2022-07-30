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


isIndented : Html msg -> Bool
isIndented element =
    case element of
        Text _ ->
            False

        Element eleName _ _ ->
            List.member eleName
                [ "blockquote", "ol", "ul", "pre" ]


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
    --> String.join "\n\n"
    --> [ "Block-level elements"
    --> , "In this article, we'll examine HTML block-level elements and how they differ from inline-level elements https://developer.mozilla.org/en-US/docs/Web/HTML/Inline_elements ."
    --> , "HTML (Hypertext Markup Language) elements ... by CSS in the Flow Layout https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Flow_Layout . A Block-level element occupies ... contents, thereby creating a \"block\"."
    --> , "    Note: A block-level element always starts on a new line and ... as it can)."
    --> , "See also"
    --> , "    1. Inline elements"
    --> , "    2. display"
    --> , "    3. Block and Inline Layout in Normal Flow"
    --> ]

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

                            Element "blockquote" _ children ->
                                ( acc ++ [ last ]
                                , children
                                    |> textplainFromHtml_helper (indent + 4) (always "")
                                    |> String.append (prefix (indent + 4) ++ prefixEachChild (List.length acc))
                                )

                            Element "aside" _ children ->
                                ( acc ++ [ last ]
                                , children
                                    |> textplainFromHtml_helper (indent + 4) (always "")
                                    |> String.append (prefix (indent + 4) ++ prefixEachChild (List.length acc))
                                )

                            Element _ _ children ->
                                if isBlockElement curr then
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

    [ """<h1 class="javascript:yo"> hello </h1>"""
    , """<a onclick='yo' data-other='yo' href="javascript :alert('Hi')">Cli>ckMe</a><script>alert("hello");</script>"""
    , """<b onmouseover=alert('Wufff!')>click me!</b>"""
    , """blah"/><script>alert("hello");</script>"""
    , """<b onmouseover=alert(‘XSS!‘)></b>"""
    , """<body style="javascript:yo" onload=alert(‘something’)></body>"""
    , """<script>alert("hello");</script>"""
    , """<scr<script>ipt>alert(‘XSS’)</script>"""
    , """<SCRIPT>yo</SCRIPT>"""
    , """<IMG SRC=j&#X41vascript:alert('test2')>"""
    , """<IMG SRC="j&#X41vascript:alert('test2')">"""
    , """<a onclick='yo' href="javascript :alert('Hi')">ClickMe</a><scr<script>ipt>alert("hello");</script>"""
    , """<img src="data:text/html;base64,PHNjcmlwdD5hbGVydCgndGVzdDMnKTwvc2NyaXB0Pg">"""
    , """< h1>strict</h1>"""
    , """<h1>strict</ h1>"""
    , ""
    ]
    |> List.map String.trim
    |> List.filterMap (sanitize defaultSanitizeConfig)
    --> [ "<h1 class=\"javascript:yo\">&#32;hello&#32;</h1>"
    --> , "<a data-other=\"yo\">Cli&gt;ckMe</a>"
    --> , "blah&quot;/&gt;"
    --> , "<b></b>"
    --> , "<body></body>"
    --> , "<img>"
    --> ,"<img>"
    --> ]

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
-}
escapeHtml : String -> String
escapeHtml rawText =
    ElmEscapeHtml.escape rawText
