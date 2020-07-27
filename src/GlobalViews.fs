module Global.View

open Feliz
open Feliz.style
open Fable.React

let verifyPage =
    Html.div[
        prop.children[
            Html.video[
                prop.muted true
                prop.controls false
                prop.loop true
                prop.autoPlay true
                prop.src "img/Video_Background.mp4"
                prop.type' "video/mp4"
                prop.style[
                    Feliz.style.position.absolute
                    //Feliz.style.margin(0,0,0,0)
                    Feliz.style.minHeight 450
                    Feliz.style.minWidth 1200
                    Feliz.style.zIndex 0
                ]
                prop.children[
                    
                ]
            ]
            Html.div[
                prop.className "columns is-centered"
                prop.children[
                    Html.div[
                        prop.className "column is-2"
                        prop.text "Checking if Git is installed"
                        prop.style[
                            Feliz.style.marginTop 275
                            Feliz.style.fontSize 25
                            Feliz.style.color "white"
                            Feliz.style.zIndex 1
                            Feliz.style.whitespace.nowrap
                            fontWeight.bold
                        ]
                    ]
                ]
            ]
            Html.div[
                prop.className "columns is-centered"
                prop.children[
                    Html.div[
                        prop.className "column is-2"
                        prop.style[
                            Feliz.style.color "white"
                            Feliz.style.marginLeft 250
                            Feliz.style.zIndex 1
                        ]
                        prop.children[
                            Html.i[
                                prop.className "fa fa-cog fa-spin fa-2x"
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]

let verifyFailedPage ( msg : seq<ReactElement> ) =
    Html.div[
        prop.style[
            Feliz.style.backgroundImage "url(img/RadioTower.jpg)"
            backgroundSize.cover
            backgroundRepeat.noRepeat
            Feliz.style.minHeight 600
        ]
        prop.children[
            Html.div[
                prop.className "columns is-centered"
                prop.children[
                    Html.div[
                        prop.style[
                            Feliz.style.marginTop 10
                            Feliz.style.fontSize 25
                            Feliz.style.marginLeft 0
                            fontWeight.bold
                            Feliz.style.color "green"
                        ]
                        prop.className "column is-2"
                        prop.children[
                            str "RCO Handling"
                        ]
                    ]                
                ]
            ]
            Html.div[
                prop.className "columns is-centered"
                prop.style[
                    Feliz.style.marginTop 10
                    Feliz.style.fontSize 25
                    fontWeight.bold
                    Feliz.style.color "green"
                ]
                prop.children[
                    Html.div[
                        prop.className "column is-4"
                        prop.style[
                            Feliz.style.marginTop 50
                            Feliz.style.color "red"
                            Feliz.style.whitespace.nowrap
                            fontWeight.bold
                        ]
                        prop.children(msg)
                    ]                
                ]
            ]
        ]
    ]
