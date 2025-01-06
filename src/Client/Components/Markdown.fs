namespace Components


open Fable.Core

module Markdown =


    [<JSX.Component>]
    let View (props: {| md: string |}) =
        JSX.jsx
            $"""
            import {{ MuiMarkdown, getOverrides }} from 'mui-markdown';
            import Typography from '@mui/material/Typography';

            <MuiMarkdown 
                options={{ {{
                    disableParsingRawHTML : true,
                    overrides : {{
                        ...getOverrides(),
                        h1 : 
                            {{
                                component : Typography,
                                props : {{  fontSize : 24, fontWeight : "bold", color : "#0d47a1" }}
                            }},
                        h2 : 
                            {{
                                component : Typography,
                                props : {{ sx : {{ fontSize : 20, fontWeight : "bold", paddingTop: 4, paddingBottom : 1, color :  "#0d47a1" }} }}
                            }},
                        h3 : 
                            {{
                                component : Typography,
                                props : {{ sx : {{ fontSize : 18, fontWeight : "bold", paddingTop: 4, paddingBottom : 1, color :  "#0d47a1" }} }}
                            }},
                        h4 : 
                            {{
                                component : Typography,
                                props : {{ sx : {{ fontSize : 16, fontWeight : "bold", paddingTop: 4, paddingBottom : 1, color :  "#0d47a1" }} }}
                            }},
                        h5 : 
                            {{
                                component : Typography,
                                props : {{ sx : {{ fontSize : 14, fontWeight : "bold", paddingTop: 4, paddingBottom : 1, color :  "#0d47a1" }} }}
                            }},
                        h6 : 
                            {{
                                component : Typography,
                                props : {{ sx : {{ fontSize : 14, fontWeight : "bold", paddingTop: 4, paddingBottom : 1, color :  "#0d47a1" }} }}
                            }},
                        p : 
                            {{
                                component : Typography,
                                props : {{ sx : {{ fontSize : 12, paddingTop: 2, paddingBottom : 2 }} }}
                            }},
                        a : 
                            {{
                                component : Typography,
                                props : {{ sx : {{ fontSize : 12 }} }}
                            }}
                    }}
                }}  }}
                >
                {props.md}
            </MuiMarkdown>
            """
