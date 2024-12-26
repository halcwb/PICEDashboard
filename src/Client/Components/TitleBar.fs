namespace Components

open System
open Fable.Core
open Fable.React
open Feliz
open Browser.Types


open Elmish
open Fable.Core.JsInterop


module TitleBar =


    [<JSX.Component>]
    let View
        (props:
            {|
                title: string
                toggleSideMenu: unit -> unit
            |})
        =


        let sx1 = {| flexGrow = 1 |}
        let sx2 = {| mr = 2 |}

        JSX.jsx
            $"""
        import AppBar from '@mui/material/AppBar';
        import Box from '@mui/material/Box';
        import Toolbar from '@mui/material/Toolbar';
        import Typography from '@mui/material/Typography';
        import Button from '@mui/material/Button';
        import IconButton from '@mui/material/IconButton';
        import MenuIcon from '@mui/icons-material/Menu';
        import Menu from '@mui/material/Menu';
        import MenuItem from '@mui/material/MenuItem';

        <Box sx={sx1}>
            <AppBar position="static">
                <Toolbar>
                    <IconButton
                        size="large"
                        edge="start"
                        color="inherit"
                        aria-label="menu"
                        sx={sx2}
                        onClick={props.toggleSideMenu}
                        >
                        <MenuIcon />

                    </IconButton>
                    <Typography variant="body1" component="div" sx={sx1}>
                        {props.title}
                    </Typography>

                </Toolbar>
            </AppBar>
        </Box>
        """
