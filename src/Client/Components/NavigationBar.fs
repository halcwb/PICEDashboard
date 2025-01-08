namespace Components

open Fable.Core


module NavigationBar =


    [<JSX.Component>]
    let View
        (props:
            {|
                title: string
                showPerc: (unit -> unit) option
                skipFirst: unit -> unit
                skipPrev: unit -> unit
                skipNext: unit -> unit
                skipLast: unit -> unit
                stop: unit -> unit
            |})
        =
        let sx = {| flexGrow = 1 |}

        if props.showPerc.IsSome then
            JSX.jsx
                $"""
                import React from 'react';
                import Toolbar from "@mui/material/Toolbar";
                import Typography from "@mui/material/Typography";
                import IconButton from "@mui/material/IconButton";
                import FirstPageIcon from '@mui/icons-material/FirstPage';
                import SkipPreviousIcon from "@mui/icons-material/SkipPrevious";
                import SkipNextIcon from "@mui/icons-material/SkipNext";
                import LastPageIcon from '@mui/icons-material/LastPage';
                import StopIcon from '@mui/icons-material/Stop';
                import EqualizerIcon from '@mui/icons-material/Equalizer';

                <Toolbar disableGutters={true}>
                    <Typography sx={sx}>
                        {props.title}
                    </Typography>
                    <IconButton onClick={props.showPerc.Value}>
                        <EqualizerIcon/>
                    </IconButton>
                    <IconButton onClick={props.skipFirst}>
                        <FirstPageIcon/>
                    </IconButton>
                    <IconButton onClick={props.skipPrev}>
                        <SkipPreviousIcon/>
                    </IconButton>
                    <IconButton onClick={props.skipNext}>
                        <SkipNextIcon/>
                    </IconButton>
                    <IconButton onClick={props.skipLast}>
                        <LastPageIcon/>
                    </IconButton>
                    <IconButton onClick={props.stop}>
                        <StopIcon/>
                    </IconButton>
                </Toolbar>
                """

        else
            JSX.jsx
                $"""
                import React from 'react';
                import Toolbar from "@mui/material/Toolbar";
                import Typography from "@mui/material/Typography";
                import IconButton from "@mui/material/IconButton";
                import FirstPageIcon from '@mui/icons-material/FirstPage';
                import SkipPreviousIcon from "@mui/icons-material/SkipPrevious";
                import SkipNextIcon from "@mui/icons-material/SkipNext";
                import LastPageIcon from '@mui/icons-material/LastPage';
                import StopIcon from '@mui/icons-material/Stop';

                <Toolbar disableGutters={true}>
                    <Typography sx={sx}>
                        {props.title}
                    </Typography>
                    <IconButton onClick={props.skipFirst}>
                        <FirstPageIcon/>
                    </IconButton>
                    <IconButton onClick={props.skipPrev}>
                        <SkipPreviousIcon/>
                    </IconButton>
                    <IconButton onClick={props.skipNext}>
                        <SkipNextIcon/>
                    </IconButton>
                    <IconButton onClick={props.skipLast}>
                        <LastPageIcon/>
                    </IconButton>
                    <IconButton onClick={props.stop}>
                        <StopIcon/>
                    </IconButton>
                </Toolbar>
                """
