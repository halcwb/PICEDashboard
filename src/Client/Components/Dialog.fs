namespace Components


module Dialog =


    open Fable.Core

    let private renderMd (md: string) =
        let props = {| md = md |}
        Markdown.View props


    [<JSX.Component>]
    let View
        (props:
            {|
                title: string
                content: string
                dispatch: (unit -> unit)
            |})
        =
        JSX.jsx
            $"""
        import Dialog from '@mui/material/Dialog';
        import DialogTitle from '@mui/material/DialogTitle';
        import DialogContent from '@mui/material/DialogContent';
        import DialogActions from '@mui/material/DialogActions';
        import Button from '@mui/material/Button';

        <Dialog open={true} onClose={props.dispatch}>
            <DialogTitle>{props.title |> renderMd}</DialogTitle>
            <DialogContent>
                {props.content |> renderMd}
            </DialogContent>
            <DialogActions>
                <Button onClick={props.dispatch} color="primary">
                    Ok
                </Button>
            </DialogActions>
        </Dialog>
        """
