from IPython.terminal.prompts import Prompts, Token


class MyPrompt(Prompts):
    def in_prompt_tokens(self, cli=None):
        return [
            (Token.Prompt, '['),
            (Token.PromptNum, str(self.shell.execution_count)),
            (Token.Prompt, ']  '),
        ]

    def continuation_prompt_tokens(self, width=None):
        if width is None:
            width = self._width()
        return [
            (Token.Prompt, (' ' * (width - 5)) + '...  '),
        ]

    def rewrite_prompt_tokens(self):
        width = self._width()
        return [
            (Token.Prompt, ('-' * (width - 3)) + '>  '),
        ]

    def out_prompt_tokens(self):
        return [
            (Token.OutPrompt, '['),
            (Token.OutPromptNum, str(self.shell.execution_count)),
            (Token.OutPrompt, ']  '),
        ]


c.TerminalIPythonApp.display_banner = False

c.InteractiveShell.autocall = 1
c.InteractiveShell.colors = 'linux'
c.InteractiveShell.history_length = 1000

c.TerminalInteractiveShell.autoformatter = 'black'
c.TerminalInteractiveShell.confirm_exit = False
c.TerminalInteractiveShell.highlighting_style = 'legacy'
c.TerminalInteractiveShell.editing_mode = 'vi'
c.TerminalInteractiveShell.extra_open_editor_shortcuts = True
c.TerminalInteractiveShell.prompt_includes_vi_mode = False
c.TerminalInteractiveShell.prompts_class = MyPrompt
c.TerminalInteractiveShell.term_title = False
c.TerminalInteractiveShell.true_color = True

c.HistoryAccessor.enabled = False
