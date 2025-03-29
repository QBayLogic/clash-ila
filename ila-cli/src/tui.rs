use std::{
    io::{self},
    time::Duration,
};

use crossterm::event::{Event as TuiEvent, KeyModifiers};
use crossterm::{
    event::{poll, read, KeyCode},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use tui::{backend::CrosstermBackend, layout::Rect, style::Style, widgets::*, *};

// TODO: make it a stateful widget, where the state is the cursor
// This is because you need to know the size it renders at to know when to make the cursor move

#[derive(Debug, PartialEq, Eq, Clone)]
struct TextPromptState {
    title: String,
    input: String,
    cursor: usize,
    cursor_offset: usize,
    visible: usize,
    render_bounds: (u16, u16),
}

impl TextPromptState {
    fn calculate_visible(&mut self, area: Rect) {
        self.visible = self
            .input
            .len()
            .saturating_sub(area.width as usize)
            .saturating_sub(self.cursor_offset);
    }

    fn left(&mut self) {
        self.cursor = self.cursor.saturating_sub(1);
        if self.cursor != 0 && self.get_cursor_x() == self.render_bounds.0 {
            self.cursor_offset = (self.cursor_offset + 1).min(self.input.len())
        }
    }

    fn right(&mut self) {
        self.cursor = self.input.len().min(self.cursor + 1);
        if self.cursor != self.input.len() && self.get_cursor_x() == self.render_bounds.1 {
            self.cursor_offset = self.cursor_offset.saturating_sub(1);
        }
    }

    fn get_cursor_x(&self) -> u16 {
        (self.render_bounds.0 + self.cursor.saturating_sub(self.visible) as u16)
            .min(self.render_bounds.1)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct TextPrompt;

impl StatefulWidget for TextPrompt {
    type State = TextPromptState;

    fn render(self, area: layout::Rect, buf: &mut buffer::Buffer, state: &mut Self::State) {
        let limit: String = state
            .input
            .chars()
            .skip(state.visible)
            .take(area.width as usize)
            .collect();
        buf.set_string(area.x, area.y, limit, Style::default());
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum TuiState {
    Main,
    InPrompt(TextPromptState),
}

pub struct TuiSession {
    term: Terminal<CrosstermBackend<io::Stdout>>,
    state: TuiState,
}

impl TuiSession {
    pub fn new() -> Result<TuiSession, io::Error> {
        enable_raw_mode()?;

        let mut stdout = io::stdout();
        execute!(stdout, EnterAlternateScreen)?;
        let backend = CrosstermBackend::new(stdout);
        Ok(TuiSession {
            term: Terminal::new(backend)?,
            state: TuiState::Main,
        })
    }

    pub fn render(&mut self) {
        let _ = self.term.draw(|f| {
            let size = f.size();
            let title_block = Block::default().title("ILA - Port ").borders(Borders::ALL);
            f.render_widget(title_block, size);

            if let TuiState::InPrompt(text_prompt) = &mut self.state {
                let width = size.width.saturating_sub(10);
                let around = Rect::new(
                    size.x + size.width.div_ceil(2).saturating_sub(width / 2),
                    size.y + size.height.div_ceil(2).saturating_sub(1),
                    width.min(size.width),
                    3.min(size.height),
                );
                let decoration = Block::default()
                    .title(text_prompt.title.as_str())
                    .borders(Borders::ALL);
                f.render_widget(decoration, around);

                let center = Rect::new(
                    around.x + 1,
                    around.y + 1,
                    around.width.saturating_sub(2),
                    around.height.saturating_sub(2),
                );

                text_prompt.calculate_visible(center);
                text_prompt.render_bounds.0 = center.x;
                text_prompt.render_bounds.1 = center.x + center.width;

                f.set_cursor(text_prompt.get_cursor_x(), center.y);
                f.render_stateful_widget(TextPrompt, center, text_prompt);
            }
        });
    }

    pub fn main_loop(&mut self) {
        self.render();

        loop {
            if let Ok(true) = poll(Duration::ZERO) {
                let event = match read() {
                    Ok(TuiEvent::Key(key)) => key,
                    Ok(TuiEvent::Resize(..)) => {
                        self.render();
                        continue;
                    }
                    _ => continue,
                };

                if event.code == KeyCode::Esc {
                    self.state = TuiState::Main
                }

                match (&mut self.state, event.code, event.modifiers) {
                    (TuiState::Main, KeyCode::Char('c'), KeyModifiers::CONTROL) => {
                        break;
                    }
                    (TuiState::Main, KeyCode::Char('v'), _) => {
                        let default = "dump.vcd-abcdefghijklmnopqrstuvwxyz1234567890";
                        self.state = TuiState::InPrompt(TextPromptState {
                            title: format!("Save VCD file (default: {default})"),
                            input: default.to_string(),
                            cursor: default.len(),
                            visible: 0,
                            cursor_offset: 0,
                            render_bounds: (0, u16::MAX),
                        })
                    }
                    (TuiState::InPrompt(text_prompt), KeyCode::Char(c), _) => {
                        text_prompt.input.insert(text_prompt.cursor, c);
                        text_prompt.right();
                    }
                    (TuiState::InPrompt(text_prompt), KeyCode::Backspace, _) => {
                        if text_prompt.cursor != 0 && text_prompt.cursor <= text_prompt.input.len()
                        {
                            text_prompt.input.remove(text_prompt.cursor - 1);
                            text_prompt.left();
                        }
                    }
                    (TuiState::InPrompt(text_prompt), KeyCode::Delete, _) => {
                        if text_prompt.cursor != text_prompt.input.len()
                            && text_prompt.cursor <= text_prompt.input.len()
                        {
                            text_prompt.input.remove(text_prompt.cursor);
                        }
                    }
                    (TuiState::InPrompt(text_prompt), KeyCode::Left, _) => {
                        text_prompt.left();
                    }
                    (TuiState::InPrompt(text_prompt), KeyCode::Right, _) => {
                        text_prompt.right();
                    }
                    _ => (),
                }

                if TuiState::Main == self.state {}

                self.render();
            }
        }
    }
}

impl Drop for TuiSession {
    fn drop(&mut self) {
        let _ = execute!(self.term.backend_mut(), LeaveAlternateScreen);
        let _ = disable_raw_mode();
    }
}
