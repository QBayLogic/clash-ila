use std::{
    io::{self},
    time::Duration,
};

use crossterm::{
    cursor::MoveTo,
    event::{Event as TuiEvent, KeyModifiers},
};
use crossterm::{
    event::{poll, read, KeyCode},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use tui::{backend::CrosstermBackend, layout::Rect, style::Style, widgets::*, *};

#[derive(Debug, PartialEq, Eq, Clone)]
struct TextPrompt {
    title: String,
    input: String,
    cursor: usize,
    visible: usize,
}

impl Widget for TextPrompt {
    fn render(self, area: layout::Rect, buf: &mut buffer::Buffer) {
        let limit: String = self
            .input
            .chars()
            .skip(self.input.len().saturating_sub(area.width as usize))
            .collect();
        buf.set_string(area.x, area.y, limit, Style::default());
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum TuiState {
    Main,
    InPrompt(TextPrompt),
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
            let title_block = Block::default().title("ILA").borders(Borders::ALL);
            f.render_widget(title_block, size);

            if let TuiState::InPrompt(text_prompt) = self.state.clone() {
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

                f.set_cursor(
                    (center.x + text_prompt.cursor.saturating_sub(text_prompt.visible) as u16)
                        .min(size.width),
                    center.y,
                );

                f.render_widget(text_prompt, center);
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
                        let default = "dump.vcd";
                        self.state = TuiState::InPrompt(TextPrompt {
                            title: format!("Save VCD file (default: {default})"),
                            input: default.to_string(),
                            cursor: default.len(),
                            visible: 0,
                        })
                    }
                    (TuiState::InPrompt(text_prompt), KeyCode::Char(c), _) => {
                        text_prompt.input.insert(text_prompt.cursor, c);
                        text_prompt.cursor += 1;
                    }
                    (TuiState::InPrompt(text_prompt), KeyCode::Backspace, _) => {
                        if text_prompt.cursor != 0 && text_prompt.cursor <= text_prompt.input.len()
                        {
                            text_prompt.input.remove(text_prompt.cursor - 1);
                            text_prompt.cursor = text_prompt.cursor.saturating_sub(1);
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
                        text_prompt.cursor = text_prompt.cursor.saturating_sub(1);
                    }
                    (TuiState::InPrompt(text_prompt), KeyCode::Right, _) => {
                        text_prompt.cursor = text_prompt.input.len().min(text_prompt.cursor + 1);
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
