
use std::io::{self};

use serialport::ClearBuffer;
use tui::{backend::CrosstermBackend, widgets::*, *};
use crossterm::{execute, terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen}};

pub struct TuiSession(Terminal<CrosstermBackend<io::Stdout>>);

impl TuiSession {
    pub fn new() -> Result<TuiSession, io::Error> {
        enable_raw_mode()?;

        let mut stdout = io::stdout();
        execute!(stdout, EnterAlternateScreen)?;
        let backend = CrosstermBackend::new(stdout);
        Ok(TuiSession(Terminal::new(backend)?))
    }

    pub fn render(&mut self) {
        let _ = self.0.draw(|f| {
            let size = f.size();
            let block = Block::default()
                .title("ILA")
                .borders(Borders::NONE);
            f.render_widget(block, size);
        });
    }
}

impl Drop for TuiSession {
    fn drop(&mut self) {
        let _ = execute!(self.0.backend_mut(), LeaveAlternateScreen);
        let _ = disable_raw_mode();
    }
}

