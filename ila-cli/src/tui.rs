use std::io::{Read, Write};
use std::{io, time::Duration};

use crossterm::event::{Event as TuiEvent, KeyEvent, KeyModifiers};
use crossterm::{
    event::{poll, read, KeyCode},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use ratatui::{
    backend::CrosstermBackend,
    layout::{Constraint, Layout, Margin, Rect},
    style::Style,
    widgets::*,
    *,
};

use crate::communication::{
    perform_register_operation, IlaRegisters, RegisterOutput, SignalCluster,
};
use crate::config::IlaConfig;
use crate::vcd::write_to_vcd;

/// The keybind text displayed in the TUI
const KEYBIND_TEXT: &'static str = r#"  C-c   ---   Exit
  space ---   Read samples (if triggered)
  t     ---   Change trigger point
  c     ---   Change trigger logic
  r     ---   Reset trigger
  s     ---   Request sample to be sent again
  v     ---   Write signals to VCD dump
"#;

/// The reason to prompt the user with, mostly important to decide what to do next after a user has
/// inputted data to the prompt
#[derive(Debug, PartialEq, Eq, Clone)]
enum PromptReason {
    /// Prompt for the filename to save the VCD too
    SaveVcd,
    /// Prompt to change the trigger
    ChangeTrigger,
}

/// The state of the TUI
#[derive(Debug, PartialEq, Eq, Clone)]
enum TuiState {
    /// The TUI is in an idle state
    Main,
    /// The TUI is currently prompting the user for input
    InPrompt(TextPromptState),
}

/// A TUI session
///
/// This struct owns the TUI and once it goes out of scope, so will the TUI
/// It handles everything from rendering to interacting with the FPGA, and that's why it also needs
/// the ILA configuration.
pub struct TuiSession<'a> {
    /// The terminal backend to render the TUI on
    term: Terminal<CrosstermBackend<io::Stdout>>,
    /// In what state is the TUI currently at?
    state: TuiState,
    /// The ILA configuration, specifying certain aspects of the ILA
    config: &'a IlaConfig,
    /// A log for interactions to log their activity too, regularly gets truncated to fit the
    /// screen during rendering
    log: Vec<String>,
    /// A list of signal clusters captured by the ILA
    captured: Vec<SignalCluster>,
}

impl<'a> TuiSession<'a> {
    /// Create a new TUI session associated with a certain IlaConfig
    ///
    /// * `config` - The ILA configuration the TUI should use to properly communicate with the ILA
    pub fn new(config: &'a IlaConfig) -> Result<TuiSession<'a>, io::Error> {
        enable_raw_mode()?;

        let mut stdout = io::stdout();
        execute!(stdout, EnterAlternateScreen)?;
        let backend = CrosstermBackend::new(stdout);

        Ok(TuiSession {
            term: Terminal::new(backend)?,
            state: TuiState::Main,
            config,
            log: Vec::with_capacity(64),
            captured: vec![],
        })
    }

    /// Render the TUI
    pub fn render(&mut self) {
        let _ = self.term.draw(|f| {
            let size = f.area();
            let title_block = Block::default()
                .title("ILA - Port /dev/???")
                .borders(Borders::ALL);
            f.render_widget(title_block, size);

            let main_layout = Layout::default()
                .direction(layout::Direction::Vertical)
                .constraints([
                    Constraint::Length(KEYBIND_TEXT.lines().count() as u16 + 2),
                    Constraint::Min(1),
                ])
                .split(size.inner(Margin {
                    vertical: 1,
                    horizontal: 1,
                }));
            let info_layout = Layout::default()
                .direction(layout::Direction::Vertical)
                .margin(1)
                .constraints([Constraint::Min(1), Constraint::Min(1)])
                .split(main_layout[1]);

            // Ensure the lines fit within the Paragraph's range
            // If a string spans multiple lines / overflows, too bad*!*
            self.log = self
                .log
                .drain(
                    self.log
                        .len()
                        .saturating_sub(info_layout[1].height as usize)..,
                )
                .collect();

            let help_menu = Paragraph::new(KEYBIND_TEXT)
                .block(Block::default().title("Keybinds").borders(Borders::ALL));
            let info_menu_block = Block::default().title("Info").borders(Borders::ALL);
            let info_info_section =
                Paragraph::new(format!("Received {} captures", self.captured.len()));
            let info_log_section = Paragraph::new(
                self.log
                    .iter()
                    .map(|s| format!("{s}\n"))
                    .collect::<String>(),
            )
            .block(Block::default().borders(Borders::TOP));

            f.render_widget(help_menu, main_layout[0]);
            f.render_widget(info_menu_block, main_layout[1]);
            f.render_widget(info_info_section, info_layout[0]);
            f.render_widget(info_log_section, info_layout[1]);

            if let TuiState::InPrompt(text_prompt) = &mut self.state {
                let width = size.width.saturating_sub(10);
                let around = Rect::new(
                    size.x + size.width.div_ceil(2).saturating_sub(width / 2),
                    size.y + size.height.div_ceil(2).saturating_sub(1),
                    width.min(size.width),
                    3.min(size.height),
                );

                f.render_widget(Clear, around);
                let decoration = Block::default().title(title).borders(Borders::ALL);
                f.render_widget(decoration, around);

                let center = Rect::new(
                    around.x + 1,
                    around.y + 1,
                    around.width.saturating_sub(2),
                    around.height.saturating_sub(2),
                );

                text_prompt.render(center, f, true);
            }
        });
    }

    /// Handle the keypresses. Returns wether or not it should break out of the main loop or not
    fn on_key_event<T: Read + Write>(&mut self, event: KeyEvent, tx_port: &mut T) -> bool {
        match (&mut self.state, event.code, event.modifiers) {
            (TuiState::Main, KeyCode::Char('c'), KeyModifiers::CONTROL) => {
                return true;
            }
            (TuiState::Main, KeyCode::Char('r'), _) => {
                if let Err(err) =
                    perform_register_operation(tx_port, &self.config, &IlaRegisters::TriggerReset)
                {
                    self.log.push(format!("Error: {err}"));
                }
            }
            (TuiState::Main, KeyCode::Char(' '), _) => {
                let indices: Vec<u32> = (0_u32..self.config.buffer_size as u32).collect();
                match perform_register_operation(
                    tx_port,
                    &self.config,
                    &IlaRegisters::Buffer(indices),
                ) {
                    Ok(RegisterOutput::BufferContent(cluster)) => self.captured.push(cluster),
                    Ok(_) => self
                        .log
                        .push(format!("Unexpected output when reading buffer")),
                    Err(err) => self.log.push(format!("Error: {err}")),
                }
            }
            (TuiState::Main, KeyCode::Char('t'), _) => {
                self.state = TuiState::InPrompt(TextPromptState::new(
                    format!("Change trigger point [0-{}]", self.config.buffer_size),
                    Some("0"),
                    PromptReason::ChangeTrigger,
                ));
            }
            (TuiState::Main, KeyCode::Char('1'), _) => {
                perform_register_operation(tx_port, &self.config, &IlaRegisters::TriggerSelect(1))
                    .expect("cri");
                perform_register_operation(
                    tx_port,
                    &self.config,
                    &IlaRegisters::Compare(vec![0x00, 0x00, 0x00, 0xc8]),
                )
                .expect("cri");
                perform_register_operation(
                    tx_port,
                    &self.config,
                    &IlaRegisters::Mask(vec![0x00, 0x00, 0x01, 0xff]),
                )
                .expect("cri");
            }
            (TuiState::Main, KeyCode::Char('v'), _) => {
                self.state = TuiState::InPrompt(TextPromptState::new(
                    "Save VCD file (default: dump.vcd)",
                    Some("dump.vcd"),
                    PromptReason::SaveVcd,
                ));
            }
            (TuiState::InPrompt(_), KeyCode::Esc, _) => {
                self.log.push(format!("Cancelled save"));
                self.state = TuiState::Main;
            }
            (TuiState::InPrompt(prompt), KeyCode::Enter, _) => {
                // Handle the case of whenever a prompt gets completed
                // I want to move this to a seperate function, however due to borrow limits I can't
                // and that's kind of very annoying
                match prompt.reason {
                    PromptReason::SaveVcd => {
                        if let Some(sample) = self.captured.last() {
                            if let Err(err) =
                                write_to_vcd(sample, &self.config, prompt.input.clone())
                            {
                                self.log.push(format!("Error when saving VCD: {}", err));
                            } else {
                                self.log.push(format!("Saved succesfully!"));
                            }
                        } else {
                            self.log.push(format!("Nothing to save"));
                        }
                    }
                    PromptReason::ChangeTrigger => match prompt.input.parse() {
                        Ok(n) if n > self.config.buffer_size as u32 => {
                            self.log
                                .push(format!("Invalid input; must be specified range"));
                        }
                        Ok(n) => {
                            self.log.push(
                                match perform_register_operation(
                                    tx_port,
                                    self.config,
                                    &IlaRegisters::TriggerPoint(n),
                                ) {
                                    Ok(_) => String::from("Trigger point change made"),
                                    Err(err) => format!("Error: {err}"),
                                },
                            );
                        }
                        Err(_) => {
                            self.log
                                .push(format!("Invalid input; must be a unsigned 32 bit number"));
                        }
                    },
                }

                self.state = TuiState::Main;
            }
            (TuiState::InPrompt(text_prompt), KeyCode::Char(c), _) => {
                text_prompt.input.insert(text_prompt.cursor, c);
                text_prompt.right();
            }
            (TuiState::InPrompt(text_prompt), KeyCode::Backspace, _) => {
                if text_prompt.cursor != 0 && text_prompt.cursor <= text_prompt.input.len() {
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

        self.render();

        false
    }

    /// The main TUI loop
    ///
    /// Handles everything from rendering to the input of the TUI interface
    pub fn main_loop<T: Read + Write>(&mut self, mut tx_port: T) {
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

                if self.on_key_event(event, &mut tx_port) {
                    break;
                };
            }
        }
    }
}

impl Drop for TuiSession<'_> {
    // Make sure that when the TuiSession goes out of scope, we exit the alternative screen, as to
    // not break the user's terminal session (hopefully)
    fn drop(&mut self) {
        let _ = execute!(self.term.backend_mut(), LeaveAlternateScreen);
        let _ = disable_raw_mode();
    }
}
