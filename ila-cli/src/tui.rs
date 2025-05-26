use std::io::{Read, Write};
use std::path::Path;
use std::time::Instant;
use std::{io, time::Duration};

use crossterm::event::{Event as TuiEvent, KeyEvent, KeyModifiers};
use crossterm::{
    event::{poll, read, KeyCode},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use ratatui::style::Stylize;
use ratatui::text::{Line, Text};
use ratatui::{
    backend::CrosstermBackend,
    layout::{Constraint, Layout, Margin, Rect},
    widgets::*,
    *,
};

use crate::communication::{
    perform_register_operation, IlaRegisters, RegisterOutput, SignalCluster,
};
use crate::config::IlaConfig;
use crate::predicates::IlaPredicate;
use crate::predicates::PredicateTarget;
use crate::predicates_tui::State as PredState;
use crate::ui::textinput::TextPromptState;
use crate::vcd::write_to_vcd;

/// The keybind text displayed in the TUI
const KEYBIND_TEXT: &'static str = r#"  CTRL-c ---   Exit
  space  ---   Read samples (if triggered)
  t      ---   Change trigger point
  p      ---   Change trigger predicates
  c      ---   Change capture predicates
  r      ---   Re-arm trigger
  a      ---   Toggle auto trigger re-arm
  v      ---   Write signals to VCD dump
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
#[derive(Debug, Clone)]
enum TuiState<'a> {
    /// The TUI is in an idle state
    Main,
    /// The TUI is currently prompting the user for input
    InPrompt(TextPromptState<PromptReason>),
    /// Manages the trigger predicates
    Predicates(PredState<'a>),
}

/// The response of the TUI key event handler
#[derive(Debug, Clone, Copy)]
enum KeyResponse {
    /// Keep the current state
    Nothing,
    /// Quit the program
    QuitProgram,
    /// Changes to the ILA were applied, possibly re-arm the trigger
    AppliedChanges,
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
    state: TuiState<'a>,
    /// The ILA configuration, specifying certain aspects of the ILA
    config: &'a IlaConfig,
    /// A log for interactions to log their activity too, regularly gets truncated to fit the
    /// screen during rendering
    log: Vec<String>,
    /// A list of signal clusters captured by the ILA
    captured: Vec<SignalCluster>,
    /// Checks if the predicate is triggered or not
    triggered: bool,
    /// The time since the last triggered check has been performed
    last_trigger_check: Instant,
    /// If a configuration change has been made and this is set, it will rearm the trigger as well
    /// Otherwise it will only upload the new changes
    auto_reset: bool,
    /// The connected device path
    device_path: String,
}

impl<'a> TuiSession<'a> {
    /// Create a new TUI session associated with a certain IlaConfig
    ///
    /// * `config` - The ILA configuration the TUI should use to properly communicate with the ILA
    pub fn new(config: &'a IlaConfig, device_path: &Path) -> Result<TuiSession<'a>, io::Error> {
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
            triggered: false,
            last_trigger_check: Instant::now(),
            auto_reset: false,
            device_path: device_path.display().to_string(),
        })
    }

    fn render_main(&mut self) {
        let _ = self.term.draw(|f| {
            let size = f.area();
            let title_block = Block::default()
                .title(format!("ILA - Port {}", self.device_path))
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
                .constraints([Constraint::Length(3), Constraint::Fill(1)])
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
            let info_info_section = Paragraph::new(Text::from_iter([
                Line::from_iter([
                    "Received ".into(),
                    self.captured.len().to_string().bold().blue(),
                    " captured".into(),
                ]),
                Line::from_iter([
                    "The ILA is ".into(),
                    match self.triggered {
                        true => "TRIGGERED".bold().green(),
                        false => "NOT TRIGGERED".bold().red(),
                    },
                ]),
                Line::from_iter([
                    "Auto-rearm is ".into(),
                    match self.auto_reset {
                        true => "ENABLED".bold().green(),
                        false => "DISABLED".bold().red(),
                    },
                ]),
            ]));
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

                let title = match text_prompt.reason {
                    PromptReason::SaveVcd => "Save VCD file (default: dump.vcd)".into(),
                    PromptReason::ChangeTrigger => {
                        format!("Change trigger point [0-{}]", self.config.buffer_size)
                    }
                };

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

    /// Render the TUI
    pub fn render(&mut self) {
        match &mut self.state {
            TuiState::Main => self.render_main(),
            TuiState::InPrompt(_) => self.render_main(),
            TuiState::Predicates(state) => {
                state.render(&mut self.term);
            }
        }
    }

    /// Handle the keypresses. Returns wether or not it should break out of the main loop or not
    fn on_key_event<T: Read + Write>(&mut self, event: KeyEvent, tx_port: &mut T) -> KeyResponse {
        let response = match (&mut self.state, event.code, event.modifiers) {
            (_, KeyCode::Char('c'), KeyModifiers::CONTROL) => KeyResponse::QuitProgram,
            (TuiState::Main, KeyCode::Char('r'), _) => {
                if let Err(err) =
                    perform_register_operation(tx_port, self.config, &IlaRegisters::TriggerReset)
                {
                    self.log.push(format!("Error: {err}"));
                } else {
                    self.log.push("Re-armed trigger succesfully".into());
                }
                KeyResponse::Nothing
            }
            (TuiState::Main, KeyCode::Char('a'), _) => {
                self.auto_reset = !self.auto_reset;
                KeyResponse::Nothing
            }
            (TuiState::Main, KeyCode::Char(' '), _) => {
                self.triggered = match perform_register_operation(
                    tx_port,
                    &self.config,
                    &IlaRegisters::TriggerState,
                ) {
                    Ok(RegisterOutput::TriggerState(state)) => state,
                    _ => {
                        self.log.push("Unable to check for trigger status".into());
                        false
                    }
                };

                if !self.triggered {
                    self.log
                        .push("System is not triggered, refusing to read samples".into());
                } else {
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
                KeyResponse::Nothing
            }
            (TuiState::Main, KeyCode::Char('t'), _) => {
                self.state = TuiState::InPrompt(TextPromptState::new(
                    Some("0"),
                    PromptReason::ChangeTrigger,
                ));
                KeyResponse::Nothing
            }
            (TuiState::Main, KeyCode::Char('p'), _) => {
                if let Ok(predicate) =
                    IlaPredicate::from_ila(tx_port, self.config, PredicateTarget::Trigger)
                {
                    self.state = TuiState::Predicates(PredState::new(&self.config, predicate));
                } else {
                    self.log.push(
                        "Unable to retrieve current trigger predicate configuration from the ILA"
                            .to_string(),
                    );
                }
                KeyResponse::Nothing
            }
            (TuiState::Main, KeyCode::Char('c'), _) => {
                if let Ok(predicate) =
                    IlaPredicate::from_ila(tx_port, self.config, PredicateTarget::Capture)
                {
                    self.state = TuiState::Predicates(PredState::new(&self.config, predicate));
                } else {
                    self.log.push(
                        "Unable to retrieve current capture predicate configuration from the ILA"
                            .to_string(),
                    );
                }
                KeyResponse::Nothing
            }
            (TuiState::Main, KeyCode::Char('v'), _) => {
                self.state = TuiState::InPrompt(TextPromptState::new(
                    Some("dump.vcd"),
                    PromptReason::SaveVcd,
                ));
                KeyResponse::Nothing
            }
            (TuiState::InPrompt(_), KeyCode::Esc, _) => {
                self.log.push("Cancelled save".to_string());
                self.state = TuiState::Main;
                KeyResponse::Nothing
            }
            (TuiState::InPrompt(prompt), KeyCode::Enter, _) => {
                // Handle the case of whenever a prompt gets completed
                // I want to move this to a seperate function, however due to borrow limits I can't
                // and that's kind of very annoying
                let response = match prompt.reason {
                    PromptReason::SaveVcd => {
                        if let Some(sample) = self.captured.last() {
                            if let Err(err) =
                                write_to_vcd(sample, self.config, prompt.input.clone())
                            {
                                self.log.push(format!("Error when saving VCD: {}", err));
                            } else {
                                self.log.push("Saved succesfully!".to_string());
                            }
                        } else {
                            self.log.push("Nothing to save".to_string());
                        }
                        KeyResponse::Nothing
                    }
                    PromptReason::ChangeTrigger => match prompt.input.parse() {
                        Ok(n) if n > self.config.buffer_size as u32 => {
                            self.log
                                .push("Invalid input; must be specified range".to_string());
                            KeyResponse::Nothing
                        }
                        Ok(n) => {
                            let (msg, response) = match perform_register_operation(
                                tx_port,
                                self.config,
                                &IlaRegisters::TriggerPoint(n),
                            ) {
                                Ok(_) => (
                                    String::from("Trigger point change made"),
                                    KeyResponse::AppliedChanges,
                                ),
                                Err(err) => (format!("Error: {err}"), KeyResponse::Nothing),
                            };
                            self.log.push(msg);
                            response
                        }
                        Err(_) => {
                            self.log.push(
                                "Invalid input; must be a unsigned 32 bit number".to_string(),
                            );
                            KeyResponse::Nothing
                        }
                    },
                };

                self.state = TuiState::Main;
                response
            }
            (TuiState::InPrompt(text_prompt), keycode, _) => {
                text_prompt.handle_input(keycode);
                KeyResponse::Nothing
            }
            _ => KeyResponse::Nothing,
        };

        self.render();

        response
    }

    /// The main TUI loop
    ///
    /// Handles everything from rendering to the input of the TUI interface
    pub fn main_loop<T: Read + Write>(&mut self, mut tx_port: T) {
        self.render();

        loop {
            if let Ok(true) = poll(Duration::from_millis(100)) {
                let raw_event = read();
                let mut should_rearm = false;

                // This is structured a bit weirdly, due to me not expecting the TUI to be very
                // complicated at first. It expanded more than I initially anticipated
                //
                // Rewriting it would take quite some time, time I do not have at the moment
                if let TuiState::Predicates(state) = &mut self.state {
                    match raw_event {
                        Ok(ref event) => {
                            let stop_program =
                                state.handle_event(&mut tx_port, &self.config, event);
                            self.render();

                            match stop_program {
                                crate::predicates_tui::PredicateEventResponse::QuitProgram => {
                                    return
                                }
                                crate::predicates_tui::PredicateEventResponse::MainMenu((
                                    log,
                                    changes,
                                )) => {
                                    self.state = TuiState::Main;
                                    self.log.push(log);
                                    should_rearm = changes;
                                }
                                crate::predicates_tui::PredicateEventResponse::Nothing => continue,
                            }
                        }
                        Err(_) => (),
                    }
                }

                let event = match raw_event {
                    Ok(TuiEvent::Key(key)) => key,
                    Ok(TuiEvent::Resize(..)) => {
                        self.render();
                        continue;
                    }
                    _ => continue,
                };

                match self.on_key_event(event, &mut tx_port) {
                    KeyResponse::Nothing => (),
                    KeyResponse::QuitProgram => break,
                    KeyResponse::AppliedChanges => should_rearm = true,
                };

                if should_rearm && self.auto_reset {
                    let log_message = match perform_register_operation(
                        &mut tx_port,
                        &self.config,
                        &IlaRegisters::TriggerReset,
                    ) {
                        Ok(_) => "Auto-rearmed the trigger",
                        Err(_) => "Failed to auto-rearm the trigger",
                    };
                    self.log.push(log_message.into())
                }
            }

            let now = Instant::now();
            let duration = now.duration_since(self.last_trigger_check);
            if duration >= Duration::from_millis(500) {
                self.triggered = match perform_register_operation(
                    &mut tx_port,
                    &self.config,
                    &IlaRegisters::TriggerState,
                ) {
                    Ok(RegisterOutput::TriggerState(state)) => state,
                    _ => {
                        self.log.push("Unable to check for trigger status".into());
                        false
                    }
                };

                self.render();
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
