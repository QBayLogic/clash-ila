use std::{io::Stdout, ops::AddAssign};

use crate::{
    config::IlaSignal,
    ui::textinput::{TextPrompt, TextPromptState},
};
use crossterm::event::{Event, KeyCode, KeyEvent, KeyModifiers};
use num::{bigint::ParseBigIntError, BigUint, Num};
use ratatui::{
    layout::{Constraint, Flex, Layout, Margin, Offset, Rect},
    prelude::CrosstermBackend,
    style::Stylize,
    widgets::{Block, Borders, Tabs, WidgetRef},
    Frame, Terminal,
};

use crate::ui::checkbox::Checkbox;
use crate::ui::listbox::Listbox;

#[derive(Debug, Clone, PartialEq)]
pub enum PredicateOperation {
    And,
    Or,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PredicateTarget {
    Trigger,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IlaPredicates {
    pub target: PredicateTarget,
    pub operation: PredicateOperation,
    pub trigger_select: u32,
    pub mask: Vec<u8>,
    pub compare: Vec<u8>,
}

trait PredicatePage {
    /// Render the predicate page
    fn render(&self, state: &mut State, f: &mut Frame<'_>, area: Rect);
    /// Navigate the predicate page
    fn navigate(&self, state: &mut State, event: &Event);
}

/// An enum for determining in which base a user wrote numeric inputs too
///
/// This is indicates by the prefix of the number, if the number has no prefix it is assumed to be
/// base-10 (Decimal)
enum NumericState {
    Decimal,
    Hex,
    Octal,
    Binary,
}

impl NumericState {
    /// Create a `NumericState` from a string, will look at the first two characters to see if it
    /// has a prefix or not
    fn new(str: &str) -> Self {
        match str.get(0..2) {
            Some("0x") => NumericState::Hex,
            Some("0X") => NumericState::Hex,
            Some("0b") => NumericState::Binary,
            Some("0B") => NumericState::Binary,
            Some("08") => NumericState::Octal,
            _ => NumericState::Decimal,
        }
    }

    /// If this `NumericState` has a prefix to identify itself within a string
    fn has_prefix(&self) -> bool {
        match self {
            NumericState::Decimal => false,
            _ => true,
        }
    }

    /// Attempt to parse the number into a `BigUint` based on the `NumericState`
    fn parse(&self, str: &str) -> Result<BigUint, ParseBigIntError> {
        BigUint::from_str_radix(
            str,
            match self {
                NumericState::Decimal => 10,
                NumericState::Hex => 16,
                NumericState::Octal => 8,
                NumericState::Binary => 2,
            },
        )
    }

    /// A short hand function, combining both `new()` and `parse()` into one
    fn immediate_parse(str: &str) -> Result<BigUint, ParseBigIntError> {
        let state = Self::new(str);
        let non_prefix = match state.has_prefix() {
            true => str.get(2..).unwrap_or(""),
            false => str,
        };

        state.parse(non_prefix)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct GeneralPage;

impl PredicatePage for GeneralPage {
    fn render(&self, state: &mut State, f: &mut Frame<'_>, area: Rect) {
        let layout = Layout::new(
            ratatui::layout::Direction::Vertical,
            [
                Constraint::Length(2),
                Constraint::Length(1),
                Constraint::Length(3),
                Constraint::Length(1),
                Constraint::Fill(0),
            ],
        )
        .flex(ratatui::layout::Flex::Start)
        .spacing(0)
        .split(area);

        f.render_widget("General predicate settings".bold(), layout[0]);

        f.render_widget("Predicate operation", layout[1]);
        state
            .predicate_op_ui_state
            .render_ref(layout[2], f.buffer_mut());
        f.render_widget("Predicate Selector", layout[3]);
        state
            .predicate_select_ui_state
            .render_ref(layout[4], f.buffer_mut());
    }

    fn navigate(&self, state: &mut State, event: &Event) {
        let consumed = state.predicate_op_ui_state.handle_input(event)
            || state.predicate_select_ui_state.handle_input(event);

        if consumed {
            return;
        };

        #[derive(Debug, Clone, Copy)]
        enum Direction {
            Up,
            Down,
        }

        let direction = match event {
            Event::Key(KeyEvent {
                code: KeyCode::Up, ..
            }) => Direction::Up,
            Event::Key(KeyEvent {
                code: KeyCode::Down,
                ..
            }) => Direction::Down,
            _ => return,
        };

        state.selected = match (&state.selected, direction) {
            (Selected::Operation, Direction::Down) => Selected::Select,
            (Selected::Operation, Direction::Up) => Selected::Operation,
            (Selected::Select, Direction::Up) => Selected::Operation,
            (Selected::Select, Direction::Down) => Selected::Select,
        };

        match &state.selected {
            Selected::Operation => {
                state.predicate_op_ui_state.set_focus(true);
                state.predicate_op_ui_state.set_hover(usize::MAX);
            }
            Selected::Select => {
                state.predicate_select_ui_state.set_focus(true);
                state.predicate_select_ui_state.set_hover(0);
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct MaskComparePage {
    is_mask: bool,
}

impl MaskComparePage {
    fn get_input_states<'a>(&self, state: &'a mut State) -> &'a mut Vec<TextPromptState<usize>> {
        match self.is_mask {
            true => &mut state.mask_state,
            false => &mut state.compare_state,
        }
    }
}

impl PredicatePage for MaskComparePage {
    fn render(&self, state: &mut State, f: &mut Frame<'_>, area: Rect) {
        let layout = Layout::new(
            ratatui::layout::Direction::Vertical,
            [
                Constraint::Length(1),
                Constraint::Length(2),
                Constraint::Length(2),
                Constraint::Fill(0),
            ],
        )
        .flex(Flex::Start)
        .split(area);

        let title = match self.is_mask {
            true => "Predicate mask",
            false => "Predicate compare value",
        };
        let description = match self.is_mask {
            true => "Filter bits before data is given to the predicate, allows you to ignore bits you don't care about",
            false => "The value given to the predicate to compare the incoming data to",
        };

        f.render_widget(title.bold(), layout[0]);
        f.render_widget(description, layout[1]);

        let signals = state.signals;
        let is_valid =
            self.get_input_states(state)
                .iter()
                .zip(signals)
                .all(|(input_state, signal)| {
                    NumericState::immediate_parse(&input_state.input)
                        .is_ok_and(|uint| uint <= BigUint::new(vec![2]).pow(signal.width as u32))
                });
        let is_valid_label = match is_valid {
            true => "Provided input is valid".green(),
            false => "Provided input is INVALID".red(),
        };

        f.render_widget(is_valid_label, layout[2]);

        let base_area = layout[3];
        for (index, signal) in state.signals.iter().enumerate() {
            let element_active = index == state.mask_compare_cursor_position;

            let signal_label = format!(
                " {} - input range [0 - {:#x}] ",
                signal.name,
                BigUint::new(vec![2]).pow(signal.width as u32)
            );

            let borders = Block::new().title(signal_label).borders(Borders::ALL);

            let y_pos = base_area.y + index as u16 * 3;
            let border_area = Rect::new(base_area.x, y_pos, base_area.width, 3);
            let input_area = Rect::new(base_area.x + 2, y_pos + 1, base_area.width - 2, 1);
            let input_select = Rect::new(base_area.x, y_pos + 1, 2, 1);

            let input_select_text = if index == state.mask_compare_cursor_position {
                "> "
            } else {
                ""
            };

            f.render_widget(borders, border_area);
            f.render_widget(input_select_text, input_select);

            self.get_input_states(state)[index].render(input_area, f, element_active);
        }
    }

    fn navigate(&self, state: &mut State, event: &Event) {
        state.mask_compare_cursor_position = match event {
            Event::Key(KeyEvent {
                code: KeyCode::Up, ..
            }) => state.mask_compare_cursor_position.saturating_sub(1),
            Event::Key(KeyEvent {
                code: KeyCode::Down,
                ..
            }) => {
                (state.mask_compare_cursor_position + 1).min(self.get_input_states(state).len() - 1)
            }
            _ => state.mask_compare_cursor_position,
        };

        let Event::Key(KeyEvent {
            code: key_event, ..
        }) = event
        else {
            return;
        };

        let index = state.mask_compare_cursor_position;
        let input_states = self.get_input_states(state);
        let input_state = &mut input_states[index];

        input_state.handle_input(*key_event);
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Tab {
    General(GeneralPage),
    MaskCompare(MaskComparePage),
}

impl Tab {
    fn names() -> [&'static str; 3] {
        ["General", "Mask", "Compare"]
    }

    fn index(&self) -> usize {
        match self {
            Tab::General(_) => 0,
            Tab::MaskCompare(MaskComparePage { is_mask: true }) => 1,
            Tab::MaskCompare(MaskComparePage { is_mask: false }) => 2,
        }
    }

    fn prev(&self) -> Tab {
        match self {
            Tab::General(_) => Tab::MaskCompare(MaskComparePage { is_mask: false }),
            Tab::MaskCompare(MaskComparePage { is_mask: true }) => Tab::General(GeneralPage),
            Tab::MaskCompare(MaskComparePage { is_mask: false }) => {
                Tab::MaskCompare(MaskComparePage { is_mask: true })
            }
        }
    }

    fn next(&self) -> Tab {
        match self {
            Tab::General(_) => Tab::MaskCompare(MaskComparePage { is_mask: true }),
            Tab::MaskCompare(MaskComparePage { is_mask: true }) => {
                Tab::MaskCompare(MaskComparePage { is_mask: false })
            }
            Tab::MaskCompare(MaskComparePage { is_mask: false }) => Tab::General(GeneralPage),
        }
    }

    fn render(&self, state: &mut State, f: &mut Frame<'_>, area: Rect) {
        match self {
            Tab::General(page) => page.render(state, f, area),
            Tab::MaskCompare(page) => page.render(state, f, area),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Selected {
    Operation,
    Select,
}

#[derive(Debug, Clone)]
pub struct State<'a> {
    tab: Tab,
    selected: Selected,
    predicates: IlaPredicates,
    predicate_op_ui_state: Listbox,
    predicate_select_ui_state: Checkbox,
    signals: &'a [IlaSignal],
    mask_compare_cursor_position: usize,
    mask_state: Vec<TextPromptState<usize>>,
    compare_state: Vec<TextPromptState<usize>>,
}

impl<'a> State<'_> {
    pub fn new(predicate_names: Vec<String>, signals: &[IlaSignal]) -> State {
        State {
            tab: Tab::General(GeneralPage),
            selected: Selected::Operation,
            predicates: IlaPredicates {
                target: PredicateTarget::Trigger,
                operation: PredicateOperation::Or,
                trigger_select: 0,
                mask: Vec::new(),
                compare: Vec::new(),
            },
            predicate_op_ui_state: {
                let mut list = Listbox::new(vec!["OR", "AND"], 0);
                list.set_focus(true);
                list
            },
            predicate_select_ui_state: Checkbox::new(predicate_names),
            signals,
            mask_compare_cursor_position: 0,
            mask_state: vec![TextPromptState::new(None::<String>, 0_usize); signals.len()],
            compare_state: vec![TextPromptState::new(None::<String>, 0_usize); signals.len()],
        }
    }

    pub fn render(&mut self, terminal: &mut Terminal<CrosstermBackend<Stdout>>) {
        let _ = terminal.draw(|f| {
            let max_area = f.area();
            let tabs = Tabs::new(Tab::names()).select(self.tab.index());
            f.render_widget(tabs, max_area);

            let tab_area = max_area.inner(Margin::new(0, 1));
            f.render_widget(Block::new().borders(Borders::ALL), tab_area);

            self.tab
                .clone() // Pesky borrow
                .render(self, f, tab_area.inner(Margin::new(1, 1)));
        });
    }

    pub fn next_tab(&mut self) {
        self.tab = self.tab.next();
    }

    pub fn prev_tab(&mut self) {
        self.tab = self.tab.prev();
    }

    fn manage_page_navigation(&mut self, event: &Event) {
        match self.tab {
            Tab::General(page) => page.navigate(self, event),
            Tab::MaskCompare(page) => page.navigate(self, event),
        }
    }

    pub fn handle_event(&mut self, event: &Event) -> bool {
        if match event {
            Event::Key(KeyEvent {
                code: KeyCode::Char('c'),
                modifiers: KeyModifiers::CONTROL,
                ..
            }) => {
                return true;
            }
            Event::Key(KeyEvent {
                code: KeyCode::Left,
                modifiers: KeyModifiers::CONTROL,
                ..
            }) => {
                self.prev_tab();
                true
            }
            Event::Key(KeyEvent {
                code: KeyCode::Right,
                modifiers: KeyModifiers::CONTROL,
                ..
            }) => {
                self.next_tab();
                true
            }
            _ => false,
        } {
            return false;
        }

        self.manage_page_navigation(event);

        false
    }
}
