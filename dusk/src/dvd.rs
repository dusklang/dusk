// LICENSE INFO: Some of the code in this file is taken from imgui-rs sample code, which is licensed under MIT:
//
//     Copyright (c) 2021 The imgui-rs Developers
//
//     Permission is hereby granted, free of charge, to any person obtaining a copy
//     of this software and associated documentation files (the "Software"), to deal
//     in the Software without restriction, including without limitation the rights
//     to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
//     copies of the Software, and to permit persons to whom the Software is
//     furnished to do so, subject to the following conditions:
//
//     The above copyright notice and this permission notice shall be included in all
//     copies or substantial portions of the Software.
//
//     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//     IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//     FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//     AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//     LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
//     OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
//     SOFTWARE.
//

use std::collections::HashSet;
use std::time::{Instant, Duration};
use std::sync::mpsc::{self, Receiver, Sender};
use std::cmp::max;

use glium::glutin;
use glium::glutin::event::{Event, WindowEvent};
use glium::glutin::event_loop::{ControlFlow, EventLoop};
use glium::glutin::window::WindowBuilder;
use glium::{Display, Surface};
use imgui::*;
use imgui::sys::igGetMainViewport;
use imgui_glium_renderer::Renderer;
use imgui_winit_support::{HiDpiMode, WinitPlatform};
use interprocess::local_socket::LocalSocketStream;
use libdusk::source_info::SourceFileLocation;
use libdusk::index_vec::*;
use libdusk::dvd::socket_name;
use rect_packer::Packer;
use libdusk::dire::ast::ItemId;

use libdusk::dvd::{Message, Response, self};

enum MessageState {
    Paused { last_message: Option<Message>, },
    Running,
    RunningUntilNextCompilationState,
    CompilerHasExit,
}

#[derive(PartialEq, Clone, Debug)]
enum CompilationState {
    Begun,
    AddingBuiltins,
    ParsingInputFile(SourceFileLocation),
    AddingType1Dependencies,
    AddingMetaDependencies,
}

struct Item {
    text: String,
}

enum ScrollTarget {
    Item(ItemId),
    Dependency(ItemId, ItemId),
}

struct UiState {
    running: bool,
    scrolling: [f32; 2],
    items: IndexVec<ItemId, Item>,
    undirected_edges: IndexVec<ItemId, Vec<ItemId>>,
    directed_edges: IndexVec<ItemId, Vec<ItemId>>,
    metadependencies: IndexVec<ItemId, Vec<ItemId>>,

    rx: Receiver<Message>,
    tx: Sender<Response>,

    message_state: MessageState,
    compilation_state: CompilationState,

    scroll_to_new_items: bool,
    scroll_target: Option<ScrollTarget>,
    id_jump: i32,
}

impl UiState {
    fn new(rx: Receiver<Message>, tx: Sender<Response>) -> Self {
        Self {
            running: true,
            scrolling: [0.0, 0.0],
            items: Default::default(),
            undirected_edges: Default::default(),
            directed_edges: Default::default(),
            metadependencies: Default::default(),
            rx,
            tx,

            message_state: MessageState::Paused { last_message: None },
            compilation_state: CompilationState::Begun,

            scroll_to_new_items: true,
            scroll_target: None,
            id_jump: 0,
        }
    }

    fn is_scrolling_to(&self, item: ItemId) -> bool {
        if let Some(target) = &self.scroll_target {
            match *target {
                ScrollTarget::Item(target) => item == target,
                ScrollTarget::Dependency(a, b) => item == a || item == b,
            }
        } else {
            false
        }
    }
}

/// Packs rectangles into an infinitely-sized canvas.
/// Traffics in `f32`s for better interop with code in this module, but internally truncates them to integers.
struct InfinitePacker {
    packer: Packer,
    state: PackerState,
    stage: usize,
    canvas_width: f32,
    canvas_height: f32,
}

#[derive(Copy, Clone)]
enum PackerState {
    Side(usize),
    Bottom(usize),
    Corner,
}

impl InfinitePacker {
    fn new(canvas_width: f32, canvas_height: f32) -> Self {
        let config = rect_packer::Config {
            width: canvas_width as i32,
            height: canvas_height as i32,

            border_padding: 12,
            rectangle_padding: 24,
        };
        Self {
            packer: Packer::new(config),
            state: PackerState::Corner,
            stage: 0,

            canvas_width,
            canvas_height,
        }
    }

    fn next_packer(&mut self) {
        self.packer = Packer::new(self.packer.config());
        match &mut self.state {
            PackerState::Bottom(index) => {
                *index += 1;
                if *index >= self.stage {
                    self.state = PackerState::Side(0);
                }
            },
            PackerState::Side(index) => {
                *index += 1;
                if *index >= self.stage {
                    self.state = PackerState::Corner;
                }
            },
            PackerState::Corner => {
                self.stage += 1;
                self.state = PackerState::Bottom(0);
            },
        }
    }

    fn grow_canvas(&mut self) {
        let scaling_factor = max(2, self.stage + 1);
        self.canvas_width *= scaling_factor as f32;
        self.canvas_height *= scaling_factor as f32;
        self.stage = 1;
        self.state = PackerState::Bottom(0);

        let config = rect_packer::Config {
            width: self.canvas_width as i32,
            height: self.canvas_height as i32,
            ..self.packer.config()
        };

        self.packer = Packer::new(config);
    }

    fn pack(&mut self, width: f32, height: f32) -> [f32; 2] {
        let pos = if let Some(pos) = self.packer.pack(width as i32, height as i32, false) {
            pos
        } else {
            self.next_packer();
            loop {
                if let Some(pos) = self.packer.pack(width as i32, height as i32, false) {
                    break pos;
                } else {
                    self.grow_canvas();
                }
            }
        };

        let [x_offset, y_offset] = match self.state {
            PackerState::Bottom(index) => {
                [self.canvas_width * index as f32, self.canvas_height * self.stage as f32]
            },
            PackerState::Side(index) => {
                [self.canvas_width * self.stage as f32, self.canvas_height * index as f32]
            },
            PackerState::Corner => {
                [self.canvas_width * self.stage as f32, self.canvas_height * self.stage as f32]
            },
        };

        [pos.x as f32 + x_offset, pos.y as f32 + y_offset]
    }
}

fn run_ui(state: &mut UiState, ui: &mut Ui) {
    let (pos, size) = unsafe {
        let viewport = igGetMainViewport();
        ((*viewport).WorkPos, (*viewport).WorkSize)
    };
    Window::new("Dusk Visual Debugger")
        .size([size.x, size.y], Condition::Always)
        .position([pos.x, pos.y], Condition::Always)
        .resizable(false)
        .collapsible(false)
        .title_bar(false)
        .build(ui, || {
            state.scroll_target = None;
            let mut scroll_override = false;
            fn handle_message(message: &Message, state: &mut UiState) {
                match *message {
                    Message::WillBeginAddingBuiltins => {
                        state.compilation_state = CompilationState::AddingBuiltins;
                    },
                    Message::WillBeginParsingInputFile(ref location) => {
                        state.compilation_state = CompilationState::ParsingInputFile(location.clone());
                    },
                    Message::WillAddType1Dependencies => {
                        state.compilation_state = CompilationState::AddingType1Dependencies;
                    },
                    Message::WillAddMetaDependencies => {
                        state.compilation_state = CompilationState::AddingMetaDependencies;
                    },
                    Message::WillExit => {
                        state.message_state = MessageState::CompilerHasExit;
                    },
                    Message::DidAddExpr { id, item_id, ref text } => {
                        let text = format!("expr{}:\n{}", id.index(), text);
                        state.items.push_at(item_id, Item { text });
                        state.scroll_target = Some(ScrollTarget::Item(item_id));
                    },
                    Message::DidAddDecl { id, item_id, ref text } => {
                        let text = format!("decl{}:\n{}", id.index(), text);
                        state.items.push_at(item_id, Item { text });
                        state.scroll_target = Some(ScrollTarget::Item(item_id));
                    },
                    Message::DidAddTirType1Dependency { depender, dependee } => {
                        state.undirected_edges.resize_with(state.items.len(), Default::default);
                        state.directed_edges.resize_with(state.items.len(), Default::default);
                        state.undirected_edges[depender].push(dependee);
                        state.undirected_edges[dependee].push(depender);
                        state.directed_edges[depender].push(dependee);

                        state.scroll_target = Some(ScrollTarget::Dependency(depender, dependee));
                    },
                    Message::DidAddTirMetaDependency { depender, dependee } => {
                        state.metadependencies.resize_with(state.items.len(), Default::default);
                        state.metadependencies[depender].push(dependee);

                        state.scroll_target = Some(ScrollTarget::Dependency(depender, dependee));
                    },
                    _ => {},
                }
            }
            match &mut state.message_state {
                MessageState::Paused { last_message } => {
                    let mut new_message = None;
                    if last_message.is_none() {
                        if let Ok(message) = state.rx.recv_timeout(Duration::from_millis(1)) {
                            *last_message = Some(message);
                            new_message = last_message.clone();
                        }
                    }
                    if let Some(message) = last_message {
                        ui.text_wrapped(format!("Message: {:?}", message));
                    } else {
                        ui.text_wrapped("Message: no message");
                    };
                    ui.text_wrapped(format!("Compilation state: {:?}", state.compilation_state));
                    if ui.button("Next") && last_message.is_some() {
                        state.tx.send(Response::Continue).unwrap();
                        *last_message = None; // Remove message because we don't want to send multiple Continue responses
                    }
                    ui.checkbox("Scroll to new items / dependencies", &mut state.scroll_to_new_items);
                    if ui.button("Run") {
                        if last_message.is_some() {
                            state.tx.send(Response::Continue).unwrap();
                        }
                        state.message_state = MessageState::Running;
                    } else if ui.button("Skip to next compilation state") {
                        if last_message.is_some() {
                            state.tx.send(Response::Continue).unwrap();
                        }
                        state.message_state = MessageState::RunningUntilNextCompilationState;
                    }
                    if ui.input_int("Scroll to item id", &mut state.id_jump).enter_returns_true(true).build() {
                        if let Ok(jump_to) = state.id_jump.try_into() {
                            let id = ItemId::from_usize(jump_to);
                            if id.index() < state.items.len() {
                                state.scroll_target = Some(ScrollTarget::Item(id));
                                scroll_override = true;
                            }
                        }
                    }
                    if let Some(new_message) = new_message {
                        handle_message(&new_message, state);
                    }
                },
                MessageState::Running => {
                    // In the worst case, this could take over a second, causing the FPS to drop to a slide show.
                    // However, that would require 1000 messages in a row to take close to (but not more than)
                    // 1 millisecond, and messages tend to arrive much more frequently than that.
                    //
                    // In summary: this is a little iffy, but probably fine in practice.
                    for _ in 0..1000 {
                        if let Ok(message) = state.rx.recv_timeout(Duration::from_millis(1)) {
                            state.tx.send(Response::Continue).unwrap();
                            handle_message(&message, state);
                        } else {
                            break;
                        }
                    }
                    ui.text_wrapped("Running...");
                    if ui.button("Pause") {
                        state.message_state = MessageState::Paused { last_message: None };
                    }
                },
                MessageState::RunningUntilNextCompilationState => {
                    // In the worst case, this could take over a second, causing the FPS to drop to a slide show.
                    // However, that would require 1000 messages in a row to take close to (but not more than)
                    // 1 millisecond, and messages tend to arrive much more frequently than that.
                    //
                    // In summary: this is a little iffy, but probably fine in practice.
                    let initial_compilation_state = state.compilation_state.clone();
                    let mut transition_message = None;
                    for _ in 0..1000 {
                        if let Ok(message) = state.rx.recv_timeout(Duration::from_millis(1)) {
                            handle_message(&message, state);
                            if state.compilation_state != initial_compilation_state {
                                transition_message = Some(message);
                                break;
                            } else {
                                state.tx.send(Response::Continue).unwrap();
                            }
                        } else {
                            break;
                        }
                    }
                    if transition_message.is_some() {
                        state.message_state = MessageState::Paused { last_message: transition_message };
                    } else {
                        ui.text_wrapped("Running...");
                        if ui.button("Pause") {
                            state.message_state = MessageState::Paused { last_message: None };
                        }
                    }
                },
                MessageState::CompilerHasExit => {
                    ui.text_wrapped("Compiler has exit.");
                }
            }
            ui.text_wrapped("Drag with right mouse button");
            ui.text_wrapped("Use mouse wheel to scroll vertically");
            ui.text_wrapped("Hold CTRL while using mouse wheel to scroll horizontally");
            let p0 = ui.cursor_screen_pos();
            let mut canvas_size = ui.content_region_avail();
            for size in &mut canvas_size {
                if *size < 50.0 {
                    *size = 50.0;
                }
            }
            let p1 = [p0[0] + canvas_size[0], p0[1] + canvas_size[1]];
            let io = ui.io();
            let draw_list = ui.get_window_draw_list();
            draw_list.add_rect(p0, p1, ImColor32::from_rgb(50, 50, 50)).filled(true).build();
            draw_list.add_rect(p0, p1, ImColor32::WHITE).build();

            ui.invisible_button_flags("canvas", canvas_size, ButtonFlags::MOUSE_BUTTON_LEFT | ButtonFlags::MOUSE_BUTTON_RIGHT);
            let is_hovered = ui.is_item_hovered();
            let is_active = ui.is_item_active();

            if is_active && ui.is_mouse_dragging_with_threshold(MouseButton::Right, 0.0) {
                state.scrolling[0] += io.mouse_delta[0];
                state.scrolling[1] += io.mouse_delta[1];
            }
            if is_hovered {
                state.scrolling[!io.key_ctrl as usize] += io.mouse_wheel * 20.0;
                state.scrolling[io.key_ctrl as usize]  -= io.mouse_wheel_h * 20.0;
            }

            const GRID_STEP: f32 = 50.0;
            draw_list.with_clip_rect(p0, p1, || {
                let mut visited = IndexVec::new();
                visited.resize(state.items.len(), false);

                let mut components = Vec::new();
                fn find_component(state: &UiState, visited: &mut IndexVec<ItemId, bool>, component: &mut Vec<ItemId>, item: ItemId) -> bool {
                    if visited[item] { return false; }
                    visited[item] = true;

                    component.push(item);

                    for &edge in &state.undirected_edges[item] {
                        find_component(state, visited, component, edge);
                    }

                    return true;
                }

                state.undirected_edges.resize_with(state.items.len(), Default::default);
                state.directed_edges.resize_with(state.items.len(), Default::default);

                for (id, _item) in state.items.iter_enumerated() {
                    let mut component = Vec::new();
                    if find_component(state, &mut visited, &mut component, id) {
                        components.push(component);
                    }
                }

                let mut positions = IndexVec::<ItemId, [f32; 2]>::new();
                positions.resize(state.items.len(), [0.0, 0.0]);

                let mut levels = IndexVec::<ItemId, u32>::new();
                levels.resize(state.items.len(), 0);
                for visited in &mut visited {
                    *visited = false;
                }

                fn get_level(state: &UiState, visited: &mut IndexVec<ItemId, bool>, levels: &mut IndexVec<ItemId, u32>, item: ItemId) -> u32 {
                    if !visited[item] {
                        visited[item] = true;
                        levels[item] = state.directed_edges[item].iter().map(|&dependency| {
                            get_level(state, visited, levels, dependency) + 1
                        }).max().unwrap_or(0);
                    }
                    levels[item]
                }

                const HORI_SPACING: f32 = 5.0;
                const VERT_SPACING: f32 = 15.0;
                const ITEM_HORI_TEXT_MARGIN: f32 = 10.0;

                // Compute the size of each item
                let mut item_sizes = IndexVec::<ItemId, [f32; 2]>::new();
                item_sizes.resize(state.items.len(), [0.0, 0.0]);
                for ([width, height], item) in item_sizes.iter_mut().zip(&state.items) {
                    let text_lines: Vec<_> = item.text.lines().collect();
                    *width = 7.0 * text_lines.iter().map(|line| line.len()).max().unwrap() as f32 + ITEM_HORI_TEXT_MARGIN;
                    *height = 3.0 + 13.0 * text_lines.len() as f32;
                }

                // Compute the position of each item
                let mut packer = InfinitePacker::new(canvas_size[0], canvas_size[1]);
                for component in &mut components {
                    let max_level = component.iter().map(|&item| get_level(state, &mut visited, &mut levels, item)).max().unwrap();
                    // Deal with items in decreasing level order (top to bottom)
                    component.sort_by_key(|&item| u32::MAX - levels[item]);
                    // TODO: reuse memory
                    let mut level_widths = Vec::new();
                    level_widths.resize(max_level as usize + 1, 0.0f32);
                    // Vertical offset of the current level, relative to the top of the component
                    let mut level_y_offset: f32 = 0.0;
                    let mut prev_level = max_level;
                    // max height of an item in the current level
                    let mut max_height: f32 = f32::NEG_INFINITY;

                    // Compute positions of each item relative to to the top-left of the component's bounding box
                    for &item in &*component {
                        let level = levels[item];
                        // Check whether we moved on to a new level.
                        // (Because of the sort_by_key() call above, all items in a given level are guaranteed to be
                        // together)
                        if level != prev_level {
                            level_y_offset += max_height + VERT_SPACING;
                            max_height = f32::NEG_INFINITY;
                            prev_level = level;
                        }
                        let [width, height] = item_sizes[item];
                        max_height = f32::max(height, max_height);

                        let level_width = &mut level_widths[level as usize];
                        if *level_width != 0.0 {
                            *level_width += HORI_SPACING;
                        }
                        let x_pos = *level_width;
                        let y_pos = level_y_offset;
                        positions[item] = [x_pos, y_pos];
                        *level_width += width;
                    }

                    let component_bb_width = level_widths.iter().copied().reduce(f32::max).unwrap();
                    let component_bb_height = level_y_offset + max_height;

                    // Horizontally center each level within its component, by first calculating horizontal offsets for each level:
                    let mut center_offsets = Vec::new();
                    center_offsets.reserve(level_widths.len());
                    for &width in &level_widths {
                        center_offsets.push((component_bb_width - width) / 2.0);
                    }
                    // Then applying the level offsets to each item's position
                    for &item in &*component {
                        let level = levels[item];
                        positions[item][0] += center_offsets[level as usize];
                    }

                    let [x, y] = packer.pack(component_bb_width, component_bb_height);
                    for &item in &*component {
                        positions[item][0] += x as f32;
                        positions[item][1] += y as f32;
                    }
                }
                let mut highlighted_item = None;
                const HIGHLIGHT_COLOR: ImColor32 = ImColor32::from_rgb(255, 0, 0);
                fn draw_edges(draw_list: &DrawListMut, origin: [f32; 2], state: &UiState, positions: &IndexVec<ItemId, [f32; 2]>, sizes: &IndexVec<ItemId, [f32; 2]>, item: ItemId, color: ImColor32, thickness: f32) {
                    macro_rules! adjust {
                        ($x:expr, $y: expr) => {{
                            [$x + origin[0], $y + origin[1]]
                        }}
                    }
                    let pos = positions[item];
                    let size = sizes[item];
                    for &dep in &state.directed_edges[item] {
                        let dep_pos = positions[dep];
                        let dep_size = sizes[dep];
                        draw_list.add_line(adjust!(pos[0] + size[0] / 2.0, pos[1] + size[1]), adjust!(dep_pos[0] + dep_size[0] / 2.0, dep_pos[1]), color).thickness(thickness).build();
                    }
                }


                // Scroll to new item, if necessary
                if state.scroll_to_new_items || scroll_override {
                    if let Some(scroll_target) = &state.scroll_target {
                        let mut scroll_offset = [-state.scrolling[0], -state.scrolling[1]];
                        let bottom_right = |item: ItemId| -> [f32; 2] {
                            let top_left = positions[item];
                            let item_size = item_sizes[item];
                            [top_left[0] + item_size[0], top_left[1] + item_size[1]]
                        };
                        let (top_left, bottom_right) = match *scroll_target {
                            ScrollTarget::Item(item) => {
                                (positions[item], bottom_right(item))
                            },
                            ScrollTarget::Dependency(a, b) => {
                                let a_top_left = positions[a];
                                let b_top_left = positions[b];
                                let a_bottom_right = bottom_right(a);
                                let b_bottom_right = bottom_right(b);
                                let top_left = [
                                    f32::min(a_top_left[0], b_top_left[0]),
                                    f32::min(a_top_left[1], b_top_left[1]),
                                ];
                                let bottom_right = [
                                    f32::max(a_bottom_right[0], b_bottom_right[0]),
                                    f32::max(a_bottom_right[1], b_bottom_right[1]),
                                ];
                                (top_left, bottom_right)
                            },
                        };
                        
                        const MARGIN: f32 = 12.0;
                        if top_left[0] < scroll_offset[0] {
                            scroll_offset[0] = top_left[0] - MARGIN;
                        } else if scroll_offset[0] + canvas_size[0] < bottom_right[0] {
                            scroll_offset[0] = bottom_right[0] - canvas_size[0] + MARGIN;
                        }
    
                        if top_left[1] < scroll_offset[1] {
                            scroll_offset[1] = top_left[1] - MARGIN;
                        } else if scroll_offset[1] + canvas_size[1] < bottom_right[1] {
                            scroll_offset[1] = bottom_right[1] - canvas_size[1] + MARGIN;
                        }
    
                        state.scrolling[0] = -scroll_offset[0];
                        state.scrolling[1] = -scroll_offset[1];
                    }
                }

                let origin = [p0[0] + state.scrolling[0], p0[1] + state.scrolling[1]];
                macro_rules! adjust {
                    ($x:expr, $y: expr) => {{
                        [$x + origin[0], $y + origin[1]]
                    }}
                }

                {
                    // Draw horizontal grid lines
                    let mut x = state.scrolling[0] % GRID_STEP;
                    let line_colour = ImColor32::from_rgba(200, 200, 200, 40);
                    while x < canvas_size[0] {
                        draw_list.add_line([p0[0] + x, p0[1]], [p0[0] + x, p1[1]], line_colour).build();
                        x += GRID_STEP;
                    }

                    // Draw vertical grid lines
                    let mut y = state.scrolling[1] % GRID_STEP;
                    while y < canvas_size[0] {
                        draw_list.add_line([p0[0], p0[1] + y], [p1[0], p0[1] + y], line_colour).build();
                        y += GRID_STEP;
                    }
                }

                // Draw metadependency edges
                for (a, metadependencies) in state.metadependencies.iter_enumerated() {
                    for &b in metadependencies {
                        let a_pos = positions[a];
                        let a_size = item_sizes[a];
                        let b_pos = positions[b];
                        let b_size = item_sizes[b];

                        // This code would make some dependencies look nicer, but would also make it ambiguous which item is dependent vs. being depended on 
                        // if b_pos[1] < a_pos[1] {
                        //     swap(&mut a_pos, &mut b_pos);
                        //     swap(&mut a_size, &mut b_size);
                        // }

                        draw_list.add_line(adjust!(a_pos[0] + a_size[0] / 2.0, a_pos[1] + a_size[1]), adjust!(b_pos[0] + b_size[0] / 2.0, b_pos[1]), ImColor32::from_rgb(0, 255, 0)).thickness(5.0).build();
                    }
                }
                for component in components {
                    let mut depended_items = HashSet::new();
                    for item in component {
                        let pos = positions[item];
                        let size = item_sizes[item];
                        let rect = [
                            adjust!(pos[0], pos[1]),
                            adjust!(pos[0] + size[0], pos[1] + size[1]),
                        ];
                        let hovered =
                            ui.is_mouse_hovering_rect(rect[0], rect[1]) &&
                            ui.is_mouse_hovering_rect(p0, p1);
                        let color = if hovered {
                            highlighted_item = Some(item);
                            depended_items.extend(state.directed_edges[item].iter().copied());
                            HIGHLIGHT_COLOR
                        } else if depended_items.contains(&item) {
                            ImColor32::from_rgb(200, 100, 100)
                        } else if state.is_scrolling_to(item) {
                            ImColor32::from_rgb(100, 200, 100)
                        } else {
                            ImColor32::WHITE
                        };
                        draw_list.add_rect(rect[0], rect[1], color).filled(true).rounding(5.0).build();
                        draw_list.add_text([rect[0][0] + ITEM_HORI_TEXT_MARGIN / 2.0, rect[0][1]], ImColor32::BLACK, &state.items[item].text);
                        if !hovered {
                            draw_edges(&draw_list, origin, state, &positions, &item_sizes, item, ImColor32::WHITE, 1.0);
                        }
                    }
                }
                if let Some(item) = highlighted_item {
                    draw_edges(&draw_list, origin, state, &positions, &item_sizes, item, HIGHLIGHT_COLOR, 2.0);
                }
            });
        });
    
}

pub fn dvd_main() {
    let event_loop = EventLoop::new();
    let context = glutin::ContextBuilder::new().with_vsync(true);
    let builder = WindowBuilder::new()
        .with_title("Dusk Visual Debugger")
        .with_inner_size(glutin::dpi::LogicalSize::new(1024f64, 768f64));
    let display = Display::new(builder, context, &event_loop).expect("Failed to initialize display");

    let mut imgui = Context::create();
    imgui.set_ini_filename(None);

    let (msg_tx, msg_rx) = mpsc::channel();
    let (resp_tx, resp_rx) = mpsc::channel();
    std::thread::spawn(move || {
        let mut stream = LocalSocketStream::connect(socket_name()).unwrap();
        while let Ok(message) = dvd::receive_value(&mut stream) {
            msg_tx.send(message).unwrap();
            let response: Response = resp_rx.recv().unwrap();
            dvd::send_value(&mut stream, response);
        }
    });

    let mut platform = WinitPlatform::init(&mut imgui);
    {
        let gl_window = display.gl_window();
        let window = gl_window.window();

        let dpi_mode = if let Ok(factor) = std::env::var("IMGUI_EXAMPLE_FORCE_DPI_FACTOR") {
            // Allow forcing of HiDPI factor for debugging purposes
            match factor.parse::<f64>() {
                Ok(f) => HiDpiMode::Locked(f),
                Err(e) => panic!("Invalid scaling factor: {}", e),
            }
        } else {
            HiDpiMode::Default
        };

        platform.attach_window(imgui.io_mut(), window, dpi_mode);
    }

    let mut renderer = Renderer::init(&mut imgui, &display).expect("Failed to initialize renderer");
    let mut last_frame = Instant::now();
    let mut state = UiState::new(msg_rx, resp_tx);
    state.running = true;
    event_loop.run(move |event, _, control_flow| match event {
        Event::NewEvents(_) => {
            let now = Instant::now();
            imgui.io_mut().update_delta_time(now - last_frame);
            last_frame = now;
        }
        Event::MainEventsCleared => {
            let gl_window = display.gl_window();
            platform
                .prepare_frame(imgui.io_mut(), gl_window.window())
                .expect("Failed to prepare frame");
            gl_window.window().request_redraw();
        }
        Event::RedrawRequested(_) => {
            let mut ui = imgui.frame();

            run_ui(&mut state, &mut ui);
            if !state.running {
                *control_flow = ControlFlow::Exit;
            }

            let gl_window = display.gl_window();
            let mut target = display.draw();
            target.clear_color_srgb(1.0, 1.0, 1.0, 1.0);
            platform.prepare_render(&mut ui, gl_window.window());
            let draw_data = ui.render();
            renderer
                .render(&mut target, draw_data)
                .expect("Rendering failed");
            target.finish().expect("Failed to swap buffers");
        }
        Event::WindowEvent {
            event: WindowEvent::CloseRequested,
            ..
        } => *control_flow = ControlFlow::Exit,
        event => {
            let gl_window = display.gl_window();
            platform.handle_event(imgui.io_mut(), gl_window.window(), &event);
        }
    });
}
