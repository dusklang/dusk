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
use libdusk::index_vec::*;
use dusk_dire::hir::ItemId;

use libdusk::dvd::{Message, Response, self};

enum MessageState {
    Paused { last_message: Option<Message>, },
    Running,
    CompilerHasExit,
}

struct Item;

struct UiState {
    running: bool,
    scrolling: [f32; 2],
    items: IndexVec<ItemId, Item>,
    undirected_edges: IndexVec<ItemId, Vec<ItemId>>,
    directed_edges: IndexVec<ItemId, Vec<ItemId>>,

    rx: Receiver<Message>,
    tx: Sender<Response>,

    message_state: MessageState,
}

impl UiState {
    fn new(rx: Receiver<Message>, tx: Sender<Response>) -> Self {
        Self {
            running: true,
            scrolling: [0.0, 0.0],
            items: Default::default(),
            undirected_edges: Default::default(),
            directed_edges: Default::default(),
            rx,
            tx,

            message_state: MessageState::Paused { last_message: None },
        }
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
            fn handle_message(message: &Message, state: &mut UiState) {
                match *message {
                    Message::WillExit => {
                        state.message_state = MessageState::CompilerHasExit;
                    },
                    Message::DidAddExpr { item_id, .. } => state.items.push_at(item_id, Item),
                    Message::DidAddDecl { item_id, .. } => state.items.push_at(item_id, Item),
                    Message::DidAddTirType1Dependency { depender, dependee } => {
                        state.undirected_edges.resize_with(state.items.len(), Default::default);
                        state.directed_edges.resize_with(state.items.len(), Default::default);
                        state.undirected_edges[depender].push(dependee);
                        state.undirected_edges[dependee].push(depender);
                        state.directed_edges[depender].push(dependee);
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
                    if ui.button("Next") && last_message.is_some() {
                        state.tx.send(Response::Continue).unwrap();
                        *last_message = None; // Remove message because we don't want to send multiple Continue responses
                    }
                    if ui.button("Run") {
                        if last_message.is_some() {
                            state.tx.send(Response::Continue).unwrap();
                        }
                        state.message_state = MessageState::Running;
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

            let origin = [p0[0] + state.scrolling[0], p0[1] + state.scrolling[1]];
            // let mouse_pos_in_canvas = [io.mouse_pos[0] - origin[0], io.mouse_pos[1] - origin[1]];

            if is_active && ui.is_mouse_dragging_with_threshold(MouseButton::Right, 0.0) {
                state.scrolling[0] += io.mouse_delta[0];
                state.scrolling[1] += io.mouse_delta[1];
            }
            if is_hovered {
                state.scrolling[!io.key_ctrl as usize] += io.mouse_wheel * 20.0;
                state.scrolling[io.key_ctrl as usize]  -= io.mouse_wheel_h * 20.0;
            }

            macro_rules! adjust {
                ($x:expr, $y: expr) => {{
                    [$x + origin[0], $y + origin[1]]
                }}
            }

            const GRID_STEP: f32 = 50.0;
            draw_list.with_clip_rect(p0, p1, || {
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

                let mut single_items = Vec::new();
                for (id, _item) in state.items.iter_enumerated() {
                    let mut component = Vec::new();
                    if find_component(state, &mut visited, &mut component, id) {
                        if component.len() == 1 {
                            single_items.extend(component);
                        } else {
                            components.push(component);
                        }
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
                const ITEM_VERT_SIZE: f32 = 15.0;
                const ITEM_HORI_TEXT_MARGIN: f32 = 10.0;
                let mut item_sizes = IndexVec::<ItemId, [f32; 2]>::new();
                item_sizes.resize(state.items.len(), [0.0, 0.0]);
                let text = "This is some really long text!";
                for [width, _height] in &mut item_sizes {
                    *width = 7.14 * text.len() as f32 + ITEM_HORI_TEXT_MARGIN;
                }
                // The horizontal offset of the current component, relative to the far left of the canvas
                let mut x_offset: f32 = 10.0;
                for component in &mut components {
                    component.sort_by_key(|&item| u32::MAX - levels[item]);
                    let max_level = component.iter().map(|&item| get_level(state, &mut visited, &mut levels, item)).max().unwrap();
                    // TODO: reuse memory
                    let mut level_widths = Vec::new();
                    level_widths.resize(max_level as usize + 1, 0.0f32);
                    for &item in &*component {
                        let [width, _height] = item_sizes[item];
                        let level = levels[item];
                        let level_width = &mut level_widths[level as usize];
                        if *level_width != 0.0 {
                            *level_width += HORI_SPACING;
                        }
                        let x_pos = x_offset + *level_width;
                        let y_pos = (max_level - level) as f32 * (ITEM_VERT_SIZE + VERT_SPACING);
                        positions[item] = [x_pos, y_pos];
                        *level_width += width;
                    }

                    let max_width = level_widths.iter().copied().reduce(f32::max).unwrap();

                    // Horizontally center each level within its component, by first calculating horizontal offsets for each level:
                    let mut center_offsets = Vec::new();
                    center_offsets.reserve(level_widths.len());
                    for &width in &level_widths {
                        center_offsets.push((max_width - width) / 2.0);
                    }
                    // Then applying the level offsets to each item's position
                    for &item in &*component {
                        let level = levels[item];
                        positions[item][0] += center_offsets[level as usize];
                    }
                    x_offset += max_width;
                    x_offset += 50.0;
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
                        draw_list.add_line(adjust!(pos[0] + size[0] / 2.0, pos[1] + ITEM_VERT_SIZE), adjust!(dep_pos[0] + dep_size[0] / 2.0, dep_pos[1]), color).thickness(thickness).build();
                    }
                }
                for component in components {
                    let mut depended_items = HashSet::new();
                    for item in component {
                        let pos = positions[item];
                        let size = item_sizes[item];
                        let rect = [
                            adjust!(pos[0], pos[1]),
                            adjust!(pos[0] + size[0], pos[1] + ITEM_VERT_SIZE),
                        ];
                        let hovered = ui.is_mouse_hovering_rect(rect[0], rect[1]);
                        let color = if hovered {
                            highlighted_item = Some(item);
                            depended_items.extend(state.directed_edges[item].iter().copied());
                            HIGHLIGHT_COLOR
                        } else if depended_items.contains(&item) {
                            ImColor32::from_rgb(200, 100, 100)
                        } else {
                            ImColor32::WHITE
                        };
                        draw_list.add_rect(rect[0], rect[1], color).filled(true).rounding(5.0).build();
                        draw_list.add_text([rect[0][0] + ITEM_HORI_TEXT_MARGIN / 2.0, rect[0][1]], ImColor32::BLACK, text);
                        if !hovered {
                            draw_edges(&draw_list, origin, state, &positions, &item_sizes, item, ImColor32::WHITE, 1.0);
                        }
                        
                        x += size[0] + HORI_SPACING;
                    }
                    y += ITEM_VERT_SIZE + VERT_SPACING;
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
        let mut stream = LocalSocketStream::connect("@DUSK_VISUAL_DEBUGGER").unwrap();
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
