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


use glium::glutin;
use glium::glutin::event::{Event, WindowEvent};
use glium::glutin::event_loop::{ControlFlow, EventLoop};
use glium::glutin::window::WindowBuilder;
use glium::{Display, Surface};
use imgui::*;
use imgui::sys::igGetMainViewport;
use imgui_glium_renderer::Renderer;
use imgui_winit_support::{HiDpiMode, WinitPlatform};
use rand::Rng;
use std::time::Instant;

struct Rectangle {
    origin: [f32; 2],
    size: [f32; 2],
}

struct Header {
    label: String,
    y_pos: f32,
    x_right: f32,
}

struct UiState {
    running: bool,
    scrolling: [f32; 2],
    rectangles: Vec<Rectangle>,
    headers: Vec<Header>,
    rectangles_left: f32,
}

impl Default for UiState {
    fn default() -> Self {
        Self {
            running: true,
            scrolling: [0.0, 0.0],
            rectangles: Vec::new(),
            headers: Vec::new(),
            rectangles_left: 0.0,
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
            }

            macro_rules! adjust {
                ($x:expr, $y: expr) => {{
                    [$x + origin[0], $y + origin[1]]
                }}
            }

            const GRID_STEP: f32 = 50.0;
            draw_list.with_clip_rect(p0, p1, || {
                let mut x = state.scrolling[0] % GRID_STEP;
                let line_colour = ImColor32::from_rgba(200, 200, 200, 40);
                while x < canvas_size[0] {
                    draw_list.add_line([p0[0] + x, p0[1]], [p0[0] + x, p1[1]], line_colour).build();
                    x += GRID_STEP;
                }
                let mut y = state.scrolling[1] % GRID_STEP;
                while y < canvas_size[0] {
                    draw_list.add_line([p0[0], p0[1] + y], [p1[0], p0[1] + y], line_colour).build();
                    y += GRID_STEP;
                }

                const X_PADDING: f32 = 10.0;
                // Draw boxes
                if state.rectangles.is_empty() {
                    let mut y = 0.0;
                    state.rectangles_left = X_PADDING;
                    for i in 0..100 {
                        let mut level_x = 0.0;
                        let mut level_max_height = 0.0;
                        y += 50.0; // padding

                        let header_y_pos = y - 5.0;
                        for _ in 0..100 {
                            level_x += X_PADDING; // padding
                            let width = rand::thread_rng().gen_range(40.0..60.0);
                            let height = rand::thread_rng().gen_range(40.0..60.0);
                            state.rectangles.push(Rectangle { origin: [level_x, y], size: [width, height] });
                            if height > level_max_height { level_max_height = height; }
                            level_x += width;
                        }
                        state.headers.push(
                            Header {
                                label: format!("Level {}", i),
                                y_pos: header_y_pos,
                                x_right: level_x,
                            }
                        );
                        y += level_max_height;
                    }

                }
                for header in &state.headers {
                    const ESTIMATED_HEADER_WIDTH: f32 = 55.0;
                    let rectangles_left = state.rectangles_left + origin[0];
                    let rectangles_right = header.x_right + origin[0] - ESTIMATED_HEADER_WIDTH;
                    let screen_left = X_PADDING + p0[0];
                    let mut x = if rectangles_left > screen_left {
                        rectangles_left
                    } else {
                        screen_left
                    };
                    if x > rectangles_right {
                        x = rectangles_right;
                    }
                    draw_list.add_text([x, header.y_pos + origin[1] - ui.current_font_size()], ImColor32::WHITE, &header.label);
                }
                for rect in &state.rectangles {
                    draw_list.add_rect(adjust!(rect.origin[0], rect.origin[1]), adjust!(rect.origin[0] + rect.size[0], rect.origin[1] + rect.size[1]), ImColor32::WHITE).filled(true).build();
                }
            });
        });
    
}

fn main() {
    let event_loop = EventLoop::new();
    let context = glutin::ContextBuilder::new().with_vsync(true);
    let builder = WindowBuilder::new()
        .with_title("Dusk Visual Debugger")
        .with_inner_size(glutin::dpi::LogicalSize::new(1024f64, 768f64));
    let display =
        Display::new(builder, context, &event_loop).expect("Failed to initialize display");

    let mut imgui = Context::create();
    imgui.set_ini_filename(None);

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
    let mut state = UiState::default();
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
