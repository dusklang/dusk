#include <windows.h>
#include <commctrl.h>
#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <thread>

OVERLAPPED overlapped;
uint8_t buf[1024];

HWND hwnd;
bool waiting = false;

LRESULT CALLBACK WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

int WINAPI wWinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, PWSTR pCmdLine, int nCmdShow) {
    InitCommonControls();

    std::thread io_thread([&]() {
        HANDLE pipe = CreateFileA("\\\\.\\pipe\\DUSK_VISUAL_DEBUGGER", GENERIC_READ | GENERIC_WRITE, 0, nullptr, OPEN_EXISTING, 0, NULL);
        assert(pipe != INVALID_HANDLE_VALUE);

        while(true) {
            DWORD num_bytes = 0;
            // TODO: buffer overflow!
            BOOL succ = ReadFile(pipe, buf, 4, &num_bytes, nullptr);
            assert(succ);

            assert(num_bytes == 4);
            uint32_t msg_size = *(uint32_t*)buf;
            // TODO: buffer overflow!
            succ = ReadFile(pipe, buf, msg_size, &num_bytes, nullptr);
            if(!succ) DestroyWindow(hwnd);

            assert(msg_size == num_bytes);
            buf[msg_size] = 0;

            MessageBoxA(nullptr, (LPCSTR)buf, (LPCSTR)buf, MB_OK);
        }
    });
    io_thread.detach();

    const wchar_t CLASS_NAME[] = L"dvd Window Class";
    WNDCLASS wc = {
        .lpfnWndProc = WindowProc,
        .hInstance = hInstance,
        .hCursor = LoadCursor(nullptr, IDC_ARROW),
        .lpszClassName = CLASS_NAME,
    };
    RegisterClass(&wc);

    hwnd = CreateWindowEx(
        0,
        CLASS_NAME,
        L"dvd",
        WS_OVERLAPPEDWINDOW,

        CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT,

        NULL,
        NULL,
        hInstance,
        NULL
    );
    if(hwnd == NULL) return 0;

    ShowWindow(hwnd, nCmdShow);

    MSG msg = {};
    while(true) {
        while(PeekMessage(&msg, hwnd, 0, 0, PM_REMOVE)) {
            TranslateMessage(&msg);
            DispatchMessage(&msg);
        }
    }
}

LRESULT CALLBACK WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam) {
    switch(uMsg) {
    case WM_QUIT:
        DestroyWindow(hwnd);
        return 0;
    case WM_DESTROY:
        PostQuitMessage(0);
        return 0;
    case WM_PAINT:
        {
            PAINTSTRUCT ps;
            HDC hdc = BeginPaint(hwnd, &ps);

            FillRect(hdc, &ps.rcPaint, (HBRUSH) (COLOR_WINDOW+1));
            EndPaint(hwnd, &ps);
            return 0;
        }
    }
    return DefWindowProc(hwnd, uMsg, wParam, lParam);
}
