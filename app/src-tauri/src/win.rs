// Windows だけの下回り。Job Object で「java とその子孫」をひとまとめにし、
// ランチャーが (強制終了も含め) 死んだ瞬間に OS が全部倒すようにする
// (KILL_ON_JOB_CLOSE — Unix のプロセスグループ + シグナルの代わり)。
#![cfg(windows)]

use std::os::windows::io::AsRawHandle;
use std::process::Child;
use std::sync::atomic::{AtomicIsize, Ordering};

use windows_sys::Win32::Foundation::HANDLE;
use windows_sys::Win32::System::JobObjects::{
    AssignProcessToJobObject, CreateJobObjectW, JobObjectExtendedLimitInformation,
    SetInformationJobObject, TerminateJobObject, JOBOBJECT_EXTENDED_LIMIT_INFORMATION,
    JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE,
};

// Job のハンドル (0 = 未作成)。プロセス終了時に OS が閉じる → その瞬間に道連れが働くので、
// 自前で CloseHandle は呼ばない。
static JOB: AtomicIsize = AtomicIsize::new(0);

/// Job を 1 つ作り「ハンドルが閉じたら中身を皆殺し」を仕掛ける。
/// 作れなくても続行する (道連れの保険が無くなるだけで起動はできる)。
pub fn init_job() {
    unsafe {
        let job = CreateJobObjectW(std::ptr::null(), std::ptr::null());
        if job.is_null() {
            return;
        }
        let mut info: JOBOBJECT_EXTENDED_LIMIT_INFORMATION = std::mem::zeroed();
        info.BasicLimitInformation.LimitFlags = JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;
        SetInformationJobObject(
            job,
            JobObjectExtendedLimitInformation,
            &info as *const _ as *const core::ffi::c_void,
            std::mem::size_of_val(&info) as u32,
        );
        JOB.store(job as isize, Ordering::SeqCst);
    }
}

/// 起動した子プロセスを Job に入れる (以後その子孫も全部 Job の中)。
pub fn assign(child: &Child) {
    let job = JOB.load(Ordering::SeqCst);
    if job == 0 {
        return;
    }
    unsafe {
        AssignProcessToJobObject(job as HANDLE, child.as_raw_handle() as HANDLE);
    }
}

/// Job の中身を今すぐ全部倒す (通常終了の経路で使う)。
pub fn terminate_all() {
    let job = JOB.load(Ordering::SeqCst);
    if job != 0 {
        unsafe {
            TerminateJobObject(job as HANDLE, 1);
        }
    }
}

/// エラーを窓で見せる。コンソールが無い (windows_subsystem = "windows") ので、
/// 起動できない理由はこれでしか伝わらない。
pub fn message_box(title: &str, text: &str) {
    use windows_sys::Win32::UI::WindowsAndMessaging::{MessageBoxW, MB_ICONERROR, MB_OK};
    let wide = |s: &str| s.encode_utf16().chain(std::iter::once(0)).collect::<Vec<u16>>();
    let title_w = wide(title);
    let text_w = wide(text);
    unsafe {
        MessageBoxW(std::ptr::null_mut(), text_w.as_ptr(), title_w.as_ptr(), MB_OK | MB_ICONERROR);
    }
}
