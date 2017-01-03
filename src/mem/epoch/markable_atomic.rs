use std::marker::PhantomData;
use std::mem;
use std::ptr;
use std::sync::atomic::{self, Ordering};

use super::{Owned, Shared, Guard};

#[derive(Debug)]
pub struct MarkableAtomic<T> {
    val: atomic::AtomicUsize,
    _marker: PhantomData<*const T>,
}

unsafe impl<T: Sync> Send for MarkableAtomic<T> {}
unsafe impl<T: Sync> Sync for MarkableAtomic<T> {}

/// Tags an even usize with a boolean marker bit.
fn tag_val(p: usize, b: bool) -> usize {
    debug_assert!(p as usize & 1 == 0);
    p | b as usize
}

/// Retrieves the original value and the boolean marker bit from a tagged usize.
fn untag_val(t: usize) -> (usize, bool) {
    let mark = t & 1;
    (t - mark, mark == 1)
}

fn opt_shared_into_usize<T>(val: Option<Shared<T>>) -> usize {
    val.as_ref().map(Shared::as_raw).unwrap_or(ptr::null_mut()) as usize
}

fn opt_owned_as_usize<T>(val: &Option<Owned<T>>) -> usize {
    val.as_ref().map(Owned::as_raw).unwrap_or(ptr::null_mut()) as usize
}

fn opt_owned_into_usize<T>(val: Option<Owned<T>>) -> usize {
    let ptr = val.as_ref().map(Owned::as_raw).unwrap_or(ptr::null_mut());
    mem::forget(val);
    ptr as usize
}

impl<T> MarkableAtomic<T> {
    #[cfg(feature = "nightly")]
    pub const fn null(b: bool) -> MarkableAtomic<T> {
        debug_assert!(mem::align_of::<T>() >= 2);

        MarkableAtomic {
            val: atomic::AtomicUsize::new(tag_val(0, b)),
            _marker: PhantomData,
        }
    }

    #[cfg(not(feature = "nightly"))]
    pub fn null(b: bool) -> MarkableAtomic<T> {
        debug_assert!(mem::align_of::<T>() >= 2);

        MarkableAtomic {
            val: atomic::AtomicUsize::new(tag_val(0, b)),
            _marker: PhantomData,
        }
    }

    pub unsafe fn from_ptr(data: *mut T, mark: bool) -> MarkableAtomic<T> {
        debug_assert!(mem::align_of::<T>() >= 2);

        MarkableAtomic {
            val: atomic::AtomicUsize::new(tag_val(data as usize, mark)),
            _marker: PhantomData,
        }
    }

    pub fn new(data: T, b: bool) -> MarkableAtomic<T> {
        debug_assert!(mem::align_of::<T>() >= 2);

        MarkableAtomic {
            val: atomic::AtomicUsize::new(tag_val(Box::into_raw(Box::new(data)) as usize, b)),
            _marker: PhantomData,
        }
    }

    pub fn load<'a>(&self, ord: Ordering, _: &'a Guard) -> (Option<Shared<'a, T>>, bool) {
        let p = untag_val(self.val.load(ord));
        (unsafe { Shared::from_raw(p.0 as *mut _) }, p.1)
    }

    pub fn store(&self, val: Option<Owned<T>>, mark: bool, ord: Ordering) {
        self.val.store(tag_val(opt_owned_into_usize(val), mark), ord);
    }

    pub fn store_and_ref<'a>(&self, val: Owned<T>, mark: bool, ord: Ordering, _: &'a Guard)
                             -> Shared<'a, T>
    {
        unsafe {
            let shared = Shared::from_owned(val);
            self.store_shared(Some(shared), mark, ord);
            shared
        }
    }

    pub fn store_shared(&self, val: Option<Shared<T>>, mark: bool, ord: Ordering) {
        self.val.store(tag_val(opt_shared_into_usize(val), mark), ord);
    }

    pub fn cas(&self, old: Option<Shared<T>>, old_mark: bool, new: Option<Owned<T>>, new_mark: bool, ord: Ordering)
               -> Result<(), Option<Owned<T>>>
    {
        if self.val.compare_and_swap(tag_val(opt_shared_into_usize(old), old_mark),
                                     tag_val(opt_owned_as_usize(&new), new_mark),
                                     ord) == opt_shared_into_usize(old)
        {
            mem::forget(new);
            Ok(())
        } else {
            Err(new)
        }
    }

    pub fn cas_and_ref<'a>(&self, old: Option<Shared<T>>, old_mark: bool, new: Owned<T>, new_mark: bool, ord: Ordering, _: &'a Guard)
                           -> Result<Shared<'a, T>, Owned<T>>
    {
        if self.val.compare_and_swap(tag_val(opt_shared_into_usize(old), old_mark),
                                     tag_val(new.as_raw() as usize, new_mark),
                                     ord) == opt_shared_into_usize(old)
        {
            Ok(unsafe { Shared::from_owned(new) } )
        } else {
            Err(new)
        }
    }

    pub fn cas_shared(&self, old: Option<Shared<T>>, old_mark: bool, new: Option<Shared<T>>, new_mark: bool, ord: Ordering)
                      -> bool
    {
        self.val.compare_and_swap(tag_val(opt_shared_into_usize(old), old_mark),
                                  tag_val(opt_shared_into_usize(new), new_mark),
                                  ord) == opt_shared_into_usize(old)
    }

    // .. etc
    // compare_arc_exchange_mark
    //swaps..




}
