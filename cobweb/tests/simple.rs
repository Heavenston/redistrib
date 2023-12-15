use std::{error::Error, any::{TypeId, Any}, sync::Arc};

use cobweb::events::{GlobalEventHandler, Event};


#[tokio::test]
async fn events() -> Result<(), Box<dyn Error>> {
    #[derive(Debug, Clone, PartialEq, Eq)]
    struct EInt(u32);

    impl Event for EInt {
        fn event_type_id(&self) -> TypeId { TypeId::of::<Self>() }
        fn to_any(self: Arc<Self>) -> Arc<dyn Any> { self as Arc<dyn Any> }
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    struct EStr(String);

    impl Event for EStr {
        fn event_type_id(&self) -> TypeId { TypeId::of::<Self>() }
        fn to_any(self: Arc<Self>) -> Arc<dyn Any> { self as Arc<dyn Any> }
    }

    let mut a = GlobalEventHandler::new();
    let mut b = a.reconnect();

    a.subscribe::<EInt>();

    // only a is subed for this
    b.emit(EInt(5));
    assert_eq!(a.next_event().await.typed::<EInt>(), EInt(5).typed());
    assert!(a.try_next_event().is_none());
    assert!(b.try_next_event().is_none());

    // nobody subed for this
    b.emit(EStr("test".into()));
    assert!(a.try_next_event().is_none());
    assert!(b.try_next_event().is_none());

    a.subscribe::<EStr>();

    // Consuming a type doesn't consume other types
    b.emit(EStr("test2".into()));
    b.emit(EInt(42));
    assert_eq!(
        a.try_next_event_of::<EInt>(),
        Some(EInt(42).typed())
    );
    assert_eq!(
        a.try_next_event_of::<EInt>(),
        None
    );
    assert_eq!(
        a.try_next_event_of::<EStr>(),
        Some(EStr("test2".to_string()).typed())
    );
    assert_eq!(
        a.try_next_event_of::<EStr>(),
        None
    );

    // multible type revc
    b.emit(EInt(999));
    b.emit(EStr("The first".into()));
    b.emit(EStr("The second".into()));
    b.emit(EInt(1000));

    // ordering of different type of events isn't garenteed so this test
    // may break at any time
    // reasoning: it probably shouldn't be undefined so let's try to keep it
    // consistent for now until a good way of solving it comes to mind
    assert_eq!(
        a.try_next_event().and_then(|x| x.try_typed::<EInt>()),
        Some(EInt(999).typed())
    );
    assert_eq!(
        a.try_next_event().and_then(|x| x.try_typed::<EInt>()),
        Some(EInt(1000).typed())
    );
    assert_eq!(
        a.try_next_event().and_then(|x| x.try_typed::<EStr>()),
        Some(EStr("The first".into()).typed())
    );
    assert_eq!(
        a.try_next_event().and_then(|x| x.try_typed::<EStr>()),
        Some(EStr("The second".into()).typed())
    );
    
    Ok(())
}
