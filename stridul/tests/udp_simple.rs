use bytes::BytesMut;
use stridul::*;
use tokio::{net::UdpSocket, io::AsyncReadExt};




#[tokio::test]
pub async fn udp_simple() -> anyhow::Result<()> {
    let _ = env_logger::builder()
        .is_test(true)
        .try_init();

    let (socket_a, socket_a_driver) = StridulSocket::<StridulUDPStrategy>::new(
        UdpSocket::bind("127.0.0.1:0").await?
    );
    log::trace!("Connected socket a at addr {:?}", socket_a.local_addr()?);
    socket_a_driver.self_driving();
    let (socket_b, socket_b_driver) = StridulSocket::<StridulUDPStrategy>::new(
        UdpSocket::bind("127.0.0.1:0").await?
    );
    log::trace!("Connected socket b at addr {:?}", socket_b.local_addr()?);
    socket_b_driver.self_driving();

    let a2b = socket_a.get_or_create_stream(23598, socket_b.local_addr()?).await;
    let b2a = socket_b.get_or_create_stream(23598, socket_a.local_addr()?).await;

    let a2b_message = b"This is a message for magesty the queen";
    a2b.write(a2b_message).await?;
    a2b.flush().await?;
    let b2a_message = b"Here is the response of the message";
    b2a.write(b2a_message).await?;
    b2a.flush().await?;

    let mut a2b_received = BytesMut::zeroed(a2b_message.len());
    b2a.reader().read_exact(&mut a2b_received).await?;
    assert_eq!(&a2b_received[..], &a2b_message[..]);

    let mut b2a_received = BytesMut::zeroed(b2a_message.len());
    a2b.reader().read_exact(&mut b2a_received).await?;
    assert_eq!(&b2a_received[..], &b2a_message[..]);

    Ok(())
}
