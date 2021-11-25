# bookmovie

https://ulearn.me/course/fpintroduction/Bookmovie_a70f02a7-edeb-4b2e-81cf-5326abf2c712

# Setup
`sqlite3 bookmovie.db < seeds.sql`
`cabal run bookmovie`

# Endpoints

- `curl -H 'Content-Type: application/json' -X GET http://localhost:3000/api/timetable` - получить все сеансы
- `curl -H 'Content-Type: application/json' -X GET http://localhost:3000/api/timetable/{timeSlotId}/seats` - получить список доступных мест для сеанса
- `curl -H 'Content-Type: application/json' -X GET http://localhost:3000/api/stats` - получить статистику запросов
- `curl -H 'Content-Type: application/json' -X POST http://localhost:3000/api/timetable/{timeSlotId}/preliminary-booking/{seatId}` - забронировать место
- `curl -H 'Content-Type: application/json' -X POST http://localhost:3000/api/checkout/{bookingId}` - оплатить забранированное место
- `curl -H 'Content-Type: application/json' -X POST http://localhost:3000/api/refund/{bookingId}` - отмена бронирования/оплаты
- `curl -H 'Content-Type: application/json' -X POST http://localhost:3000/api/preliminary-booking/ -d '{"timeSlotId": {timeSlotId}, "seatIds": [{seatId}]}'` - бронирование нескольких мест
- `curl -H 'Content-Type: application/json' -X POST http://localhost:3000/api/checkout/ -d '[{bookingId}]'` - оплатить несколько забронированных мест
- `curl -H 'Content-Type: application/json' -X POST http://localhost:3000/api/refund/ -d '[{bookingId}]'` - отмена нескольких броней
