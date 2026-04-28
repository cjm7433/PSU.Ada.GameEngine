#include <spa/param/audio/format-utils.h>
#include <spa/pod/builder.h>
#include <spa/param/format-utils.h>

#include <stdint.h>

static uint8_t buffer[256];

void* build_audio_format()
{
  struct spa_pod_builder b;
  spa_pod_builder_init(&b, buffer, sizeof(buffer));

  return spa_format_audio_raw_build(
    &b,
    SPA_PARAM_EnumFormat,
    &(struct spa_audio_info_raw) {
      .format = SPA_AUDIO_FORMAT_S16,
      .rate = 44100,
      .channels = 2
    }
  );
}
