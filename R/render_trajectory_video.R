#' Render video from trajectory
#'
#' The function repeatedly calls plot_position function to generate preview
#' of the trajectory motion.
#' Extra time is added before and after the motion sequence (2 seconds each).
#'
#' @param filename the file name of output video (e.g. "trajectory.mp4")
#' @param trajectory tibble with trajectory data
#' @param settings list with basic properties
#' @param targets Which objects should be treated as targets
#' @param outdir output directory where the video is saved
#' @param ... Parameters passed to `plot_position`
#'
#' @return No return value
#' @export
#'
#' @examples
#' \dontrun{
#' # this example can take 30 seconds to run
#' # set ffmpeg in necessary
#' # ani.options(ffmpeg = "/PATH/TO/ffmpeg/ffmpeg")
#' render_trajectory_video("trajectory.mp4", trajectory8c,
#'   new_settings(show_labels = TRUE),
#'   targets = 1:4
#' )
#' }
render_trajectory_video <- function(
  filename,
  trajectory,
  settings = default_settings(),
  targets = NULL,
  outdir = getwd(),
  ...
) {
  # animation parameters
  fps <- 25
  video_width <- 600
  video_height <- 600
  preview_seconds <- 2
  respond_seconds <- 2

  tmin <- min(trajectory$time)
  tmax <- max(trajectory$time)
  tlen <- tmax - tmin
  oopt <- animation::ani.options(
    interval = 1 / fps,
    nmax = fps * tlen * 2,
    outdir = outdir,
    ani.width = video_width,
    ani.height = video_height
  )
  animation::saveVideo(
    {
      # preview
      for (i in 1:(preview_seconds * fps)) {
        p <- estimate_position_for_time(trajectory, tmin)
        fig <- plot_position(p, settings = settings, targets = targets, ...)
        print(fig)
      }
      for (tim in seq(tmin, tmax, 1 / fps)) {
        p <- estimate_position_for_time(trajectory, tim)
        fig <- plot_position(p, settings = settings, targets = targets, ...)
        print(fig)
      }
      for (i in 1:(respond_seconds * fps)) {
        p <- estimate_position_for_time(trajectory, tmax)
        fig <- plot_position(p, settings = settings, targets = targets, ...)
        print(fig)
      }
    },
    video.name = filename,
    other.opts = "-pix_fmt yuv420p -b 600k",
    clean = T
  )
  animation::ani.options(oopt)
}
