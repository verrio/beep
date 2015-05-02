/*  beep - just what it sounds like, makes the console beep - but with
 * precision control.  See the man page for details.
 *
 * Try beep -h for command line args
 *
 * Original code is copyright (C) Johnathan Nightingale <johnath@johnath.com>, 2000.
 * BSD spkr.c code parts are copyright (C) Eric S. Raymond <esr@snark.thyrsus.com>, 1990
 * FreeBSD code parts are copyright (C) Andrew A. Chernov <ache@astral.msk.su>, unknown date
 * Code merge, cleanup and extra bugs by Olivier Verriest <olivier.verriest@remoteloop.net>, 2015
 *
 * This code may distributed only under the terms of the GNU Public License 
 * which can be found at http://www.gnu.org/copyleft or in the file COPYING 
 * supplied with this code.
 *
 * This code is not distributed with warranties of any kind, including implied
 * warranties of merchantability or fitness for a particular use or ability to 
 * breed pandas in captivity, it just can't be done.
 */

#include <fcntl.h>
#include <getopt.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <linux/kd.h>
#include <linux/input.h>

/* I don't know where this number comes from, I admit that freely.  A 
   wonderful human named Raine M. Ekman used it in a program that played
   a tune at the console, and apparently, it's how the kernel likes its
   sound requests to be phrased.  If you see Raine, thank him for me.  
   
   June 28, email from Peter Tirsek <peter@tirsek.com>:
   
   This number represents the fixed frequency of the original PC XT's
   timer chip (the 8254 AFAIR), which is approximately 1.193 MHz. This
   number is divided with the desired frequency to obtain a counter value,
   that is subsequently fed into the timer chip, tied to the PC speaker.
   The chip decreases this counter at every tick (1.193 MHz) and when it
   reaches zero, it toggles the state of the speaker (on/off, or in/out),
   resets the counter to the original value, and starts over. The end
   result of this is a tone at approximately the desired frequency. :)
*/
#ifndef CLOCK_TICK_RATE
#define CLOCK_TICK_RATE 1193180
#endif

#define VERSION_STRING "beep-2.0"

/* Meaningful Defaults */
#define DEFAULT_FREQ       pitchtab[33] /* Middle A */
#define DEFAULT_LENGTH     200   		/* milliseconds */
#define DEFAULT_REPS       1			/* no repeat */
#define DEFAULT_DELAY      100   		/* milliseconds */
#define DEFAULT_END_DELAY  FALSE
#define DEFAULT_ACTION	   play_beep	/* play beep without stdin parsing */
#define DEFAULT_TEMPO      120          /* default tempo */
#define DEFAULT_VALUE      4            /* default value (quarter-note) */
#define DEFAULT_OCTAVE     2            /* default octave */

/* Other Constants */
#define BUFFER_SIZE        4096
#define FALSE              0
#define TRUE               1
#define SECS_PER_MIN       60           /* seconds per minute */
#define WHOLE_NOTE         4            /* quarter notes per whole note */
#define MIN_VALUE          64           /* the most we can divide a note by */
#define FILLTIME           8            /* for articulation, break note in parts */
#define STACCATO           6            /* 6/8 = 3/4 of note is filled */
#define NORMAL             7            /* 7/8ths of note interval is filled */
#define LEGATO             8            /* all of note interval is filled */
#define MIN_TEMPO          32           /* minimum tempo */
#define MAX_TEMPO          255          /* maximum tempo */
#define NUM_MULT           3            /* numerator of dot multiplier */
#define DENOM_MULT         2            /* denominator of dot multiplier */

/* letter to half-tone:  A   B  C  D  E  F  G */
static int notetab[8] = {9, 11, 0, 2, 4, 5, 7};

/*
 * This is the American Standard A440 Equal-Tempered scale with frequencies
 * rounded to nearest integer. Thank Goddess for the good ol' CRC Handbook...
 * our octave 0 is standard octave 2.
 */
#define OCTAVE_NOTES    12              /* semitones per octave */
static int pitchtab[] =
{
/*        C     C#    D     D#    E     F     F#    G     G#    A     A#    B*/
/* 0 */   65,   69,   73,   78,   82,   87,   93,   98,  103,  110,  117,  123,
/* 1 */  131,  139,  147,  156,  165,  175,  185,  196,  208,  220,  233,  247,
/* 2 */  262,  277,  294,  311,  330,  349,  370,  392,  415,  440,  466,  494,
/* 3 */  523,  554,  587,  622,  659,  698,  740,  784,  831,  880,  932,  988,
/* 4 */ 1047, 1109, 1175, 1245, 1319, 1397, 1480, 1568, 1661, 1760, 1865, 1975,
/* 5 */ 2093, 2217, 2349, 2489, 2637, 2794, 2960, 3136, 3322, 3520, 3729, 3951,
/* 6 */ 4186, 4435, 4698, 4978, 5274, 5588, 5920, 6272, 6644, 7040, 7459, 7902,
};

typedef struct beep_parms_t {
	float freq;     /* tone frequency (Hz)      */
	int length;     /* tone length    (ms)      */
	int reps;       /* # of repetitions         */
	int delay;      /* delay between reps  (ms) */
	int end_delay;  /* do we delay after last rep? */
	void (* beep_handler)(struct beep_parms_t *); /* handler for beep processing
  	  	  We have five options here:
  	  	  	 - emit a single beep of specified frequency/duration
		     - parse the input as a BSD-compatible /dev/speaker music macro language
		     - parse the input and play ANSI-delimited music macro language strings
		     - beep after a line of input
		     - beep after a character of input
		     The last three of these modes pass the text back out again
		     (ANSI melody sequences will be filtered out), so that beep
		     can be tucked appropriately into a text processing pipe.
		  */
	int verbose;    /* verbose output? */
	struct beep_parms_t *next;  /* in case -n/--new is used. */
} beep_parms_t;

enum { BEEP_TYPE_CONSOLE, BEEP_TYPE_EVDEV };

/* globals necessary for signal handlers.*/
int console_fd = -1;
int console_type = BEEP_TYPE_CONSOLE;
char *console_device = NULL;
char sinbuf[BUFFER_SIZE];

/* beep <freq> Hz for <length> milliseconds */
void do_beep(int freq, int length) {
	int period = (freq != 0 ? (int) (CLOCK_TICK_RATE / freq) : freq);

	if (console_type == BEEP_TYPE_CONSOLE) {
		if (ioctl(console_fd, KIOCSOUND, period) < 0) {
			putchar('\a'); /* Output the only beep we can, in an effort to fall back on usefulness */
			perror("ioctl");
		}
		usleep(1000 * length);
		if (ioctl(console_fd, KIOCSOUND, 0) < 0)
			perror("ioctl");
	} else {
		/* BEEP_TYPE_EVDEV */
		struct input_event e;

		e.type = EV_SND;
		e.code = SND_TONE;
		e.value = freq;

		/* start timer */
		if (write(console_fd, &e, sizeof(struct input_event)) < 0) {
			putchar('\a'); /* See above */
			perror("write");
		}

		usleep(1000 * length);
		e.value = 0;
		/* disable timer */
		if (write(console_fd, &e, sizeof(struct input_event)) < 0)
			perror("write");
	}
}

void play_beep(beep_parms_t *parms) {
	int i;

	if (parms->verbose == 1)
		fprintf(stderr, "[DEBUG] %d times %d ms beeps (%d delay between, "
				"%d delay after) @ %.2f Hz\n", parms->reps, parms->length,
				parms->delay, parms->end_delay, parms->freq);

	/* Beep */
	for (i = 0; i < parms->reps; i++) {                  /* start beep */
		do_beep(parms->freq, parms->length);
		if (parms->end_delay || (i + 1 < parms->reps))
			usleep(1000 * parms->delay);                 /* wait...    */
	}                                                    /* repeat.    */
}

/* read character from buffer, try to refill buffer if empty */
char *read_char(char *curr) {
	if (++curr < &sinbuf[BUFFER_SIZE] && *curr)
		return curr;
	if (fgets(sinbuf, BUFFER_SIZE, stdin) && sinbuf[0])
		return sinbuf;
	return 0;
}

/* beep on every UTF-8 character */
void play_by_char(beep_parms_t *parms) {
	char *c = &sinbuf[BUFFER_SIZE];
	while ((c = read_char(c))) {
		if ((*c & 0xC0) != 0x80)
			play_beep(parms);
		putchar(*c);
		fflush(stdout);
	}
}

/* beep on every line */
void play_by_newline(beep_parms_t *parms) {
	int expect = FALSE;
	sinbuf[BUFFER_SIZE-1] = 'A';
	while (fgets(sinbuf, BUFFER_SIZE, stdin)) {
		fputs(sinbuf, stdout);
		expect = sinbuf[BUFFER_SIZE-1] == '\0'; /* expect more beeps */
		if (sinbuf[BUFFER_SIZE-2] == '\n' || sinbuf[BUFFER_SIZE-1] == 'A') /* fgets returned before buffer was full */
			play_beep(parms);
		sinbuf[BUFFER_SIZE-2] = 'A';
		sinbuf[BUFFER_SIZE-1] = 'A';
	}
	if (expect) play_beep(parms);
}

/* play tone of proper duration for given rhythm signature */
void play_note(beep_parms_t *parms, int pitch, int value, int sustain, int whole, int fill) {
	int sound, silence, snum = 10, sdenom = 1;

	/* this weirdness avoids floating-point arithmetic */
	for (; sustain; sustain--) {
		/* sustain dots extend time with 3/2 of its previous value.
		 * retained for compatibility with IBM BASIC PLAY */
		snum *= NUM_MULT;
		sdenom *= DENOM_MULT;
	}

	if (!value || !sdenom)
		return;

	if (pitch == -1) {
		/* rest */
		do_beep(0, whole * snum / (value * sdenom));
	} else {
		/* note */
		sound = (whole * snum) / (value * sdenom) - (whole * (FILLTIME - fill)) / (value * FILLTIME);
		silence = whole * (FILLTIME - fill) * snum / (FILLTIME * value * sdenom);

		if (parms->verbose == 1)
			fprintf(stderr, "[DEBUG] pitch %d for %d ticks, rest for %d ticks\n", pitch, sound, silence);

		do_beep(pitchtab[pitch], sound);
		if (fill != LEGATO)
			do_beep(0, silence);
	}
}

/* parse stdin as MML melody and beep accordingly */
void parse_mml_stream(beep_parms_t *parms, int ansi_delimited) {
	char *c = &sinbuf[BUFFER_SIZE];
	int marked = FALSE;                                             /* are we inside an ANSI-delimited sequence? */
	int octave = DEFAULT_OCTAVE;                                    /* currently selected octave */
	int whole = (100 * SECS_PER_MIN * WHOLE_NOTE) / DEFAULT_TEMPO;  /* whole-note time at current tempo, in ticks */
	int value = DEFAULT_VALUE;                                      /* whole divisor for note time, quarter note = 1 */
	int fill = NORMAL;                                              /* controls spacing of notes */
	int tempo = DEFAULT_TEMPO;                                      /* current tempo */
	int octtrack = FALSE;                                           /* octave-tracking mode on? */
	int octprefix = TRUE;                                           /* override current octave-tracking state? */
	int pitch;                                                      /* current pitch */
	int oldfill;
	int lastpitch = OCTAVE_NOTES * DEFAULT_OCTAVE;
	int sustain;
	int timeval = value;

#define GETCHAR(c)	(c = read_char(c))

#define GETNUM(v,c)   do { v = 0; while((c = read_char(c)) && isdigit(*c)) \
							v = v * 10 + (*c - '0'); } while (0)

	GETCHAR(c);
	while(c) {
		if (ansi_delimited & !marked) {
			/* find next ANSI sequence */
			while (1) {
				/* loop till <esc>[M was found */
				if (*c != 0x1b) {
					putchar(*c);
					fflush(stdout);
					if (!GETCHAR(c)) return;
					continue;
				}
				if (!GETCHAR(c)) {
					putchar(0x1b);
					fflush(stdout);
					return;
				}
				if (*c != '[') {
					putchar(0x1b);
					fflush(stdout);
					continue;
				}
				if (!GETCHAR(c)) {
					putchar(0x1b);
					putchar('[');
					fflush(stdout);
					return;
				}
				if (*c != 'M' && *c != 'm') {
					putchar(0x1b);
					putchar('[');
					fflush(stdout);
					continue;
				}
				if (!GETCHAR(c)) return; /* ignore background/foreground info */
				if (parms->verbose == 1)
					fprintf(stderr, "[DEBUG] found start of ANSI music macro sequence.\n");
				marked = TRUE;
				if (!GETCHAR(c)) return;
				break;
			}
		}

		*c = toupper(*c);
		switch (*c) {
		case 'A':
		case 'B':
		case 'C':
		case 'D':
		case 'E':
		case 'F':
		case 'G':
			/* compute pitch */
			pitch = notetab[*c - 'A'] + octave * OCTAVE_NOTES;

			/* this may be followed by an accidental sign */
			if (GETCHAR(c)) {
				if (*c == '#' || *c == '+')
					++pitch;
				else if (*c == '-')
					--pitch;
				else
					--c;
			}

			/*
			 * if octave-tracking mode is on, and there has been no octave-
			 * setting prefix, find the version of the current letter note
			 * closest to the last regardless of octave.
			 */
			if (octtrack && !octprefix) {
				if (abs(pitch - lastpitch) > abs(pitch + OCTAVE_NOTES - lastpitch)) {
					++octave;
					pitch += OCTAVE_NOTES;
				}

				if (abs(pitch - lastpitch) > abs(pitch - OCTAVE_NOTES - lastpitch)) {
					--octave;
					pitch -= OCTAVE_NOTES;
				}
			}
			octprefix = 0;
			lastpitch = pitch;

			/* ...which may in turn be followed by an override time value */
			if (c) GETNUM(timeval, c);
			if (timeval <= 0 || timeval > MIN_VALUE)
				timeval = value;

			/* ...and/or sustain dots */
			for (sustain = 0; c && *c == '.'; GETCHAR(c))
				sustain++;

			/* ...and/or a slur mark */
			oldfill = fill;
			if (c && (*c == '_' || *c == '&')) {
				fill = LEGATO;
				GETCHAR(c);
			}

			/* time to emit the actual note */
			play_note(parms, pitch, timeval, sustain, whole, fill);
			fill = oldfill;
			break;

		case 'O':
			if (GETCHAR(c)) {
				if (*c == 'N' || *c == 'n') {
					octprefix = octtrack = 0;
					GETCHAR(c);
				} else if (*c == 'L' || *c == 'l') {
					octtrack = TRUE;
					GETCHAR(c);
				} else {
					--c;
					GETNUM(octave, c);
					if (octave >= sizeof(pitchtab) / sizeof(pitchtab[0]) / OCTAVE_NOTES)
							octave = DEFAULT_OCTAVE;
					octprefix = TRUE;
				}
			}
			break;

		case '>':
			if (octave < sizeof(pitchtab) / sizeof(pitchtab[0]) / OCTAVE_NOTES - 1)
				octave++;
			octprefix = TRUE;
			GETCHAR(c);
			break;

		case '<':
			if (octave > 0)
				octave--;
			octprefix = TRUE;
			GETCHAR(c);
			break;

		case 'N':
			GETNUM(pitch, c);
			for (sustain = 0; c && *c == '.'; GETCHAR(c))
				sustain++;
			oldfill = fill;
			if (c && (*c == '_' || *c == '&')) {
				fill = LEGATO;
				GETCHAR(c);
			}
			play_note(parms, pitch - 1, value, sustain, whole, fill);
			fill = oldfill;
			break;

		case 'L':
			GETNUM(value, c);
			if (value <= 0 || value > MIN_VALUE)
				value = DEFAULT_VALUE;
			break;

		case 'P':
		case 'R':
		case '~':
			/* this may be followed by an override time value */
			GETNUM(timeval, c);
			if (timeval <= 0 || timeval > MIN_VALUE)
				timeval = value;
			for (sustain = 0; c && *c == '.'; GETCHAR(c))
				sustain++;
			play_note(parms, -1, timeval, sustain, whole, fill);
			break;

		case 'T':
			GETNUM(tempo, c);
			if (tempo < MIN_TEMPO || tempo > MAX_TEMPO)
				tempo = DEFAULT_TEMPO;
			whole = (100 * SECS_PER_MIN * WHOLE_NOTE) / tempo;
			break;

		case 'M':
			if (GETCHAR(c)) {
				if (*c == 'N' || *c == 'n')
					fill = NORMAL;
				else if (*c == 'L' || *c == 'l')
					fill = LEGATO;
				else if (*c == 'S' || *c == 's')
					fill = STACCATO;
				GETCHAR(c);
			}
			break;

		case 'V': /* ignore volume */
			while (GETCHAR(c) && isdigit(*c));
			break;

		case 0x0e: /* ANSI end marker */
		case '\r':
		case '\n':
		case 0x03:
		case 0x04:
		case ';':
		case ',':
			marked = FALSE;
		case '\t':
		case ' ': /* ignore whitespace */
			GETCHAR(c);
			break;

		default:
			if (parms->verbose == 1)
				fprintf(stderr, "[DEBUG] ignored invalid music code character 0x%x.\n", *c & 0xff);
			GETCHAR(c);
		}
	}
}

/* parse MML with ANSI delimiters */
void play_ansi_mml(beep_parms_t *parms) {
	parse_mml_stream(parms, 1);
}

/* parse MML without ANSI delimiters */
void play_mml(beep_parms_t *parms) {
	parse_mml_stream(parms, 0);
}

/* if we get interrupted, it would be nice to not leave the speaker beeping in perpetuity */
void handle_signal(int signum) {
	if (console_device)
		free(console_device);

	switch (signum) {
	case SIGINT:
	case SIGTERM:
		if (console_fd >= 0) {
			/* Kill the sound, quit gracefully */
			do_beep(0,0);
			close(console_fd);
			exit(signum);
		} else {
			/* Just quit gracefully */
			exit(signum);
		}
	}
}

/* print usage and exit */
void usage_bail(const char *executable_name) {
	printf("Usage:\n%s [-f freq] [-l length] [-r reps] [-d delay] "
			"[-D delay] [-s] [-c] [-a] [-b] [--verbose | --debug] [-e device]\n",
			executable_name);
	printf("%s [Options...] [-n] [--new] [Options...] ... \n", executable_name);
	printf("%s [-h] [--help]\n", executable_name);
	printf("%s [-v] [-V] [--version]\n", executable_name);
	exit(1);
}

/* Parse the command line.  argv should be untampered, as passed to main.
 * Beep parameters returned in result, subsequent parameters in argv will over-
 * ride previous ones.
 * 
 * Currently valid parameters:
 *  "-f <frequency in Hz>"
 *  "-l <tone length in ms>"
 *  "-r <repetitions>"
 *  "-d <delay in ms>"
 *  "-D <delay in ms>" (similar to -d, but delay after last repetition as well)
 *  "-s" (beep after each line of input from stdin, echo line to stdout)
 *  "-c" (beep after each char of input from stdin, echo char to stdout)
 *  "-a/--ansi" (interpret stdin as ANSI melody, echo chars to stdout)
 *  "-b/--mml" (interpret stdin as BSD-compatible music macro language)
 *  "--verbose/--debug"
 *  "-h/--help"
 *  "-v/-V/--version"
 *  "-n/--new"
 *
 * March 29, 2002 - Daniel Eisenbud points out that c should be int, not char,
 * for correctness on platforms with unsigned chars.
 */
void parse_command_line(int argc, char **argv, beep_parms_t *result) {
	int c;

	struct option opt_list[9] =
			{ { "help", 0, NULL, 'h' },
			{"version", 0, NULL, 'V'},
			{"new", 0, NULL, 'n'},
			{"ansi", 0, NULL, 'a'},
			{"mml", 0, NULL, 'b'},
			{"verbose", 0, NULL, 'X'},
			{"debug", 0, NULL, 'X'},
			{"device", 1, NULL, 'e'},
			{0,0,0,0}};
	while ((c = getopt_long(argc, argv, "f:l:r:d:D:scabhvVne:", opt_list, NULL)) != EOF) {
		int argval = -1; /* handle parsed numbers for various arguments */
		float argfreq = -1;
		switch (c) {
		case 'f': /* freq */
			if (!sscanf(optarg, "%f", &argfreq) || (argfreq >= 20000 /* ack! */)
					|| (argfreq <= 0))
				usage_bail(argv[0]);
			else if (result->freq != 0)
				fprintf(stderr, "WARNING: multiple -f values given, only last "
						"one is used.\n");
			result->freq = argfreq;
			break;
		case 'l': /* length */
			if (!sscanf(optarg, "%d", &argval) || (argval < 0))
				usage_bail(argv[0]);
			else
				result->length = argval;
			break;
		case 'r': /* repetitions */
			if (!sscanf(optarg, "%d", &argval) || (argval < 0))
				usage_bail(argv[0]);
			else
				result->reps = argval;
			break;
		case 'd': /* delay between reps - WITHOUT delay after last beep*/
			if (!sscanf(optarg, "%d", &argval) || (argval < 0))
				usage_bail(argv[0]);
			else {
				result->delay = argval;
				result->end_delay = FALSE;
			}
			break;
		case 'D': /* delay between reps - WITH delay after last beep */
			if (!sscanf(optarg, "%d", &argval) || (argval < 0))
				usage_bail(argv[0]);
			else {
				result->delay = argval;
				result->end_delay = TRUE;
			}
			break;
		case 's':
			result->beep_handler = play_by_newline;
			break;
		case 'c':
			result->beep_handler = play_by_char;
			break;
		case 'a':
			result->beep_handler = play_ansi_mml;
			break;
		case 'b':
			result->beep_handler = play_mml;
			break;
		case 'v':
		case 'V': /* also --version */
			printf("%s\n", VERSION_STRING);
			exit(0);
			break;
		case 'n': /* also --new - create another beep */
			if (result->freq == 0)
				result->freq = DEFAULT_FREQ;
			result->next = (beep_parms_t *)malloc(sizeof(beep_parms_t));
			result->next->freq = 0;
			result->next->length = DEFAULT_LENGTH;
			result->next->reps = DEFAULT_REPS;
			result->next->delay = DEFAULT_DELAY;
			result->next->end_delay = DEFAULT_END_DELAY;
			result->next->beep_handler = play_beep;
			result->next->verbose = result->verbose;
			result->next->next = NULL;
			result = result->next; /* yes, I meant to do that. */
			break;
		case 'X' : /* --debug / --verbose */
			result->verbose = TRUE;
			break;
		case 'e' : /* also --device */
			console_device = strdup(optarg);
			break;
		case 'h' : /* notice that this is also --help */
		default :
			usage_bail(argv[0]);
		}
	}
	if (result->freq == 0)
		result->freq = DEFAULT_FREQ;
}  

int main(int argc, char **argv) {
	beep_parms_t *parms = (beep_parms_t *) malloc(sizeof(beep_parms_t));
	parms->freq = 0;
	parms->length = DEFAULT_LENGTH;
	parms->reps = DEFAULT_REPS;
	parms->delay = DEFAULT_DELAY;
	parms->end_delay = DEFAULT_END_DELAY;
	parms->beep_handler = DEFAULT_ACTION;
	parms->verbose = FALSE;
	parms->next = NULL;

	signal(SIGINT, handle_signal);
	signal(SIGTERM, handle_signal);
	parse_command_line(argc, argv, parms);

	/* try to snag the console */
	if (console_device)
		console_fd = open(console_device, O_WRONLY);
	else if ((console_fd = open("/dev/tty0", O_WRONLY)) == -1)
		console_fd = open("/dev/vc/0", O_WRONLY);

	if (console_fd == -1) {
		fprintf(stderr, "Could not open %s for writing\n",
				console_device != NULL ?
						console_device : "/dev/tty0 or /dev/vc/0");
		printf("\a"); /* Output the only beep we can, in an effort to fall back on usefulness */
		perror("open");
		exit(EXIT_FAILURE);
	}

	/* determine console capabilities */
	if (ioctl(console_fd, EVIOCGSND(0)) != -1)
		console_type = BEEP_TYPE_EVDEV;
	else
		console_type = BEEP_TYPE_CONSOLE;

	/* handle all specified beeps. Each iteration will play, then free() one parms instance. */
	while (parms) {
		beep_parms_t *next = parms->next;
		if (parms->beep_handler != DEFAULT_ACTION) {
			/* in this case, beep is probably part of a pipe, in which case POSIX
			 says stdin and out should be fully buffered.  This however means very
			 laggy performance with beep just twiddling it's thumbs until a buffer
			 fills. Thus, kill the buffering.  In some situations, this too won't
			 be enough, namely if we're in the middle of a long pipe, and the
			 processes feeding us stdin are buffered, we'll have to wait for them,
			 not much to be done about that. */
			setvbuf(stdin, NULL, _IONBF, 0);
			setvbuf(stdout, NULL, _IONBF, 0);
		}
		parms->beep_handler(parms);
		free(parms);
		parms = next;
	}

	free(console_device);

	return EXIT_SUCCESS;
}
