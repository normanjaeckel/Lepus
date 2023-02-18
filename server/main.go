package main

import (
	"context"
	"fmt"
	"log"
	"net/http"
	"os"
	"os/signal"
	"strconv"
	"sync"

	"github.com/normanjaeckel/Lepus/server/allocation"
	"github.com/normanjaeckel/Lepus/server/public"
	"golang.org/x/sys/unix"
)

const (
	Host           string = "localhost"
	PortEnvVarName string = "LEPUS_PORT"
)

type Logger interface {
	Printf(format string, v ...any)
}

func main() {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	logger := log.Default()

	port := os.Getenv(PortEnvVarName)
	if port == "" {
		port = "8000"
	} else if _, err := strconv.Atoi(port); err != nil {
		logger.Fatalf("Error: Environment variable %s must be an integer and not %q", PortEnvVarName, port)
	}

	onSignals(logger, cancel)

	addr := fmt.Sprintf("%s:%s", Host, port)
	if err := start(ctx, logger, addr); err != nil {
		logger.Fatalf("Error: %v", err)
	}
}

// onSignals starts a goroutine that listens to the operating system signals
// SIGTERM and SIGINT. On incomming signal (SIGTERM or SIGINT), the cancel
// function is called. If SIGINT comes in a second time, os.Exit(1) is called to
// abort the process.
func onSignals(log Logger, cancel context.CancelFunc) {
	go func() {
		msg := "Received operating system signal: %s"

		sigTerm := make(chan os.Signal, 1)
		signal.Notify(sigTerm, unix.SIGTERM)
		sigInt := make(chan os.Signal, 1)
		signal.Notify(sigInt, unix.SIGINT)

		select {
		case s := <-sigInt:
			log.Printf(msg, s.String())
		case s := <-sigTerm:
			log.Printf(msg, s.String())
		}
		cancel()

		s := <-sigInt
		log.Printf(msg, s.String())
		log.Printf("Process aborted")
		os.Exit(1)
	}()
}

// start initiates the HTTP server and lets it listen on the given address.
func start(ctx context.Context, logger Logger, addr string) error {
	mux := sync.Mutex{}

	s := &http.Server{
		Addr:    addr,
		Handler: handler(logger, &mux),
	}

	go func() {
		<-ctx.Done()
		logger.Printf("Server is shuting down")
		if err := s.Shutdown(context.Background()); err != nil {
			logger.Printf("Error: Shutting down server: %v", err)
		}
	}()

	logger.Printf("Server starts and listens on %q", addr)
	err := s.ListenAndServe()
	if err != http.ErrServerClosed {
		return fmt.Errorf("server exited: %w", err)
	}
	logger.Printf("Server is down")

	return nil
}

func handler(logger Logger, mux *sync.Mutex) http.Handler {
	serveMux := http.NewServeMux()

	// Allocation
	serveMux.Handle("/allocation", allocation.Handle(logger))

	// Root
	serveMux.HandleFunc("/", public.Files)

	return serveMux
}
