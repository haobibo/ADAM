fft_demo = function(len=200){
	Fs = 1000;                    # Sampling frequency
	T = 1/Fs;                     # Sample time
	L = len;                      # Length of signal
	t = (0:L-1)*T;                # Time vector
	# Sum of a 50 Hz sinusoid and a 120 Hz sinusoid
	x = 0.7*sin(2*pi*50*t) + sin(2*pi*120*t); 
	y = x + 3*runif(length(t)) - 1.5;   # Sinusoids plus noise

	
	#plot(
	#	 Fs*t[1:L],x[1:L]
	#	,'l'
	#	,xlab = 'time (milliseconds)'
	#	,main = 'Signal Corrupted with Zero-Mean Random Noise'
	#	,col='blue'
	#)
	
	#lines(
	#	 Fs*t[1:L],y[1:L]
	#	,'l'
	#	,xlab = 'time (milliseconds)'
	#	,main = 'Signal Corrupted with Zero-Mean Random Noise'
	#	,col='red'
	#)

	Y = fft(y)[1:floor(len/2)]/L;
	f = Fs/2*seq(0,1,length=length(Y))	#length=NFFT/2+1

	# Plot single-sided amplitude spectrum.
	plot(f,
		2*abs(Y[1:length(Y)]),	#1:(NFFT/2+1)
		'h',
		main = 'Single-Sided Amplitude Spectrum of y(t)',
		xlab = 'Frequency (Hz)',
		ylab = '|Y(f)|'
	)
	
	order_num = order( Y, decreasing = TRUE )
	return (order_num[1:3])
}