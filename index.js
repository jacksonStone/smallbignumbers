export class BigNumber {
  constructor(value) {
    // If the value is given as a string like "123.456"
    if (typeof value === "string") {
      // Parse the string to extract scale and BigInt value
      const [intPart, fracPart = ""] = value.split(".");

      // Scale is the length of the fractional part
      // I considered making this a big int as well, however storing a number greater than MAX_SAFE_INTEGER will cause overflows for all systems in the forseeable future.
      // as it will result in 9.007199254740991 Petabytes per number instance to represent the string. To operate on it usually requires some level of copying, so maybe requires
      // a system with 20+ petabytes accessible to the JS runtime... If you have a server farm that large, you should not be using this library.
      this.scale = fracPart.length;

      // Combine integer part and fractional part into one string
      const combined = intPart + fracPart;

      // Convert to BigInt
      this.value = BigInt(combined);
    }
    // If the value is given as an already scaled BigInt + scale
    else if (typeof value === "object" && value.bigInt !== undefined) {
      this.value = value.bigInt;
      this.scale = value.scale;
    }
    // Otherwise assume it's a plain number with no decimals (not recommended for large values)
    else {
      this.value = BigInt(value);
      this.scale = 0;
    }
  }

  // Convert our internal representation back to a string
  toString() {
    const strValue = this.value.toString();
    if (this.scale === 0) {
      return strValue;
    }

    // Insert decimal point in the correct place from the right
    let intPart = '';
    let integerPartLength = strValue.length - this.scale;
    if (integerPartLength <= 0) {
      // Pad with leading zeros if fraction is longer than integer part
      intPart = "0";
      integerPartLength = 0;
    }
    else if (integerPartLength === 1 && strValue[0] === "-") {
      intPart = "-0";
      integerPartLength = 1;
    } else {
      intPart = strValue.slice(0, integerPartLength);
    }
    const fracPart = strValue.slice(integerPartLength);
    // Trim trailing zeros from the fractional part
    let i = fracPart.length - 1;
    // Iterate backwards until we find a character that's not the trailing character
    while (i >= 0 && fracPart[i] === "0") {
      i--;
    }
    return intPart + "." + fracPart.slice(0, i + 1);
  }

  // Create an instance from a BigInt and a scale
  static fromBigInt(bigInt, scale) {
    return new BigNumber({ bigInt, scale });
  }

  // Add another BigNumber
  add(other) {
    // Align the scale
    const [val1, val2, scale] = this._alignScales(this, other);
    // Perform BigInt addition
    const sum = val1 + val2;
    return BigNumber.fromBigInt(sum, scale);
  }

  // Subtract another BigNumber
  subtract(other) {
    // Align the scale
    const [val1, val2, scale] = this._alignScales(this, other);
    // Perform BigInt subtraction
    const diff = val1 - val2;
    return BigNumber.fromBigInt(diff, scale);
  }

  // Multiply by another BigNumber
  multiply(other) {
    // If we multiply two numbers with scale `s1` and `s2`,
    // the resulting scale will be `s1 + s2`.
    const product = this.value * other.value;
    const newScale = this.scale + other.scale;
    return BigNumber.fromBigInt(product, newScale);
  }

  // Divide by another BigNumber
  divide(other, precision = 10) {
    // We can use a trick:
    // (this.value / 10^this.scale) / (other.value / 10^other.scale)
    // -> (this.value * 10^other.scale) / (other.value * 10^this.scale)

    // Convert to a scaled integer to preserve precision
    // e.g., multiply numerator by 10^(precision)
    // This ensures we get enough significant digits in the quotient.
    const scaleFactor = BigInt("1" + "0".repeat(precision));
    const numerator = this.value * scaleFactor * BigInt("1" + "0".repeat(other.scale));
    const denominator = other.value * BigInt("1" + "0".repeat(this.scale));

    // Divide with one extra digit of precision
    const rawQuotient = (numerator * 10n) / denominator;

    // Add or subtract 5 based on sign for proper rounding
    const roundedQuotient = rawQuotient >= 0n
      ? (rawQuotient + 5n) / 10n
      : (rawQuotient - 5n) / 10n;

    // The resulting scale is `precision` because we effectively multiplied by 10^precision
    return BigNumber.fromBigInt(roundedQuotient, precision);
  }

  // A helper to align scales between two BigNumbers
  _alignScales(a, b) {
    if (a.scale === b.scale) {
      return [a.value, b.value, a.scale];
    } else if (a.scale > b.scale) {
      // Scale up b
      const scaleDiff = a.scale - b.scale;
      const scaledB = b.value * BigInt(BigInt("1" + "0".repeat(scaleDiff)));
      return [a.value, scaledB, a.scale];
    } else {
      // Scale up a
      const scaleDiff = b.scale - a.scale;
      const scaledA = a.value * BigInt(BigInt("1" + "0".repeat(scaleDiff)));
      return [scaledA, b.value, b.scale];
    }
  }
  // Precision is only leveraged for fractional exponents
  pow(exponent, precision = 20) {
    // Handle special cases
    if (exponent.toString() === "0") return new BigNumber("1");
    if (exponent.toString() === "1") return this;

    // For integer exponents, use faster integer power algorithm
    if (exponent.scale === 0) {
      return this._integerPow(exponent);
    }

    // For decimal exponents, use exp(ln(x) * exponent)
    // x^y = e^(ln(x) * y)
    const lnResult = this.ln(precision + 3); // Provide extra precision for ln to avoid compoundingrounding errors
    const multiplied = lnResult.multiply(exponent);
    return BigNumber
      .exp(multiplied, precision + 3) // Provide extra precision for exp to avoid compoundingrounding errors
      .roundToPrecision(precision);
  }
  
  static e(precision = 50) {
    // 1) If our eCache is already at or above the requested precision, return it.
    if (eCache.scale >= precision) {
      return eCache.roundToPrecision(precision);
    }
  
    // 2) We'll compute e = 2^(1/ln(2)) at a higher internal precision
    const workingPrecision = precision + 10;
  
    // 3) Get 1/ln(2) at the working precision
    const one = new BigNumber("1");
    const two = new BigNumber("2");
    const oneOverLn2 = one.divide(this.ln2(workingPrecision), workingPrecision);
  
    // 4) Raise 2 to that power
    //    2^(1/ln(2)) = e
    const eApprox = two.pow(oneOverLn2, workingPrecision);
  
    // 5) Update our cache if this is the highest precision we’ve computed
    eCache = eApprox;
  
    // 6) Finally, return it rounded to the requested precision
    return eApprox.roundToPrecision(precision);
  }
  // Natural logarithm using Taylor series
  ln(precision = 50) {
    const internalPrecision = precision + 3;
    if (this.value <= 0n) {
      throw new Error("Cannot calculate ln of negative number or zero");
    }

    // For values close to 1, use Taylor series directly
    // ln(x) = 2( z + z^3/3 + z^5/5 + z^7/7 + ...) where z = (x-1)/(x+1)
    const one = new BigNumber("1");
    const two = new BigNumber("2");

    // Normalize number to be close to 1 by dividing by power of 2
    let power = 0;
    let normalized = this;
    while (normalized.compare(two) >= 0) {
      normalized = normalized.divide(two, internalPrecision);
      power++;
    }
    while (normalized.compare(one) < 0) {
      normalized = normalized.multiply(two);
      power--;
    }

    const z = normalized.subtract(one).divide(normalized.add(one), internalPrecision);
    let sum = z;
    let term = z;
    let z2 = z.multiply(z);

    for (let i = 3; i < internalPrecision * 2; i += 2) {
      term = term.multiply(z2);
      const nextTerm = term.divide(new BigNumber(i.toString()), internalPrecision);
      sum = sum.add(nextTerm);

      // Check for convergence
      if (nextTerm.isZero()) break;
    }

    sum = sum.multiply(two);

    // Add back ln(2) * power
    if (power !== 0) {
      const ln2 = BigNumber.ln2(precision * 2);
      sum = sum.add(ln2.multiply(new BigNumber(power.toString())));
    }

    return sum.roundToPrecision(precision);
  }

  static factorial(n) {
    // Naive factorial implementation using BigNumber
    let result = new BigNumber("1");
    for (let i = 1n; i <= n; i++) {
      result = result.multiply(new BigNumber(i.toString()));
    }
    return result;
  }
  
  static pi(precision = 50) {
    if (piCache.scale >= precision) {
      return piCache.roundToPrecision(precision);
    }
    // We’ll compute pi using the Chudnovsky algorithm:
    //    π = 426880 * sqrt(10005) / Σ (k=0..∞) [(-1)^k * (6k)! * (13591409 + 545140134k)]
    //                                          -----------------------------------------
    //                                          (3k)!(k!)^3 * 640320^(3k)
    //
    // We’ll keep adding terms until the next one is effectively zero at our working precision.
  
    // Increase internal precision slightly to help final rounding
    const workingPrecision = precision + 5;
  
    const zero = new BigNumber("0");
    const one = new BigNumber("1");
    const minusOne = new BigNumber("-1");
    let sum = zero;
    let k = 0n;
  
    // Precompute BigNumber(640320) and BigNumber(13591409)
    const C_640320 = new BigNumber("640320");
    const C_13591409 = new BigNumber("13591409");
    const C_545140134 = new BigNumber("545140134");
    let loopCount = 0;
    while (true) {
      loopCount++;
      // Compute (-1)^k
      const sign = (k % 2n === 0n) ? one : minusOne;
  
      // (6k)!
      const sixKFactorial = BigNumber.factorial(6n * k);
  
      // (3k)!
      const threeKFactorial = BigNumber.factorial(3n * k);
  
      // (k!)^3
      const kFactorial = BigNumber.factorial(k);
      const kFactorial3 = kFactorial.multiply(kFactorial).multiply(kFactorial);
  
      // 13591409 + 545140134k
      const linearTerm = C_13591409.add(C_545140134.multiply(new BigNumber(k.toString())));
  
      // 640320^(3k)
      const denominatorPower = C_640320.pow(new BigNumber((3n * k).toString()));
  
      // term = [(-1)^k * (6k)! * (13591409 + 545140134k)] / [(3k)!(k!)^3 * 640320^(3k)]
      let term = sixKFactorial
        .multiply(linearTerm)
        .multiply(sign)
        .divide(threeKFactorial.multiply(kFactorial3).multiply(denominatorPower), workingPrecision);
  
      if (term.isZero()) {
        // When the new term is effectively 0 at this precision, break
        break;
      }
  
      sum = sum.add(term);
      k++;
    }
    // Now compute pi:
    // pi = 426880 * sqrt(10005) / sum

    const sqrt10005 = new BigNumber("10005").sqrt(workingPrecision);
    const factor = new BigNumber("426880").multiply(sqrt10005);

    const piApprox = factor.divide(sum, workingPrecision);

    const piApproxRounded = piApprox.roundToPrecision(precision);
    // Cache this since it's newly most percise PI
    piCache = piApproxRounded;
    
    
    return piApproxRounded;
  }

  static ln2(precision = 50) {
    // We'll internally keep some extra digits to avoid rounding issues
    if (ln2Cache.scale >= precision) {
      return ln2Cache.roundToPrecision(precision);
    }
    const workingPrecision = precision + 10;

    // We'll accumulate the series in sum
    let sum = new BigNumber("0");
    const one = new BigNumber("1");
    const two = new BigNumber("2");

    let k = 1;
    let powerOfTwo = new BigNumber("1"); // Will hold 2^k

    while (true) {
      // Incrementally get 2^k
      powerOfTwo = powerOfTwo.multiply(two);

      // denominator = k * 2^k
      const denominator = new BigNumber(k.toString()).multiply(powerOfTwo);

      // term = 1 / (k * 2^k)
      const term = one.divide(denominator, workingPrecision);

      // If term is effectively 0 at this working precision, we're done
      if (term.isZero()) {
        break;
      }

      sum = sum.add(term);
      k++;
    }

    // Cache this Ln if it's the biggest seen
    if (ln2Cache.scale < sum.scale) {
      ln2Cache = sum;
    }
    return sum.roundToPrecision(precision);
  }



  // Add to BigNumber class
  roundToPrecision(targetPrecision) {
    if (targetPrecision >= this.scale) {
      return this;
    }

    // First, get an extra digit for rounding
    const digitsToRemove = this.scale - targetPrecision;
    const divisor = BigInt("1" + "0".repeat(digitsToRemove));

    // Get the last digit
    const lastDigit = (this.value / (divisor / 10n)) % 10n;

    // Round down by default, round up if last digit >= 5
    const roundedValue = lastDigit >= 5n
      ? (this.value + divisor) / divisor
      : this.value / divisor;

    return BigNumber.fromBigInt(roundedValue, targetPrecision);
  }

  // Exponential function using Taylor series
  static exp(x, precision = 20) {
    // e^x = 1 + x + x^2/2! + x^3/3! + ...
    let sum = new BigNumber("1");
    let term = new BigNumber("1");

    for (let i = 1; i < precision * 2; i++) {
      term = term.multiply(x).divide(new BigNumber(i.toString()), precision);
      sum = sum.add(term);

      // Check for convergence
      if (term.isZero()) break;
    }

    return sum;
  }

  // Helper method for integer powers
  _integerPow(exponent) {
    let result = new BigNumber("1");
    let base = this;
    let exp = exponent.value;

    if (exp < 0n) {
      base = new BigNumber("1").divide(base, this.scale);
      exp = -exp;
    }

    while (exp > 0n) {
      if (exp % 2n === 1n) {
        result = result.multiply(base);
      }
      base = base.multiply(base);
      exp = exp / 2n;
    }
    return result;
  }

  // Helper method to compare two BigNumbers
  compare(other) {
    const [a, b] = this._alignScales(this, other);
    if (a < b) return -1;
    if (a > b) return 1;
    return 0;
  }

  // Helper method to check if number is zero
  isZero() {
    return this.value === 0n;
  }

  sqrt(precision = 50) {
    if (this.value === 0n) return this;
  
    // Choose an initial guess: half of x or something simpler
    let guess = this.divide(new BigNumber("2"), precision * 2);
  
    while (true) {
      // newGuess = (guess + x/guess) / 2
      const newGuess = guess.add(
        this.divide(guess, precision * 2)
      ).divide(new BigNumber("2"), precision * 2);
  
      // Check if they are the same at the desired precision
      if (newGuess.compare(guess) === 0) {
        // Round final to requested precision
        return newGuess.roundToPrecision(precision);
      }
      guess = newGuess;
    }
  } 
}
let eCache = new BigNumber("2.7183");
let ln2Cache = new BigNumber("0.6932");
let piCache = new BigNumber("3.1416");