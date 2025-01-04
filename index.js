export class BigNumber {
  constructor(value) {
    // If the value is given as a string like "123.456"
    if (typeof value === "string") {
      // Parse the string to extract scale and BigInt value
      const [intPart, fracPart = ""] = value.split(".");

      // Scale is the length of the fractional part
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
    const integerPartLength = strValue.length - this.scale;
    if (integerPartLength <= 0) {
      // Pad with leading zeros if fraction is longer than integer part
      return "0." + strValue.padStart(this.scale, "0");
    }

    const intPart = strValue.slice(0, integerPartLength);
    const fracPart = strValue.slice(integerPartLength);
    return intPart + "." + fracPart;
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

    // Add 5 to the last digit and then divide by 10 to round
    const roundedQuotient = (rawQuotient + 5n) / 10n;

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

  // Natural logarithm using Taylor series
  ln(precision = 20) {
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
  static pi(precision = 50) {
    // If we already computed pi at or beyond this precision, reuse cache
    if (piCache.scale >= precision) {
      return piCache.roundToPrecision(precision);
    }
    const workingPrecision = precision + 10; // extra digits
    const one = new BigNumber("1");
    const negOne = new BigNumber("-1");
    const twelve = new BigNumber("12");

    // Chudnovsky constants
    const a = new BigNumber("13591409");
    const b = new BigNumber("545140134");
    // We use +640320 and rely on (-1)^k for sign flips
    const c = new BigNumber("640320");

    // sum = Σ_k [ (-1)^k * (6k)! * (13591409 + 545140134*k ) ]
    //             / [ (3k)!(k!)^3 * (640320)^(3k + 1.5 ) ]
    let sum = new BigNumber("0");

    // Factorials we need for each k
    let factorial6k = new BigNumber("1");  // (6*0)!
    let factorial3k = new BigNumber("1");  // (3*0)!
    let factorialk = new BigNumber("1");  // (k)!
    let sign = new BigNumber("1");         // (-1)^k

    // We'll track c^(3k) and multiply by sqrt(c) = c^(1/2)
    let cPow3k = new BigNumber("1");        // c^(3*0)
    const cPow3 = c.pow(new BigNumber("3"));  // for c^(3*(k+1)) each iteration
    const cPowHalf = c.sqrt(workingPrecision); // c^(0.5), constant

    // Roughly 14 digits of π per iteration
    const iterations = Math.ceil(workingPrecision / 14) + 1;

    for (let k = 0; k < iterations; k++) {
      // numerator = (-1)^k * (6k)! * (a + b*k)
      const kBN = new BigNumber(k.toString());
      const numerator = sign
        .multiply(factorial6k)
        .multiply(a.add(b.multiply(kBN)));

      // denominator = (3k)!(k!)^3 * c^(3k) * c^(1/2)
      const denominator = factorial3k
        .multiply(factorialk.pow(new BigNumber("3")))
        .multiply(cPow3k)
        .multiply(cPowHalf);

      // Add the term to the sum
      const term = numerator.divide(denominator, workingPrecision);
      sum = sum.add(term);

      // Flip sign for next iteration
      sign = sign.multiply(negOne);

      // Update factorials for next k
      // (6(k+1))! = (6k)! * (6k+1)*(6k+2)*(6k+3)*(6k+4)*(6k+5)*(6k+6)
      for (let i = 6 * k + 1; i <= 6 * (k + 1); i++) {
        factorial6k = factorial6k.multiply(new BigNumber(i.toString()));
      }
      // (3(k+1))! = (3k)! * (3k+1)*(3k+2)*(3k+3)
      for (let i = 3 * k + 1; i <= 3 * (k + 1); i++) {
        factorial3k = factorial3k.multiply(new BigNumber(i.toString()));
      }
      // (k+1)! = k! * (k+1)
      factorialk = factorialk.multiply(new BigNumber((k + 1).toString()));

      // c^(3(k+1)) = c^(3k) * c^3
      cPow3k = cPow3k.multiply(cPow3);
    }

    // According to Chudnovsky: 1/pi = 12 * sum(...)
    // => pi = 1 / (12 * sum)
    const denominator = sum.multiply(twelve);
    const piValue = one.divide(denominator, workingPrecision);

    // Update piCache if we got more precision than ever
    if (piValue.scale > piCache.scale) {
      piCache = piValue;
    }
    // Return π with the requested precision
    return piValue.roundToPrecision(precision);
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

  sqrt(precision = 20) {
    return this.pow(new BigNumber("0.5"), precision);
  }
}
let ln2Cache = new BigNumber("0.6931471805599453094172321214581765680755001343602552541206800094933936219696947156058633269964186875");
let piCache = new BigNumber("3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679");
