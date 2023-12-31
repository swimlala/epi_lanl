CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS      N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-01T23:28:33Z UW 3.1 conversion   
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7T   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    88   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8<   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8D   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8H   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8P   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8X   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8`   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8d   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8l   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9l   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9x   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z           9|   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    =|   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        >|   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    B|   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        C|   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o        G|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K|   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o        L|   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    P|   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o        Q|   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o        U|   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    Y|   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o        Z|   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ^|   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o        _|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  c|   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    c�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    f�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    i�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  l�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    l�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    l�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    l�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    l�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  l�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    m(   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    m8   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    m<   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         mL   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         mP   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        mT   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    mXArgo profile    3.1 1.2 19500101000000  5900960 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130110405  20190523124445  1514_5041_175                   2C  D   APEX                            2041                            062805                          846 @����x1   @����x@6o\(��cTz�G�1   GPS     Primary sampling: mixed [deeper than nominal 500dbar: discrete; nominal 500dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C���DٚDl�D"  D.ffD:��DG� DT3D`l�Dl�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�  @���@���A��A<��A\��A|��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B33B33B33B'33B/33B733B?33BG33BO33BW33B_33Bg33Bo33Bw33B33B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BÙ�BǙ�B˙�Bϙ�Bә�Bי�Bۙ�Bߙ�B㙚B癚B뙚BB�B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC�� D��D` D"3D.Y�D:� DGs3DTfD`` Dl�f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�l�A��A�O�Aљ�A�x�A�ZA�I�A�=qA�5?A�+A�"�A��A��A�oA�
=A�A���A���A��A��A��TA�ĜA��A�-AʅAȉ7A�(�A�Q�A�  A��7A�K�A���A�VA�dZA���A�-A�O�A��A��A�;dA��RA��mA�jA���A�O�A��
A�C�A�dZA�x�A��7A� �A��/A��7A��yA�n�A�t�A�&�A��A��A��A�bA�=qA�VA��A��\A�?}A�t�A���A�VA�I�A��\A�dZA�x�A��9A�~�A��A�|�A��A��A���A�ƨA��TA�9XA�bA���A���A��A�bNA�%A�/A�bNA�t�A���A�?}A���A�G�A�?}A}�A|�DA|A�A{��Aw��Au�hAs�mAqhsAo�wAoAnbNAl��Ak�wAj�uAh^5Ag\)Ag7LAf�RAe�hAbE�A_XA]�FA\�+AZ��AY�AYp�AY
=AXM�AV�\AT��ATZAS`BAR�jARbAOXAM�AK`BAJM�AI|�AH�\AG7LAF��AE��AE
=ADM�AC�ABZAA+A@A?XA>z�A=7LA;S�A:=qA: �A9�;A9��A9x�A9%A8��A7ƨA6  A4��A3��A2�uA2ZA1��A0�`A0Q�A0  A/��A/��A.z�A-��A,��A+��A+|�A)|�A'��A'A&M�A%�A%?}A$�A#dZA"A�A 1'AA^5AbAt�A�RA��AĜAA�AK�A��A�A�A��A9XA|�A�AM�A`BA�A{A?}A~�A^5AZAA�A�+A�A�PA7LA
�/A	��A	7LA��A��Ax�AVA��A�A�Ar�A�^A��A1AVA �\@��T@��@�7L@�1@��R@�D@�@���@���@��H@���@�  @�@�9@�|�@�@��`@�?}@���@�hs@��@�;d@�J@�hs@�
=@��^1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�l�A��A�O�Aљ�A�x�A�ZA�I�A�=qA�5?A�+A�"�A��A��A�oA�
=A�A���A���A��A��A��TA�ĜA��A�-AʅAȉ7A�(�A�Q�A�  A��7A�K�A���A�VA�dZA���A�-A�O�A��A��A�;dA��RA��mA�jA���A�O�A��
A�C�A�dZA�x�A��7A� �A��/A��7A��yA�n�A�t�A�&�A��A��A��A�bA�=qA�VA��A��\A�?}A�t�A���A�VA�I�A��\A�dZA�x�A��9A�~�A��A�|�A��A��A���A�ƨA��TA�9XA�bA���A���A��A�bNA�%A�/A�bNA�t�A���A�?}A���A�G�A�?}A}�A|�DA|A�A{��Aw��Au�hAs�mAqhsAo�wAoAnbNAl��Ak�wAj�uAh^5Ag\)Ag7LAf�RAe�hAbE�A_XA]�FA\�+AZ��AY�AYp�AY
=AXM�AV�\AT��ATZAS`BAR�jARbAOXAM�AK`BAJM�AI|�AH�\AG7LAF��AE��AE
=ADM�AC�ABZAA+A@A?XA>z�A=7LA;S�A:=qA: �A9�;A9��A9x�A9%A8��A7ƨA6  A4��A3��A2�uA2ZA1��A0�`A0Q�A0  A/��A/��A.z�A-��A,��A+��A+|�A)|�A'��A'A&M�A%�A%?}A$�A#dZA"A�A 1'AA^5AbAt�A�RA��AĜAA�AK�A��A�A�A��A9XA|�A�AM�A`BA�A{A?}A~�A^5AZAA�A�+A�A�PA7LA
�/A	��A	7LA��A��Ax�AVA��A�A�Ar�A�^A��A1AVA �\@��T@��@�7L@�1@��R@�D@�@���@���@��H@���@�  @�@�9@�|�@�@��`@�?}@���@�hs@��@�;d@�J@�hs@�
=@��^1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB}�B|�Bx�Bx�Bz�B{�Bz�B{�B{�B|�B|�B|�B|�B{�B{�Bz�By�By�By�By�By�Bx�Bu�Bv�BiyB]/BXBN�BA�B-B+B+B.B0!B7LB5?B2-B1'B0!B/B%�B�B!�B(�B$�B�B �B�B1BB�B��B��BŢB��B�mB�mB��B�RB��B��B�BffBI�B@�B=qB2-B"�BDB��B�yB�BĜB�FB�3B�B��Bn�BP�BB�B.B"�BoBB
�B
�/B
��B
��B
��B
�^B
��B
��B
�VB
�1B
�B
w�B
iyB
N�B
I�B
G�B
<jB
%�B
{B
+B	��B	�yB	�`B	�5B	��B	��B	ƨB	�^B	�?B	�3B	�B	��B	�oB	�B	z�B	s�B	l�B	hsB	ffB	cTB	`BB	W
B	P�B	M�B	J�B	F�B	A�B	;dB	/B	,B	.B	,B	33B	1'B	.B	.B	)�B	$�B	#�B	!�B	�B	�B	uB	{B	1B	  B��B��B��B��B��B��B�B�B�fB�HB�B�
B��B��B��B��B��B��BȴBÖB�wB�^B�RB�9B�!B�B��B��B��B��B��B��B��B�oB�hB�\B�\B�\B�PB�DB�+B�B�B� B~�B{�By�Bx�Bv�Bu�Bs�Bp�Bo�Bq�Bn�BjBiyBffBbNB_;B]/B^5BZB[#BZBXBT�BQ�BQ�BO�BP�BR�BM�BJ�BG�BF�BG�BE�BB�B@�B>wB;dB:^B8RB8RB8RB6FB5?B5?B49B2-B33B49B49B2-B33B2-BP�B�B��B��B	=qB	s�B	��B	��B	�y1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B� B~�B{�Bx�B{�B{�B{�B{�B{�B|�B|�B|�B|�B|�B{�Bz�By�By�By�Bz�By�Bz�B~�B~�Bl�B`BB[#B[#BL�B1'B,B0!B1'B7LB=qB9XB49B5?B49B5?B)�B�B$�B,B'�B"�B$�B �BJB	7B�B�B��BǮB��B�sB�B��B�dB�B��B�7Bk�BK�BB�B@�B9XB)�BbB��B�B�#BǮB�LB�?B�'B��Bt�BT�BE�B1'B$�B�B%B
�B
�HB
�
B
��B
ŢB
�wB
�B
��B
�bB
�=B
�B
{�B
p�B
O�B
J�B
I�B
F�B
+B
�B
PB	��B	�B	�fB	�TB	�
B	��B	��B	�qB	�FB	�?B	�3B	�B	��B	�1B	}�B	x�B	n�B	jB	gmB	ffB	e`B	]/B	Q�B	P�B	L�B	H�B	H�B	A�B	49B	/B	0!B	.B	5?B	33B	/B	0!B	+B	'�B	%�B	#�B	�B	�B	�B	�B	JB	B��B��B��B��B��B��B��B�B�B�`B�/B�B�B��B��B��B��B��B��BŢB��B�jB�^B�^B�?B�B�B��B��B��B��B��B��B��B�uB�bB�hB�hB�\B�PB�7B�%B�B�B� B}�Bz�Bz�Bx�Bw�Bv�Bs�Bp�Bs�Bp�BjBiyBjBe`BbNB_;B`BB\)B\)B]/BZBVBS�BR�BQ�BQ�BS�BP�BL�BJ�BI�BI�BH�BD�BD�BC�B=qB<jB:^B;dB;dB9XB7LB7LB6FB5?B6FB6FB6FB49B33B2-BP�B�B��B��B	=qB	s�B	��B	��B	�y1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<D��<#�
<#�
<#�
<u<e`B<#�
<#�
<#�
<#�
<D��<49X<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<49X<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<e`B<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<T��<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<D��<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.2 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201112181658102011121816581020111218165810  AO  ARGQ                                                                        20111130110405  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130110405  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20111218165810  IP                  G�O�G�O�G�O�                