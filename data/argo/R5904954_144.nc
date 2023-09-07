CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:21Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  <   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  <�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  ?X   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  @    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  B�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  E@   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  E�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  H�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  I0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  K�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  Np   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  O   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  Q�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  R`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  U    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    U0   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    X0   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    [0   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ^0   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ^\   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ^`   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ^d   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ^h   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ^l   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ^�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ^�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ^�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ^�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ^�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ^�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ^�Argo profile    3.1 1.2 19500101000000  20181005191721  20181005191721  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��d���1   @��eWM@5���F�dx�\)1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @y��@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffBffB  B   B(  B0ffB8ffB@  BH  BP  BX  B`  Bh  BpffBx  B�  B�  B�  B�  B�  B�33B�  B�  B�  C   C�C  C  C  C
  C  C  C  C  C�C  C  C  C  CL  CN  CO�fCR  CT  CU�fCX  CZ  C\  C^  C_�fCb  Cd  Cf  Ch  Ci�fCl  Cn  Cp  Cq�fCs�fCu�fCx  Cz  C|  C~  C�fC�  C��3C�  C��3C��3C��3C��3C�  C�  C��3C�  C�  C��C�  C�  C�  C��C��C�  C�  C��3C��3C��C��C��C�  C��3C��C�  C�  C�  C�  C�  C��C��3C��C�  C�  C�  C�  C��C��C�  C��3C�  C�  C�  C�  C�  C��3C��3C��3C�  C��C��C��C�  C��3C��C��C��C��C��C��C��C��C��C�  C��3C��fC��3C��3C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C��3C��3C�  Dy��D�+�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�(�@�(�A�A#�AC�Ac�A��
A��
A��
A��
A��
A��
A��
A��
B �B	Q�BQ�B�B �B(�B1Q�B9Q�B@�BH�BP�BX�B`�Bh�BqQ�Bx�B�u�B�u�B�u�B�u�B�u�B��B�u�B�u�B�u�C :�CT{C:�C:�C:�C
:�C:�C:�C:�C:�CT{C:�C:�C:�C:�CL:�CN:�CP!GCR:�CT:�CV!GCX:�CZ:�C\:�C^:�C`!GCb:�Cd:�Cf:�Ch:�Cj!GCl:�Cn:�Cp:�Cr!GCt!GCv!GCx:�Cz:�C|:�C~:�C��C�qC��C�qC��C��C��C��C�qC�qC��C�qC�qC�7C�qC�qC�qC�*>C�*>C�qC�qC��C��C�*>C�*>C�*>C�qC��C�*>C�qC�qC�qC�qC�qC�*>C��C�*>C�qC�qC�qC�qC�7C�*>C�qC��C�qC�qC�qC�qC�qC��C��C��C�qC�*>C�*>C�7C�qC��C�*>C�*>C�7C�*>C�*>C�*>C�*>C�*>C�*>C�qC��C��C��C��C�qC�qC�qC�qC��C��C�qC�qC�qC�qC��C��C�qDy�=D�33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A؉7A؋DA؋DA؍PA؍PA؍PA؋DA؋DA؍PA؏\A؏\A؍PAؑhAؕ�AؓuAؑhA؏\A؏\A؍PA؇+A؁A�~�A�~�A�t�A�jA�A�A׾wA��A�VA��mA�%A��A�ƨA�VA�C�A��wA��;A��yA�  A��A�A�I�A��-A��!A���A��A�|�A�ffA�E�A�x�A�XA���A�1'A�"�A^jA]33A[/AXM�AUƨAS�#AQ;dAO��ANjAJ^5AG��AD��ABffA@��A?��A?&�A>��A>ffA=t�A;��A:��A:bNA9�A9/A8E�A6��A5&�A3�PA25?A0��A/�A-t�A+p�A)7LA'x�A'A&�RA%�TA%A$(�A"��A"bA!l�A �AS�A�+A�A�/A~�A{A�A
��A
E�A	p�A	�A(�Ap�A�HAI�A�PA�A��AA1Ax�A �A n�@�K�@�@�V@��w@��R@��7@�bN@���@�dZ@�M�@�&�@�E�@��@��m@���@�ff@�&�@�  @柾@��/@��@�5?@�7L@�r�@���@ݺ^@ܓu@�+@���@ٲ-@׾w@�M�@��/@���@��y@��@�1'@ύP@�~�@�ff@���@�
=@���@ʏ\@�E�@x�O@h�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A؉7A؋DA؋DA؍PA؍PA؍PA؋DA؋DA؍PA؏\A؏\A؍PAؑhAؕ�AؓuAؑhA؏\A؏\A؍PA؇+A؁A�~�A�~�A�t�A�jA�A�A׾wA��A�VA��mA�%A��A�ƨA�VA�C�A��wA��;A��yA�  A��A�A�I�A��-A��!A���A��A�|�A�ffA�E�A�x�A�XA���A�1'A�"�A^jA]33A[/AXM�AUƨAS�#AQ;dAO��ANjAJ^5AG��AD��ABffA@��A?��A?&�A>��A>ffA=t�A;��A:��A:bNA9�A9/A8E�A6��A5&�A3�PA25?A0��A/�A-t�A+p�A)7LA'x�A'A&�RA%�TA%A$(�A"��A"bA!l�A �AS�A�+A�A�/A~�A{A�A
��A
E�A	p�A	�A(�Ap�A�HAI�A�PA�A��AA1Ax�A �A n�@�K�@�@�V@��w@��R@��7@�bN@���@�dZ@�M�@�&�@�E�@��@��m@���@�ff@�&�@�  @柾@��/@��@�5?@�7L@�r�@���@ݺ^@ܓu@�+@���@ٲ-@׾w@�M�@��/@���@��y@��@�1'@ύP@�~�@�ff@���@�
=@���@ʏ\@�E�@x�O@h�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bo�Bo�Bo�Bo�Bo�Bn�Bn�Bl�Be`BW
B_;Bp�Bv�B��B�PBx�BffBT�BK�BA�B5?B�BJB�B��B�!B��B�DBt�BVB9XB�BVBB
�B
�BB	e`B	]/B	Q�B	C�B	5?B	+B	�B	�B	VB��B�B�fB�;B�B�
B��B��B��B��BǮBĜBB�}B�wB�dB�LB�3B�B�B��B��B��B��B��B��B��B��B��B��B�{B�oB�bB�\B�\BS�Be`B^5B\)B[#BZBW
BS�BR�BO�BN�BM�BK�BH�BG�BE�BC�BB�B?}B=qB;dB9XB8RB7LB6FB5?B5?B49B33B1'B1'B/B/B-B.B-B-B-B,B,B,B+B,B,B-B-B,B+B,B+B,B,B-B.B/B/B0!B0!B2-B2-B2-B33B2-B49B7LB8RB8RB8RB
�B
�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bo�Bo�Bo�Bo�Bo�Bn�Bn�Bl�Be`BW
B_;Bp�Bv�B��B�PBx�BffBT�BK�BA�B5?B�BJB�B��B�!B��B�DBt�BVB9XB�BVBB
�B
�BB	e`B	]/B	Q�B	C�B	5?B	+B	�B	�B	VB��B�B�fB�;B�B�
B��B��B��B��BǮBĜBB�}B�wB�dB�LB�3B�B�B��B��B��B��B��B��B��B��B��B��B�{B�oB�bB�\B�\BS�Be`B^5B\)B[#BZBW
BS�BR�BO�BN�BM�BK�BH�BG�BE�BC�BB�B?}B=qB;dB9XB8RB7LB6FB5?B5?B49B33B1'B1'B/B/B-B.B-B-B-B,B,B,B+B,B,B-B-B,B+B,B+B,B,B-B.B/B/B0!B0!B2-B2-B2-B33B2-B49B7LB8RB8RB8RB
�B
�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.23 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191721                              AO  ARCAADJP                                                                    20181005191721    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191721  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191721  QCF$                G�O�G�O�G�O�8000            