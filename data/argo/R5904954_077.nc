CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:06Z creation      
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
_FillValue                  �  <l   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  =,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  @(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  @�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  C�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  F�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  G�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  J�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  K\   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  NX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  QT   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  R   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  U   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  X�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    X�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    [�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ^�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  a�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    b(   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    b,   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    b0   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    b4   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  b8   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    bx   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    b�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    b�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         b�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         b�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        b�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    b�Argo profile    3.1 1.2 19500101000000  20181005191706  20181005191706  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               MA   AO  6557                            2B  A   APEX                            7468                            062512                          846 @�¥OV(1   @�¥��QV@5��E��d(bM��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      MA   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BO��BX  B`  Bh  Bp  BxffB�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�C�C�C�C
  C  C  C�fC�fC  C  C  C�C�C  C�fC"  C$  C&  C(�C*�C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CE�fCH  CJ  CL  CN  CP  CR  CT  CU�fCW�fCZ  C\  C^  C`  Cb  Cd  Cf  Cg�fCj�Cl�Cn  Cp  Cq�fCs�fCu�fCw�fCy�fC|  C~�C��C�  C�  C�  C��3C��3C�  C�  C��3C��3C��3C�  C�  C�  C�  C��C��C�  C��3C��3C�  C��C�  C�  C��C��C��C��C��C��C��C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C��C��C��fC��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��3C��C�  C��3Dy��D�L�D�l{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�  @���A ��A ��A@��A`��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB 33B33B33B33B 33B(33B033B833B@33BH33BO��BX33B`33Bh33Bp33Bx��B�L�B�L�B��B��B��B��B��B��B��B��B��B��B��gB��gB��B��B��B�L�B��B��B��B��B��B��B��B��B��B��B��B��B��B��C �C&gC&gC&gC&gC
�C�C�C�3C�3C�C�C�C&gC&gC�C�3C"�C$�C&�C(&gC*&gC,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CE�3CH�CJ�CL�CN�CP�CR�CT�CU�3CW�3CZ�C\�C^�C`�Cb�Cd�Cf�Cg�3Cj&gCl&gCn�Cp�Cq�3Cs�3Cu�3Cw�3Cy�3C|�C~&gC�3C�fC�fC�fC���C���C�fC�fC���C���C���C�fC�fC�fC�fC�3C�3C�fC���C���C�fC�3C�fC�fC�3C�3C�3C�3C�3C�3C�3C�fC�fC�fC�  C�fC�fC�fC�fC���C�fC�3C�3C���C���C���C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�3C�3C���C�3C�fC���Dy�
D�NgD�n11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�1'A�5?A�33A�33A�=qA�C�A�C�A�G�A�I�A�G�A�E�A�E�A�E�A�K�A�K�A�K�A�I�Aڕ�A�`BA�ƨA��;A�$�A��yAиRAЃA�;dA�^5A�E�A�
=Aɏ\A��A��/AċDA���A�z�A���A���A�|�A��wA�5?A��;A��RA��A�S�A�$�A� �A���A�;dA���A��uA�~�A�`BA��A�/A�1A��A��`A��A�;dA�XA��A�VA���A��RA�O�A��mA�A�ȴA�ƨA�l�A��A�/A�r�A��A�7LA��#A��^A�^5A��uA�
=A�$�A���A���A�?}A���A���A�I�A��A�ZA��A��A��DA���A�K�A�S�A���A�bNA���A�%A��A~�/A}��A|�yA{�A{�7AzȴAy��Awl�AudZAtE�As�TAs/Aq"�An�AkAh��AeVA_��A\z�AZ�AW�-AUXAS�7AQO�AP�AOp�AMl�AL~�AJbNAI&�AIoAG�AE?}AB�/AA�TAA�PA?�PA="�A;�7A;K�A;?}A;&�A8v�A5VA41'A3;dA2�jA1��A1?}A0(�A-��A,�/A,��A*�!A)�PA(��A'l�A&ffA&�A%t�A#�A"�jA"�A!33A��Ap�A�DA��A�TA�
AG�A�yA��A%AA�A��AĜA9XA�wAS�A�DA��A
=A�AȴA��A�mAO�@�[W@v+k@hg811111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�1'A�5?A�33A�33A�=qA�C�A�C�A�G�A�I�A�G�A�E�A�E�A�E�A�K�A�K�A�K�A�I�Aڕ�A�`BA�ƨA��;A�$�A��yAиRAЃA�;dA�^5A�E�A�
=Aɏ\A��A��/AċDA���A�z�A���A���A�|�A��wA�5?A��;A��RA��A�S�A�$�A� �A���A�;dA���A��uA�~�A�`BA��A�/A�1A��A��`A��A�;dA�XA��A�VA���A��RA�O�A��mA�A�ȴA�ƨA�l�A��A�/A�r�A��A�7LA��#A��^A�^5A��uA�
=A�$�A���A���A�?}A���A���A�I�A��A�ZA��A��A��DA���A�K�A�S�A���A�bNA���A�%A��A~�/A}��A|�yA{�A{�7AzȴAy��Awl�AudZAtE�As�TAs/Aq"�An�AkAh��AeVA_��A\z�AZ�AW�-AUXAS�7AQO�AP�AOp�AMl�AL~�AJbNAI&�AIoAG�AE?}AB�/AA�TAA�PA?�PA="�A;�7A;K�A;?}A;&�A8v�A5VA41'A3;dA2�jA1��A1?}A0(�A-��A,�/A,��A*�!A)�PA(��A'l�A&ffA&�A%t�A#�A"�jA"�A!33A��Ap�A�DA��A�TA�
AG�A�yA��A%AA�A��AĜA9XA�wAS�A�DA��A
=A�AȴA��A�mAO�@�[W@v+k@hg811111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�fB�fB�mB�mB�fB�fB�fB�fB�fB�fB�mB�mB�mB�fB�fB�fB�`B�/BǮB�3B�!B�9B�RB�dB�wB��B��B�)B�B��BB#�BB�BT�BZBcTBl�B�=B�hB��B��B��B��B��B��B�qB��B��B�wB�qB�dB�?B�-B��B�oB�7B�By�Bl�B^5BYBT�BO�BG�BB�B<jB(�B�B\B  B�B�B�B��BŢB�^B�B��B��B��B�VBt�BiyBhsB_;BQ�B;dB,B�BB
�B
��B
�3B
��B
�%B
�%B
�DB
�B
q�B
e`B
[#B
T�B
M�B
F�B
C�B
<jB
49B
$�B
�B
PB
	7B
B	��B	�)B	��B	�FB	��B	x�B	bNB	S�B	<jB	,B	#�B	�B	�B	uB	+B��B��B�B��B��B�B�NB�#B�B��BĜB�qB�jB�dB�RB�3B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B��B��B
~B
�B
(�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B�fB�fB�mB�mB�fB�fB�fB�fB�fB�fB�mB�mB�mB�fB�fB�fB�`B�/BǮB�3B�!B�9B�RB�dB�wB��B��B�)B�B��BB#�BB�BT�BZBcTBl�B�=B�hB��B��B��B��B��B��B�qB��B��B�wB�qB�dB�?B�-B��B�oB�7B�By�Bl�B^5BYBT�BO�BG�BB�B<jB(�B�B\B  B�B�B�B��BŢB�^B�B��B��B��B�VBt�BiyBhsB_;BQ�B;dB,B�BB
�B
��B
�3B
��B
�%B
�%B
�DB
�B
q�B
e`B
[#B
T�B
M�B
F�B
C�B
<jB
49B
$�B
�B
PB
	7B
B	��B	�)B	��B	�FB	��B	x�B	bNB	S�B	<jB	,B	#�B	�B	�B	uB	+B��B��B�B��B��B�B�NB�#B�B��BĜB�qB�jB�dB�RB�3B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B��B��B
~B
�B
(�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.05 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191706                              AO  ARCAADJP                                                                    20181005191706    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191706  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191706  QCF$                G�O�G�O�G�O�8000            