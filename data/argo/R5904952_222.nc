CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS      N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:55Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    =�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ?   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    C�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  D�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  I0   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    M�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  N�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    SP   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Tp   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  X�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    c   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  d0   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  h�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    h�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    k�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    n�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  q�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    r   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    r   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    r   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    r   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  r   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    r\   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    rl   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    rp   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         r�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         r�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        r�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    r�Argo profile    3.1 1.2 19500101000000  20181005190555  20181005190555  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��韫 h1   @���-��@0����o�c��-V1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�  @�  A   A   AA��A`  A���A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B���B�  B�  C   C  C  C�fC  C
  C�C  C  C  C  C  C  C  C  C  C   C"  C#�fC&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD�CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C��3C��3C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  D   D � DfD� D  Dy�D��D� D��Dy�D��D� D��Dy�D��Dy�D  D� D	  D	� D	��D
� D  D� D  D�fD  Dy�D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D�fDfD� DfD�fDy�fD�5qD��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��H@��HAp�A%p�AG
>Aep�A��A��RA��RA��RA¸RAҸRA�RA�RB\)B	\)B\)B\)B!\)B)\)B1\)B9\)BA\)BI\)BQ\)BYBa\)Bi\)Bq\)By\)B��B��B��B��B��B��B��B��B��B��B��B��B�z�B��B��B��B�z�BĮBȮB̮BЮBԮBخB��GB�B�B�B�B�B�z�B��B��C W
CW
CW
C=pCW
C
W
Cp�CW
CW
CW
CW
CW
CW
CW
CW
CW
C W
C"W
C$=pC&W
C(W
C*W
C,W
C.W
C0W
C2W
C4W
C6W
C8W
C:W
C<W
C>W
C@W
CBW
CDp�CFW
CHW
CJW
CLW
CNW
CPW
CRW
CTW
CVW
CXW
CZW
C\W
C^W
C`W
CbW
CdW
CfW
ChW
CjW
ClW
CnW
CpW
CrW
CtW
CvW
CxW
CzW
C|W
C~W
C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C��C�+�C�+�C�8RC�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�8RC�8RC�+�C�+�C�8RC�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C��C�+�C�8RC�+�C�+�C�+�C�8RC�8RC�8RC�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�8RC�8RC�+�C��C��C��C�+�C�+�C�8RC�+�C�+�C�+�C�+�C�+�C�+�C��C�+�C�+�C�+�C�+�C�+�C�+�C�+�D �D ��D)D��D�D�]D]D��D]D�]D]D��D]D�]D]D�]D�D��D	�D	��D
]D
��D�D��D�D�)D�D�]D�D��D�D�)D�D��D�D��D�D��D�D��D�D��D�D��D�D�)D�D��D�D�)D)D��D)D�)Dy�)D�@RD�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A��Aɺ^A�jA�?}A��A�A��A��A��TA��yA���A�  A��AȲ-A�A�AǮA�A�A�7LA��
A�A���A�n�A��TA���A��A��HA��#AɶFAȕ�A�jA�M�A�1A�jAƙ�A��A�z�A�
=A�E�A�K�A�XA��hA�ĜA��A�ȴA�ĜA�p�A�t�A���A��wA�K�A��A�x�A���A�ZA�K�A���A���A���A�5?A��!A�ZA��A��A�M�A��yA��^A���A��A�ffA��A���A�
=A��TA��A�l�A�r�A��TA�ZA��HA�t�A�33A�1A��yA��;A�;dA�$�A� �A�C�A���A���A�l�A�?}A�oA�n�A��7A�VA�~�A�x�A���A~�RAz^5Ay?}Av�\As`BAp�\Ao|�Al�Ak�mAfA�AaVA]A\��A[%AY��AU�ASK�AR�AO�AM+AJ��AI\)AG��AD�DAD$�AC%A@�yA>�/A<�A;�mA:ȴA:  A8�/A7��A6�A5K�A4�yA3|�A2 �A0��A-�
A-33A,��A+��A+
=A)��A(z�A&�yA&9XA%XA$jA"�A!G�A ��A��A�uA�RA9XA��A��A�A�A�A��A9XA?}A��A�/A�DA�A�A �AG�AE�A
=A	�^A
  A
$�A	�7A\)A�AS�A�A��A��AdZ@�dZ@��7@��P@��7@��@��@�V@���@���@�@�R@�@�b@�@�@�S�@��y@�\@�n�@�E�@�$�@��@��@��m@��y@��@��@�~�@��#@�/@�@��@Гu@�\)@�-@�@��#@�?}@���@̃@�Q�@�ƨ@�+@�V@ɡ�@ɑh@�`B@�/@���@�z�@�b@�\)@�|�@�1'@���@ȴ9@���@�;d@�b@ȼj@��
@ƸR@�Ĝ@��H@���@���@�p�@��`@� �@��@��@���@���@�A�@��@���@�"�@���@�5?@�n�@�/@�@��#@���@��^@��-@�hs@�?}@���@�`B@���@�dZ@�$�@��+@���@�Z@�Z@�A�@�+@�U2@��@nȴ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A��Aɺ^A�jA�?}A��A�A��A��A��TA��yA���A�  A��AȲ-A�A�AǮA�A�A�7LA��
A�A���A�n�A��TA���A��A��HA��#AɶFAȕ�A�jA�M�A�1A�jAƙ�A��A�z�A�
=A�E�A�K�A�XA��hA�ĜA��A�ȴA�ĜA�p�A�t�A���A��wA�K�A��A�x�A���A�ZA�K�A���A���A���A�5?A��!A�ZA��A��A�M�A��yA��^A���A��A�ffA��A���A�
=A��TA��A�l�A�r�A��TA�ZA��HA�t�A�33A�1A��yA��;A�;dA�$�A� �A�C�A���A���A�l�A�?}A�oA�n�A��7A�VA�~�A�x�A���A~�RAz^5Ay?}Av�\As`BAp�\Ao|�Al�Ak�mAfA�AaVA]A\��A[%AY��AU�ASK�AR�AO�AM+AJ��AI\)AG��AD�DAD$�AC%A@�yA>�/A<�A;�mA:ȴA:  A8�/A7��A6�A5K�A4�yA3|�A2 �A0��A-�
A-33A,��A+��A+
=A)��A(z�A&�yA&9XA%XA$jA"�A!G�A ��A��A�uA�RA9XA��A��A�A�A�A��A9XA?}A��A�/A�DA�A�A �AG�AE�A
=A	�^A
  A
$�A	�7A\)A�AS�A�A��A��AdZ@�dZ@��7@��P@��7@��@��@�V@���@���@�@�R@�@�b@�@�@�S�@��y@�\@�n�@�E�@�$�@��@��@��m@��y@��@��@�~�@��#@�/@�@��@Гu@�\)@�-@�@��#@�?}@���@̃@�Q�@�ƨ@�+@�V@ɡ�@ɑh@�`B@�/@���@�z�@�b@�\)@�|�@�1'@���@ȴ9@���@�;d@�b@ȼj@��
@ƸR@�Ĝ@��H@���@���@�p�@��`@� �@��@��@���@���@�A�@��@���@�"�@���@�5?@�n�@�/@�@��#@���@��^@��-@�hs@�?}@���@�`B@���@�dZ@�$�@��+@���@�Z@�Z@�A�@�+@�U2@��@nȴ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	s�B	t�B	t�B	s�B	u�B	|�B	�%B	�hB	��B	�^B	��B	�B	�B	�sB	�ZB	��B
 �B
5?B
C�B
r�B
�'B
�}B
�)BBbB{B�B�B�B
=BBB+BhB1'BF�BffBv�B�=B��B�FB��B�/B��BPB �B&�B/B49B49B49B49B6FB0!B0!B0!B5?B;dB@�BA�B:^B9XB;dB>wBD�BF�BD�BF�BT�BN�B;dB1'B�BB�HB�#B�/B��B�B��B|�B]/BI�B8RB�BB
�`B
ĜB
��B
�=B
s�B
p�B
w�B
|�B
r�B
S�B
N�B
.B	��B	�B	�B	�dB	��B	ŢB	�XB	��B	��B	�\B	�%B	k�B	L�B	9XB	33B	-B	$�B	uB	%B��B�B�mB�/B�B��BĜBB�wB�^B�?B�B�B�B��B��B��B��B��B��B�B�B��B�B�B��B��B��B��B��B�B�B�B�B�B�B�B��B�B�B�B�-B�jB�jB�LB�?B�dB�XB�?B�LB�qBĜBƨB��B��B��BŢB�qB�}BŢBǮBŢB�qB�^B�XB�RB�^B�^B�3B�!B�FB�9B�'B�'B�!B�!B�!B�!B�-B�?B�FB�-B�FB�LB�RB��BÖBŢBǮBɺB��B��B��B�
B�;B�HB�TB�fB�sB�yB�B	VB	VB	hB	uB	uB	�B	�B	"�B	%�B	(�B	)�B	0!B	49B	6FB	8RB	9XB	;dB	@�B	D�B	E�B	I�B	Q�B	^5B	ffB	aHB	cTB	jB	u�B	w�B	v�B	s�B	o�B	o�B	o�B	p�B	t�B	x�B	z�B	|�B	}�B	}�B	|�B	{�B	{�B	� B	� B	� B	�B	��B	��B	��B	��B	�B	�B	�B	�B	�3B	�3B	�'B	�!B	�B	�-B	�9B	�'B	�-B	�FB	�?B
�B
/B
<6222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222B	s�B	t�B	t�B	s�B	u�B	|�B	�%B	�hB	��B	�^B	��B	�B	�B	�sB	�ZB	��B
 �B
5?B
C�B
r�B
�'B
�}B
�)BBbB{B�B�B�B
=BBB+BhB1'BF�BffBv�B�=B��B�FB��B�/B��BPB �B&�B/B49B49B49B49B6FB0!B0!B0!B5?B;dB@�BA�B:^B9XB;dB>wBD�BF�BD�BF�BT�BN�B;dB1'B�BB�HB�#B�/B��B�B��B|�B]/BI�B8RB�BB
�`B
ĜB
��B
�=B
s�B
p�B
w�B
|�B
r�B
S�B
N�B
.B	��B	�B	�B	�dB	��B	ŢB	�XB	��B	��B	�\B	�%B	k�B	L�B	9XB	33B	-B	$�B	uB	%B��B�B�mB�/B�B��BĜBB�wB�^B�?B�B�B�B��B��B��B��B��B��B�B�B��B�B�B��B��B��B��B��B�B�B�B�B�B�B�B��B�B�B�B�-B�jB�jB�LB�?B�dB�XB�?B�LB�qBĜBƨB��B��B��BŢB�qB�}BŢBǮBŢB�qB�^B�XB�RB�^B�^B�3B�!B�FB�9B�'B�'B�!B�!B�!B�!B�-B�?B�FB�-B�FB�LB�RB��BÖBŢBǮBɺB��B��B��B�
B�;B�HB�TB�fB�sB�yB�B	VB	VB	hB	uB	uB	�B	�B	"�B	%�B	(�B	)�B	0!B	49B	6FB	8RB	9XB	;dB	@�B	D�B	E�B	I�B	Q�B	^5B	ffB	aHB	cTB	jB	u�B	w�B	v�B	s�B	o�B	o�B	o�B	p�B	t�B	x�B	z�B	|�B	}�B	}�B	|�B	{�B	{�B	� B	� B	� B	�B	��B	��B	��B	��B	�B	�B	�B	�B	�3B	�3B	�'B	�!B	�B	�-B	�9B	�'B	�-B	�FB	�?B
�B
/B
<6222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.34 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190555                              AO  ARCAADJP                                                                    20181005190555    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190555  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190555  QCF$                G�O�G�O�G�O�8000            