CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:06Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @T   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  B   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  J�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Q�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Xx   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Z4   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  a   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  b�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  rX   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y<   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  z�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �8   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �<   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �@   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �D   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �H   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181024140806  20181024140806  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @ק�r:K1   @ק�WM@3��n���c�C��%1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   A   @9��@�  @�  A   A   A@  A^ffA~ffA�  A�  A�  A�  A���A�  A�33B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B���B���B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�33B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�C  C  C  C  C  C  C  C   C"  C$  C&  C(  C)�fC,  C.  C0  C2  C4  C6  C8  C:  C<  C>�C@�CB�CD  CF  CH  CJ  Cz�C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  D   D y�D ��D� D  Dy�D  D�fD  D� D  D�fD  Dy�D  D�fD  D� D	  D	� D
  D
� D  D� DfD� D  D� D  D� D  D� D  Dy�D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D�fD  D�fD  D� D  D� D   D � D!  D!� D"  D"� D#  D#y�D$  D$� D%  D%� D&  D&� D'  D'� D(  D(y�D(��D)� D*  D*� D+  D+� D,  D,� D-  D-y�D.  D.� D/  D/� D0fD0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6y�D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� D@��DA� DBfDB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH�fDI  DI� DJ  DJ� DJ��DK� DLfDL� DM  DM� DN  DN� DO  DO� DPfDP�fDQ  DQ�fDRfDR� DS  DS� DT  DT� DT��DUy�DU��DV� DW  DW�fDXfDX� DY  DY� DZ  DZ� D[  D[�fD\  D\� D]fD]� D^  D^y�D_  D_� D`  D`�fDa  Da� Db  Db� Dc  Dc�fDdfDd� De  De�fDf  Df� Dg  Dy}qD�J�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @?\)@��H@��HAp�A!p�AAp�A_�
A�
A��RA��RA��RA��RAхA�RA��B \)B\)B\)B\)B \)B(\)B0\)B8\)B@\)BH\)BP\)BX\)B`\)Bh\)Bp\)Bx\)B�.B�.B�.B�.B�.B�.B���B���B�.B���B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B���B���B�.B�aGB�.B�.B�.B�.B�.B�.B�.C 
C
C
C
C
C

C
C
C0�C
C
C
C
C
C
C
C 
C"
C$
C&
C(
C)�pC,
C.
C0
C2
C4
C6
C8
C:
C<
C>0�C@0�CB0�CD
CF
CH
CJ
Cz0�C|
C~
C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C���C��C��C��C��C��C��C��C��C��C�RC�RC��C��C��C��C��C��C��C��C�RC��C��C��C��C��C��C���C��C��C��C��C��C�RC��C��C��C��C��C��C��C�RC�RC�RC��C��C���C��C��C��C��C��C��C�RC��C���C��C��C��C��C��C��C��C�RC��C��C��C��C��C��C��C��C��C���C��C��C��C���C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C���C��C��C���C��C��C��C��C��C��C��C��C��C��C��C�RC�RC��C��C��D �D ]D �]D��D�D]D�D�)D�D��D�D�)D�D]D�D�)D�D��D	�D	��D
�D
��D�D��D)D��D�D��D�D��D�D��D�D]D�D��D�D]D�D��D�D��D�D��D�D��D�D��D�]D��D�D��D�D��D�D��D�D�)D�D�)D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#]D$�D$��D%�D%��D&�D&��D'�D'��D(�D(]D(�]D)��D*�D*��D+�D+��D,�D,��D-�D-]D.�D.��D/�D/��D0)D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6]D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��D@�]DA��DB)DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH�)DI�DI��DJ�DJ��DJ�]DK��DL)DL��DM�DM��DN�DN��DO�DO��DP)DP�)DQ�DQ�)DR)DR��DS�DS��DT�DT��DT�]DU]DU�]DV��DW�DW�)DX)DX��DY�DY��DZ�DZ��D[�D[�)D\�D\��D])D]��D^�D^]D_�D_��D`�D`�)Da�Da��Db�Db��Dc�Dc�)Dd)Dd��De�De�)Df�Df��Dg�Dy�4D�M�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A̕�ȂhA̋DA�~�ÁA̍PȀ\A̓uA̗�A̗�A̕�A�r�A�+A��#AˁA�ȴA��Aɛ�A�hsA�7LA�(�A�$�A� �A���AȓuA��A��
A�ƨAǼjAǸRAǶFAǶFAǴ9Aǥ�AǅA�VA�v�A�5?Aš�A�M�A��A���Aĉ7AăA�~�A�33AìAÁA�bA���A\A�^5A��A�ƨA��A���A�l�A�^5A��TA���A���A���A�A�^5A���A��+A�A���A��^A�M�A��A���A�+A�A�dZA�+A�|�A��A���A�dZA�G�A�ƨA���A���A��A���A���A���A��!A��
A�r�A��yA��A��/A��A�hsA�$�A�+A�1A��TA��Ac�Aa�A_?}A]C�A\��A[�;A[oAY��AX��AT��AQ�PAO��AL�!AK�AJĜAI��AHZAG��AGAE�-AB�9A@�DA?oA>�A<^5A;%A:Q�A9\)A6$�A4jA37LA0��A.��A. �A-�PA-+A,��A+x�A+"�A*�A*�DA)+A&9XA$A#�A!K�A��A^5A  AA�A�DA��A��An�A�A��AXA��A(�At�A�+A{A�A�^AS�AjAA�FA7LA�DAbNA(�A�A+AjA(�A��A�AA�A{A�A��A�^A��A�7A
�yA	oA1'AĜA;dAJA�yAQ�A�mA7LA �jA z�@�5?@��;@��@��u@�Z@�9X@��@���@�V@��@�V@�  @�P@�w@��
@��@�;d@�@��/@@��-@�V@��;@�-@�  @�F@��;@��@�9@�+@��#@�t�@���@�ȴ@���@ם�@ҸR@���@�~�@͙�@�A�@�l�@�
=@��H@���@ʧ�@�=q@�{@��/@ȓu@�I�@�b@���@�t�@�+@�=q@š�@�K�@��H@��@�7L@�%@���@��u@�C�@�ȴ@�J@�G�@�Ĝ@��m@�ȴ@�@��-@��@�hs@�X@���@�
=@�ȴ@��!@���@���@��\@�v�@�E�@�5?@�M�@�-@��@�{@�@��@��^@���@�A�@�1@���@���@�dZ@�;d@�"�@�
=@���@���@�G�@���@��@���@���@���@���@���@� �@���@�;d@��@���@���@���@��+@�$�@���@��-@�?}@���@�I�@�1@��@�S�@�E�@��7@��7@��7@�x�@�`B@�G�@�/@��`@��j@�z�@��
@���@�S�@�@���@�^5@��-@���@�@�O�@�Ĝ@�j@��@�ƨ@��@�t�@�;d@�@���@��+@�n�@�$�@�O�@��@���@�V@�V@���@���@�r�@�Z@� �@��@�dZ@�o@�n�@�5?@�E�@�M�@�5?@�@��h@���@�bN@�j@�9X@���@��@�@�ȴ@���@�$�@���@��@��@��@���@���@��@���@�|�@��@���@��@�ƨ@�~�@���@��-@��7@�hs@�V@�&�@��`@��@�9X@���@�ƨ@��@�"�@�ȴ@���@���@�x�@�V@���@��@�(�@��@�l�@���@�ff@�ff@�n�@�E�@�E�@�M�@�^5@�E�@��-@��@�v`@s�6111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A̕�ȂhA̋DA�~�ÁA̍PȀ\A̓uA̗�A̗�A̕�A�r�A�+A��#AˁA�ȴA��Aɛ�A�hsA�7LA�(�A�$�A� �A���AȓuA��A��
A�ƨAǼjAǸRAǶFAǶFAǴ9Aǥ�AǅA�VA�v�A�5?Aš�A�M�A��A���Aĉ7AăA�~�A�33AìAÁA�bA���A\A�^5A��A�ƨA��A���A�l�A�^5A��TA���A���A���A�A�^5A���A��+A�A���A��^A�M�A��A���A�+A�A�dZA�+A�|�A��A���A�dZA�G�A�ƨA���A���A��A���A���A���A��!A��
A�r�A��yA��A��/A��A�hsA�$�A�+A�1A��TA��Ac�Aa�A_?}A]C�A\��A[�;A[oAY��AX��AT��AQ�PAO��AL�!AK�AJĜAI��AHZAG��AGAE�-AB�9A@�DA?oA>�A<^5A;%A:Q�A9\)A6$�A4jA37LA0��A.��A. �A-�PA-+A,��A+x�A+"�A*�A*�DA)+A&9XA$A#�A!K�A��A^5A  AA�A�DA��A��An�A�A��AXA��A(�At�A�+A{A�A�^AS�AjAA�FA7LA�DAbNA(�A�A+AjA(�A��A�AA�A{A�A��A�^A��A�7A
�yA	oA1'AĜA;dAJA�yAQ�A�mA7LA �jA z�@�5?@��;@��@��u@�Z@�9X@��@���@�V@��@�V@�  @�P@�w@��
@��@�;d@�@��/@@��-@�V@��;@�-@�  @�F@��;@��@�9@�+@��#@�t�@���@�ȴ@���@ם�@ҸR@���@�~�@͙�@�A�@�l�@�
=@��H@���@ʧ�@�=q@�{@��/@ȓu@�I�@�b@���@�t�@�+@�=q@š�@�K�@��H@��@�7L@�%@���@��u@�C�@�ȴ@�J@�G�@�Ĝ@��m@�ȴ@�@��-@��@�hs@�X@���@�
=@�ȴ@��!@���@���@��\@�v�@�E�@�5?@�M�@�-@��@�{@�@��@��^@���@�A�@�1@���@���@�dZ@�;d@�"�@�
=@���@���@�G�@���@��@���@���@���@���@���@� �@���@�;d@��@���@���@���@��+@�$�@���@��-@�?}@���@�I�@�1@��@�S�@�E�@��7@��7@��7@�x�@�`B@�G�@�/@��`@��j@�z�@��
@���@�S�@�@���@�^5@��-@���@�@�O�@�Ĝ@�j@��@�ƨ@��@�t�@�;d@�@���@��+@�n�@�$�@�O�@��@���@�V@�V@���@���@�r�@�Z@� �@��@�dZ@�o@�n�@�5?@�E�@�M�@�5?@�@��h@���@�bN@�j@�9X@���@��@�@�ȴ@���@�$�@���@��@��@��@���@���@��@���@�|�@��@���@��@�ƨ@�~�@���@��-@��7@�hs@�V@�&�@��`@��@�9X@���@�ƨ@��@�"�@�ȴ@���@���@�x�@�V@���@��@�(�@��@�l�@���@�ff@�ff@�n�@�E�@�E�@�M�@�^5@�E�@��-@��@�v`@s�6111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�TB
�TB
�TB
�TB
�TB
�TB
�TB
�TB
�TB
�TB
�ZB
�sB
�B
��BBVB�B&�B&�B)�B+B+B,B/B7LBN�BQ�BVBXBZB\)B_;BaHBe`Bm�B�B�JB�bB��B��B��B�B�LB�RB�^BƨB�B�B�ZB�yB�B�B��B��BB+B	7B�B"�B!�B!�B!�B"�B(�BA�BG�BF�B@�B6FB7LBe`Bn�Bl�Bs�Bv�Bz�By�Bx�Bv�Br�Bl�BhsBdZB`BB\)BVBJ�B>wB0!B)�B%�B �B�B�B+B��B�B�B��B��B|�B	B�B	6FB	(�B	�B	�B	{B	\B		7B	B�B�ZB�)B��B��B��B��BȴBƨBÖB��B�dB�LB�?B�-B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�VB�DB�DB�7B�1B�+B�%B�B�B�B�%B�DB�DB�DB�DB�JB�JB�JB�PB�VB�VB�bB��B��B�B�B�B�B�'B�!B�B�B�B�B�!B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�{B�{B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�3B�FB�?B�?B�FB�LB�?B�9B�RB�RB�XB�dB�dB�XB�XB�dB�^B�^B�^B��B��BÖBĜBɺB��B��B��B��B��B��B��B��B��B��B��B��B�
B�B�#B�5B�yB�B�B�B�B�B��B��B��B��B	B	B	+B	JB	bB	hB	oB	uB	uB	{B	%�B	)�B	,B	,B	.B	/B	33B	7LB	9XB	;dB	<jB	<jB	<jB	=qB	>wB	B�B	E�B	F�B	H�B	H�B	H�B	H�B	G�B	G�B	G�B	H�B	I�B	J�B	Q�B	XB	[#B	^5B	aHB	cTB	cTB	jB	p�B	s�B	t�B	v�B	w�B	x�B	y�B	}�B	~�B	� B	�B	�%B	�DB	�PB	�PB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�!B	�!B	�'B	�'B	�3B	�?B	�LB	�XB	�RB	�LB	�XB	�^B	�^B	�jB	�qB	�wB	ÖB	ƨB	ƨB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�)B	�/B	�/B	�5B	�5B	�5B	�/B	�)B	�;B	�BB	�;B	�/B	�5B	�;B	�HB	�ZB	�mB	�sB	�B	�B	�B	�yB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 B
%,111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
�TB
�TB
�TB
�TB
�TB
�TB
�TB
�TB
�TB
�TB
�ZB
�sB
�B
��BBVB�B&�B&�B)�B+B+B,B/B7LBN�BQ�BVBXBZB\)B_;BaHBe`Bm�B�B�JB�bB��B��B��B�B�LB�RB�^BƨB�B�B�ZB�yB�B�B��B��BB+B	7B�B"�B!�B!�B!�B"�B(�BA�BG�BF�B@�B6FB7LBe`Bn�Bl�Bs�Bv�Bz�By�Bx�Bv�Br�Bl�BhsBdZB`BB\)BVBJ�B>wB0!B)�B%�B �B�B�B+B��B�B�B��B��B|�B	B�B	6FB	(�B	�B	�B	{B	\B		7B	B�B�ZB�)B��B��B��B��BȴBƨBÖB��B�dB�LB�?B�-B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�VB�DB�DB�7B�1B�+B�%B�B�B�B�%B�DB�DB�DB�DB�JB�JB�JB�PB�VB�VB�bB��B��B�B�B�B�B�'B�!B�B�B�B�B�!B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�{B�{B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�3B�FB�?B�?B�FB�LB�?B�9B�RB�RB�XB�dB�dB�XB�XB�dB�^B�^B�^B��B��BÖBĜBɺB��B��B��B��B��B��B��B��B��B��B��B��B�
B�B�#B�5B�yB�B�B�B�B�B��B��B��B��B	B	B	+B	JB	bB	hB	oB	uB	uB	{B	%�B	)�B	,B	,B	.B	/B	33B	7LB	9XB	;dB	<jB	<jB	<jB	=qB	>wB	B�B	E�B	F�B	H�B	H�B	H�B	H�B	G�B	G�B	G�B	H�B	I�B	J�B	Q�B	XB	[#B	^5B	aHB	cTB	cTB	jB	p�B	s�B	t�B	v�B	w�B	x�B	y�B	}�B	~�B	� B	�B	�%B	�DB	�PB	�PB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�!B	�!B	�'B	�'B	�3B	�?B	�LB	�XB	�RB	�LB	�XB	�^B	�^B	�jB	�qB	�wB	ÖB	ƨB	ƨB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�)B	�/B	�/B	�5B	�5B	�5B	�/B	�)B	�;B	�BB	�;B	�/B	�5B	�;B	�HB	�ZB	�mB	�sB	�B	�B	�B	�yB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 B
%,111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.09 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140806                              AO  ARCAADJP                                                                    20181024140806    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140806  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140806  QCF$                G�O�G�O�G�O�0               