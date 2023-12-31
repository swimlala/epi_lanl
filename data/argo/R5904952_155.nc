CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:40Z creation      
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
resolution        =���   axis      Z        l  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  L    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  Sl   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  mh   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �$   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �4   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �8   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �<   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181005190540  20181005190540  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��)���&1   @��*%�@0�V��c��hr�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�33@�  A   AffA>ffA`  A~ffA�  A�  A�  A�33A�33A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�33C �C�C  C�fC  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C��C�  C�  C�  C�  C��C��C��C�  C��C��C�  C�  C�  C�  C�  C��3C�  C��3C��3C�  C�  C�  C�  C��3C��3C�  C�  C�  C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C��3C�  C��C�  C�  C��3C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C��C�  C�  D   D � D  Dy�D  D� D  D� D  Dy�D��Dy�D  D� D��Dy�D  D� D	  D	y�D
  D
� D  D� D��D� D  D� DfD� D  D� D  Dy�D��Dy�D  D� D  D�fD  D� D  Dy�D��D� DfD�fD  D� D��D� DfD� D  D� DfD�fD  D� D  D� D  D� D fD � D ��D!� D"  D"� D#  D#� D#��D$y�D%  D%�fD&  D&y�D&��D'y�D(  D(� D)  D)� D*fD*�fD+fD+�fD,fD,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3fD3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D;��D<y�D<��D=y�D=��D>� D?  D?y�D?��D@� DA  DA�fDB  DBy�DC  DC� DD  DD� DE  DE� DE��DF�fDG  DG� DHfDH� DI  DI� DI��DJ� DK  DKy�DL  DL� DM  DM�fDN  DNy�DO  DO� DP  DP� DQ  DQ�fDR  DR� DS  DS� DS��DT� DU  DUy�DU��DVy�DV��DW� DX  DX� DYfDY�fDZfDZ� D[  D[�fD\  D\� D\��D]� D]��D^y�D^��D_� D`fD`�fDafDa� Db  Dby�Db��Dc� DdfDd�fDefDe� De��Dfy�Dg  Dg� Dh  Dhy�Dh��Di� DjfDj� Dj��Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq�fDr  Dry�Ds  Ds�fDt  Dt� DufDu�fDv  Dv� Dw  Dw� DwٚDy�{D�'
D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�p�@�=qA�A#�AC�Ae�A�A��\A��\A��\A�A�A�\A�\BG�B	G�BG�BG�B!G�B)G�B1G�B9G�BAG�BIG�BQG�BYG�BaG�BiG�BqG�ByG�B���B���B���B���B���B���B���B���B�p�B���B���B���B���B���B���B���B���Bģ�Bȣ�Ḅ�BУ�Bԣ�Bأ�Bܣ�B��B��B��B��B��
B���B���B��
C k�Ck�CQ�C8RCQ�C
Q�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�C Q�C"Q�C$Q�C&Q�C(Q�C*Q�C,Q�C.Q�C0Q�C2Q�C4Q�C6Q�C8Q�C:Q�C<Q�C>Q�C@Q�CBQ�CDQ�CFQ�CHQ�CJQ�CLQ�CNQ�CPQ�CRQ�CTQ�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�5�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�5�C�5�C�(�C�5�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�5�C�5�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�5�C�(�C�)C�(�C�5�C�(�C�(�C�(�C�(�C�5�C�5�C�5�C�(�C�5�C�5�C�(�C�(�C�(�C�(�C�(�C�)C�(�C�)C�)C�(�C�(�C�(�C�(�C�)C�)C�(�C�(�C�(�C�)C�(�C�(�C�(�C�)C�(�C�(�C�(�C�(�C�(�C�(�C�)C�)C�(�C�5�C�(�C�(�C�)C�(�C�)C�(�C�(�C�(�C�(�C�(�C�5�C�(�C�(�C�(�C�(�C�(�C�(�C�5�C�5�C�(�C�(�D {D �{D{D�D{D�{D{D�{D{D�DD�D{D�{DD�D{D�{D	{D	�D
{D
�{D{D�{DD�{D{D�{D�D�{D{D�{D{D�DD�D{D�{D{D��D{D�{D{D�DD�{D�D��D{D�{DD�{D�D�{D{D�{D�D��D{D�{D{D�{D{D�{D �D �{D!D!�{D"{D"�{D#{D#�{D$D$�D%{D%��D&{D&�D'D'�D({D(�{D){D)�{D*�D*��D+�D+��D,�D,�{D-{D-�{D.{D.�{D/{D/�{D0{D0�{D1{D1�{D2{D2�{D3�D3�{D4{D4�{D5{D5�{D6{D6�{D7{D7�{D8{D8�{D9{D9�{D:{D:�{D;{D;�{D<D<�D=D=�D>D>�{D?{D?�D@D@�{DA{DA��DB{DB�DC{DC�{DD{DD�{DE{DE�{DFDF��DG{DG�{DH�DH�{DI{DI�{DJDJ�{DK{DK�DL{DL�{DM{DM��DN{DN�DO{DO�{DP{DP�{DQ{DQ��DR{DR�{DS{DS�{DTDT�{DU{DU�DVDV�DWDW�{DX{DX�{DY�DY��DZ�DZ�{D[{D[��D\{D\�{D]D]�{D^D^�D_D_�{D`�D`��Da�Da�{Db{Db�DcDc�{Dd�Dd��De�De�{DfDf�Dg{Dg�{Dh{Dh�DiDi�{Dj�Dj�{DkDk�{Dl{Dl�{Dm{Dm�{Dn{Dn�{Do{Do�{Dp{Dp�{Dq{Dq��Dr{Dr�Ds{Ds��Dt{Dt�{Du�Du��Dv{Dv�{Dw{Dw�{Dw�Dy��D�1GD��)1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A̓A͇+A͋DA͏\A͓uA͑hA͕�A͕�A͗�A͛�A͛�Aͣ�A͡�A͡�A͙�A͙�A͛�A͛�A͑hA�~�A͑hA͕�ÁA͕�AͼjA��mA��mA���A�ƨA;wA�ĜA�ĜA�ĜAͶFA͑hA�r�A�bNA�bNA�\)A�=qA�-A�&�A���A�ȴḀ�A̧�Ḁ�Ạ�A̬A̲-A̴9A̼jA̮A̝�A̸RA��;A��A���A̰!Ȧ+A�jA��A˙�A���A��A�=qA��A��wA��A��DA�9XA�9XA���A��!A��`A�p�A���A���A�I�A���A�1'A�ƨA��uA���A�
=A�ƨA��HA�\)A���A��#A�33A���A��FA�ĜA��A�ffA��A�(�A���A��/A�1A��7A��A�"�A�/APbNAO�AO�AK��AD��AB��AA��A@�A=�#A:�+A6��A5�A4-A3�PA3&�A1�;A0�!A/��A/p�A,��A+"�A*E�A)C�A({A'&�A&1A$�RA$$�A#x�A#&�A"bAA-A��A?}Az�A�wAA~�AbNA�mA\)AȴAƨA+A�A�7A�!A�#A%A��A�A��A�RA��AS�A	��AĜAl�A��A-A
=A�A%A�\A$�AA�^A�A �DA ��A ��A �jA VA M�@��@�dZ@�S�@��y@�V@���@�G�@��u@�|�@�x�@��7@�X@��@��`@�I�@�C�@�=q@��@�`B@�z�@�\@�P@�M�@�@�?}@�  @��#@���@� �@ߕ�@�@�^5@��/@�A�@۝�@��H@�/@�(�@��#@��/@���@ԣ�@Ѻ^@�z�@�M�@�?}@�Q�@�j@�1'@�1@�j@�I�@���@�l�@ʇ+@���@�`B@ɡ�@�X@ț�@�  @��@�`B@���@ě�@ă@�r�@ēu@ģ�@�z�@��
@°!@\@�@��y@�S�@�t�@���@\@�E�@��T@�&�@�Ĝ@�A�@�b@���@��F@�t�@�S�@�C�@�"�@��+@��@��^@�bN@� �@��@�|�@�K�@�33@��@���@���@���@��@�/@���@���@��@��m@��@��@���@�~�@�@�hs@�&�@��@���@�r�@�j@�r�@�I�@�
=@�`B@�7L@�&�@��@�(�@�S�@�
=@���@�M�@�5?@��#@�%@�j@��@�(�@��
@��w@��@�|�@���@��h@�p�@�O�@��j@�(�@��@��F@�;d@��\@��#@��h@�`B@�X@�G�@�7L@�G�@���@���@��@���@�r�@�(�@��P@�ȴ@�{@��@��#@�@��-@�x�@�/@���@��@��u@��D@�j@�I�@�A�@��w@�dZ@�;d@��@��@�V@��@��#@���@��7@��7@�G�@���@� �@���@�|�@�|�@���@���@�E�@��T@�x�@�&�@��`@���@�b@��F@���@�C�@��@�V@��@���@��7@�%@��j@�z�@�1@���@��@��y@���@�=q@�-@�$�@��@�@��-@���@��7@��7@��7@�`B@�/@�%@��@��@�I�@��@��
@���@�dZ@�C�@�"�@���@��+@�v�@�-@�{@��-@�hs@���@���@�bN@�1@��
@���@�dZ@�33@�@���@�n�@�=q@��@���@�x�@�V@�z�@�Q�@��@��@�33@�@��H@�~�@�$�@���@���@���@��@���@��j@�z�@�l�@�C�@�33@��@�
=@�
=@�@� i@y��@h�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A̓A͇+A͋DA͏\A͓uA͑hA͕�A͕�A͗�A͛�A͛�Aͣ�A͡�A͡�A͙�A͙�A͛�A͛�A͑hA�~�A͑hA͕�ÁA͕�AͼjA��mA��mA���A�ƨA;wA�ĜA�ĜA�ĜAͶFA͑hA�r�A�bNA�bNA�\)A�=qA�-A�&�A���A�ȴḀ�A̧�Ḁ�Ạ�A̬A̲-A̴9A̼jA̮A̝�A̸RA��;A��A���A̰!Ȧ+A�jA��A˙�A���A��A�=qA��A��wA��A��DA�9XA�9XA���A��!A��`A�p�A���A���A�I�A���A�1'A�ƨA��uA���A�
=A�ƨA��HA�\)A���A��#A�33A���A��FA�ĜA��A�ffA��A�(�A���A��/A�1A��7A��A�"�A�/APbNAO�AO�AK��AD��AB��AA��A@�A=�#A:�+A6��A5�A4-A3�PA3&�A1�;A0�!A/��A/p�A,��A+"�A*E�A)C�A({A'&�A&1A$�RA$$�A#x�A#&�A"bAA-A��A?}Az�A�wAA~�AbNA�mA\)AȴAƨA+A�A�7A�!A�#A%A��A�A��A�RA��AS�A	��AĜAl�A��A-A
=A�A%A�\A$�AA�^A�A �DA ��A ��A �jA VA M�@��@�dZ@�S�@��y@�V@���@�G�@��u@�|�@�x�@��7@�X@��@��`@�I�@�C�@�=q@��@�`B@�z�@�\@�P@�M�@�@�?}@�  @��#@���@� �@ߕ�@�@�^5@��/@�A�@۝�@��H@�/@�(�@��#@��/@���@ԣ�@Ѻ^@�z�@�M�@�?}@�Q�@�j@�1'@�1@�j@�I�@���@�l�@ʇ+@���@�`B@ɡ�@�X@ț�@�  @��@�`B@���@ě�@ă@�r�@ēu@ģ�@�z�@��
@°!@\@�@��y@�S�@�t�@���@\@�E�@��T@�&�@�Ĝ@�A�@�b@���@��F@�t�@�S�@�C�@�"�@��+@��@��^@�bN@� �@��@�|�@�K�@�33@��@���@���@���@��@�/@���@���@��@��m@��@��@���@�~�@�@�hs@�&�@��@���@�r�@�j@�r�@�I�@�
=@�`B@�7L@�&�@��@�(�@�S�@�
=@���@�M�@�5?@��#@�%@�j@��@�(�@��
@��w@��@�|�@���@��h@�p�@�O�@��j@�(�@��@��F@�;d@��\@��#@��h@�`B@�X@�G�@�7L@�G�@���@���@��@���@�r�@�(�@��P@�ȴ@�{@��@��#@�@��-@�x�@�/@���@��@��u@��D@�j@�I�@�A�@��w@�dZ@�;d@��@��@�V@��@��#@���@��7@��7@�G�@���@� �@���@�|�@�|�@���@���@�E�@��T@�x�@�&�@��`@���@�b@��F@���@�C�@��@�V@��@���@��7@�%@��j@�z�@�1@���@��@��y@���@�=q@�-@�$�@��@�@��-@���@��7@��7@��7@�`B@�/@�%@��@��@�I�@��@��
@���@�dZ@�C�@�"�@���@��+@�v�@�-@�{@��-@�hs@���@���@�bN@�1@��
@���@�dZ@�33@�@���@�n�@�=q@��@���@�x�@�V@�z�@�Q�@��@��@�33@�@��H@�~�@�$�@���@���@���@��@���@��j@�z�@�l�@�C�@�33@��@�
=@�
=@�@� i@y��@h�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B#�B#�B#�B#�B$�B$�B#�B#�B$�B%�B&�B+B0!B8RB>wBA�BC�BM�BdZBm�B{�B�B�1B�bB��B��B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B�VB�+B�B�B�B� B�B�B�B�%B�B�%B�PB��B��B��B��B��B��B�B�-B�jB��B�ZB��BB1BJB �BF�B`BBq�Bu�Bp�Br�Bu�Bw�B�Bz�BgmBQ�BF�B6FB!�BB�B�B�B��B��B�7Bs�BL�B�B
��B
�B
��B
�?B
��B
��B
~�B
XB
P�B�B�B�sB��B�3B�B��B��B��B�bB��B�B�B�!B�-B�!B�B�B��B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B��B�B�B�B�-B�?B�9B�?B�FB�?B�?B�FB�RB�qBBĜBƨBɺB��BɺB��B��B��B��B��B��B�B�/B�
B��B��B��B�HB�mB�fB�TB�HB�TB�fB�`B�mB�sB�B�B�B�B��B��B��B��B��B	B	B	B	B	B	B	B	B	B	bB	bB	�B	�B	�B	�B	'�B	-B	/B	6FB	7LB	8RB	8RB	8RB	7LB	9XB	@�B	A�B	D�B	D�B	D�B	D�B	E�B	G�B	H�B	I�B	J�B	K�B	K�B	J�B	K�B	N�B	P�B	R�B	W
B	ZB	[#B	[#B	[#B	\)B	`BB	bNB	ffB	gmB	hsB	hsB	hsB	iyB	hsB	hsB	iyB	k�B	o�B	p�B	q�B	q�B	s�B	t�B	u�B	u�B	t�B	w�B	x�B	x�B	}�B	�B	�B	�B	�B	�+B	�+B	�+B	�+B	�1B	�1B	�1B	�1B	�1B	�1B	�1B	�7B	�DB	�\B	�PB	�PB	�PB	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�9B	�?B	�?B	�?B	�LB	�XB	�^B	�^B	�dB	�jB	�jB	�jB	�qB	�qB	�qB	�}B	��B	ÖB	ŢB	ƨB	ƨB	ƨB	ĜB	ĜB	ŢB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�#B	�#B	�)B	�/B	�5B	�BB	�NB	�NB	�TB	�ZB	�TB	�TB	�TB	�TB	�`B	�`B	�yB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
+B
+B
1B
1B
1B
	7B
	7B
DB
JB
JB
PB
VB
VB
VB
\B
hB
oB
{B
{B
�B
�B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
(sB
0;2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B#�B#�B#�B#�B$�B$�B#�B#�B$�B%�B&�B+B0!B8RB>wBA�BC�BM�BdZBm�B{�B�B�1B�bB��B��B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B�VB�+B�B�B�B� B�B�B�B�%B�B�%B�PB��B��B��B��B��B��B�B�-B�jB��B�ZB��BB1BJB �BF�B`BBq�Bu�Bp�Br�Bu�Bw�B�Bz�BgmBQ�BF�B6FB!�BB�B�B�B��B��B�7Bs�BL�B�B
��B
�B
��B
�?B
��B
��B
~�B
XB
P�B�B�B�sB��B�3B�B��B��B��B�bB��B�B�B�!B�-B�!B�B�B��B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B��B�B�B�B�-B�?B�9B�?B�FB�?B�?B�FB�RB�qBBĜBƨBɺB��BɺB��B��B��B��B��B��B�B�/B�
B��B��B��B�HB�mB�fB�TB�HB�TB�fB�`B�mB�sB�B�B�B�B��B��B��B��B��B	B	B	B	B	B	B	B	B	B	bB	bB	�B	�B	�B	�B	'�B	-B	/B	6FB	7LB	8RB	8RB	8RB	7LB	9XB	@�B	A�B	D�B	D�B	D�B	D�B	E�B	G�B	H�B	I�B	J�B	K�B	K�B	J�B	K�B	N�B	P�B	R�B	W
B	ZB	[#B	[#B	[#B	\)B	`BB	bNB	ffB	gmB	hsB	hsB	hsB	iyB	hsB	hsB	iyB	k�B	o�B	p�B	q�B	q�B	s�B	t�B	u�B	u�B	t�B	w�B	x�B	x�B	}�B	�B	�B	�B	�B	�+B	�+B	�+B	�+B	�1B	�1B	�1B	�1B	�1B	�1B	�1B	�7B	�DB	�\B	�PB	�PB	�PB	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�9B	�?B	�?B	�?B	�LB	�XB	�^B	�^B	�dB	�jB	�jB	�jB	�qB	�qB	�qB	�}B	��B	ÖB	ŢB	ƨB	ƨB	ƨB	ĜB	ĜB	ŢB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�#B	�#B	�)B	�/B	�5B	�BB	�NB	�NB	�TB	�ZB	�TB	�TB	�TB	�TB	�`B	�`B	�yB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
+B
+B
1B
1B
1B
	7B
	7B
DB
JB
JB
PB
VB
VB
VB
\B
hB
oB
{B
{B
�B
�B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
(sB
0;2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.32 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190540                              AO  ARCAADJP                                                                    20181005190540    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190540  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190540  QCF$                G�O�G�O�G�O�8000            