CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-02-13T10:17:16Z AOML 3.0 creation; 2016-08-07T21:17:45Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A|   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Ct   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  KP   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  MH   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U$   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  x�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  z|   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �P   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �,   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �\   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �\   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �\   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �\   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �    HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20160213101716  20160807141745  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               fA   AO  5285_8895_102                   2C  D   APEX                            6487                            072314                          846 @וQy\�1   @וRWC�@/����F�d�bM��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    fA   B   B   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B/��B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���Bۙ�B���B�  B�  B�33B�  B���B�  B�  C   C  C  C  C  C
  C  C�C  C�fC  C  C  C  C  C  C   C"  C$  C&�C(�C)�fC,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,y�D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX�fDY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDy��D���D�P D�s3D�� D��3D�&fD�� D��fD��D�C3D�vfD��3D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @Z�H@���@У�AQ�A(Q�AHQ�AhQ�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B{B
{B{B{B"{B*{B1�B:{BB{BJ{BR{BZ{Bb{Bj{Br{Bz{B�
=B�
=B�
=B�=pB�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B��
Bܣ�B��
B�
=B�
=B�=pB�
=B��
B�
=B�
=C �C�C�C�C�C
�C�C��C�Ck�C�C�C�C�C�C�C �C"�C$�C&��C(��C*k�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�D !HD �HD'�D�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD	!HD	�HD
!HD
�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD !HD �HD!!HD!�HD"!HD"�HD#!HD#�HD$!HD$�HD%!HD%�HD&!HD&�HD'!HD'�HD(!HD(�HD)!HD)�HD*!HD*�HD+!HD+�HD,!HD,��D-!HD-�HD.!HD.�HD/!HD/�HD0!HD0�HD1!HD1�HD2!HD2�HD3!HD3�HD4!HD4�HD5!HD5�HD6!HD6�HD7!HD7�HD8!HD8�HD9!HD9�HD:!HD:�HD;!HD;�HD<!HD<�HD=!HD=�HD>!HD>�HD?!HD?�HD@!HD@�HDA!HDA�HDB!HDB�HDC!HDC�HDD!HDD�HDE!HDE�HDF!HDF�HDG!HDG�HDH!HDH�HDI!HDI�HDJ!HDJ�HDK!HDK�HDL!HDL�HDM!HDM�HDN!HDN�HDO!HDO�HDP!HDP�HDQ!HDQ�HDR!HDR�HDS!HDS�HDT!HDT�HDU!HDU�HDV!HDV�HDW!HDW�HDX!HDX��DY!HDY�HDZ!HDZ�HD[!HD[�HD\!HD\�HD]!HD]�HD^!HD^�HD_!HD_�HD`!HD`�HDa!HDa�HDb!HDb�HDc!HDc�HDd!HDd�HDe!HDe�HDf!HDf�HDg!HDg�HDh!HDh�HDi!HDi�HDj!HDj�HDk!HDk�HDl!HDl�HDm!HDm�HDn!HDn�HDo!HDo�HDp!HDp�HDq!HDq�HDr!HDr�HDs!HDs�HDt!HDt��Dy�D�
>D�`�D���D���D���D�7
D���D��
D�-qD�S�D��
D���D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��#A��/A��/A��/A��;A��HA��;A��HA��HA��;A��HA���A���A���A�ĜA�ĜA���A̺^A̰!A̩�A̡�A̝�A̙�A̝�A̡�Ạ�A̧�A̧�A̸RA�ĜA�ƨA�ȴA���A���A���A���A��
A��A��/A��;A��HA��;A��`A��mA��A��A���A���A���A�%A�{A��A�$�A�-A�&�A��yȂhA��#A�/A�AĴ9A���A��A�ȴA���A���A� �A�9XA��jA�ƨA�A��A�9XA��A��;A� �A�\)A��TA���A�
=A���A�$�A���A�t�A��/A���A���A��A�O�A�|�A��A��PA�A�A��-A�(�A�A�A�\)A�v�A��A��FA��#A�bNA�1A�G�A}O�Av5?As�
Aq�-Am�mAix�Ag��Ae33AcVAaG�A]�AVQ�ASdZAP^5AJ��AG�AD��AAS�A?A>��A=�A:�A4��A2��A17LA0�A-��A,E�A*bA'��A'�A'�TA'?}A$ĜA n�AZA\)A�\A{A��A?}A��A�AVA�yA��AQ�A��A�A�wA�A1'AE�AjAr�A~�Av�AffA�;A�HA �A�jA��A�yAVA/A33AG�A�A��A�HA7LAz�AK�A�A�-A|�AO�A��A��A��A�A�`A�wA�\Av�A��A��A��AJAXA�A��AVA�mA�PAO�A�!AE�Ap�A��AVA��Ax�A��A9XA(�A�mA�FA�A%A
~�A
$�A	�FA��A��Ax�A
=A�+AQ�A{A��A  A��A?}A��A��AVA5?A(�A�
A�hAXA��A��An�A�
A�Al�A;dA �yA ��A v�A 9X@�;d@��@���@���@�&�@��/@�bN@�A�@�
=@��^@�`B@���@���@�V@�p�@�V@��@��j@�Z@�P@�ȴ@�E�@�@�?}@�V@�j@�b@���@��`@웦@��@�@�j@�C�@��@� �@���@�l�@�\@�O�@�1@�dZ@��@�^5@��#@�p�@���@��;@��@ޗ�@�E�@�J@���@�t�@���@�V@���@ٙ�@ؓu@�A�@�ƨ@ׅ@�+@���@֟�@�ff@��@���@�hs@ԣ�@�Q�@�A�@�1@�
=@ҧ�@��T@�O�@�G�@�G�@�G�@�O�@�X@�X@�X@�X@�X@��@ΰ!@�?}@̬@̓u@̃@�j@�Z@�A�@�1'@�1@��
@˥�@�S�@�+@ʧ�@��@ə�@�x�@�V@�Ĝ@�Q�@�|�@�"�@��@ư!@Ƨ�@Ə\@�@őh@�X@�%@ēu@��m@��@�@���@��h@�&�@��j@�Z@���@�
=@��@�X@�Z@��@��+@���@���@���@�Z@�(�@���@���@��F@�S�@��@��\@��@��7@�G�@�/@��@���@�Ĝ@��9@��u@���@�ȴ@�v�@�-@�$�@�$�@��@��@��@�{@���@�@��@��h@��`@�j@�Q�@�b@��@���@�{@���@�x�@� �@�dZ@�"�@�-@��^@�`B@��@�Ĝ@��F@�t�@�S�@�33@���@��y@��R@���@�{@��^@�G�@��@��/@��9@�z�@�A�@�b@���@�l�@�dZ@�K�@�o@���@�V@�J@��@��T@���@�hs@�V@��`@��9@�r�@�1'@��@��@�\)@��@�~�@�5?@�@�@��#@��-@���@���@�x�@�O�@�/@��@���@�9X@�  @��;@��F@��P@��-@�n�@��@���@�E�@z�!@o\)@c��@R�\@I�@?��@9X@17L@)��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111 A��#A��/A��/A��/A��;A��HA��;A��HA��HA��;A��HA���A���A���A�ĜA�ĜA���A̺^A̰!A̩�A̡�A̝�A̙�A̝�A̡�Ạ�A̧�A̧�A̸RA�ĜA�ƨA�ȴA���A���A���A���A��
A��A��/A��;A��HA��;A��`A��mA��A��A���A���A���A�%A�{A��A�$�A�-A�&�A��yȂhA��#A�/A�AĴ9A���A��A�ȴA���A���A� �A�9XA��jA�ƨA�A��A�9XA��A��;A� �A�\)A��TA���A�
=A���A�$�A���A�t�A��/A���A���A��A�O�A�|�A��A��PA�A�A��-A�(�A�A�A�\)A�v�A��A��FA��#A�bNA�1A�G�A}O�Av5?As�
Aq�-Am�mAix�Ag��Ae33AcVAaG�A]�AVQ�ASdZAP^5AJ��AG�AD��AAS�A?A>��A=�A:�A4��A2��A17LA0�A-��A,E�A*bA'��A'�A'�TA'?}A$ĜA n�AZA\)A�\A{A��A?}A��A�AVA�yA��AQ�A��A�A�wA�A1'AE�AjAr�A~�Av�AffA�;A�HA �A�jA��A�yAVA/A33AG�A�A��A�HA7LAz�AK�A�A�-A|�AO�A��A��A��A�A�`A�wA�\Av�A��A��A��AJAXA�A��AVA�mA�PAO�A�!AE�Ap�A��AVA��Ax�A��A9XA(�A�mA�FA�A%A
~�A
$�A	�FA��A��Ax�A
=A�+AQ�A{A��A  A��A?}A��A��AVA5?A(�A�
A�hAXA��A��An�A�
A�Al�A;dA �yA ��A v�A 9X@�;d@��@���@���@�&�@��/@�bN@�A�@�
=@��^@�`B@���@���@�V@�p�@�V@��@��j@�Z@�P@�ȴ@�E�@�@�?}@�V@�j@�b@���@��`@웦@��@�@�j@�C�@��@� �@���@�l�@�\@�O�@�1@�dZ@��@�^5@��#@�p�@���@��;@��@ޗ�@�E�@�J@���@�t�@���@�V@���@ٙ�@ؓu@�A�@�ƨ@ׅ@�+@���@֟�@�ff@��@���@�hs@ԣ�@�Q�@�A�@�1@�
=@ҧ�@��T@�O�@�G�@�G�@�G�@�O�@�X@�X@�X@�X@�X@��@ΰ!@�?}@̬@̓u@̃@�j@�Z@�A�@�1'@�1@��
@˥�@�S�@�+@ʧ�@��@ə�@�x�@�V@�Ĝ@�Q�@�|�@�"�@��@ư!@Ƨ�@Ə\@�@őh@�X@�%@ēu@��m@��@�@���@��h@�&�@��j@�Z@���@�
=@��@�X@�Z@��@��+@���@���@���@�Z@�(�@���@���@��F@�S�@��@��\@��@��7@�G�@�/@��@���@�Ĝ@��9@��u@���@�ȴ@�v�@�-@�$�@�$�@��@��@��@�{@���@�@��@��h@��`@�j@�Q�@�b@��@���@�{@���@�x�@� �@�dZ@�"�@�-@��^@�`B@��@�Ĝ@��F@�t�@�S�@�33@���@��y@��R@���@�{@��^@�G�@��@��/@��9@�z�@�A�@�b@���@�l�@�dZ@�K�@�o@���@�V@�J@��@��T@���@�hs@�V@��`@��9@�r�@�1'@��@��@�\)@��@�~�@�5?@�@�@��#@��-@���@���@�x�@�O�@�/@��@���@�9X@�  @��;@��FG�O�@��-@�n�@��@���@�E�@z�!@o\)@c��@R�\@I�@?��@9X@17L@)��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
!�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
"�B
'�B
(�B
)�B
-B
-B
/B
0!B
5?B
8RB
8RB
7LB
7LB
:^B
<jB
=qB
?}B
C�B
L�B
R�B
VB
XB
\)B
^5B
_;B
`BB
aHB
cTB
dZB
e`B
gmB
hsB
iyB
jB
n�B
q�B
r�B
s�B
t�B
w�B
{�B
~�B
�B
�=B
�\B
��B
ɺB&�B�-B]/B�bB�XB��B�B��B  BJBoB\B
=BhB�B�B\B1BBB��B��B�mB�BǮB��B�qB�RB�B��B��B�+BffBW
BL�B@�B5?BoB�B��B�B�7Bu�BM�B'�B
�B
{�B
PB	��B	�jB	�B	��B	~�B	s�B	e`B	XB	J�B	5?B	�B		7B��B�yB�#B��BɺBŢB��B�^B�!B�B��B��B��B��B�!B�BB�HB�B��B��B�fB�B	VB	�B	�B	(�B	7LB	aHB	x�B	�B	z�B	��B	�B	�?B	�jB	��B	�/B	�`B	�sB	�B	�B	�B	�B	�B	�B	�TB	�NB	��B
B	��B
B
B
%B
%B
VB
oB
�B
'�B
)�B
&�B
!�B
�B
�B
�B
�B
\B
JB
\B
oB

=B
B
B
B
1B
\B
�B
�B
�B
�B
�B
�B
�B
"�B
'�B
(�B
'�B
%�B
$�B
"�B
!�B
!�B
!�B
!�B
!�B
 �B
 �B
�B
�B
 �B
�B
�B
uB
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
uB
uB
oB
oB
hB
hB
bB
\B
VB
PB
JB
DB

=B

=B

=B
	7B
	7B
	7B
	7B

=B
JB
DB
JB
PB
PB
JB
DB
	7B
	7B
1B
1B
1B
+B
%B
B
B
B
B
B
B
B
B
B
B
B
  B
  B	��B	��B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
%B
%B
%B
%B
%B
%B
%B
+B
+B
+B
+B
1B
1B
1B
1B
1B
	7B

=B

=B
DB
DB
DB
JB
JB
PB
VB
VB
VB
\B
\B
\B
\B
bB
hB
oB
uB
uB
uB
uB
uB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
(�B
.B
.B
1'B
:^B
?}B
G�B
L�B
VB
[#B
bNB
ff11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111 B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
"�B
'�B
(�B
)�B
,�B
,�B
.�B
/�B
5B
82B
8.B
7)B
7*B
:;B
<HB
=NB
?YB
CsB
L�B
R�B
U�B
W�B
\B
^B
_B
`!B
a"B
c1B
d5B
e=B
gHB
hNB
iSB
j[B
nrB
q�B
r�B
s�B
t�B
w�B
{�B
~�B
��B
�B
�:B
�sB
ɏB&�B�B]B�7B�.B��B�XB��B��B!BCB1B
B?BfBXB1BB�B�B��B��B�DB��BǀB�WB�CB�'B��B��B�|B��Bf6BV�BL�B@QB5B=B�yBʎB��B�Bu�BM�B'�B
�TB
{�B
"B	��B	�=B	��B	�pB	~�B	s�B	e9B	W�B	J�B	5B	sB		B��B�SB��B��BɔB�{B�dB�:B��B��B��B��B��B��B��B��B�gB�B�TB��B��B�;B�TB	)B	nB	�B	(�B	7B	aB	x�B	��B	z�B	��B	��B	�
B	�7B	ϨB	��B	�+B	�>B	�iB	�vB	�B	�B	�yB	�\B	�B	�B	��B
 �B	��B
�B
�B
�B
�B
!B
7B
{B
'�B
)�B
&�B
!�B
tB
oB
hB
JB
$B
B
%B
8B

B
�B
�B
�B
�B
&B
PB
VB
VB
\B
hB
]B
hB
"�B
'�B
(�B
'�B
%�B
$�B
"�B
!�B
!�B
!�B
!�B
!�B
 �B
 �B
�B
�B
 �B
zB
[B
=B
7B
JB
TB
ZB
fB
pB
qB
uB
yB
uB
tB
tB
qB
sB
oB
mB
hB
bB
bB
ZB
[B
[B
YB
YB
VB
MB
HB
BB
=B
;B
4B
4B
-B
-B
)B
"B
B
B
B
B

B

B

B
�B
�B
�B
�B

B
B
B
B
B
B
B

B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
 �B
 �B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B

 B
B
B
B
B
B
B
B
B
B
B
B
B
 B
&B
*B
1B
7B
8B
:B
9B
9B
>B
=B
EB
@B
CB
CB
JB
KB
JB
PB
MB
QB
PB
QB
QB
VB
UB
VB
WB
]B
\B
bB
cB
jB
jB
jB
jB
hB
iB
iB
iB
iB
iB
jB
oB
pB
nB
lB
pG�O�B
uB
rB
(�B
-�B
-�B
0�B
:B
?>B
GrB
L�B
U�B
Z�B
bB
f'11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111 <#�
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
G�O�<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.52 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417452016080714174520160807141745  AO  ARCAADJP                                                                    20160213101716    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160213101716  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160213101716  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141745  IP                  G�O�G�O�G�O�                