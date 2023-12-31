CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-14T20:16:05Z AOML 3.0 creation; 2016-06-01T00:08:23Z UW 3.1 conversion     
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
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150214201605  20160531170823  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               jA   AO  4055_7112_106                   2C  D   APEX                            5374                            041511                          846 @�:[��1   @�:[�� @:B��`A��dF���m1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    jA   A   A   @�33@�  A   A   A@  A`  A�  A�  A�33A�  A�  A�  A�  A�  B   B  B  BffB ffB(ffB0  B7��B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDys3D� D�FfD���D�` D��D�6fD�|�D��3D�	�D�9�D�|�D��3D�3D�<�D�y�D�� D�fD�@ D�s3D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��
@У�AQ�A(Q�AHQ�AhQ�A�(�A�(�A�\)A�(�A�(�A�(�A�(�A�(�B{B
{B{Bz�B"z�B*z�B2{B9�BB{BJ{BR{BZ{Bb{Bj{Br{Bz{B�
=B�
=B�
=B�
=B�
=B�
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
=B��
B�
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
=C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�O\C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�O\C�O\C�B�C�B�C�B�C�B�C�B�C�B�C�B�D !HD �HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD	!HD	�HD
!HD
�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD !HD �HD!!HD!�HD"!HD"�HD#!HD#�HD$!HD$�HD%!HD%�HD&!HD&�HD'!HD'�HD(!HD(�HD)!HD)�HD*!HD*�HD+!HD+�HD,!HD,�HD-!HD-�HD.!HD.�HD/!HD/�HD0!HD0�HD1!HD1�HD2!HD2�HD3!HD3�HD4!HD4�HD5!HD5�HD6!HD6�HD7!HD7�HD8!HD8�HD9!HD9�HD:!HD:�HD;!HD;�HD<!HD<�HD=!HD=�HD>!HD>�HD?!HD?�HD@!HD@�HDA!HDA�HDB!HDB�HDC!HDC�HDD!HDD�HDE!HDE�HDF!HDF�HDG!HDG�HDH!HDH�HDI!HDI�HDJ!HDJ�HDK!HDK�HDL!HDL�HDM!HDM�HDN!HDN�HDO!HDO�HDP!HDP�HDQ!HDQ�HDR!HDR�HDS!HDS�HDT!HDT�HDU!HDU�HDV!HDV�HDW!HDW�HDX!HDX�HDY!HDY�HDZ!HDZ�HD[!HD[�HD\!HD\�HD]!HD]�HD^!HD^�HD_!HD_�HD`!HD`�HDa!HDa�HDb!HDb�HDc!HDc�HDd!HDd�HDe!HDe�HDf!HDf�HDg!HDg�HDh!HDh�HDi!HDi�HDj!HDj�HDk!HDk�HDl!HDl�HDm!HDm�HDn!HDn�HDo!HDo�HDp!HDp�HDq!HDq�HDr!HDr�HDs!HDs�HDt!HDt��Dy�{D� �D�W
D��>D�p�D�qD�G
D��qD���D�>D�J>D��qD���D��D�MqDڊ>D��D�
D�P�D��D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�7LA�9XA�9XA�33A�7LA�;dA�;dA�C�A�C�A�E�A�G�A�G�A�G�A�G�A�I�A�K�A�K�A�G�A�;dA�C�A�I�A�I�A�G�A�?}A�A�A�G�A�G�A�E�A�A�A�E�A�=qA�;dA�=qA�A�A�;dA�=qA�&�A�&�A�+A�5?A�?}A�?}A�33A�5?A�9XA�7LA�5?A�=qA�/A�"�A��A�+A�7LA�-A��A���A��`A��/A��/A���A���A���A���A���A��jA��9A���A��DA�t�A�ZA�7LA��
A�jA��#A���A��A���A��A�Q�A���A�A�A��9A�+A���A�oA���A�{A��`A���A�?}A���A�Q�A� �A�bA��yA���A�ZA���A�ĜA��PA��;A�E�A���A��
A���A�oA%A|�yA{��Az�Az�Ay�^Ay7LAw�Av�RAu�Au%Atr�As�
Asp�Ar�Aq�Ao�Al�Aj~�Aj-AiO�Ahr�Af~�Ac��Ab�DA_��A_C�A^�jA];dA\9XA[�A[|�AW�PAT~�AR�AR�uARffAR5?AQ�7AN�jALQ�AK�7AKC�AJv�AJ1'AI33AH��AG�AGAG�FAG33AE��AEoAD��AB�jAAG�AAVA@��A@�A>=qA;�PA9|�A9%A8�yA8bNA7�A6�A5�FA57LA4ĜA4n�A3�;A2��A1�-A/�;A.1A-��A,�`A,�9A+�A+`BA+�A*bA(�jA&5?A$-A#��A#�A#dZA#A"��A"v�A"{A!A!�A�A  A�^AG�AbNA�A�wA �A��A{A  A��A�A�-Av�A"�A�/A�!AE�A�9A��A"�A��A{Ax�A
��A	�;A��AbA�AJA&�A��A"�A��A��A�A�TAA 5?@���@��T@�V@�Z@�ȴ@�^5@��@��F@��@���@��+@�M�@���@��7@��@��`@�@�@��@���@��`@��D@���@�o@�5?@���@�&�@�I�@��@���@��@��@�@�|�@��@���@ޏ\@�hs@���@���@���@�b@ׅ@��@���@ԛ�@�5?@�Ĝ@�z�@���@·+@́@���@���@ȴ9@ƸR@��@ź^@���@�1@Õ�@��H@�{@���@�K�@���@�b@�C�@��!@�n�@���@��@���@��9@�j@�j@�I�@�  @���@���@�G�@�&�@�%@�ƨ@��#@�(�@��@��
@��@��P@�K�@�^5@�A�@�=q@�V@�|�@���@�n�@�-@���@�7L@���@�Q�@���@���@��y@�p�@��D@��m@�
=@�M�@�/@�Ĝ@���@�I�@�|�@�K�@�C�@��@�-@���@���@��7@�/@��@�1@�ƨ@��@�
=@���@��@�V@�Ĝ@�bN@��@���@�o@�ff@�J@��@���@���@�x�@��@�I�@�(�@��@���@��R@�V@��@��@��9@���@�r�@�Z@��
@��@��@�dZ@�;d@���@�n�@�E�@��@��^@�p�@�G�@�/@���@�Ĝ@��D@�9X@��;@��
@���@��@�t�@�K�@�+@�"�@�"�@�"�@��@���@�ff@��@��-@�x�@�O�@��@��@���@��9@���@���@��u@�z�@��@��@��P@�t�@�S�@�"�@���@�{@���@�hs@�O�@�/@��`@���@���@��u@�j@l�@~E�@|��@|�/@|��@|Z@{��@{�@{dZ@{dZ@{S�@z~�@z^5@z=q@zJ@y��@y��@y�7@x��@w|�@vff@u?}@t��@t�@u/@up�@u�h@u�-@u��@u�-@up�@t��@sC�@k33@c@YG�@R��@N{@H1'@CS�@;��@65?@/;d@)7L@#��@;d@�^@@��@I�@Ĝ@��@M�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�7LA�9XA�9XA�33A�7LA�;dA�;dA�C�A�C�A�E�A�G�A�G�A�G�A�G�A�I�A�K�A�K�A�G�A�;dA�C�A�I�A�I�A�G�A�?}A�A�A�G�A�G�A�E�A�A�A�E�A�=qA�;dA�=qA�A�A�;dA�=qA�&�A�&�A�+A�5?A�?}A�?}A�33A�5?A�9XA�7LA�5?A�=qA�/A�"�A��A�+A�7LA�-A��A���A��`A��/A��/A���A���A���A���A���A��jA��9A���A��DA�t�A�ZA�7LA��
A�jA��#A���A��A���A��A�Q�A���A�A�A��9A�+A���A�oA���A�{A��`A���A�?}A���A�Q�A� �A�bA��yA���A�ZA���A�ĜA��PA��;A�E�A���A��
A���A�oA%A|�yA{��Az�Az�Ay�^Ay7LAw�Av�RAu�Au%Atr�As�
Asp�Ar�Aq�Ao�Al�Aj~�Aj-AiO�Ahr�Af~�Ac��Ab�DA_��A_C�A^�jA];dA\9XA[�A[|�AW�PAT~�AR�AR�uARffAR5?AQ�7AN�jALQ�AK�7AKC�AJv�AJ1'AI33AH��AG�AGAG�FAG33AE��AEoAD��AB�jAAG�AAVA@��A@�A>=qA;�PA9|�A9%A8�yA8bNA7�A6�A5�FA57LA4ĜA4n�A3�;A2��A1�-A/�;A.1A-��A,�`A,�9A+�A+`BA+�A*bA(�jA&5?A$-A#��A#�A#dZA#A"��A"v�A"{A!A!�A�A  A�^AG�AbNA�A�wA �A��A{A  A��A�A�-Av�A"�A�/A�!AE�A�9A��A"�A��A{Ax�A
��A	�;A��AbA�AJA&�A��A"�A��A��A�A�TAA 5?@���@��T@�V@�Z@�ȴ@�^5@��@��F@��@���@��+@�M�@���@��7@��@��`@�@�@��@���@��`@��D@���@�o@�5?@���@�&�@�I�@��@���@��@��@�@�|�@��@���@ޏ\@�hs@���@���@���@�b@ׅ@��@���@ԛ�@�5?@�Ĝ@�z�@���@·+@́@���@���@ȴ9@ƸR@��@ź^@���@�1@Õ�@��H@�{@���@�K�@���@�b@�C�@��!@�n�@���@��@���@��9@�j@�j@�I�@�  @���@���@�G�@�&�@�%@�ƨ@��#@�(�@��@��
@��@��P@�K�@�^5@�A�@�=q@�V@�|�@���@�n�@�-@���@�7L@���@�Q�@���@���@��y@�p�@��D@��m@�
=@�M�@�/@�Ĝ@���@�I�@�|�@�K�@�C�@��@�-@���@���@��7@�/@��@�1@�ƨ@��@�
=@���@��@�V@�Ĝ@�bN@��@���@�o@�ff@�J@��@���@���@�x�@��@�I�@�(�@��@���@��R@�V@��@��@��9@���@�r�@�Z@��
@��@��@�dZ@�;d@���@�n�@�E�@��@��^@�p�@�G�@�/@���@�Ĝ@��D@�9X@��;@��
@���@��@�t�@�K�@�+@�"�@�"�@�"�@��@���@�ff@��@��-@�x�@�O�@��@��@���@��9@���@���@��u@�z�@��@��@��P@�t�@�S�@�"�@���@�{@���@�hs@�O�@�/@��`@���@���@��u@�j@l�@~E�@|��@|�/@|��@|Z@{��@{�@{dZ@{dZ@{S�@z~�@z^5@z=q@zJ@y��@y��@y�7@x��@w|�@vff@u?}@t��@t�@u/@up�@u�h@u�-@u��@u�-@up�@t��@sC�@k33@c@YG�@R��@N{@H1'@CS�@;��@65?@/;d@)7L@#��@;d@�^@@��@I�@Ĝ@��@M�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�}B�wB�}B�}B�}B�}B�wB�wB�}B�}B�wB�wB�wB�qB�qB�qB�wB�wB�qB�jB�dB�jB�qB�qB�qB�jB�jB�qB�jB�jB�qB�dB�^B�XB�^B�dB�^B�RB�FB�9B�-B�3B�'B�'B�'B�!B�!B�B�B�B��B��B��B��B�\B~�BO�B(�B�}B@�B�B  BŢB��B�7B~�By�BgmB^5BP�B.B�BoB	7BBB
��B
��B
��B
�B
�B
�`B
�;B
��B
ƨB
�'B
��B
�VB
�B
y�B
jB
aHB
[#B
T�B
Q�B
L�B
C�B
9XB
33B
-B
'�B
"�B
�B
�B
VB	��B	�B	�BB	�/B	�B	��B	�}B	�!B	��B	�uB	�\B	�7B	~�B	w�B	s�B	m�B	S�B	B�B	:^B	8RB	7LB	49B	.B	"�B	�B	�B	�B	oB	bB	PB	
=B		7B		7B	+B	B	  B��B��B�B�B�B�B�`B�5B�B��B��B��B��B��B��BȴBǮBŢBÖB��B�jB�RB�3B�B�B�B��B��B��B��B��B��B�oB�VB�PB�JB�DB�7B�1B�+B�%B�B� Bz�Bw�Bv�Bs�Bq�Bp�Bm�BhsBe`BcTBbNB`BB\)BYBR�BL�BK�BJ�BG�BC�BA�B@�B>wB<jB:^B8RB6FB33B0!B.B+B(�B%�B%�B$�B#�B"�B �B�B�B�B�B�B�B�B�B�B�B{B{B{BuBuBuBoBoBhBbB\B\BVBVBPBJBJBDB
=B	7B+B1B+B+B%B%BBBBB%B%B	7BDBDBDB	7B+B+B	7B	7B1B1B1B+B1BDBVBVBVBVBbBbBbBhBbBuB�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B$�B$�B#�B%�B)�B0!B1'B1'B1'B1'B1'B33B9XBA�BD�BK�BN�BO�BO�BQ�BR�BS�BVBXBYBZBaHBe`BgmBjBm�Bt�Bv�Bw�By�B~�B~�B~�B� B�%B�=B�JB�JB�VB�oB�{B��B��B��B��B��B��B��B��B��B��B�B�B�'B�-B�-B�3B�9B�LB�qB�wBBƨBǮB��B��B��B�B�B�B�#B�;B�HB�NB�TB�ZB�B�B�B�B�B��B��B��B��B��B	  B	B	
=B	DB	JB	VB	oB	{B	�B	�B	�B	�B	�B	�B	�B	!�B	$�B	&�B	'�B	+B	.B	0!B	2-B	33B	49B	5?B	7LB	<jB	C�B	D�B	E�B	G�B	I�B	O�B	VB	[#B	^5B	_;B	aHB	e`B	ffB	hsB	iyB	jB	s�B	y�B	�B	�B	�B	�%B	�1B	�=B	�DB	�=B	�=B	�PB	�PB	�PB	�VB	�VB	�\B	�bB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	ȴB	�#B	�B
B

=B
�B
�B
)�B
2-B
=qB
D�B
L�B
R�B
YB
^5B
ffB
jB
o�B
r�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�XB�aB�_B�aB�aB�]B�_B�YB�YB�WB�[B�[B�YB�`B�`B�YB�YB�SB�KB�UB�SB�UB�SB�MB�MB�UB�SB�MB�MB�MB�EB�IB�GB�MB�MB�GB�DB�;B�BB�EB�GB�GB�BB�@B�IB�BB�BB�GB�<B�2B�1B�3B�<B�3B�%B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B�}B�2B~�BO�B(�B�JB@NB�B��B�oB��B�B~�By�Bg<B^BP�B-�ByB;B	B�B �B
��B
��B
��B
�xB
�LB
�.B
�B
ҾB
�vB
��B
��B
�%B
��B
y�B
jPB
aB
Z�B
T�B
Q�B
L�B
CiB
9)B
3B
,�B
'�B
"�B
�B
mB
)B	��B	�^B	�B	��B	��B	ͧB	�RB	��B	��B	�JB	�.B	�B	~�B	w�B	s�B	mhB	S�B	BfB	:6B	8(B	7#B	4B	-�B	"�B	sB	cB	VB	GB	:B	&B	
B		B		B	B	�B��B��B��B�B�hB�cB�YB�9B�B��B��B��B��B��BͬBˠBȌBǆB�yB�nB�]B�BB�+B�
B��B��B��B��B��B��B��B��B�tB�IB�/B�)B�$B�B�B�B�B��B��B�Bz�Bw�Bv�Bs�Bq�Bp}BmjBhPBe:Bc/Bb(B`B\BX�BR�BL�BK�BJ�BG�BCrBAbB@\B>TB<EB:7B8/B6!B3B/�B-�B*�B(�B%�B%�B$�B#�B"�B �ByBjBhB{B[BnBMBGB[BBB;BTB:BOB5BRBIB0B*B=B5B3BBB+B	BB B
B�B�B
B�BB�B�B�B�B�B�B�B�B�BBBB�BBB	B�B
B
B�BB�BBB/BBB<B8B!B%B!BNBCBiBqBvBxB~B�B�B�B�B�B�BtB�B �B$�B$�B#�B%�B)�B/�B0�B0�B0�B0�B0�B3	B9-BA\BDpBK�BN�BO�BO�BQ�BR�BS�BU�BW�BX�BY�BaBe3Bg=BjPBm`Bt�Bv�Bw�By�B~�B~�B~�B�B��B�B�B�B�&B�@B�JB�RB�UB�bB�hB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�?B�DB�]B�tB�zBʎBѸB��B��B��B��B��B�B�B�B�B�"B�LB�YB�\B�hB�B��B��B��B��B��B��B	�B	
B	B	B	!B	7B	DB	KB	OB	OB	OB	WB	cB	xB	!�B	$�B	&�B	'�B	*�B	-�B	/�B	1�B	2�B	4 B	5B	7B	<0B	C[B	DcB	EkB	GuB	I�B	O�B	U�B	Z�B	]�B	_B	aB	e'B	f.B	h;B	i@B	jCB	s}B	y�B	��B	��B	��B	��B	��B	�B	�	B	�B	�B	�B	�B	�B	�B	�B	�!B	�)B	�2B	�9B	�KB	�^B	�jB	�sB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�xB	��B	�jB
 �B
	�B
BB
zB
)�B
1�B
=4B
D^B
L�B
R�B
X�B
]�B
f'B
j=B
o`B
rrB
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.52 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708232016053117082320160531170823  AO  ARCAADJP                                                                    20150214201605    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150214201605  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150214201605  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170823  IP                  G�O�G�O�G�O�                