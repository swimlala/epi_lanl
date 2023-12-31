CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-07-25T12:36:13Z creation;2018-07-25T12:36:15Z conversion to V3.1;2019-12-23T06:18:01Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        @  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     @  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     @  _�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  n�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     @  r�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     @  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     @  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     @  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     @  �<   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     @  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  ټ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �p   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �t   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �x   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �|   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180725123613  20200120021521  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               MA   JA  I2_0675_077                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @�t���[ 1   @�t�5� @9+�u%�c+���o1   GPS     A   B   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffB  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DE��DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D��3D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ D�|�D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�<�Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�3D�9�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�
=@У�AQ�A(Q�AHQ�AhQ�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B{B
z�B{B{B"{B*{B2{B:{BB{BJ{BR{BZ{Bb{Bj{Br{Bz{B�
=B�=qB�
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
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�5�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�D !HD �HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD	!HD	�HD
!HD
�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD !HD �HD!!HD!�HD"!HD"�HD#!HD#�HD$!HD$�HD%!HD%�HD&!HD&�HD'!HD'�HD(!HD(�HD)!HD)�HD*!HD*�HD+!HD+�HD,!HD,�HD-!HD-�HD.!HD.�HD/!HD/�HD0!HD0�HD1!HD1�HD2!HD2�HD3!HD3�HD4!HD4�HD5!HD5�HD6!HD6�HD7!HD7�HD8!HD8�HD9!HD9�HD:!HD:�HD;!HD;�HD<!HD<�HD=!HD=�HD>!HD>�HD?!HD?�HD@!HD@�HDA!HDA�HDB!HDB�HDC!HDC�HDD!HDD�HDE!HDE�HDF�DF�HDG!HDG�HDH!HDH�HDI!HDI�HDJ!HDJ�HDK!HDK�HDL!HDL�HDM!HDM�HDN!HDN�HDO!HDO�HDP!HDP�HDQ!HDQ�HDR!HDR�HDS!HDS�HDT!HDT�HDU!HDU�HDV!HDV�HDW!HDW�HDX!HDX�HDY!HDY�HDZ!HDZ�HD[!HD[�HD\!HD\�HD]!HD]�HD^!HD^�HD_!HD_�HD`!HD`�HDa!HDa�HDb!HDb�HDc!HDc�HDd!HDd�HDe!HDe�HDf!HDf�HDg!HDg�HDh!HDh�HDi!HDi�HDj!HDj�HDk!HDk�HDl!HDl�HDm!HDm�HDn!HDn�HDo!HDo�HDp!HDp�HDq!HDq�HDr!HDr�HDs!HDs�HDt!HDt�HDu!HDu�HDv!HDv�HDw!HDw�HDx!HDx�HDy!HDy�HDz!HDz�HD{!HD{�HD|!HD|�HD}!HD}�HD~!HD~�HD!HD�HD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D�D�ФD��D�P�DÐ�D�ФD��D�P�DĐ�D�ФD��D�P�DŐ�D�ФD��D�P�DƐ�D�ФD��D�P�Dǐ�D���D��D�P�DȐ�D�ФD��D�P�Dɐ�D�ФD��D�P�Dʐ�D�ФD��D�P�Dː�D�ФD��D�P�D̐�D�ФD��D�P�D͐�D�ФD��D�P�Dΐ�D�ФD��D�P�Dϐ�D�ФD��D�P�DА�D�ФD��D�P�Dѐ�D�ФD��D�P�DҍqD�ФD��D�P�DӐ�D�ФD��D�P�DԐ�D�ФD��D�P�DՐ�D�ФD��D�MqD֐�D�ФD��D�P�Dא�D�ФD��D�P�Dؐ�D�ФD��D�P�Dِ�D�ФD��D�P�Dڐ�D�ФD��D�P�Dې�D�ФD��D�P�Dܐ�D�ФD��D�P�Dݐ�D�ФD��D�P�Dސ�D�ФD��D�P�Dߐ�D�ФD��D�P�D���D�ФD��D�P�DᐤD�ФD��D�P�D␤D�ФD��D�P�D㐤D�ФD��D�P�D䐤D�ФD��D�P�D吤D�ФD��D�P�D搤D�ФD��D�P�D琤D�ФD��D�P�D萤D�ФD��D�P�D鐤D�ФD��D�P�DꐤD�ФD��D�P�D됤D�ФD��D�P�D쐤D���D��D�P�D퐤D�ФD��D�P�DD�ФD��D�P�DD�ФD��D�P�D�D�ФD��D�P�D�D�ФD��D�P�D�D�ФD��D�P�D�D�ФD��D�J=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A��
A��A���A��
A��A���Aʟ�A�^5A�Q�A�O�A�M�A�=qA�$�Aɰ!A�ffA�-A��`A��/A�M�A�K�A���A�ĜA�33A���A��#A�`BA�  A���A�ƨA��A���A��A�ȴA�z�A�(�A�x�A�A��A�-A���A�r�A��
A��A�33A��A�r�A�XA��RA� �A���A�x�A���A���A��mA�oA�bNA��A��wA��A�A�A���A���A���A��A�A���A��7A�/A��A�oA���A���A��^A��#A�S�A��
A�?}A��!A��A�r�A���A��A�A�^5A�VA�E�A�1A��HA�bA�t�A��#A�VA��A���A�bNA��`A�\)A���A��wA��PA�1'A��wA��A�+A���A�33A��+A�7LA�-A�JA�A~{A~A�A~r�A}%Azn�AxM�AwO�Au�PArjAp~�An�!Am&�Ak��Ai�TAg��AfQ�Ae|�Ad�yAd$�Aa�^AaoA_�#AZ1'AV��ATr�AS�AO�^ALbNAJ�uAI;dAH��AI��AHZAH�AI\)AIdZAHĜAG\)AE�ADVAChsAB�`ABM�A@bNA>��A=A=
=A;�hA:ZA9"�A8ZA7��A6��A6I�A5��A4��A4�jA4Q�A4�A2�A0��A/��A/x�A.��A.5?A-A,�/A+��A*�yA*bNA)t�A(�A'?}A&��A&(�A%x�A$(�A"�A"(�A ��A   A�hA��A�+A\)A-AhsA�HAA��A9XA�mA�Az�A�mA�#A��A�A�FA�;A��A�-Al�AVAz�AA9XA
�/A
=qA
�A	�A	+A�AbAO�A��A�A;dA��A5?AK�A�yAȴA�uA�wAoA =q@�S�@���@��@�7L@��@�~�@�b@�-@���@���@�G�@��
@�@��@���@��@���@�G�@�I�@�$�@�@�Q�@�1'@�t�@�x�@���@���@�dZ@�~�@݉7@� �@�C�@�r�@���@�G�@�Q�@�;d@ҟ�@�7L@�j@Ͼw@�t�@�^5@̛�@˥�@�o@�v�@�$�@ɉ7@�@Ƈ+@�@�G�@�7L@�b@î@å�@öF@�\)@��@�@°!@�=q@�%@�33@�-@��^@��@�`B@�7L@�V@���@��
@�dZ@�"�@�~�@��@�C�@��R@�J@�O�@�%@��@�A�@�ƨ@�C�@�ȴ@�ff@�O�@���@�z�@�l�@�
=@���@���@��@��D@�1@�|�@�@��@�j@�b@�  @���@���@�=q@��h@�p�@�Ĝ@��@���@�33@�ƨ@��@���@�hs@�x�@�A�@���@��;@��@���@��m@���@�K�@��y@�M�@�$�@�J@���@�p�@�7L@��`@���@�dZ@�+@�o@�ȴ@��+@��@��#@��h@�p�@�?}@�/@���@���@�z�@�bN@�bN@�A�@��@��F@�\)@��@���@�5?@���@���@�G�@���@��@�Ĝ@�9X@��m@�ƨ@��@��@���@�|�@�\)@�;d@�"�@�@���@�n�@�$�@�J@���@���@���@��h@��7@�hs@��@��`@���@�r�@�I�@�(�@�1@��@���@�ƨ@���@�\)@�+@��@���@���@��+@�M�@�5?@��@��#@���@��7@�X@�/@�O�@�X@�`B@�X@�O�@���@��`@���@�z�@�I�@��@��F@���@�dZ@�K�@��@���@�ȴ@�~�@�J@���@���@�p�@�?}@��@��j@�r�@�(�@��@�1@���@���@��P@�K�@�
=@���@��R@��R@��@���@�~�@�=q@��@���@���@��7@�?}@��@��/@��j@�z�@�bN@�A�@��@��;@��@���@�C�@�"�@��@��!@�V@�@��^@��@�`B@�&�@���@�Ĝ@�bN@�(�@�@�@��@l�@;d@~�R@}��@}?}@}V@|�/@|��@|�@{�m@{�F@{�@{dZ@{o@{@{@y�#@yG�@x��@x�9@x�9@x��@x  @xb@w+@v��@v{@u�T@vff@v$�@up�@u��@u@u�-@u�-@t�j@t��@t�@t�@s��@s��@sS�@r�@r^5@q�#@qG�@p��@p�u@o��@n��@nv�@nE�@n$�@m�@m��@m�-@m�@l�@lj@k��@kt�@j�H@j~�@i��@i�7@i7L@h��@hr�@hb@g�@g\)@f��@fE�@f5?@fE�@fV@fff@f$�@e�@eO�@d�/@dI�@d(�@d(�@d(�@d(�@d�@c�m@cC�@c"�@c@b��@b��@b-@a��@a��@aX@a��@a��@aG�@`bN@_|�@_�@^ȴ@^E�@]��@]�-@]�@]`B@]/@\�@\�/@\�/@\�D@[�m@[�@Z�H@Z��@Z��@Y��@Y%@X�u@X�@XQ�@X �@W;d@V�+@V$�@U�T@Up�@U�@T��@T�D@TI�@S�m@Sƨ@St�@S33@R��@R��@R~�@RM�@R-@RJ@Qx�@PĜ@P�9@P �@O�w@O�P@O+@N��@N{@M�@M�h@M`B@M?}@L�/@L��@L�@Lz�@K�m@K33@K"�@K"�@K"�@J�!@I��@I�7@I7L@H�@H1'@Hb@G+@FV@F5?@F{@F@E��@E�-@E�@E`B@EO�@E�@D�j@D��@Dz�@DI�@D�@C�
@C�@C33@B�!@BM�@A�^@Ax�@AG�@A7L@A&�@A�@@�9@?�;@?\)@?�@>�y@>��@>v�@>ff@>E�@>$�@>{@=�@<�/@<Z@;��@;�F@;t�@;C�@;"�@:�@:�H@:^5@:J@9�#@9��@9hs@9X@9G�@9�@8Ĝ@8 �@7�@7�@7�P@7l�@7K�@7�@7
=@7
=@6�y@6��@6ff@6$�@5�@5�T@5�-@5p�@5V@4�/@4�@4��@4j@4I�@49X@3��@3�F@3t�@3C�@3o@3@2��@2��@2n�@2^5@2^5@2^5@2M�@1�#@1hs@1%@0��@0r�@0r�@0A�@0b@/�;@/�w@/|�@/;d@.��@.5?@.{@-�@-@-��@-�h@-�@-O�@-V@,�/@,��@,Z@+�m@+�@+"�@+@*��@*�\@*-@*J@)�@)��@)x�@)G�@)&�@)%@)%@(��@(Ĝ@(r�@(  @'\)@'+@'
=@&ȴ@&v�@&V@&5?@%�@%@%�h@%�@$��@$�j@$z�@$1@#��@#dZ@#33@"�H@"�!@"�\@"^5@!��@!�7@!�7@!hs@!&�@ ��@ ��@ bN@ A�@ b@�@�@|�@�y@�R@v�@ff@V@5?@@@��@`B@V@�/@�@I�@��@�@o@��@��@�\@~�@=q@��@��@&�@��@��@�@�@A�@ �@�@�@�w@��@�P@�@
=@
=@�@v�@E�@5?@5?@$�@��@��@�h@/@V@�@�/@�j@�j@��@9X@�m@��@�@S�@C�@"�@"�@@�H@��@��@~�@M�@�@�@J@��@�^@��@��@��@�7@7L@%@�`@�`@��@��@Ĝ@�9@�9@��@�@Q�@A�@1'@ �@b@�@�;@�@|�@\)@+@
=@��@�R@��@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A��
A��A���A��
A��A���Aʟ�A�^5A�Q�A�O�A�M�A�=qA�$�Aɰ!A�ffA�-A��`A��/A�M�A�K�A���A�ĜA�33A���A��#A�`BA�  A���A�ƨA��A���A��A�ȴA�z�A�(�A�x�A�A��A�-A���A�r�A��
A��A�33A��A�r�A�XA��RA� �A���A�x�A���A���A��mA�oA�bNA��A��wA��A�A�A���A���A���A��A�A���A��7A�/A��A�oA���A���A��^A��#A�S�A��
A�?}A��!A��A�r�A���A��A�A�^5A�VA�E�A�1A��HA�bA�t�A��#A�VA��A���A�bNA��`A�\)A���A��wA��PA�1'A��wA��A�+A���A�33A��+A�7LA�-A�JA�A~{A~A�A~r�A}%Azn�AxM�AwO�Au�PArjAp~�An�!Am&�Ak��Ai�TAg��AfQ�Ae|�Ad�yAd$�Aa�^AaoA_�#AZ1'AV��ATr�AS�AO�^ALbNAJ�uAI;dAH��AI��AHZAH�AI\)AIdZAHĜAG\)AE�ADVAChsAB�`ABM�A@bNA>��A=A=
=A;�hA:ZA9"�A8ZA7��A6��A6I�A5��A4��A4�jA4Q�A4�A2�A0��A/��A/x�A.��A.5?A-A,�/A+��A*�yA*bNA)t�A(�A'?}A&��A&(�A%x�A$(�A"�A"(�A ��A   A�hA��A�+A\)A-AhsA�HAA��A9XA�mA�Az�A�mA�#A��A�A�FA�;A��A�-Al�AVAz�AA9XA
�/A
=qA
�A	�A	+A�AbAO�A��A�A;dA��A5?AK�A�yAȴA�uA�wAoA =q@�S�@���@��@�7L@��@�~�@�b@�-@���@���@�G�@��
@�@��@���@��@���@�G�@�I�@�$�@�@�Q�@�1'@�t�@�x�@���@���@�dZ@�~�@݉7@� �@�C�@�r�@���@�G�@�Q�@�;d@ҟ�@�7L@�j@Ͼw@�t�@�^5@̛�@˥�@�o@�v�@�$�@ɉ7@�@Ƈ+@�@�G�@�7L@�b@î@å�@öF@�\)@��@�@°!@�=q@�%@�33@�-@��^@��@�`B@�7L@�V@���@��
@�dZ@�"�@�~�@��@�C�@��R@�J@�O�@�%@��@�A�@�ƨ@�C�@�ȴ@�ff@�O�@���@�z�@�l�@�
=@���@���@��@��D@�1@�|�@�@��@�j@�b@�  @���@���@�=q@��h@�p�@�Ĝ@��@���@�33@�ƨ@��@���@�hs@�x�@�A�@���@��;@��@���@��m@���@�K�@��y@�M�@�$�@�J@���@�p�@�7L@��`@���@�dZ@�+@�o@�ȴ@��+@��@��#@��h@�p�@�?}@�/@���@���@�z�@�bN@�bN@�A�@��@��F@�\)@��@���@�5?@���@���@�G�@���@��@�Ĝ@�9X@��m@�ƨ@��@��@���@�|�@�\)@�;d@�"�@�@���@�n�@�$�@�J@���@���@���@��h@��7@�hs@��@��`@���@�r�@�I�@�(�@�1@��@���@�ƨ@���@�\)@�+@��@���@���@��+@�M�@�5?@��@��#@���@��7@�X@�/@�O�@�X@�`B@�X@�O�@���@��`@���@�z�@�I�@��@��F@���@�dZ@�K�@��@���@�ȴ@�~�@�J@���@���@�p�@�?}@��@��j@�r�@�(�@��@�1@���@���@��P@�K�@�
=@���@��R@��R@��@���@�~�@�=q@��@���@���@��7@�?}@��@��/@��j@�z�@�bN@�A�@��@��;@��@���@�C�@�"�@��@��!@�V@�@��^@��@�`B@�&�@���@�Ĝ@�bN@�(�@�@�@��@l�@;d@~�R@}��@}?}@}V@|�/@|��@|�@{�m@{�F@{�@{dZ@{o@{@{@y�#@yG�@x��@x�9@x�9@x��@x  @xb@w+@v��@v{@u�T@vff@v$�@up�@u��@u@u�-@u�-@t�j@t��@t�@t�@s��@s��@sS�@r�@r^5@q�#@qG�@p��@p�u@o��@n��@nv�@nE�@n$�@m�@m��@m�-@m�@l�@lj@k��@kt�@j�H@j~�@i��@i�7@i7L@h��@hr�@hb@g�@g\)@f��@fE�@f5?@fE�@fV@fff@f$�@e�@eO�@d�/@dI�@d(�@d(�@d(�@d(�@d�@c�m@cC�@c"�@c@b��@b��@b-@a��@a��@aX@a��@a��@aG�@`bN@_|�@_�@^ȴ@^E�@]��@]�-@]�@]`B@]/@\�@\�/@\�/@\�D@[�m@[�@Z�H@Z��@Z��@Y��@Y%@X�u@X�@XQ�@X �@W;d@V�+@V$�@U�T@Up�@U�@T��@T�D@TI�@S�m@Sƨ@St�@S33@R��@R��@R~�@RM�@R-@RJ@Qx�@PĜ@P�9@P �@O�w@O�P@O+@N��@N{@M�@M�h@M`B@M?}@L�/@L��@L�@Lz�@K�m@K33@K"�@K"�@K"�@J�!@I��@I�7@I7L@H�@H1'@Hb@G+@FV@F5?@F{@F@E��@E�-@E�@E`B@EO�@E�@D�j@D��@Dz�@DI�@D�@C�
@C�@C33@B�!@BM�@A�^@Ax�@AG�@A7L@A&�@A�@@�9@?�;@?\)@?�@>�y@>��@>v�@>ff@>E�@>$�@>{@=�@<�/@<Z@;��@;�F@;t�@;C�@;"�@:�@:�H@:^5@:J@9�#@9��@9hs@9X@9G�@9�@8Ĝ@8 �@7�@7�@7�P@7l�@7K�@7�@7
=@7
=@6�y@6��@6ff@6$�@5�@5�T@5�-@5p�@5V@4�/@4�@4��@4j@4I�@49X@3��@3�F@3t�@3C�@3o@3@2��@2��@2n�@2^5@2^5@2^5@2M�@1�#@1hs@1%@0��@0r�@0r�@0A�@0b@/�;@/�w@/|�@/;d@.��@.5?@.{@-�@-@-��@-�h@-�@-O�@-V@,�/@,��@,Z@+�m@+�@+"�@+@*��@*�\@*-@*J@)�@)��@)x�@)G�@)&�@)%@)%@(��@(Ĝ@(r�@(  @'\)@'+@'
=@&ȴ@&v�@&V@&5?@%�@%@%�h@%�@$��@$�j@$z�@$1@#��@#dZ@#33@"�H@"�!@"�\@"^5@!��@!�7@!�7@!hs@!&�@ ��@ ��@ bN@ A�@ b@�@�@|�@�y@�R@v�@ff@V@5?@@@��@`B@V@�/@�@I�@��@�@o@��@��@�\@~�@=q@��@��@&�@��@��@�@�@A�@ �@�@�@�w@��@�P@�@
=@
=@�@v�@E�@5?@5?@$�@��@��@�h@/@V@�@�/@�j@�j@��@9X@�m@��@�@S�@C�@"�@"�@@�H@��@��@~�@M�@�@�@J@��@�^@��@��@��@�7@7L@%@�`@�`@��@��@Ĝ@�9@�9@��@�@Q�@A�@1'@ �@b@�@�;@�@|�@\)@+@
=@��@�R@��@��G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114B�?B�?B�?B�?B�?B�?B�?B�?B�XB�qB�}B�}B�}B�}B�}BƨBȴBȴB��B��B�/B �B8RBQ�B_;BdZBaHB\)BXBQ�BB�BF�BI�BN�BXB[#BVBS�BVBXB[#B]/Be`BhsBaHBffBs�Br�By�B�B�1B�Bu�Bt�Bv�Bw�Br�BiyBe`BaHBXBP�BM�BT�BYB_;BdZBgmBe`Be`Be`BW
B\)BH�B.B�B�BoB+B��B�B�yB�B�jB��B�JB�1B~�Bn�B^5BN�BC�B5?B �B	7B
��B
�ZB
��B
��B
�^B
�B
��B
��B
�\B
�1B
y�B
u�B
jB
_;B
H�B
-B
{B	��B	�B	��B
	7B
VB
hB

=B
B	�B	��B	��B	�B	��B	�7B	v�B	ffB	_;B	\)B	XB	R�B	?}B	6FB	-B	
=B�B�5B�
B��B�B��B��B�BƨB��B�5B��B	B	B��B��B�B�B�B�B�`B�;B�
B��B��BB�wB�XB�RB�?B�'B�'B�B�B�B��B��B��B��B��B��B�oB�bB�JB�=B�1B�+B�B~�By�Bw�Bu�Br�Bo�BhsBgmBdZBaHB`BB^5B\)B[#BVBT�BQ�BO�BK�BH�BG�BE�BF�BF�BH�BI�BH�BD�B?}B:^B<jB;dB:^B9XB7LB6FB5?B49B49B49B33B2-B1'B0!B/B/B-B-B,B,B)�B)�B(�B(�B&�B%�B#�B#�B"�B"�B"�B!�B!�B �B �B�B�B �B �B �B!�B �B �B�B�B!�B!�B!�B �B!�B"�B$�B$�B&�B&�B'�B'�B'�B-B-B/B/B1'B1'B33B49B49B49B5?B7LB8RB:^B<jB=qB?}B@�B@�BB�BE�BQ�BVBXBXB[#B^5B_;BcTBe`Be`BgmBiyBhsBiyBjBjBjBjBk�Bm�Bo�Bo�Bp�Bt�Bs�Bt�Bv�Bw�Bx�Bz�B|�B� B�%B�+B�=B�\B�\B�\B�oB�oB�uB�{B��B��B��B��B��B��B��B��B��B�B�B�!B�?B�LB�dB�qB��BĜB��B��B��B��B��B�
B�B�NB�fB�mB�yB�B�B�B�B�B��B��B��B��B��B	+B	bB	{B	�B	�B	�B	"�B	&�B	)�B	,B	.B	33B	5?B	7LB	<jB	<jB	<jB	=qB	@�B	A�B	E�B	G�B	I�B	H�B	I�B	H�B	I�B	I�B	J�B	N�B	Q�B	VB	XB	ZB	]/B	^5B	`BB	cTB	dZB	e`B	e`B	hsB	jB	l�B	m�B	m�B	o�B	q�B	r�B	s�B	u�B	z�B	{�B	}�B	� B	�B	�B	�%B	�+B	�1B	�=B	�DB	�DB	�JB	�VB	�\B	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�'B	�-B	�9B	�FB	�LB	�RB	�XB	�XB	�^B	�XB	�dB	�jB	�wB	�}B	��B	��B	B	ĜB	ƨB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�/B	�5B	�BB	�BB	�HB	�NB	�TB	�TB	�ZB	�`B	�fB	�mB	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
%B
B
%B
+B
+B
+B
%B
%B
+B
1B
1B

=B
DB
DB
DB
DB
DB
JB
JB
JB
JB
VB
\B
bB
bB
bB
bB
bB
bB
bB
bB
bB
bB
bB
hB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
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
�B
�B
 �B
!�B
!�B
"�B
"�B
"�B
"�B
#�B
$�B
$�B
$�B
&�B
&�B
&�B
'�B
(�B
(�B
)�B
)�B
+B
+B
+B
+B
+B
+B
+B
,B
,B
,B
,B
,B
+B
+B
+B
,B
,B
,B
-B
-B
-B
-B
-B
-B
-B
-B
.B
.B
.B
/B
.B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
2-B
33B
33B
33B
33B
33B
33B
49B
49B
5?B
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
;dB
;dB
=qB
=qB
=qB
>wB
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
L�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
T�B
S�B
S�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
VB
W
B
W
B
W
B
W
B
XB
XB
XB
XB
XB
XB
YB
YB
YB
YB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gm1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�%B�B�B�B�B�B�B�B�$B�<B�HB�cB�cB�cB�HB�tBȚBȀB̘B��B�B �B88BQ�B_Bd&BaB[�BW�BQ�BB[BF�BI�BN�BW�B[	BU�BS�BU�BW�BZ�B\�Be,Bh>BaBf2Bs�Br|By�B��B��B��Bu�Bt�Bv�Bw�Br|BiDBe,BaBW�BP�BM�BT�BX�B_Bd&BgRBe,Be,Be,BV�B[�BH�B-�B�BSB:BB��B�|B�DB��B�6B�eB�B��B~�BncB^BN�BCaB5B �B	B
��B
�&B
��B
ϫB
�*B
��B
��B
�MB
�(B
��B
y�B
u�B
jKB
_B
H�B
,�B
FB	�B	�oB	��B
	B
"B
4B

	B
�B	�B	��B	�OB	��B	�kB	�B	v�B	f2B	_B	[�B	W�B	R�B	?HB	6B	,�B	
	B�|B�B��B�UB��B��B��B��B�tB̘B�B��B	�B	 �B��B��B�B�UB�cB�WB�,B�B��BбBʌB�AB�(B�$B�B�B��B��B��B��B��B��B��B��B�kB�_B�SB�:B�.B�B�	B��B��B��B~�By�Bw�Bu�Br|BoOBh>Bg8Bd&BaB`B^B[�BZ�BU�BT�BQ�BO�BKxBH�BGzBEmBFtBFYBH�BI�BH�BDgB?.B:*B<6B;B:*B9	B7B6B5B4B4B3�B2�B1�B0�B/�B.�B.�B,�B,�B+�B+�B)�B)�B(�B(�B&�B%�B#�B#�B"�B"�B"�B!�B!�B �B �B�B�B vB vB �B!|B vB �B�B�B!�B!|B!�B vB!�B"�B$�B$�B&�B&�B'�B'�B'�B,�B,�B.�B.�B0�B0�B2�B3�B3�B3�B5B7B8B:B<6B="B?HB@OB@4BBABEmBQ�BU�BW�BW�BZ�B]�B^�BcBeBe,Bg8Bi*Bh$BiDBjKBjKBjKBjKBk6Bm]BoOBoOBpoBt�Bs�Bt�BvzBw�Bx�Bz�B|�B�B��B��B��B�B�B�B� B� B�&B�,B�?B�eB�kB�WB�|B��B��B��B��B��B��B��B��B�B�0B�<B�OB�MB�rBбBϑB̘BЗB��B��B��B�B�8B�*B�WB�iB�[B�aB�hB�B��B��B��B��B	�B	B	FB	9B	kB	�B	"�B	&�B	)�B	+�B	-�B	2�B	4�B	7B	<6B	<6B	<B	="B	@4B	A;B	ESB	GzB	I�B	HfB	IlB	HfB	I�B	I�B	JrB	N�B	Q�B	U�B	W�B	Y�B	\�B	^B	_�B	cB	d&B	eB	eB	h$B	j0B	l=B	m]B	m]B	oOB	qvB	raB	s�B	utB	z�B	{�B	}�B	�B	��B	��B	��B	��B	��B	�	B	�B	��B	��B	�B	�(B	�.B	� B	�2B	�9B	�YB	�qB	�xB	�dB	�jB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�$B	�	B	�*B	�$B	�B	�6B	�(B	�.B	�4B	�UB	�AB	�MB	�YB	�zB	�zB	ȀB	ɆB	�rB	̘B	̈́B	͟B	͟B	ΥB	бB	ѝB	ѝB	ѷB	ңB	յB	յB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�8B	�B	�DB	�KB	�0B	�KB	�6B	�=B	�=B	�=B	�]B	�IB	�cB	�cB	�OB	�UB	�|B	�|B	�hB	�hB	�nB	�tB	�zB	�zB	�zB	��B	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B

�B

�B

�B

�B

�B
�B
�B
�B
B
B
B
B
.B
B
.B
B
.B
B
.B
B
B
.B
B
:B
:B
:B
:B
:B
 B
 B
:B
 B
 B
:B
:B
 B
 B
 B
@B
@B
FB
FB
SB
9B
9B
9B
9B
SB
9B
MB
SB
SB
SB
9B
9B
SB
YB
?B
_B
EB
_B
EB
KB
eB
QB
QB
QB
kB
kB
kB
WB
]B
xB
]B
]B
~B
~B
�B
�B
�B
�B
 vB
!|B
!�B
"�B
"�B
"�B
"�B
#�B
$�B
$�B
$�B
&�B
&�B
&�B
'�B
(�B
(�B
)�B
)�B
*�B
*�B
*�B
*�B
*�B
*�B
*�B
+�B
+�B
+�B
+�B
+�B
*�B
*�B
*�B
+�B
+�B
+�B
,�B
,�B
,�B
,�B
,�B
,�B
,�B
,�B
-�B
-�B
-�B
.�B
-�B
.�B
.�B
.�B
.�B
/�B
/�B
/�B
/�B
/�B
0�B
0�B
0�B
0�B
0�B
0�B
1�B
2�B
2�B
2�B
2�B
2�B
2�B
3�B
3�B
4�B
6B
5�B
6B
5�B
6B
6�B
6�B
7B
7B
6�B
8B
8B
8B
8B
9$B
9	B
9	B
9	B
9	B
:B
:B
:*B
:B
;B
;B
=<B
="B
=<B
>BB
?.B
?.B
?HB
?HB
?.B
@4B
@OB
@4B
@4B
AUB
A;B
A;B
A;B
A;B
AUB
BAB
BAB
BAB
CaB
CGB
CGB
CGB
CGB
CaB
CGB
CGB
CaB
DgB
DgB
EmB
EmB
EmB
EmB
ESB
FYB
FYB
FtB
FYB
FYB
G_B
G_B
HfB
H�B
H�B
HfB
HfB
HfB
HfB
H�B
I�B
IlB
IlB
J�B
JrB
KxB
KxB
KxB
KxB
L~B
L~B
L�B
L~B
L~B
L~B
M�B
M�B
L�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
T�B
S�B
S�B
T�B
T�B
T�B
U�B
U�B
U�B
U�B
U�B
U�B
V�B
V�B
V�B
V�B
W�B
W�B
W�B
W�B
W�B
W�B
X�B
X�B
X�B
X�B
Y�B
Y�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[�B
[�B
[�B
[�B
[�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
]�B
]�B
^B
^B
_B
^�B
_B
^�B
^�B
_�B
_�B
_�B
_�B
`B
`B
_�B
aB
`�B
aB
`�B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
bB
c B
cB
cB
c B
c B
c B
c B
dB
d&B
d&B
dB
d&B
d&B
d&B
dB
e,B
eB
eB
e,B
e,B
eB
eB
e,B
e,B
e,B
eB
e,B
fB
f2B
fB
f2B
fB
f2B
f2B
fB
g8B
g8B
g8B
g8B
gB
g8G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.52(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201807310039402018073100394020180731003940201808010033002018080100330020180801003300JA  ARFMdecpA19c                                                                20180725213611  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180725123613  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180725123614  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180725123614  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180725123615  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180725123615  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180725123615  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180725123615  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180725123615  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180725123615                      G�O�G�O�G�O�                JA  ARUP                                                                        20180725125512                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180725154050  CV  JULD            G�O�G�O�Fä�                JM  ARSQJMQC2.0                                                                 20180726000000  CF  PSAL_ADJUSTED_QCD�9�D�9�G�O�                JM  ARSQJMQC2.0                                                                 20180726000000  CF  TEMP_ADJUSTED_QCD�9�D�9�G�O�                JM  ARCAJMQC2.0                                                                 20180730153940  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180730153940  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180731153300  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021521                      G�O�G�O�G�O�                