CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-08-20T03:35:11Z creation;2018-08-20T03:35:14Z conversion to V3.1;2019-12-23T06:16:35Z update;     
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
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  IL   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M8   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tp   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �D   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Τ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  �T   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ޴   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �X   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �h   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �l   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �|   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20180820033511  20200120021522  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               SA   JA  I2_0675_083                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @�z��Z��1   @�z�m�5 @84�*�1�c1��rG1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH�CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� DdfDd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ Dż�D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��D�  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���@У�AQ�A(Q�AHQ�AhQ�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B{B
{B{B{B"{B*{B2{B:{BB{BJ{BR{BZ{Bb{Bj{Br{Bz{B�=qB�
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
=B�
=C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH��CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�O\C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�O\C�B�C�B�C�B�C�B�C�O\C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�D !HD �HD!HD�HD!HD�HD�D�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD	!HD	�HD
!HD
�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD !HD �HD!!HD!�HD"!HD"�HD#!HD#�HD$!HD$�HD%!HD%�HD&!HD&�HD'!HD'�HD(!HD(�HD)!HD)�HD*!HD*�HD+!HD+�HD,!HD,�HD-!HD-�HD.!HD.�HD/!HD/�HD0!HD0�HD1!HD1�HD2!HD2�HD3!HD3�HD4!HD4�HD5!HD5�HD6!HD6�HD7!HD7�HD8!HD8�HD9!HD9�HD:!HD:�HD;!HD;�HD<!HD<�HD=!HD=�HD>!HD>�HD?!HD?�HD@!HD@�HDA!HDA�HDB!HDB�HDC!HDC�HDD!HDD�HDE!HDE�HDF!HDF�HDG!HDG�HDH!HDH�HDI!HDI�HDJ!HDJ�HDK!HDK�HDL!HDL�HDM!HDM�HDN!HDN�HDO!HDO�HDP!HDP�HDQ!HDQ�HDR!HDR�HDS!HDS�HDT!HDT�HDU!HDU�HDV!HDV�HDW!HDW�HDX!HDX�HDY!HDY�HDZ!HDZ�HD[!HD[�HD\!HD\�HD]!HD]�HD^!HD^�HD_!HD_�HD`!HD`�HDa!HDa�HDb!HDb�HDc!HDc�HDd'�Dd�HDe!HDe�HDf!HDf�HDg!HDg�HDh!HDh�HDi!HDi�HDj!HDj�HDk!HDk�HDl!HDl�HDm!HDm�HDn!HDn�HDo!HDo�HDp!HDp�HDq!HDq�HDr!HDr�HDs!HDs�HDt!HDt�HDu!HDu�HDv!HDv�HDw!HDw�HDx!HDx�HDy!HDy�HDz!HDz�HD{!HD{�HD|!HD|�HD}!HD}�HD~!HD~�HD!HD�HD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D�D�ФD��D�P�DÐ�D�ФD��D�P�DĐ�D�ФD��D�P�DŐ�D��qD��D�P�DƐ�D�ФD��D�P�Dǐ�D�ФD��D�P�DȐ�D�ФD��D�P�Dɐ�D�ФD��D�P�Dʐ�D�ФD��D�P�Dː�D�ФD��D�P�D̐�D�ФD��D�P�D͐�D�ФD��D�P�Dΐ�D�ФD��D�P�Dϐ�D�ФD��D�P�DА�D�ФD��D�P�Dѐ�D�ФD��D�P�DҐ�D�ФD��D�P�DӐ�D�ФD��D�P�DԐ�D�ФD��D�P�DՐ�D�ФD��D�P�D֐�D�ФD��D�P�Dא�D�ФD��D�P�Dؐ�D�ФD��D�P�Dِ�D�ФD��D�P�Dڐ�D�ФD��D�P�Dې�D�ФD��D�P�Dܐ�D�ФD��D�P�Dݐ�D�ФD��D�P�Dސ�D�ФD��D�P�Dߐ�D�ФD��D�P�D���D�ФD��D�P�DᐤD�ФD��D�P�D␤D�ФD��D�P�D㐤D�ФD��D�P�D䐤D�ФD��D�P�D吤D�ФD��D�P�D搤D�ФD��D�P�D琤D�ФD��D�P�D萤D�ФD��D�P�D鐤D�ФD��D�P�DꐤD�ФD��D�P�D됤D�ФD��D�P�D쐤D�ФD��D�P�D퐤D�ФD��D�P�DD�ФD��D�P�DD�ФD��D�P�D�D�ФD��D�P�D�D�ФD��D�P�D�D�ФD��D�P�D�D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD�qD�0�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�7LA�{A���Aѕ�A�bNA�M�A�;dA��Aв-A�ȴA���A�I�AͬA͑hA�~�A�=qA��A�A��`A�jA�7LA�A���AʼjAʴ9A�dZAɧ�A�E�A��wA�ffA�A�A�n�A�ƨA��A�t�A��yA�S�A�z�A�%A���A��A�&�A���A��A��9A�E�A�
=A��^A�9XA��-A�oA��^A�|�A�A�A���A��A�1'A��A���A�\)A��A��\A�1A�~�A��A�t�A��FA��HA���A���A��A��A�t�A�?}A�9XA��A���A���A��jA�t�A�5?A��^A��A�"�A���A�"�A���A��A�~�A��jA��A��A��A���A��/A�l�A��/A�ffA��9A���A��PA�G�A�A�A�JA�ffA�9XA�ZA�Q�A�E�A�{A�l�A���A�A�A��A�XA��A��TA�hsA��9A�\)A���A�A��`A��RA�r�A�;dA��A�XA���A���A�|�A�jA��A|��Ay��Aw�FAu��At=qAs\)Aq33AnȴAm�Ak��AjQ�Ai��Ah{AdI�A]|�A[G�AWC�AR~�AL�AK/AI�#AH  AEt�AD$�AB�RAA�A@~�A="�A;K�A:�/A:�9A9��A9��A9XA8��A8�A7�hA7+A6ZA4ȴA2�A1XA0�A0^5A/�-A.5?A+�A*�yA*ffA)��A)S�A(�9A'�wA'A&�`A&n�A%O�A#�7A"A �uA A/A\)AG�A~�A�AȴAbNAO�A5?At�A��A�A(�A��A��A/A�!AffA��A33AQ�At�A�+A�PA
�A
I�A	�FA��Ap�AVAn�A9XAA�TA�PAO�A��A��A�AK�A�RAI�A�TA ��A bNA 1'@��F@���@��@�33@�E�@�O�@��;@���@�@�C�@���@���@�%@�u@�1@�n�@���@�Q�@�  @���@�hs@��@��@߅@���@���@��#@�7L@؃@ׅ@��T@���@ԓu@��@�v�@�X@ϝ�@�~�@�-@ͺ^@�`B@�&�@�bN@�\)@�@Ȭ@ȓu@�l�@���@�/@�Z@�\)@�7L@��w@��y@�^5@�-@�=q@�x�@��@��9@���@�Q�@��;@�S�@�ȴ@�$�@��h@�&�@��D@���@��y@�-@��h@��`@�Z@�;d@���@�x�@�O�@� �@�@���@��H@��@�v�@�dZ@�;d@��@�bN@�j@��@���@���@�(�@��m@�\)@���@�A�@��
@�1'@�Q�@�z�@�I�@��@��;@�K�@�v�@�@��T@�x�@�X@���@�z�@�K�@��R@�v�@�n�@�=q@���@�`B@�%@��@�r�@�Z@�1'@��@��F@��H@�-@�J@���@��@��#@��-@��7@�`B@���@�Ĝ@���@�bN@�Q�@�9X@���@��@���@��@�5?@�x�@��@�r�@���@�o@�E�@���@��#@���@�p�@�%@��@�  @��w@�|�@�dZ@�S�@�+@�ȴ@���@�^5@���@���@���@�@�@�@���@��@�G�@��@���@���@�r�@�A�@��@�  @�  @���@���@�dZ@�K�@�C�@�"�@��y@���@���@�~�@�^5@���@�@�"�@��y@���@�5?@��T@�O�@�&�@��`@��9@�r�@�I�@�A�@��@���@���@�33@�;d@��@�o@��H@���@�5?@�=q@�E�@�-@�J@���@���@���@�V@��9@�z�@�Q�@�b@��
@�dZ@�@�
=@��y@���@�^5@�$�@�{@���@���@��h@�X@��@��`@���@��9@��D@�j@�(�@�b@��;@��@�S�@�;d@�+@�+@��@�@��H@��!@�~�@�v�@�V@��@��^@�x�@�%@��@�Q�@�9X@��@l�@~�y@~�+@}�@}�@|�D@|Z@|9X@{ƨ@{dZ@z��@z~�@z^5@z�@y�^@y�7@y�@xĜ@xQ�@xb@w�w@w|�@v��@v�+@u�@uV@tj@t(�@sdZ@r��@r�\@rn�@rn�@rM�@r-@rJ@q�#@q��@q&�@p��@p�u@pbN@pb@o�w@o�P@o|�@oK�@o�@n�@nv�@n{@m�T@m�@mV@l�/@l��@lZ@lI�@l(�@k��@k��@kdZ@j�H@jn�@j=q@i��@i��@iG�@hr�@hb@g�;@g�w@g|�@g+@f��@f�y@fȴ@f��@fff@e�T@ep�@e�@d��@d��@dZ@c�m@cS�@c33@co@b��@bn�@b=q@a��@a�#@a��@aX@`Ĝ@`  @_�@_
=@^�y@^�R@^v�@^5?@^@^$�@^E�@^$�@^@]@]��@]p�@]�@\�j@\(�@[ƨ@[�@[33@Z��@Zn�@ZJ@Y��@Yhs@Y%@XĜ@X��@X�@W�@W�w@W�P@W|�@W+@V$�@U�T@U�h@U/@T��@T��@TZ@S�
@St�@SC�@R��@Rn�@RM�@R-@Q��@Q��@Qx�@QG�@P�`@P�9@PA�@O�@O�@O�P@O+@N��@N��@Nv�@M�T@M`B@M?}@M�@L�@L�D@LI�@L(�@K�m@K��@KdZ@KdZ@KdZ@KdZ@KS�@K@J~�@Jn�@Jn�@J^5@I��@I��@I��@Ix�@IG�@H��@H�@H �@G�P@G�@Fȴ@F��@FV@E��@E�h@EO�@E�@D��@D��@Dj@DZ@C��@C�
@Cƨ@CS�@B�!@B�\@B�\@Bn�@B�@A��@A7L@@�9@@1'@?�P@>�y@>ȴ@>�R@>��@>v�@>ff@>E�@=�@=V@<��@<z�@<(�@;��@;��@;C�@;33@;"�@;o@:�@:�!@:��@:^5@:=q@9�@9x�@9�@8��@8Ĝ@8��@8r�@8Q�@8  @7��@7�@7\)@7+@6�y@6ȴ@6@5��@5�h@5p�@5`B@5O�@5V@4��@4�/@4j@49X@3�m@3dZ@3S�@3dZ@3S�@3o@2��@2��@2��@2�\@2n�@2^5@2M�@1�#@1x�@1G�@1%@0bN@0 �@0b@/��@/\)@/;d@/�@.��@.v�@.ff@.ff@.E�@.{@.@-�-@-p�@-O�@-�@,�/@,��@,�@+�F@+dZ@+o@*��@*�\@*^5@*�@)�#@)x�@)7L@(��@(��@(b@'�w@'�@'��@'|�@'\)@'
=@&�y@&ȴ@&�R@&��@&��@&v�@&5?@%�@%p�@%O�@$�@$�j@$��@$j@$I�@$I�@$Z@$9X@#��@#dZ@"�@"�\@"~�@"^5@"M�@"=q@"J@!�#@!��@!�7@!�@ Ĝ@ bN@ b@��@�P@+@�@+@�@
=@��@��@ȴ@��@V@�@�T@��@p�@?}@V@�@�@�@�@��@(�@�
@ƨ@��@t�@dZ@33@@�H@�H@��@��@�!@��@�\@~�@n�@M�@J@��@7L@�@%@%@Ĝ@��@�u@b@�;@�@��@K�@�R@��@E�@�T@��@@�-@p�@O�@V@V@�@�@�D@I�@(�@1@ƨ@��@S�@C�@33@"�@@��@�!@�\@^5@M�@=q@�@��@�@��@��@�#@��@x�@G�@��@Ĝ@bN@1'@  @�@|�@|�@\)@+@�y@�R@��@�+@ff@E�@$�@{@�@��@��@p�@O�@V@�/@��@�j@��@z�@9X@�
@��@t�@dZ@dZ@S�@@
=q@
-@
=q11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�7LA�{A���Aѕ�A�bNA�M�A�;dA��Aв-A�ȴA���A�I�AͬA͑hA�~�A�=qA��A�A��`A�jA�7LA�A���AʼjAʴ9A�dZAɧ�A�E�A��wA�ffA�A�A�n�A�ƨA��A�t�A��yA�S�A�z�A�%A���A��A�&�A���A��A��9A�E�A�
=A��^A�9XA��-A�oA��^A�|�A�A�A���A��A�1'A��A���A�\)A��A��\A�1A�~�A��A�t�A��FA��HA���A���A��A��A�t�A�?}A�9XA��A���A���A��jA�t�A�5?A��^A��A�"�A���A�"�A���A��A�~�A��jA��A��A��A���A��/A�l�A��/A�ffA��9A���A��PA�G�A�A�A�JA�ffA�9XA�ZA�Q�A�E�A�{A�l�A���A�A�A��A�XA��A��TA�hsA��9A�\)A���A�A��`A��RA�r�A�;dA��A�XA���A���A�|�A�jA��A|��Ay��Aw�FAu��At=qAs\)Aq33AnȴAm�Ak��AjQ�Ai��Ah{AdI�A]|�A[G�AWC�AR~�AL�AK/AI�#AH  AEt�AD$�AB�RAA�A@~�A="�A;K�A:�/A:�9A9��A9��A9XA8��A8�A7�hA7+A6ZA4ȴA2�A1XA0�A0^5A/�-A.5?A+�A*�yA*ffA)��A)S�A(�9A'�wA'A&�`A&n�A%O�A#�7A"A �uA A/A\)AG�A~�A�AȴAbNAO�A5?At�A��A�A(�A��A��A/A�!AffA��A33AQ�At�A�+A�PA
�A
I�A	�FA��Ap�AVAn�A9XAA�TA�PAO�A��A��A�AK�A�RAI�A�TA ��A bNA 1'@��F@���@��@�33@�E�@�O�@��;@���@�@�C�@���@���@�%@�u@�1@�n�@���@�Q�@�  @���@�hs@��@��@߅@���@���@��#@�7L@؃@ׅ@��T@���@ԓu@��@�v�@�X@ϝ�@�~�@�-@ͺ^@�`B@�&�@�bN@�\)@�@Ȭ@ȓu@�l�@���@�/@�Z@�\)@�7L@��w@��y@�^5@�-@�=q@�x�@��@��9@���@�Q�@��;@�S�@�ȴ@�$�@��h@�&�@��D@���@��y@�-@��h@��`@�Z@�;d@���@�x�@�O�@� �@�@���@��H@��@�v�@�dZ@�;d@��@�bN@�j@��@���@���@�(�@��m@�\)@���@�A�@��
@�1'@�Q�@�z�@�I�@��@��;@�K�@�v�@�@��T@�x�@�X@���@�z�@�K�@��R@�v�@�n�@�=q@���@�`B@�%@��@�r�@�Z@�1'@��@��F@��H@�-@�J@���@��@��#@��-@��7@�`B@���@�Ĝ@���@�bN@�Q�@�9X@���@��@���@��@�5?@�x�@��@�r�@���@�o@�E�@���@��#@���@�p�@�%@��@�  @��w@�|�@�dZ@�S�@�+@�ȴ@���@�^5@���@���@���@�@�@�@���@��@�G�@��@���@���@�r�@�A�@��@�  @�  @���@���@�dZ@�K�@�C�@�"�@��y@���@���@�~�@�^5@���@�@�"�@��y@���@�5?@��T@�O�@�&�@��`@��9@�r�@�I�@�A�@��@���@���@�33@�;d@��@�o@��H@���@�5?@�=q@�E�@�-@�J@���@���@���@�V@��9@�z�@�Q�@�b@��
@�dZ@�@�
=@��y@���@�^5@�$�@�{@���@���@��h@�X@��@��`@���@��9@��D@�j@�(�@�b@��;@��@�S�@�;d@�+@�+@��@�@��H@��!@�~�@�v�@�V@��@��^@�x�@�%@��@�Q�@�9X@��@l�@~�y@~�+@}�@}�@|�D@|Z@|9X@{ƨ@{dZ@z��@z~�@z^5@z�@y�^@y�7@y�@xĜ@xQ�@xb@w�w@w|�@v��@v�+@u�@uV@tj@t(�@sdZ@r��@r�\@rn�@rn�@rM�@r-@rJ@q�#@q��@q&�@p��@p�u@pbN@pb@o�w@o�P@o|�@oK�@o�@n�@nv�@n{@m�T@m�@mV@l�/@l��@lZ@lI�@l(�@k��@k��@kdZ@j�H@jn�@j=q@i��@i��@iG�@hr�@hb@g�;@g�w@g|�@g+@f��@f�y@fȴ@f��@fff@e�T@ep�@e�@d��@d��@dZ@c�m@cS�@c33@co@b��@bn�@b=q@a��@a�#@a��@aX@`Ĝ@`  @_�@_
=@^�y@^�R@^v�@^5?@^@^$�@^E�@^$�@^@]@]��@]p�@]�@\�j@\(�@[ƨ@[�@[33@Z��@Zn�@ZJ@Y��@Yhs@Y%@XĜ@X��@X�@W�@W�w@W�P@W|�@W+@V$�@U�T@U�h@U/@T��@T��@TZ@S�
@St�@SC�@R��@Rn�@RM�@R-@Q��@Q��@Qx�@QG�@P�`@P�9@PA�@O�@O�@O�P@O+@N��@N��@Nv�@M�T@M`B@M?}@M�@L�@L�D@LI�@L(�@K�m@K��@KdZ@KdZ@KdZ@KdZ@KS�@K@J~�@Jn�@Jn�@J^5@I��@I��@I��@Ix�@IG�@H��@H�@H �@G�P@G�@Fȴ@F��@FV@E��@E�h@EO�@E�@D��@D��@Dj@DZ@C��@C�
@Cƨ@CS�@B�!@B�\@B�\@Bn�@B�@A��@A7L@@�9@@1'@?�P@>�y@>ȴ@>�R@>��@>v�@>ff@>E�@=�@=V@<��@<z�@<(�@;��@;��@;C�@;33@;"�@;o@:�@:�!@:��@:^5@:=q@9�@9x�@9�@8��@8Ĝ@8��@8r�@8Q�@8  @7��@7�@7\)@7+@6�y@6ȴ@6@5��@5�h@5p�@5`B@5O�@5V@4��@4�/@4j@49X@3�m@3dZ@3S�@3dZ@3S�@3o@2��@2��@2��@2�\@2n�@2^5@2M�@1�#@1x�@1G�@1%@0bN@0 �@0b@/��@/\)@/;d@/�@.��@.v�@.ff@.ff@.E�@.{@.@-�-@-p�@-O�@-�@,�/@,��@,�@+�F@+dZ@+o@*��@*�\@*^5@*�@)�#@)x�@)7L@(��@(��@(b@'�w@'�@'��@'|�@'\)@'
=@&�y@&ȴ@&�R@&��@&��@&v�@&5?@%�@%p�@%O�@$�@$�j@$��@$j@$I�@$I�@$Z@$9X@#��@#dZ@"�@"�\@"~�@"^5@"M�@"=q@"J@!�#@!��@!�7@!�@ Ĝ@ bN@ b@��@�P@+@�@+@�@
=@��@��@ȴ@��@V@�@�T@��@p�@?}@V@�@�@�@�@��@(�@�
@ƨ@��@t�@dZ@33@@�H@�H@��@��@�!@��@�\@~�@n�@M�@J@��@7L@�@%@%@Ĝ@��@�u@b@�;@�@��@K�@�R@��@E�@�T@��@@�-@p�@O�@V@V@�@�@�D@I�@(�@1@ƨ@��@S�@C�@33@"�@@��@�!@�\@^5@M�@=q@�@��@�@��@��@�#@��@x�@G�@��@Ĝ@bN@1'@  @�@|�@|�@\)@+@�y@�R@��@�+@ff@E�@$�@{@�@��@��@p�@O�@V@�/@��@�j@��@z�@9X@�
@��@t�@dZ@dZ@S�@@
=q@
-@
=q11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BB��BÖBĜBƨBǮBȴB��B�#B�yB��B��B��B��B��B��B�B�B�B�sB�yB�mB�sB�sB�mB�mB�sB+B �B%�B-B7LB=qB=qB?}BE�BL�BJ�BN�BQ�BW
B^5B\)B^5B_;BcTBe`BgmBo�Bw�Bu�Bt�Bu�Bw�B{�B}�B�B�B�B�%B�%B�B�B�%B�B�B|�B|�Bv�B�hB��B��B�B�9BBĜBBĜBƨB��B��B�RB�9B�B��B��B�VB�Bq�BcTB[#BO�BD�B6FB �B+B
=BB��B�yBɺB�B�oB|�Bw�B�\B�-B�^B��B�BiyBB�B!�B
�HB
�RB
�?B
�!B
��B
��B
�hB
�DB
�+B
�bB
��B
��B
��B
�{B
�B
y�B
v�B
t�B
q�B
m�B
W
B
@�B
(�B
�B
1B
B	��B	�;B	ǮB	�XB	��B	��B	�hB	iyB	�B	B��B��Bv�Be`B\)BQ�BD�B<jB8RB2-B+B%�B�B%�B33BD�BF�BG�BL�BS�BT�BVBW
BL�BR�BM�BL�BM�BJ�BI�B<jB;dB>wB@�BB�BA�BA�B?}B>wB>wB<jB=qB<jB:^B8RB7LB49B1'B2-B33B2-B33B5?B6FB49B5?B49B33B5?B33B2-B2-B2-B33B33B1'B/B)�B'�B&�B%�B$�B#�B#�B"�B"�B"�B!�B"�B"�B!�B#�B"�B"�B#�B%�B&�B&�B)�B'�B'�B(�B)�B&�B&�B&�B'�B(�B)�B+B-B,B)�B'�B'�B'�B%�B%�B$�B$�B$�B"�B&�B$�B&�B$�B)�B)�B+B+B,B.B.B.B0!B0!B2-B49B9XB=qBE�BF�BE�BF�BG�BK�BK�BJ�BI�BG�BI�BL�BK�BI�BF�BF�BF�BH�BK�BN�BQ�BQ�BQ�BP�BP�BP�BP�BR�BS�BVBXBZB\)B^5B_;B_;B_;BaHB`BBaHBgmBhsBk�Bn�Bs�B}�B}�Bs�By�B� B�B�B�7B�bB��B��B��B�B�FB�LB�dBĜBƨBȴB��B��B��B��B��B�
B�B�/B�5B�HB�TB�mB�B�B�B�B��B��B��B��B��B��B��B��B	B	%B	1B		7B		7B	
=B	DB	VB	\B	bB	{B	�B	�B	�B	�B	�B	�B	�B	"�B	&�B	,B	33B	6FB	:^B	=qB	>wB	C�B	F�B	J�B	N�B	O�B	R�B	VB	YB	[#B	^5B	_;B	`BB	bNB	ffB	hsB	k�B	q�B	s�B	s�B	t�B	t�B	t�B	u�B	w�B	y�B	{�B	~�B	� B	�B	�B	�B	�+B	�+B	�7B	�DB	�JB	�PB	�VB	�\B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�'B	�-B	�3B	�3B	�LB	�dB	ÖB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�#B	�#B	�#B	�)B	�)B	�)B	�)B	�/B	�5B	�5B	�5B	�;B	�;B	�BB	�HB	�HB	�NB	�TB	�TB	�TB	�TB	�TB	�ZB	�`B	�fB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
+B
+B
1B
1B
1B
	7B
	7B
	7B

=B

=B

=B

=B
DB
DB
JB
JB
JB
PB
PB
PB
\B
\B
\B
bB
bB
bB
bB
bB
hB
hB
hB
oB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
 �B
 �B
 �B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
+B
+B
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
.B
.B
/B
/B
/B
/B
/B
/B
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
2-B
33B
33B
49B
49B
49B
49B
49B
49B
5?B
6FB
6FB
5?B
6FB
6FB
6FB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
:^B
:^B
:^B
:^B
;dB
<jB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
@�B
@�B
A�B
A�B
B�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
C�B
C�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
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
R�B
R�B
R�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
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
XB
XB
XB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
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
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
aHB
`BB
aHB
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
ffB
ffB
ffB
ffB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�[B�oB�aB�gB�tBǔBȀB˒B��B�DB��B��B��B��B��B�B�B��B�eB�>B�DB�RB�>B�>B�RB�8B�>B�B �B%�B,�B7B=<B=<B?HBEmBL�BJ�BN�BQ�BV�B^B[�B^B_Bc BeFBgRBoiBw�Bu�Bt�Bu�Bw�B{�B}�B��B��B��B�B��B�B��B��B��B��B|�B|�Bv�B�4B��B��B��B�B�[B�gB�[B�gB�tB�oB�OB�8B�B��B��B�SB�"B��BqvBc BZ�BO�BDgB6B �B�B
	B �B�B�DBɆB��B�:B|�Bw�B�(B��B�*B��B��BiDBB[B!�B
�B
�B
�B
��B
��B
�qB
�4B
�B
��B
�.B
�qB
�_B
�YB
�FB
��B
y�B
v�B
t�B
qvB
m]B
V�B
@OB
(�B
MB
�B
 �B	��B	�B	�zB	�$B	��B	�~B	�4B	iDB	SB	 �BбB��Bv�Be,B[�BQ�BDgB<6B8B1�B*�B%�B�B%�B2�BDgBFtBGzBL�BS�BT�BU�BV�BL�BR�BM�BL�BM�BJ�BIlB<6B;0B>BB@4BBABAUBAUB?HB>BB>(B<B=<B<B:*B8B7B4B0�B1�B2�B1�B2�B4�B5�B4B4�B4B2�B5B2�B1�B1�B1�B2�B2�B0�B.�B)�B'�B&�B%�B$�B#�B#�B"�B"�B"�B!�B"�B"�B!|B#�B"�B"�B#�B%�B&�B&�B)�B'�B'�B(�B)�B&�B&�B&�B'�B(�B)�B*�B,�B+�B)�B'�B'�B'�B%�B%�B$�B$�B$�B"�B&�B$�B&�B$�B)�B)�B*�B*�B+�B-�B-�B-�B/�B/�B1�B4B9	B="BEmBFtBEmBFYBGzBKxBK�BJ�BIlBGzBI�BL�BKxBI�BFtBFtBFtBH�BK�BN�BQ�BQ�BQ�BP�BP�BP�BP�BR�BS�BU�BW�BY�B[�B^B_B_B_BaB`BaBg8Bh$BkQBncBs�B}�B}�Bs�By�B�B��B��B��B�.B�MB��B��B��B�B��B�B�MB�YBȀB�xB̘BΊBϫBѷB��B��B��B��B��B�B�B�QB�IB�oB�aB�B�nB�nB��B��B��B��B��B	�B	�B	�B	�B	�B	
	B	
�B	"B	(B	B	,B	YB	KB	kB	kB	xB	jB	pB	"�B	&�B	+�B	2�B	5�B	:*B	="B	>(B	CGB	FYB	J�B	N�B	O�B	R�B	U�B	X�B	Z�B	^B	^�B	`B	a�B	fB	h>B	k6B	qvB	shB	shB	t�B	tnB	t�B	utB	w�B	y�B	{�B	~�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�(B	�B	�@B	�2B	�SB	�YB	�KB	�]B	�pB	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�aB	�SB	�tB	ȀB	�xB	͟B	͟B	ΊB	̈́B	ΊB	ϑB	бB	ϑB	ЗB	ѝB	ңB	��B	өB	��B	ּB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	�B	��B	��B	� B	� B	�B	�B	� B	�B	�B	�B	�B	�8B	�$B	�*B	�0B	�6B	�WB	�CB	�]B	�=B	�]B	�cB	�IB	�OB	�OB	�UB	�aB	�aB	�aB	�B	��B	�zB	�zB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

	B

	B
	�B
	�B

�B

�B
B
�B
�B
B
B
B
(B
B
B
B
.B
B
B
.B
4B
4B
4B
 B
&B
@B
,B
,B
FB
MB
2B
MB
MB
2B
2B
2B
MB
SB
9B
YB
YB
9B
9B
2B
2B
SB
SB
9B
9B
?B
_B
_B
eB
KB
KB
KB
kB
QB
qB
WB
qB
WB
]B
xB
]B
]B
dB
dB
dB
jB
jB
�B
�B
�B
pB
jB
jB
jB
�B
�B
pB
pB
pB
�B
 �B
 �B
 vB
!|B
!|B
!|B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
*�B
*�B
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
-�B
-�B
.�B
.�B
.�B
.�B
.�B
.�B
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
1�B
2�B
2�B
3�B
3�B
4B
4B
3�B
4B
4�B
6B
5�B
4�B
5�B
5�B
5�B
6�B
8B
8B
8B
8B
8B
8B
8B
8B
8B
9	B
9	B
:B
:*B
:*B
:*B
;B
<B
=<B
=<B
="B
="B
="B
>(B
>(B
>(B
>(B
>BB
>(B
>BB
>(B
>BB
>(B
?.B
?.B
?.B
@OB
@4B
AUB
A;B
B[B
AUB
AUB
B[B
BAB
CGB
CaB
CGB
CaB
CaB
DgB
CaB
CaB
ESB
ESB
EmB
FYB
FYB
FYB
FtB
G_B
G_B
G_B
GzB
G_B
G_B
GzB
G_B
H�B
H�B
H�B
IlB
IlB
J�B
J�B
JrB
K�B
KxB
KxB
KxB
K�B
L~B
L�B
L~B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
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
R�B
R�B
R�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
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
W�B
W�B
W�B
X�B
X�B
X�B
X�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Z�B
Z�B
Z�B
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
\�B
\�B
\�B
^B
^B
]�B
^B
^B
^�B
^�B
^�B
`B
`B
`B
_�B
`B
`�B
`B
aB
`�B
aB
`�B
aB
a�B
bB
bB
bB
bB
bB
a�B
c B
c B
cB
cB
dB
d&B
dB
dB
d&B
dB
dB
dB
d&B
e,B
eB
e,B
eB
e,B
eB
e,B
eB
e,B
e,B
f2B
f2B
fB
fB
g8B
g8B
h$B
h$B
h$B
h$B
h$B
h$B
h$B
h$B
iDB
i*B
iDB
i*B
i*B
i*B
i*B
iDB
jKB
jKB
j0B
jKB
j0B
jKB
k6B
kQB
k6B
kQB
k6B
lWB
l=B
lWB
lWB
l=B
lWB
mCB
mCB
mC11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.52(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201808250042102018082500421020180825004210201808260041142018082600411420180826004114JA  ARFMdecpA19c                                                                20180820123510  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180820033511  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180820033512  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180820033512  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180820033513  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180820033513  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180820033513  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180820033513  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180820033513  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180820033514                      G�O�G�O�G�O�                JA  ARUP                                                                        20180820035512                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180820154128  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20180824154210  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180824154210  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180825154114  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021522                      G�O�G�O�G�O�                