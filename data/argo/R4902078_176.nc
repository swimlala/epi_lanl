CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-02-20T11:00:35Z creation      
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
resolution        =���   axis      Z        h  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  _�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  o`   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  �P   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �p   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �p   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �p   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �p   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20200220110035  20200220110035  4902078 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  5439                            2B  A   NAVIS_A                         0460                            011514                          863 @�X�ֵl1   @�[b���@+yXbM��dg
=p��1   GPS     Primary sampling: averaged []                                                                                                                                                                                                                                      �A   A   A   @@  @�  @�  A   A!��AA��A`  A�  A���A�ffA�33A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�ffB�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�fC  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CG�fCJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D(��D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW�fDX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�3D�C3D�l�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @aG�@���@У�AQ�A)�AI�AhQ�A�(�A�A��\A�\)A�(�A�(�A�(�A�(�B{B
{B{B{B"{B*{B2{B:{BB{BJ{BR{BZ{Bb{Bj{Br{Bz{B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�p�B�=pB�
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
�C�C�Ck�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CHk�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�5�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�D !HD �HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD	!HD	�HD
!HD
�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD�D�HD!HD�HD!HD�HD!HD�HD!HD��D!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD !HD �HD!!HD!�HD"!HD"�HD#!HD#�HD$!HD$�HD%!HD%�HD&!HD&�HD'!HD'�HD(!HD(�HD)�D)�HD*!HD*�HD+!HD+�HD,!HD,�HD-!HD-�HD.!HD.�HD/!HD/�HD0!HD0�HD1!HD1�HD2!HD2�HD3!HD3�HD4!HD4�HD5!HD5�HD6!HD6�HD7!HD7�HD8!HD8�HD9!HD9�HD:!HD:�HD;!HD;�HD<!HD<�HD=!HD=�HD>!HD>�HD?!HD?�HD@!HD@�HDA!HDA�HDB!HDB�HDC!HDC�HDD!HDD�HDE!HDE�HDF!HDF�HDG!HDG�HDH!HDH�HDI!HDI�HDJ!HDJ�HDK!HDK�HDL!HDL�HDM!HDM�HDN!HDN�HDO!HDO�HDP!HDP�HDQ!HDQ�HDR!HDR�HDS!HDS�HDT!HDT�HDU!HDU�HDV!HDV�HDW!HDW��DX!HDX�HDY!HDY�HDZ!HDZ�HD[!HD[�HD\!HD\�HD]!HD]�HD^!HD^�HD_!HD_�HD`!HD`�HDa!HDa�HDb!HDb�HDc!HDc�HDd!HDd�HDe!HDe�HDf!HDf�HDg!HDg�HDh!HDh�HDi!HDi�HDj!HDj�HDk!HDk�HDl!HDl�HDm!HDm�HDn!HDn�HDo!HDo�HDp!HDp�HDq!HDq�HDr!HDr�HDs!HDs�HDt!HDt�HDu!HDu�HDv!HDv�HDw!HDw�HDx!HDx�HDy!HDy�HDz!HDz�HD{!HD{�HD|!HD|�HD}!HD}�HD~!HD~�HD!HD�HD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�MqD���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D��qD�ФD��D�P�D���D�ФD��D�P�D�D�ФD��D�P�DÐ�D�ФD��D�P�DĐ�D�ФD��D�P�DŐ�D�ФD��D�P�DƐ�D�ФD��D�P�Dǐ�D�ФD��D�P�DȐ�D�ФD��D�P�Dɐ�D�ФD��D�P�Dʐ�D�ФD��D�P�Dː�D�ФD��D�P�D̐�D�ФD��D�P�D͐�D�ФD��D�P�Dΐ�D�ФD��D�P�Dϐ�D�ФD��D�P�DА�D�ФD��D�P�Dѐ�D�ФD��D�P�DҐ�D�ФD��D�P�DӐ�D�ФD��D�P�DԐ�D�ФD��D�P�DՐ�D�ФD��D�P�D֐�D�ФD��D�P�Dא�D�ФD��D�P�Dؐ�D�ФD��D�P�Dِ�D�ФD��D�P�Dڐ�D�ФD��D�P�Dې�D�ФD��D�P�Dܐ�D�ФD��D�P�Dݐ�D�ФD��D�P�Dސ�D�ФD��D�P�Dߐ�D�ФD��D�P�D���D�ФD��D�P�DᐤD�ФD��D�P�D␤D�ФD��D�P�D㐤D�ФD��D�P�D䐤D�ФD��D�P�D吤D�ФD��D�P�D搤D�ФD��D�P�D琤D�ФD��D�P�D萤D�ФD��D�P�D鐤D�ФD��D�P�DꐤD�ФD��D�P�D됤D�ФD��D�P�D쐤D�ФD��D�P�D퐤D�ФD��D�P�DD�ФD��D�P�DD�ФD��D�P�D�D�ФD��D�P�D�D�ФD��D�P�D�D�ФD��D�P�D�D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�S�D�}q11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AϺ^AϸRAϼjAϾwA���A�A�A�A�ƨA�ȴA���A�ȴA�ȴA�ȴA���A���A�ȴA�ȴA���A��
A���A��A��/A��/A��/A��;A��;A��HA��HA��TA��TA��TA��`A��TA��yA��yA��yA��#A���A�%A�A�r�A�I�A���A��yA��DA���A�1'A�9XA�/A��^A��jA��TA�A�5?A��hA�bA���A�33A��jA�VA���A�~�A�+A��+A���A���A�x�A��-A���A���A��A�VA�z�A��DA�n�A�=qA�9XAzZAv�9Aq33Al�yAj=qAh5?Af��Ae;dAahsA[��AY|�AX�\AW�AV1'AS�AM��AJĜAJ  AH��AF�uAF{AC�
AB=qA@�9A>�HA=��A=�wA;��A:~�A5p�A2ZA2�\A3�A2ĜA2JA0�A.�`A.ZA-��A+�A+�;A+�PA+G�A+�A*�/A*��A*E�A)��A)A'��A&�9A&1A%&�A$9XA#ƨA"�/A"��A"JA!�PA!oA z�A"�A�!AffA �AA��A�;A33A��A��A��A�!A�RAA`BA\)A�A��A�!AbNA��A�7A�A��A��AĜAĜA��A��AjA�A��AdZAG�A/A��A��A�#AG�A~�A��A33A�+AAt�A33A
=A�A�!Ar�AI�A��A�A�A%A-A��A`BA�uA�A�7A�A��A;dA��A1A��A�PAl�A7LA
�A
ĜA
n�A	�-A	dZA	G�A	�A	�A��A��A�PAoA��Av�Ar�A�+Av�A5?A��At�A33AĜA~�AbNA{A�TA�^AS�A7LA+A��A�\AA�A�AAA\)AoA �`A (�@�dZ@�+@��R@�=q@���@�7L@���@�j@���@��y@�G�@��u@�1'@���@�|�@�n�@��-@�O�@���@���@��H@�v�@�@�@�/@��`@�I�@�
=@�ff@�J@���@�p�@�j@�@ꗍ@��@�@�&�@���@�I�@��
@�ƨ@�\)@�+@�7@�@�(�@�dZ@�!@�\@�n�@��@��@��u@�(�@��@�ƨ@߅@�33@�ff@�@��@�x�@�%@ܴ9@�z�@���@�;d@�ff@�{@��@�O�@���@��@�j@�M�@�hs@�X@�O�@�7L@ԓu@җ�@�5?@�@���@�V@���@ͩ�@�V@̼j@�r�@�I�@�t�@ʰ!@�5?@�@�&�@ȣ�@��m@��@�^5@�`B@Ĵ9@�b@Ý�@��@���@�ff@��T@��@��@��/@��@��F@�t�@�o@��7@�X@���@�z�@�A�@�  @��F@�S�@�n�@��T@�p�@�V@���@���@�Q�@�b@��w@��!@�-@���@�?}@�&�@��@���@��j@�j@��@��@��@��@���@���@�n�@�E�@��@�x�@�%@�z�@�1@���@�S�@��@���@��+@�v�@�ff@�5?@��@���@���@�`B@�&�@��@�I�@�33@��^@�hs@�7L@���@��@�%@���@��@��@��@��F@�"�@��y@�ȴ@��\@�$�@�`B@�7L@��/@�I�@��m@���@�"�@�V@���@��^@�hs@�%@���@��@�r�@��m@��P@�"�@��H@��R@���@���@�J@�&�@���@�r�@�Q�@�1'@��@���@���@�l�@�;d@�
=@�~�@��@��@��-@�p�@��`@��@��D@��@��w@�S�@�o@���@���@��+@�E�@�5?@�J@��T@�p�@�7L@�V@��/@��@�j@�(�@��m@��w@��@���@�K�@�"�@��@��@���@��\@�^5@�E�@�{@�@��@���@��^@���@��h@�hs@�X@�?}@��@���@�Z@�  @���@��P@�S�@�
=@���@�~�@�ff@�E�@��@���@���@��h@�hs@��@�Ĝ@���@�r�@�I�@�1@���@��P@�C�@�"�@��@�@��@��R@�M�@�@�X@�V@���@� �@��
@�l�@�
=@��y@��@���@���@�~�@�^5@�E�@�@��@�p�@��@���@�Ĝ@���@��D@��@�j@�I�@�b@��w@��w@�l�@��@�~�@�E�@�@�@���@��@�x�@�O�@��/@�z�@�;@~��@~v�@~$�@~@}�-@}?}@|��@|z�@{dZ@z��@y�@x  @w�w@wK�@v�y@vv�@vV@v{@up�@t��@tZ@t1@s�@s33@r�@q�^@qhs@p��@pbN@pb@o�P@nff@m��@m�T@mV@mV@l��@l��@l�@k��@kS�@j�@jn�@jM�@j=q@j�@j�@j�@j�@j-@i�@i�#@i��@i�7@h��@h  @g�;@g��@g�P@g�@f�@f��@f5?@e�@e�@e`B@e�@e��@e�@d��@cƨ@c@b��@bM�@a�^@aX@`�u@_�@_��@_;d@^�y@^��@^$�@]p�@\��@\Z@[�@["�@Z��@Zn�@ZJ@YX@XĜ@XA�@Xb@W��@Wl�@W
=@Vv�@V$�@V$�@U�@U��@U?}@U�@T�@T�@T��@TI�@S�
@S�F@SC�@R�H@R=q@Q��@Q7L@P�`@P��@P�@O�;@OK�@N��@N��@NV@N@M�@M��@M@M�@M�@L��@L��@L�@Kƨ@K��@Kt�@KC�@K"�@J�H@J��@J�@I��@I�@HĜ@HA�@G�;@G�P@G\)@F�@Fv�@F5?@F@E�T@E��@E�-@E�h@EO�@EV@D�@D1@Cƨ@C�F@C33@B��@B-@A��@A��@A��@AG�@A%@@��@@�u@@Q�@@A�@?�;@?l�@?
=@>�R@>V@=�@=��@=��@=/@<�@<�j@<Z@;�
@;��@;dZ@;@:^5@:J@9��@9X@9&�@9%@8Ĝ@8r�@8 �@7�@7�@6�y@6�R@6��@6V@6{@5�T@5p�@5�@4�j@4�@3�m@3�m@3ƨ@3�F@3��@333@2�@2�!@2�\@2M�@1��@1G�@1&�@0�9@0b@/�@/�P@/\)@/
=@.��@.ff@.ff@.@-��@-��@-�h@-�@-`B@-/@,��@,��@,�@,�D@,z�@,I�@,�@+�m@+��@+dZ@+C�@+"�@+@*��@*�\@*=q@)�#@)�^@)��@)x�@)G�@)�@(��@(�u@(r�@(A�@(b@'�@'�@'��@';d@&�R@&v�@&5?@%�T@%�-@%��@%�@%O�@%�@$�/@$�@$�D@$Z@$I�@$9X@$�@#��@#�F@#�F@#��@#��@#�@#t�@#C�@"��@"~�@!�#@!X@ ��@ �@�@��@�P@|�@;d@
=@�y@ȴ@�+@$�@{@@�@/@��@�j@��@z�@Z@1@�F@t�@S�@o@��@�!@��@M�@-@��@��@X@&�@�`@Ĝ@��@r�@Q�@1'@  @�@l�@K�@K�@;d@�y@��@v�@V@@��@�-@O�@�@�j@��@�D@�D@�D@z�@�@��@�@dZ@C�@33@"�@@�H@��@~�@^5@-@�#@��@�^@�7@7L@�@Ĝ@�u@�@bN@1'@  @�@�;@�@��@|�@K�@
=@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AϺ^AϸRAϼjAϾwA���A�A�A�A�ƨA�ȴA���A�ȴA�ȴA�ȴA���A���A�ȴA�ȴA���A��
A���A��A��/A��/A��/A��;A��;A��HA��HA��TA��TA��TA��`A��TA��yA��yA��yA��#A���A�%A�A�r�A�I�A���A��yA��DA���A�1'A�9XA�/A��^A��jA��TA�A�5?A��hA�bA���A�33A��jA�VA���A�~�A�+A��+A���A���A�x�A��-A���A���A��A�VA�z�A��DA�n�A�=qA�9XAzZAv�9Aq33Al�yAj=qAh5?Af��Ae;dAahsA[��AY|�AX�\AW�AV1'AS�AM��AJĜAJ  AH��AF�uAF{AC�
AB=qA@�9A>�HA=��A=�wA;��A:~�A5p�A2ZA2�\A3�A2ĜA2JA0�A.�`A.ZA-��A+�A+�;A+�PA+G�A+�A*�/A*��A*E�A)��A)A'��A&�9A&1A%&�A$9XA#ƨA"�/A"��A"JA!�PA!oA z�A"�A�!AffA �AA��A�;A33A��A��A��A�!A�RAA`BA\)A�A��A�!AbNA��A�7A�A��A��AĜAĜA��A��AjA�A��AdZAG�A/A��A��A�#AG�A~�A��A33A�+AAt�A33A
=A�A�!Ar�AI�A��A�A�A%A-A��A`BA�uA�A�7A�A��A;dA��A1A��A�PAl�A7LA
�A
ĜA
n�A	�-A	dZA	G�A	�A	�A��A��A�PAoA��Av�Ar�A�+Av�A5?A��At�A33AĜA~�AbNA{A�TA�^AS�A7LA+A��A�\AA�A�AAA\)AoA �`A (�@�dZ@�+@��R@�=q@���@�7L@���@�j@���@��y@�G�@��u@�1'@���@�|�@�n�@��-@�O�@���@���@��H@�v�@�@�@�/@��`@�I�@�
=@�ff@�J@���@�p�@�j@�@ꗍ@��@�@�&�@���@�I�@��
@�ƨ@�\)@�+@�7@�@�(�@�dZ@�!@�\@�n�@��@��@��u@�(�@��@�ƨ@߅@�33@�ff@�@��@�x�@�%@ܴ9@�z�@���@�;d@�ff@�{@��@�O�@���@��@�j@�M�@�hs@�X@�O�@�7L@ԓu@җ�@�5?@�@���@�V@���@ͩ�@�V@̼j@�r�@�I�@�t�@ʰ!@�5?@�@�&�@ȣ�@��m@��@�^5@�`B@Ĵ9@�b@Ý�@��@���@�ff@��T@��@��@��/@��@��F@�t�@�o@��7@�X@���@�z�@�A�@�  @��F@�S�@�n�@��T@�p�@�V@���@���@�Q�@�b@��w@��!@�-@���@�?}@�&�@��@���@��j@�j@��@��@��@��@���@���@�n�@�E�@��@�x�@�%@�z�@�1@���@�S�@��@���@��+@�v�@�ff@�5?@��@���@���@�`B@�&�@��@�I�@�33@��^@�hs@�7L@���@��@�%@���@��@��@��@��F@�"�@��y@�ȴ@��\@�$�@�`B@�7L@��/@�I�@��m@���@�"�@�V@���@��^@�hs@�%@���@��@�r�@��m@��P@�"�@��H@��R@���@���@�J@�&�@���@�r�@�Q�@�1'@��@���@���@�l�@�;d@�
=@�~�@��@��@��-@�p�@��`@��@��D@��@��w@�S�@�o@���@���@��+@�E�@�5?@�J@��T@�p�@�7L@�V@��/@��@�j@�(�@��m@��w@��@���@�K�@�"�@��@��@���@��\@�^5@�E�@�{@�@��@���@��^@���@��h@�hs@�X@�?}@��@���@�Z@�  @���@��P@�S�@�
=@���@�~�@�ff@�E�@��@���@���@��h@�hs@��@�Ĝ@���@�r�@�I�@�1@���@��P@�C�@�"�@��@�@��@��R@�M�@�@�X@�V@���@� �@��
@�l�@�
=@��y@��@���@���@�~�@�^5@�E�@�@��@�p�@��@���@�Ĝ@���@��D@��@�j@�I�@�b@��w@��w@�l�@��@�~�@�E�@�@�@���@��@�x�@�O�@��/@�z�@�;@~��@~v�@~$�@~@}�-@}?}@|��@|z�@{dZ@z��@y�@x  @w�w@wK�@v�y@vv�@vV@v{@up�@t��@tZ@t1@s�@s33@r�@q�^@qhs@p��@pbN@pb@o�P@nff@m��@m�T@mV@mV@l��@l��@l�@k��@kS�@j�@jn�@jM�@j=q@j�@j�@j�@j�@j-@i�@i�#@i��@i�7@h��@h  @g�;@g��@g�P@g�@f�@f��@f5?@e�@e�@e`B@e�@e��@e�@d��@cƨ@c@b��@bM�@a�^@aX@`�u@_�@_��@_;d@^�y@^��@^$�@]p�@\��@\Z@[�@["�@Z��@Zn�@ZJ@YX@XĜ@XA�@Xb@W��@Wl�@W
=@Vv�@V$�@V$�@U�@U��@U?}@U�@T�@T�@T��@TI�@S�
@S�F@SC�@R�H@R=q@Q��@Q7L@P�`@P��@P�@O�;@OK�@N��@N��@NV@N@M�@M��@M@M�@M�@L��@L��@L�@Kƨ@K��@Kt�@KC�@K"�@J�H@J��@J�@I��@I�@HĜ@HA�@G�;@G�P@G\)@F�@Fv�@F5?@F@E�T@E��@E�-@E�h@EO�@EV@D�@D1@Cƨ@C�F@C33@B��@B-@A��@A��@A��@AG�@A%@@��@@�u@@Q�@@A�@?�;@?l�@?
=@>�R@>V@=�@=��@=��@=/@<�@<�j@<Z@;�
@;��@;dZ@;@:^5@:J@9��@9X@9&�@9%@8Ĝ@8r�@8 �@7�@7�@6�y@6�R@6��@6V@6{@5�T@5p�@5�@4�j@4�@3�m@3�m@3ƨ@3�F@3��@333@2�@2�!@2�\@2M�@1��@1G�@1&�@0�9@0b@/�@/�P@/\)@/
=@.��@.ff@.ff@.@-��@-��@-�h@-�@-`B@-/@,��@,��@,�@,�D@,z�@,I�@,�@+�m@+��@+dZ@+C�@+"�@+@*��@*�\@*=q@)�#@)�^@)��@)x�@)G�@)�@(��@(�u@(r�@(A�@(b@'�@'�@'��@';d@&�R@&v�@&5?@%�T@%�-@%��@%�@%O�@%�@$�/@$�@$�D@$Z@$I�@$9X@$�@#��@#�F@#�F@#��@#��@#�@#t�@#C�@"��@"~�@!�#@!X@ ��@ �@�@��@�P@|�@;d@
=@�y@ȴ@�+@$�@{@@�@/@��@�j@��@z�@Z@1@�F@t�@S�@o@��@�!@��@M�@-@��@��@X@&�@�`@Ĝ@��@r�@Q�@1'@  @�@l�@K�@K�@;d@�y@��@v�@V@@��@�-@O�@�@�j@��@�D@�D@�D@z�@�@��@�@dZ@C�@33@"�@@�H@��@~�@^5@-@�#@��@�^@�7@7L@�@Ĝ@�u@�@bN@1'@  @�@�;@�@��@|�@K�@
=@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�-B�-B�-B�-B�-B�-B�-B�-B�-B�-B�-B�-B�-B�-B�-B�-B�-B�-B�-B�-B�-B�-B�-B�3B�-B�3B�3B�3B�3B�3B�3B�3B�3B�3B�3B�3B�3B�LB�jB�B
I�B
�jBr�B��B��B�B�!BĜB�XB�B��B��B�wB��B��B�PBr�BK�B,B$�B�B\BVB%B  B(�BH�B8RB'�B
��B
�B
ƨB
��B
��B
�{B
�%B
t�B
L�B
 �B
  B	�B	�XB	��B	�uB	�+B	|�B	e`B	G�B	9XB	5?B	33B	-B	,B	�B	B	B	�B	&�B	N�B	t�B	�B	��B	��B	��B	�'B	�9B	�B	�\B	�B	��B	�dB	ĜB	��B	��B	��B	�BB	�mB
1B
\B
�B
!�B
"�B
%�B
+B
/B
/B
-B
/B
)�B
)�B
0!B
,B
/B
33B
;dB
9XB
7LB
:^B
>wB
B�B
D�B
C�B
D�B
E�B
O�B
]/B
[#B
XB
W
B
YB
\)B
_;B
hsB
p�B
q�B
q�B
r�B
r�B
r�B
r�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
p�B
p�B
o�B
n�B
n�B
m�B
k�B
iyB
ffB
e`B
bNB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
cTB
dZB
dZB
dZB
cTB
cTB
cTB
bNB
`BB
]/B
XB
T�B
R�B
T�B
T�B
S�B
R�B
R�B
R�B
S�B
S�B
R�B
P�B
O�B
M�B
J�B
H�B
G�B
G�B
L�B
M�B
K�B
E�B
B�B
A�B
A�B
D�B
L�B
N�B
N�B
M�B
L�B
K�B
K�B
K�B
K�B
J�B
I�B
I�B
I�B
I�B
I�B
H�B
H�B
G�B
G�B
G�B
G�B
G�B
F�B
F�B
E�B
D�B
D�B
C�B
C�B
C�B
B�B
A�B
A�B
@�B
?}B
>wB
>wB
=qB
=qB
<jB
<jB
;dB
;dB
:^B
9XB
9XB
9XB
8RB
8RB
8RB
7LB
6FB
6FB
5?B
5?B
49B
49B
33B
33B
2-B
2-B
1'B
1'B
1'B
1'B
0!B
0!B
/B
.B
.B
-B
,B
,B
+B
)�B
)�B
+B
)�B
)�B
(�B
(�B
'�B
(�B
'�B
'�B
&�B
&�B
&�B
&�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
$�B
$�B
#�B
"�B
!�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
{B
�B
�B
�B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
'�B
(�B
(�B
(�B
(�B
(�B
+B
)�B
+B
+B
+B
+B
,B
-B
-B
-B
-B
.B
.B
.B
.B
.B
/B
/B
/B
/B
/B
/B
/B
0!B
1'B
1'B
1'B
1'B
1'B
2-B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
33B
33B
33B
33B
33B
33B
33B
49B
49B
49B
49B
49B
49B
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
49B
49B
49B
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
8RB
8RB
8RB
9XB
9XB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
;dB
<jB
<jB
<jB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
A�B
@�B
A�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
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
G�B
H�B
H�B
H�B
G�B
G�B
G�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
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
M�B
M�B
N�B
M�B
N�B
P�B
O�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
VB
VB
VB
W
B
W
B
VB
VB
VB
T�B
T�B
T�B
T�B
VB
VB
VB
W
B
W
B
XB
XB
XB
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
ZB
ZB
ZB
ZB
[#B
[#B
]/B
\)B
\)B
]/B
]/B
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
aHB
aHB
aHB
aHB
bNB
bNB
bNB
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
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
iyB
iyB
jB
jB
jB
jB
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
l�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
s�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
y�B
y�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
� B
�B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�7B
�7B
�=B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
�=B
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�JB
�JB
�JB
�JB
�JB
�JB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�VB
�VB
�\B
�\B
�bB
�b11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�4B
9oB
�BbeB�[B��B��B��B�QB�B��B��B��B�,BĳB�sB}BbeB;|B�B�BHB
�B
�B
��B
�B�B8iB(B�B
�B
��B
�]B
��B
�sB
�0B
u�B
dqB
<�B
zB	�B	��B	�B	��B	�*B	v�B	l�B	UB	7cB	)B	$�B	"�B	�B	�B		OB��B��B	6B	�B	>�B	dqB	s�B	�BB	�zB	��B	��B	��B	��B	B	s�B	�BB	�B	�QB	��B	��B	íB	��B	�"B	��B	�B
aB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
+B
)B
'B
*B
.,B
2DB
4QB
3KB
4QB
5WB
?�B
L�B
J�B
G�B
F�B
H�B
K�B
N�B
X(B
`YB
a_B
a_B
beB
beB
beB
beB
a_B
a_B
a_B
a_B
a_B
a_B
a_B
a_B
a_B
`YB
`YB
_SB
^MB
^MB
]FB
[:B
Y.B
VB
UB
RB
P�B
P�B
P�B
P�B
RB
RB
RB
S	B
TB
TB
TB
S	B
S	B
S	B
RB
O�B
L�B
G�B
D�B
B�B
D�B
D�B
C�B
B�B
B�B
B�B
C�B
C�B
B�B
@�B
?�B
=�B
:vB
8iB
7cB
7cB
<�B
=�B
;|B
5WB
2DB
1>B
1>B
4QB
<�B
>�B
>�B
=�B
<�B
;|B
;|B
;|B
;|B
:vB
9oB
9oB
9oB
9oB
9oB
8iB
8iB
7cB
7cB
7cB
7cB
7cB
6]B
6]B
5WB
4QB
4QB
3KB
3KB
3KB
2DB
1>B
1>B
08B
/2B
.,B
.,B
-&B
-&B
,B
,B
+B
+B
*B
)B
)B
)B
(B
(B
(B
'B
%�B
%�B
$�B
$�B
#�B
#�B
"�B
"�B
!�B
!�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
zB
zB
sB
mB
mB
gB
gB
aB
[B
[B

UB

UB
	OB
	OB
HB
HB
BB
<B
<B
6B
6B
0B
0B
6B
6B
6B
0B
0B
0B
6B
6B
<B
BB
HB
BB
BB
HB
HB
BB
BB
BB
BB
BB
BB
BB
BB
BB
HB
HB
HB
	OB
HB
	OB
	OB
HB
HB
	OB
	OB

UB
[B
[B
[B
[B
[B
[B
aB
aB
aB
aB
gB
aB
aB
aB
aB
aB
aB
gB
gB
gB
mB
mB
mB
sB
sB
sB
sB
sB
zB
sB
zB
zB
zB
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
"�B
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
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
#�B
#�B
#�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
'B
'B
'B
(B
(B
(B
)B
)B
(B
)B
)B
)B
)B
)B
)B
)B
)B
*B
*B
*B
*B
*B
*B
*B
*B
*B
*B
*B
*B
*B
+B
,B
,B
,B
-&B
-&B
-&B
.,B
.,B
.,B
.,B
.,B
.,B
/2B
/2B
1>B
08B
1>B
2DB
2DB
2DB
2DB
2DB
3KB
3KB
3KB
3KB
4QB
4QB
4QB
5WB
5WB
5WB
6]B
6]B
6]B
6]B
7cB
7cB
7cB
7cB
7cB
7cB
7cB
7cB
7cB
8iB
8iB
8iB
7cB
7cB
7cB
8iB
9oB
9oB
9oB
9oB
:vB
:vB
:vB
:vB
:vB
;|B
;|B
;|B
;|B
<�B
<�B
<�B
<�B
=�B
=�B
>�B
=�B
>�B
@�B
?�B
@�B
A�B
A�B
A�B
A�B
A�B
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
C�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
F�B
F�B
E�B
E�B
E�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
L�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
P�B
P�B
P�B
P�B
RB
RB
RB
S	B
S	B
S	B
TB
TB
TB
TB
TB
TB
TB
TB
TB
UB
UB
UB
UB
UB
VB
VB
VB
VB
VB
VB
VB
VB
W"B
W"B
W"B
W"B
W"B
W"B
W"B
W"B
W"B
W"B
W"B
X(B
X(B
X(B
Y.B
Y.B
Z4B
Z4B
Z4B
Z4B
Z4B
Z4B
Z4B
Z4B
Z4B
Z4B
[:B
[:B
[:B
[:B
[:B
\@B
\@B
\@B
\@B
\@B
\@B
\@B
]FB
]FB
]FB
]FB
]FB
]FB
^MB
^MB
^MB
^MB
^MB
^MB
^MB
_SB
_SB
_SB
`YB
`YB
`YB
`YB
`YB
`YB
`YB
a_B
a_B
a_B
a_B
a_B
a_B
a_B
beB
ckB
ckB
ckB
ckB
ckB
ckB
dqB
ckB
dqB
dqB
dqB
dqB
exB
exB
exB
exB
exB
f~B
f~B
f~B
f~B
f~B
f~B
f~B
f~B
f~B
g�B
g�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
j�B
j�B
j�B
i�B
i�B
j�B
j�B
j�B
j�B
k�B
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
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
o�B
p�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
x�B
x�B
y�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
}B
}B
}B
}B
}B
}B
}B
}B
~B
~B
B
B
�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      surface_pressure=-0.52 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     salinity_offset = -0.0159092                                                                                                                                                                                                                                    Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                      PSAL ADJUST [dd mm yyyy N S_off stddev] 15 06 2019 151 -0.0159092 0.0003 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                        20200220110035              20200220110035  AO  ARCAADJP                                                                    20200220110035    IP                G�O�G�O�G�O�                AO  ARCAADJS                                                                    20200220110035    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200220110035  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200220110035  QCF$                G�O�G�O�G�O�0               