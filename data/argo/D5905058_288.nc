CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2021-01-11T21:39:35Z creation;2021-01-11T21:39:38Z conversion to V3.1;2023-06-29T05:47:31Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7,   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7<   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7@   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7D   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7T   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7d   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7t   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7|   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8,   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    80   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    84   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     88   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8X   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8\   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8`   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        X  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I\   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  M4   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  `d   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �L   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  �$   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  �|   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �,   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ۼ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210111213935  20230705041504  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  I2_0675_288                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @�U�ƻZ�1   @�U��O� @6W
=p���b��{��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @333@�  @�  A   A   A@  A`  A�  A�33A�33A�  A�  A�  A�  A�  B   B��B  B  B   B(ffB0  B8  B?��BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D���D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D���D�<�D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�C3D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @Tz�@���@У�AQ�A(Q�AHQ�AhQ�A�(�A�\)A�\)A�(�A�(�A�(�A�(�A�(�B{B	�B{B{B"{B*z�B2{B:{BA�BJ{BR{BZ{Bb{Bj{Br{Bz{B�
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
=B��
B�
=B�
=C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�O\C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�O\C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�D !HD �HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD	!HD	�HD
!HD
�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD'�D�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD !HD �HD!!HD!�HD"!HD"�HD#!HD#�HD$!HD$�HD%!HD%�HD&!HD&�HD'!HD'�HD(!HD(�HD)!HD)�HD*!HD*�HD+!HD+�HD,!HD,�HD-!HD-�HD.!HD.�HD/!HD/�HD0!HD0�HD1!HD1�HD2!HD2�HD3!HD3�HD4!HD4�HD5!HD5�HD6!HD6�HD7!HD7�HD8!HD8�HD9!HD9�HD:!HD:�HD;!HD;�HD<!HD<�HD=!HD=�HD>!HD>�HD?!HD?�HD@!HD@�HDA!HDA�HDB!HDB�HDC!HDC�HDD!HDD�HDE!HDE�HDF!HDF�HDG!HDG�HDH!HDH�HDI!HDI�HDJ!HDJ�HDK!HDK�HDL!HDL�HDM!HDM�HDN!HDN�HDO!HDO�HDP!HDP�HDQ!HDQ�HDR!HDR�HDS!HDS�HDT!HDT�HDU!HDU�HDV!HDV�HDW!HDW�HDX!HDX�HDY!HDY�HDZ!HDZ�HD[!HD[�HD\!HD\�HD]!HD]�HD^!HD^�HD_!HD_�HD`!HD`�HDa!HDa�HDb!HDb�HDc!HDc�HDd!HDd�HDe!HDe�HDf!HDf�HDg!HDg�HDh!HDh�HDi!HDi�HDj!HDj�HDk!HDk�HDl!HDl�HDm!HDm�HDn!HDn�HDo!HDo�HDp!HDp�HDq!HDq�HDr!HDr�HDs!HDs�HDt!HDt��Du!HDu�HDv!HDv�HDw!HDw�HDx!HDx�HDy!HDy�HDz!HDz�HD{!HD{�HD|!HD|�HD}!HD}�HD~!HD~�HD!HD�HD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D���D��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D���D��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D���D�ФD��D�P�D�D�ФD��D�P�DÐ�D�ФD��D�P�DĐ�D�ФD��D�P�DŐ�D�ФD��D�P�DƐ�D�ФD��D�P�Dǐ�D�ФD��D�P�DȐ�D�ФD��D�P�Dɐ�D�ФD��D�P�Dʐ�D�ФD��D�P�Dː�D�ФD�qD�P�D̐�D�ФD��D�P�D͐�D�ФD��D�P�Dΐ�D�ФD��D�P�Dϐ�D�ФD��D�P�DА�D�ФD��D�P�Dѐ�D�ФD��D�P�DҐ�D�ФD��D�P�DӐ�D�ФD��D�P�DԐ�D�ФD��D�P�DՐ�D�ФD��D�P�D֐�D�ФD��D�P�Dא�D�ФD��D�P�Dؐ�D�ФD��D�P�Dِ�D�ФD��D�P�Dڐ�D�ФD��D�P�Dې�D�ФD��D�P�Dܐ�D�ФD�qD�MqDݐ�D�ФD��D�P�Dސ�D�ФD��D�P�Dߐ�D�ФD��D�P�D���D�ФD��D�P�DᐤD�ФD��D�P�D␤D�ФD��D�P�D㐤D�ФD��D�P�D䐤D�ФD��D�P�D吤D�ФD��D�P�D搤D�ФD��D�P�D琤D�ФD��D�P�D萤D�ФD��D�P�D鐤D�ФD��D�P�DꐤD�ФD��D�P�D됤D�ФD��D�P�D쐤D�ФD��D�P�D퐤D�ФD��D�P�DD�ФD��D�P�DD�ФD��D�P�D�D�ФD��D�P�D�D�ФD��D�P�D�D�ФD��D�P�D�D�ФD��D�P�D���D�ФD��D�S�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A���A���A���A���A��A��;A��;A��/A��HA��`A��mA��yA��A��yA��A��A��A��A��A��A��A��A��A��A��A��#A�^5A�bA���A��yA��#A���A���A�n�A��A���A�A¼jA� �A�JA���A��A�z�A�XA��A�S�A��TA�O�A��7A�|�A���A�ƨA�v�A��!A���A�;dA���A��A�t�A�p�A��A�  A�|�A��HA�t�A��FA���A��A��!A�JA� �A���A��HA�VA�ȴA��A���A���A��uA�A�A��A���A�  A��A�/A���A��A� �A��A��#A�x�A���A���A�\)A���A��
A���A��/A��jA�I�A��A���AoA~JA{�#Ax��At��Asx�Aq�AkAh(�AgAg/AfM�Ad�`Ab�Aa��A`z�A^��A[�mAZ$�AW�;AU�#AT�9AT(�AS�TAS�ASC�AR1AN�AMp�AL�9AJjAI%AE�TAEADȴAC�AA�wAA?}A?��A>��A<�jA;A9%A7�A6 �A49XA1�^A/�A.�uA-��A,1'A+G�A+
=A*�A*A)%A(��A'��A%�A%��A$�A$��A$bNA#S�A"A�A!p�A ��A�TA`BAQ�A|�AI�A&�AQ�A��Ax�AI�A �AA��A�FA��A�RA�A~�AAl�A~�A�Az�A�Ax�A�AI�A��A
�A	�A	x�A	33A��AVAI�A=qAVA^5AJA��A ��A �D@��w@���@��\@�x�@�I�@�S�@�5?@��@��@�{@�j@�R@�x�@�bN@�+@��T@��/@�bN@�|�@�E�@��@�@�hs@�z�@�l�@���@���@���@݁@���@ܣ�@ە�@�ȴ@١�@�z�@��@�^5@�z�@�"�@�-@У�@ϥ�@�33@̣�@˶F@ʏ\@ȃ@�K�@Ƨ�@�@þw@�=q@�x�@���@���@���@�x�@��@���@��\@�/@���@�1@�
=@�M�@�V@�=q@��@���@�`B@��9@�  @���@�;d@��R@�~�@�{@���@��@�%@�I�@���@��F@��@��@��\@�&�@��D@�r�@� �@�t�@��R@�~�@��@�/@��7@��-@�p�@�?}@���@�1'@��@�o@��y@���@�ȴ@�~�@�n�@�=q@��T@���@��@�j@�A�@���@��;@��w@�\)@�@���@��7@�x�@���@�I�@�t�@���@��R@��+@���@���@�/@��@�Ĝ@�1@��w@��@�t�@�\)@�+@��@���@�v�@�^5@�^5@�V@�M�@�@���@���@�X@�/@���@�Ĝ@��9@���@�z�@�A�@� �@���@��w@���@�|�@�t�@�l�@�S�@�+@��H@��+@�E�@��@��T@��^@���@�x�@�7L@��@��`@���@���@���@�(�@�  @��@��m@��w@��@�;d@�
=@���@�5?@�J@�@��@��#@���@�p�@�V@��9@���@��@�j@�Q�@�A�@�1'@�(�@��@���@��w@�l�@�+@�v�@��@�@��T@��-@���@�/@��@���@��9@�1'@���@�t�@�K�@�C�@�"�@�o@�@��y@��y@��H@���@���@���@��\@�E�@�$�@��@��#@�@��^@���@��h@��@�G�@��`@��D@��@��;@�ƨ@���@�|�@�t�@�\)@�"�@��H@��!@��\@�=q@��T@��^@��h@�`B@�G�@�&�@��@���@���@���@��@�1'@��@���@�ƨ@���@�l�@�dZ@�;d@�o@��y@���@��@��@��@�@��-@�&�@���@��@��@���@��u@��D@��@�bN@�(�@�@�P@
=@~5?@}�@}@}�-@|��@|I�@|1@{�
@{�F@{�@{33@z�!@z�@y��@yhs@yG�@y�@x��@xA�@x  @w�w@w��@w�P@wK�@w
=@w
=@v��@v�y@v�R@v5?@uO�@tZ@sS�@s33@r~�@q��@qhs@q7L@pĜ@pr�@o�;@ol�@n�@nff@nE�@m�T@m�h@mp�@m/@l�/@lZ@k�F@kS�@k"�@j~�@jJ@i�@i�@i�#@i��@i��@iX@hA�@h �@g��@g\)@fE�@e��@e��@e�@e?}@d�@d��@d�D@dj@d1@c33@b�!@b^5@b=q@a��@a��@a��@a�7@ahs@aX@a&�@a%@`��@`1'@_|�@_;d@_�@^ȴ@^$�@^@]��@]�@\�j@[��@["�@Z�!@Z�\@Z�\@Z^5@ZJ@Y�#@Y7L@X��@X��@X�@X�u@X�@Xr�@Xr�@Xr�@XA�@W��@W�@W��@W�@V�+@V@U�@UO�@UV@T�@Tz�@Tj@T1@SdZ@RM�@Q��@QG�@P��@P��@P��@P�@Pr�@PQ�@P  @O��@OK�@N�@N5?@N@M��@MO�@L��@L��@L�j@L�D@Lj@K��@K�
@Kƨ@Kt�@K@Jn�@JJ@I�#@I��@Ihs@I&�@H�`@Hr�@H1'@Hb@G�w@G��@Gl�@GK�@F��@Fȴ@F�R@F��@F��@F$�@F{@F{@F{@F@E��@E��@E�h@EO�@D��@D�j@Dz�@D(�@D1@C�@C@B��@B��@B��@B�\@BM�@B-@A��@A&�@@��@@�9@@r�@?�;@?\)@?;d@>�y@>�+@>�+@>�+@>v�@>V@>@=@=`B@<�@<�D@<1@;t�@;S�@:�H@:��@:~�@:M�@:�@9��@9�@9��@9��@9G�@8�u@8A�@8A�@8 �@8  @8  @7�@7�;@7�@7�P@7;d@6��@6�@6�R@6v�@6ff@6E�@5�@5@5��@5�h@5?}@5V@4�@4�@3��@3C�@333@3o@2��@2�\@2~�@2~�@2n�@2M�@2M�@2=q@2-@2J@1�#@1X@0��@0�9@0�u@0 �@0b@0b@/�@/��@/��@/\)@/+@.��@.ȴ@.��@.ff@.@-�h@-O�@,�@,��@,��@,9X@,�@+�m@+dZ@*�H@*��@*�\@*�@)��@)�#@)��@)��@)X@)�@(Ĝ@(��@(r�@(A�@( �@(  @'��@'\)@'
=@&�@&�R@&��@&�+@&V@&{@%�T@%��@%`B@%�@$��@$�D@$z�@$I�@$1@#�m@#�F@#��@#S�@#33@"�@"��@"��@"n�@"M�@"-@"�@"J@!��@!�#@!�^@!��@!x�@!&�@ �9@ �u@ r�@ 1'@ b@   @��@�w@+@ȴ@v�@E�@5?@�@�T@@��@�@p�@?}@V@�j@�@z�@�@�m@�@33@�H@�H@��@��@n�@�@�@�^@�7@G�@7L@&�@�@%@��@Ĝ@�9@Q�@��@��@|�@|�@|�@|�@\)@+@�@
=@
=@�@�+@�T@��@@�-@O�@�@V@�/@��@��@�@��@�D@j@Z@�@�
@�@dZ@S�@C�@33@o@�H@�\@=q@��@hs@7L@�@�9@r�@ �@�;@�@�@��@|�@+@�R@V@$�@{@{@{@{@@@/@/@�@�@�@�@V@�j@�D@j@I�@(�@�@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A���A���A���A���A��A��;A��;A��/A��HA��`A��mA��yA��A��yA��A��A��A��A��A��A��A��A��A��A��A��#A�^5A�bA���A��yA��#A���A���A�n�A��A���A�A¼jA� �A�JA���A��A�z�A�XA��A�S�A��TA�O�A��7A�|�A���A�ƨA�v�A��!A���A�;dA���A��A�t�A�p�A��A�  A�|�A��HA�t�A��FA���A��A��!A�JA� �A���A��HA�VA�ȴA��A���A���A��uA�A�A��A���A�  A��A�/A���A��A� �A��A��#A�x�A���A���A�\)A���A��
A���A��/A��jA�I�A��A���AoA~JA{�#Ax��At��Asx�Aq�AkAh(�AgAg/AfM�Ad�`Ab�Aa��A`z�A^��A[�mAZ$�AW�;AU�#AT�9AT(�AS�TAS�ASC�AR1AN�AMp�AL�9AJjAI%AE�TAEADȴAC�AA�wAA?}A?��A>��A<�jA;A9%A7�A6 �A49XA1�^A/�A.�uA-��A,1'A+G�A+
=A*�A*A)%A(��A'��A%�A%��A$�A$��A$bNA#S�A"A�A!p�A ��A�TA`BAQ�A|�AI�A&�AQ�A��Ax�AI�A �AA��A�FA��A�RA�A~�AAl�A~�A�Az�A�Ax�A�AI�A��A
�A	�A	x�A	33A��AVAI�A=qAVA^5AJA��A ��A �D@��w@���@��\@�x�@�I�@�S�@�5?@��@��@�{@�j@�R@�x�@�bN@�+@��T@��/@�bN@�|�@�E�@��@�@�hs@�z�@�l�@���@���@���@݁@���@ܣ�@ە�@�ȴ@١�@�z�@��@�^5@�z�@�"�@�-@У�@ϥ�@�33@̣�@˶F@ʏ\@ȃ@�K�@Ƨ�@�@þw@�=q@�x�@���@���@���@�x�@��@���@��\@�/@���@�1@�
=@�M�@�V@�=q@��@���@�`B@��9@�  @���@�;d@��R@�~�@�{@���@��@�%@�I�@���@��F@��@��@��\@�&�@��D@�r�@� �@�t�@��R@�~�@��@�/@��7@��-@�p�@�?}@���@�1'@��@�o@��y@���@�ȴ@�~�@�n�@�=q@��T@���@��@�j@�A�@���@��;@��w@�\)@�@���@��7@�x�@���@�I�@�t�@���@��R@��+@���@���@�/@��@�Ĝ@�1@��w@��@�t�@�\)@�+@��@���@�v�@�^5@�^5@�V@�M�@�@���@���@�X@�/@���@�Ĝ@��9@���@�z�@�A�@� �@���@��w@���@�|�@�t�@�l�@�S�@�+@��H@��+@�E�@��@��T@��^@���@�x�@�7L@��@��`@���@���@���@�(�@�  @��@��m@��w@��@�;d@�
=@���@�5?@�J@�@��@��#@���@�p�@�V@��9@���@��@�j@�Q�@�A�@�1'@�(�@��@���@��w@�l�@�+@�v�@��@�@��T@��-@���@�/@��@���@��9@�1'@���@�t�@�K�@�C�@�"�@�o@�@��y@��y@��H@���@���@���@��\@�E�@�$�@��@��#@�@��^@���@��h@��@�G�@��`@��D@��@��;@�ƨ@���@�|�@�t�@�\)@�"�@��H@��!@��\@�=q@��T@��^@��h@�`B@�G�@�&�@��@���@���@���@��@�1'@��@���@�ƨ@���@�l�@�dZ@�;d@�o@��y@���@��@��@��@�@��-@�&�@���@��@��@���@��u@��D@��@�bN@�(�@�@�P@
=@~5?@}�@}@}�-@|��@|I�@|1@{�
@{�F@{�@{33@z�!@z�@y��@yhs@yG�@y�@x��@xA�@x  @w�w@w��@w�P@wK�@w
=@w
=@v��@v�y@v�R@v5?@uO�@tZ@sS�@s33@r~�@q��@qhs@q7L@pĜ@pr�@o�;@ol�@n�@nff@nE�@m�T@m�h@mp�@m/@l�/@lZ@k�F@kS�@k"�@j~�@jJ@i�@i�@i�#@i��@i��@iX@hA�@h �@g��@g\)@fE�@e��@e��@e�@e?}@d�@d��@d�D@dj@d1@c33@b�!@b^5@b=q@a��@a��@a��@a�7@ahs@aX@a&�@a%@`��@`1'@_|�@_;d@_�@^ȴ@^$�@^@]��@]�@\�j@[��@["�@Z�!@Z�\@Z�\@Z^5@ZJ@Y�#@Y7L@X��@X��@X�@X�u@X�@Xr�@Xr�@Xr�@XA�@W��@W�@W��@W�@V�+@V@U�@UO�@UV@T�@Tz�@Tj@T1@SdZ@RM�@Q��@QG�@P��@P��@P��@P�@Pr�@PQ�@P  @O��@OK�@N�@N5?@N@M��@MO�@L��@L��@L�j@L�D@Lj@K��@K�
@Kƨ@Kt�@K@Jn�@JJ@I�#@I��@Ihs@I&�@H�`@Hr�@H1'@Hb@G�w@G��@Gl�@GK�@F��@Fȴ@F�R@F��@F��@F$�@F{@F{@F{@F@E��@E��@E�h@EO�@D��@D�j@Dz�@D(�@D1@C�@C@B��@B��@B��@B�\@BM�@B-@A��@A&�@@��@@�9@@r�@?�;@?\)@?;d@>�y@>�+@>�+@>�+@>v�@>V@>@=@=`B@<�@<�D@<1@;t�@;S�@:�H@:��@:~�@:M�@:�@9��@9�@9��@9��@9G�@8�u@8A�@8A�@8 �@8  @8  @7�@7�;@7�@7�P@7;d@6��@6�@6�R@6v�@6ff@6E�@5�@5@5��@5�h@5?}@5V@4�@4�@3��@3C�@333@3o@2��@2�\@2~�@2~�@2n�@2M�@2M�@2=q@2-@2J@1�#@1X@0��@0�9@0�u@0 �@0b@0b@/�@/��@/��@/\)@/+@.��@.ȴ@.��@.ff@.@-�h@-O�@,�@,��@,��@,9X@,�@+�m@+dZ@*�H@*��@*�\@*�@)��@)�#@)��@)��@)X@)�@(Ĝ@(��@(r�@(A�@( �@(  @'��@'\)@'
=@&�@&�R@&��@&�+@&V@&{@%�T@%��@%`B@%�@$��@$�D@$z�@$I�@$1@#�m@#�F@#��@#S�@#33@"�@"��@"��@"n�@"M�@"-@"�@"J@!��@!�#@!�^@!��@!x�@!&�@ �9@ �u@ r�@ 1'@ b@   @��@�w@+@ȴ@v�@E�@5?@�@�T@@��@�@p�@?}@V@�j@�@z�@�@�m@�@33@�H@�H@��@��@n�@�@�@�^@�7@G�@7L@&�@�@%@��@Ĝ@�9@Q�@��@��@|�@|�@|�@|�@\)@+@�@
=@
=@�@�+@�T@��@@�-@O�@�@V@�/@��@��@�@��@�D@j@Z@�@�
@�@dZ@S�@C�@33@o@�H@�\@=q@��@hs@7L@�@�9@r�@ �@�;@�@�@��@|�@+@�R@V@$�@{@{@{@{@@@/@/@�@�@�@�@V@�j@�D@j@I�@(�@�@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�Bz�B{�Bz�Bz�B{�B{�B{�B{�B{�B{�B{�B{�B|�B�B��B�?BB��B�)B�;B�HB��B\B%�B%�B)�B �BJBVBuB
=BB  B\B:^BH�B-B+B�B#�B&�B'�B)�B-B33B8RB9XB9XB)�B#�B�B{BoBJB%B��B�B�B��B��B�B�B�`B�#B��B�9B�-B��B��B�hB�B{�Bl�B_;BYBL�B?}B-B{B
=B
��B
�B
�sB
�#B
��B
�B
��B
�B
o�B
R�B
9XB
1'B
�B
VB	�B	�HB	��B	�-B	�\B	�DB	�%B	~�B	u�B	jB	_;B	VB	L�B	;dB	/B	"�B	�B	VB	
=B		7B	+B	B��B�B�`B�NB��B��B��B�XB�LB�?B�B�B��B��B��B��B�JB�+B�Bz�Bx�Bq�Bl�BjBhsBffBdZBdZBdZBbNB`BB`BBZBYBW
BVBS�BS�BO�BO�BO�BN�BM�BI�BG�BF�BD�BB�B@�BC�B@�B@�B@�BA�BA�BA�BC�BC�BA�B@�B?}B>wB=qB=qB?}B?}BA�BC�BB�BB�BA�BD�BE�BE�B?}B<jB9XB33B/B/B.B.B-B.B/B1'B33B33B2-B2-B1'B49B6FB7LB9XB;dB;dB;dB>wB@�B@�BB�BC�BF�BG�BH�BG�BJ�BL�BO�BO�BO�BO�BO�BP�BT�BT�BT�BS�BS�BXB[#B^5B]/B^5B`BBaHBdZBe`BdZBffBjBq�Bp�Bq�Bs�B|�B}�B{�B|�B� B� B�B�1B�=B�JB�VB�hB�oB��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�LB�XB�dB�dB�wB��BŢBƨBȴBȴB��B��B��B��B��B�B�#B�)B�)B�BB�ZB�fB�sB�sB�B�B�B�B�B��B��B��B��B��B��B	B	B	+B	bB	oB	oB	oB	�B	�B	�B	#�B	%�B	&�B	'�B	)�B	.B	2-B	6FB	;dB	<jB	=qB	>wB	?}B	B�B	G�B	J�B	L�B	N�B	N�B	O�B	O�B	R�B	S�B	W
B	YB	[#B	^5B	`BB	aHB	bNB	dZB	ffB	hsB	iyB	l�B	o�B	p�B	p�B	q�B	r�B	s�B	v�B	x�B	z�B	{�B	|�B	}�B	~�B	�B	�B	�B	�%B	�+B	�+B	�1B	�DB	�JB	�JB	�JB	�VB	�bB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�FB	�RB	�XB	�^B	�dB	�dB	�}B	��B	��B	��B	ŢB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�)B	�)B	�)B	�/B	�5B	�5B	�5B	�;B	�HB	�HB	�HB	�TB	�ZB	�`B	�fB	�fB	�mB	�mB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
1B

=B

=B
DB
JB
JB
JB
PB
PB
VB
VB
\B
bB
bB
hB
hB
oB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
$�B
%�B
%�B
&�B
&�B
&�B
'�B
(�B
'�B
'�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
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
,B
,B
,B
,B
,B
,B
,B
-B
-B
.B
/B
/B
/B
0!B
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
49B
49B
5?B
5?B
5?B
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
8RB
8RB
8RB
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
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
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
D�B
D�B
D�B
E�B
E�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
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
J�B
J�B
J�B
J�B
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
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
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
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
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
VB
W
B
W
B
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
YB
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
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
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
aHB
aHB
aHB
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
dZB
dZB
dZB
dZB
dZB
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
jB
jB
jB
jB
jB
jB
jB
k�B
jB
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
m�B
m�B
n�B
n�B
n�B
o�B
o�B
o�B
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
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�Bz�B{�Bz�Bz�B{�B{�B{�B{�B{�B{�B{�B{�B}<B��B�B�%B�uB��B�B�pB�B�`B�B&2B&2B+B"�BBBB�B�B�B�B B@�BQ�B33B/ B#�B&LB(�B)�B*�B-�B3�B9$B;dB:�B+�B&�B�B�BFB�B�B��B�MB��B�6B�B��B��B�BݲB��B�?B��B�yB�IB�B��B~BBm�B`�B[=BOBBBAB/�B�B0B iB
�tB
�B
�B
ңB
�B
�B
�zB
tTB
UMB
;B
4B
"hB
TB	��B	�B	רB	�ZB	�.B	��B	�_B	��B	xB	k�B	`�B	X_B	O�B	=�B	1�B	$�B	�B	�B	
�B		�B	B	%B	AB��B��B��B�$BбB�uB�B�$B��B� B��B��B��B�QB�eB��B�lB��B}�Bz�BsMBm�Bk�BiDBf�Bd�BeBeFBcBa|Ba�BZ�BY�BWsBVmBUMBUBP�BP�BP�BO�BN�BJ�BIBG�BE�BC�BB�BD�B@�B@�B@�BA�BA�BB�BD�BD�BBABAUB@�B?cB>�B>]B?�B@ BB[BDMBC{BC�BB'BEBF�BGzB@�B>�B:�B3�B/�B/�B.�B.�B-�B.cB/OB1�B3�B3�B2�B2�B2GB5?B7LB8RB9�B;�B<B<B>�B@�BABCGBD�BG�BH�BIRBH�BK�BM�BP�BO�BP.BPBPbBQhBU�BU�BU�BT{BUBX�B[�B_B]�B^�Ba|Ba�BeBffBeBf�BkBr�BqvBr-BtnB}�B~BB|6B}VB�OB��B��B�fB��B��B��B�4B�oB��B��B��B��B��B��B��B��B��B�B�B�B�/B�vB�LB�XB�dB��B��B� B��BƎB��B��B�B��B�&B�BңB��B�#B�CB�]B�vB�tB�B�>B�>B�QB�}B��B�B��B�?B��B��B��B��B��B	 �B	aB	�B	HB	TB	oB	�B	�B	�B	�B	#�B	%�B	'B	'�B	)�B	.B	2GB	6�B	;dB	<PB	=VB	>]B	?cB	B�B	G�B	J�B	L�B	N�B	N�B	O�B	O�B	R�B	S�B	V�B	X�B	[#B	^B	`B	a-B	b4B	dZB	f2B	hsB	iyB	lWB	oiB	poB	p�B	qvB	r�B	s�B	v�B	x�B	z�B	{�B	|�B	}�B	~�B	�B	��B	��B	�B	��B	�B	�1B	�B	�B	�B	�JB	�<B	�HB	�TB	��B	��B	�eB	��B	�qB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�"B	�/B	�oB	�FB	�8B	�$B	�DB	�0B	�B	�}B	�iB	�oB	��B	ŢB	ǮB	ɆB	ʦB	ʌB	˒B	˒B	̳B	̘B	̘B	̳B	͟B	͟B	οB	��B	��B	��B	��B	ҽB	ҽB	��B	��B	��B	��B	�B	�B	�KB	�B	�B	�B	��B	�B	�B	�5B	�;B	�-B	�-B	�HB	�:B	�@B	�FB	�2B	�LB	�8B	�RB	�8B	�XB	�yB	�_B	�B	�kB	�qB	�qB	�qB	�]B	�cB	�}B	�}B	�B	�B	�B	�B	�|B	�B	�B	��B	��B	��B	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B
  B
  B
 �B
B
�B
�B
�B
B
�B
�B
�B
�B
�B
B
B
�B
�B
�B
B
9B
?B
+B
1B

#B

XB
DB
0B
0B
JB
6B
6B
VB
<B
BB
.B
HB
NB
4B
TB
oB
uB
{B
gB
MB
gB
YB
_B
EB
yB
_B
eB
yB
�B
eB
�B
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
$�B
%�B
%�B
&�B
&�B
&�B
'�B
(�B
($B
'�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
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
+�B
,B
,B
+�B
+�B
+�B
+�B
,�B
,�B
-�B
/B
/B
/OB
0B
1B
0�B
1�B
1�B
1�B
2B
1�B
1�B
2-B
3B
3B
4B
4B
5B
5%B
5B
6B
6B
6+B
6+B
6+B
6B
6B
6FB
7LB
7LB
8B
88B
88B
9$B
9$B
9>B
9XB
:*B
:*B
:*B
:DB
:*B
:*B
:*B
;0B
;B
;JB
;0B
;JB
<B
<6B
<B
<6B
<6B
<6B
<6B
<PB
=<B
=VB
=VB
>BB
>]B
>wB
?cB
?cB
?HB
?cB
?cB
@OB
@iB
@iB
AoB
AUB
AUB
AUB
AoB
BuB
B[B
B[B
C{B
CaB
CaB
CaB
C{B
C{B
DgB
D�B
D�B
E�B
E�B
F�B
FtB
F�B
GzB
GzB
GzB
GzB
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
IlB
I�B
I�B
I�B
J�B
J�B
J�B
J�B
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
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
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
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
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
V�B
V�B
W�B
W�B
W�B
W�B
W�B
W�B
X�B
YB
X�B
X�B
X�B
Y�B
ZB
ZB
ZB
ZB
ZB
Y�B
Z�B
[	B
Z�B
[�B
[�B
[�B
[�B
[�B
[�B
\B
[�B
\�B
\�B
\�B
\�B
]B
]B
^B
]�B
]�B
^B
^B
^B
^B
^B
_B
_B
_B
_!B
_B
_!B
_B
`B
`BB
a-B
aB
aB
aB
aB
aB
aB
bB
b4B
bB
b4B
b4B
b4B
c B
c B
c B
c:B
cTB
d@B
d&B
d&B
d@B
d&B
e,B
e,B
eFB
eFB
fLB
fLB
fB
fB
f2B
f2B
fLB
f2B
f2B
fLB
gRB
gRB
g8B
gB
g8B
g8B
gRB
g8B
h>B
h$B
h$B
h>B
hXB
hsB
hXB
i*B
i_B
iDB
iDB
i_B
jKB
j0B
jKB
jeB
jKB
j0B
jKB
k6B
jKB
kkB
kQB
kkB
k6B
kQB
kkB
lWB
lWB
lWB
l�B
lqB
lWB
lqB
m]B
mwB
n}B
n}B
ncB
o�B
oiB
o�B
oiB
o�B
o�B
p�B
poB
pUB
poB
poB
poB
p�B
poB
p�B
q[B
q[B
qvB
qvB
qvB
q�B
qvB
q�B
r�B
r|B
r|B
raB
raB
ra1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.52(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202101170038432021011700384320210117003843202306231727052023062317270520230623172705202101180033382021011800333820210118003338  JA  ARFMdecpA19c                                                                20210112063934  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20210111213935  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20210111213936  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20210111213936  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20210111213937  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20210111213937  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20210111213937  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20210111213937  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20210111213938  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20210111213938                      G�O�G�O�G�O�                JA  ARUP                                                                        20210111215211                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20210112153253  CV  JULD            G�O�G�O�Fʯ6                JM  ARGQJMQC2.0                                                                 20210112153253  CV  JULD_LOCATION   G�O�G�O�FʯJ                JM  ARGQJMQC2.0                                                                 20210112153253  CV  LATITUDE        G�O�G�O�A��j                JM  ARGQJMQC2.0                                                                 20210112153253  CV  LONGITUDE       G�O�G�O��s�                JM  ARCAJMQC2.0                                                                 20210116153843  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20210116153843  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2019V1                                                       20210117153338  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20230623082705  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20230705041504                      G�O�G�O�G�O�                