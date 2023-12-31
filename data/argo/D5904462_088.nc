CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-11-20T20:17:19Z AOML 3.0 creation; 2016-08-07T21:51:23Z UW 3.1 conversion     
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
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �|   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �(   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �8   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �<   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �L   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �P   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �T   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �XArgo profile    3.1 1.2 19500101000000  20151120201719  20160807145123  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               XA   AO  5287_9017_088                   2C  D   APEX                            6529                            072314                          846 @׀��-^1   @׀"��@0��1&��d�^5?|�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    XA   B   B   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33C �C  C�fC  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D ��D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DOy�DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDys3D��fD�P D��fD��3D�	�D�@ D��3D�ɚD��D�@ D��3D�� D�3D�@ D�y�D�ɚD��D�33D�VfD��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @Z�H@���@У�AQ�A(Q�AHQ�AhQ�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B{B
{B{B{B"{B*{B2{B:{BB{BJ{BR{BZ{Bb{Bj{Br{Bz{B�
=B��
B��
B�
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
=B�
=B�
=B�=pC ��C�Ck�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�D !HD �HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD	!HD	�HD
!HD
�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD !HD �HD!�D!�HD"!HD"�HD#!HD#�HD$!HD$�HD%!HD%�HD&!HD&�HD'!HD'�HD(!HD(�HD)!HD)�HD*!HD*�HD+!HD+�HD,!HD,�HD-!HD-�HD.!HD.�HD/!HD/�HD0!HD0�HD1!HD1�HD2!HD2�HD3!HD3�HD4!HD4�HD5!HD5�HD6!HD6�HD7!HD7�HD8!HD8�HD9!HD9�HD:!HD:�HD;!HD;�HD<!HD<�HD=!HD=�HD>!HD>�HD?!HD?�HD@!HD@�HDA!HDA�HDB!HDB�HDC!HDC�HDD!HDD�HDE!HDE�HDF!HDF�HDG!HDG�HDH!HDH�HDI!HDI�HDJ!HDJ�HDK!HDK�HDL!HDL�HDM!HDM�HDN!HDN�HDO!HDO��DP!HDP�HDQ!HDQ�HDR!HDR�HDS!HDS�HDT!HDT�HDU!HDU�HDV!HDV�HDW!HDW�HDX!HDX�HDY!HDY�HDZ!HDZ�HD[!HD[�HD\!HD\�HD]!HD]�HD^!HD^�HD_!HD_�HD`!HD`�HDa!HDa�HDb!HDb�HDc!HDc�HDd!HDd�HDe!HDe�HDf!HDf�HDg!HDg�HDh!HDh�HDi!HDi�HDj!HDj�HDk!HDk�HDl!HDl�HDm!HDm�HDn!HDn�HDo!HDo�HDp!HDp�HDq!HDq�HDr!HDr�HDs!HDs�HDt!HDt�HDt�Dy�{D��
D�`�D��
D���D�>D�P�D���D��>D�*>D�P�D���D��D�#�D�P�Dڊ>D��>D�qD�C�D�g
D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aޕ�Aޕ�Aޗ�Aޙ�Aޗ�Aޝ�Aޟ�Aޟ�Aޣ�Aޡ�Aޡ�Aޡ�Aޡ�Aޝ�Aޝ�AެAް!Aް!Aް!AޮA޲-A޶FA޶FA޶FA޴9A޴9A޴9A޶FA޴9A޴9A޲-A޶FA�"�AۅA؟�A��#A�`BA�1A�A�hsA���Aκ^A͉7AʬA���A�r�A�ƨA���A�p�A��-A��TA�r�A��-A��FA�ȴA�t�A���A� �A��A���A���A��FA�JA��\A���A�v�A�ffA�n�A��`A���A� �A�z�A���A��#A���A�oA���A��A�{A�t�A���A���A�v�A��7A�l�A�A���A�ffA�9XA��A�;dA���A�JA��^A~�jA}��A|  Ax�AwK�At��Apz�Al��Ag�Ad1Ab�A`��A^��A]A]VA[�AZ�AYS�AXbNAV��AU`BAR�DAO��AN��AN1'AMAJ�uAF��AD�yAC`BAA"�A?�;A>��A=p�A;��A;l�A:�DA8�+A7
=A4�`A3�PA2��A25?A1
=A0Q�A/�mA.�A.�A-�PA,��A*ȴA)t�A)\)A'C�A#+A!x�A jA(�Av�AO�A33A&�A�AoA7LA��A-A�A  A�TA��A��A��A��A��AbAbA{AbA��A�An�AbA5?Ap�A�HAM�A��A�uA~�A�A�AĜA�jA��A��A�\AQ�A�#AO�A
�A
�!A
E�A	��A	hsA	G�A	hsA	%A1A9XAl�AC�A�A�FA��AO�A ĜA $�A M�A   @��\@���A -@�dZ@�J@�~�@��/@��@�K�@��@�{@�Z@�n�@�-@���@�hs@���@���@���@�~�@�$�@�`B@��j@���@�@�K�@�ff@�7@�bN@�+@��@�P@���@�!@�7@��m@�@�V@�l�@�~�@�ff@�j@��m@�9X@��H@�7L@�Z@�+@�+@�`B@�Ĝ@�`B@��@�
=@��`@�9X@���@�\)@�?}@��;@�\)@��@�@�7L@�Z@�1@ӝ�@҇+@�@�^5@�K�@ӥ�@��@� �@���@�V@���@��;@�t�@��@̼j@��@˅@�@ʏ\@�J@�@ɩ�@�x�@ȼj@�1'@�I�@�b@�S�@�5?@�`B@��@��@��@�X@�&�@��/@���@���@ă@� �@Õ�@�t�@�dZ@�S�@��y@°!@�@�-@��@¸R@�n�@�~�@�ȴ@��y@§�@���@�?}@��D@�l�@���@�E�@��h@�%@�1'@��
@��@��@���@�-@�{@���@��@��@��u@��F@���@��\@�^5@�=q@��T@�&�@�X@���@���@�O�@��j@�I�@�1@�o@��R@�n�@�5?@��@��#@���@�x�@�G�@�?}@��`@� �@�;d@���@�{@��^@���@�z�@�Z@�Z@�(�@� �@�9X@� �@�1@��m@�ƨ@���@�l�@�M�@���@���@��-@���@�{@���@�G�@�G�@��@��u@��@�\)@�^5@�{@�J@���@��@��u@�(�@���@��;@��F@�t�@�33@�
=@�ȴ@�^5@�~�@��\@��\@�n�@�E�@��@��7@��@��`@��j@��D@��
@�t�@�C�@�C�@�+@��H@���@��\@�v�@�ff@�V@�=q@���@��7@�x�@�X@�/@���@�Ĝ@�z�@�j@�bN@� �@��
@��P@�S�@�
=@��R@�ff@�5?@�^5@�V@�@���@�X@���@��/@�Ĝ@�I�@�Q�@�9X@�9X@�(�@���@�l�@�dZ@�S�@�+@��H@�5?@��^@��7@�O�@��u@���@���@��@��@vv�@m�T@dZ@Z�@Q��@E�@;o@4(�@/�@)x�@"�H@ȴ@�^@��@�F@b@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 Aޕ�Aޕ�Aޗ�Aޙ�Aޗ�Aޝ�Aޟ�Aޟ�Aޣ�Aޡ�Aޡ�Aޡ�Aޡ�Aޝ�Aޝ�AެAް!Aް!Aް!AޮA޲-A޶FA޶FA޶FA޴9A޴9A޴9A޶FA޴9A޴9A޲-A޶FA�"�AۅA؟�A��#A�`BA�1A�A�hsA���Aκ^A͉7AʬA���A�r�A�ƨA���A�p�A��-A��TA�r�A��-A��FA�ȴA�t�A���A� �A��A���A���A��FA�JA��\A���A�v�A�ffA�n�A��`A���A� �A�z�A���A��#A���A�oA���A��A�{A�t�A���A���A�v�A��7A�l�A�A���A�ffA�9XA��A�;dA���A�JA��^A~�jA}��A|  Ax�AwK�At��Apz�Al��Ag�Ad1Ab�A`��A^��A]A]VA[�AZ�AYS�AXbNAV��AU`BAR�DAO��AN��AN1'AMAJ�uAF��AD�yAC`BAA"�A?�;A>��A=p�A;��A;l�A:�DA8�+A7
=A4�`A3�PA2��A25?A1
=A0Q�A/�mA.�A.�A-�PA,��A*ȴA)t�A)\)A'C�A#+A!x�A jA(�Av�AO�A33A&�A�AoA7LA��A-A�A  A�TA��A��A��A��A��AbAbA{AbA��A�An�AbA5?Ap�A�HAM�A��A�uA~�A�A�AĜA�jA��A��A�\AQ�A�#AO�A
�A
�!A
E�A	��A	hsA	G�A	hsA	%A1A9XAl�AC�A�A�FA��AO�A ĜA $�A M�A   @��\@���A -@�dZ@�J@�~�@��/@��@�K�@��@�{@�Z@�n�@�-@���@�hs@���@���@���@�~�@�$�@�`B@��j@���@�@�K�@�ff@�7@�bN@�+@��@�P@���@�!@�7@��m@�@�V@�l�@�~�@�ff@�j@��m@�9X@��H@�7L@�Z@�+@�+@�`B@�Ĝ@�`B@��@�
=@��`@�9X@���@�\)@�?}@��;@�\)@��@�@�7L@�Z@�1@ӝ�@҇+@�@�^5@�K�@ӥ�@��@� �@���@�V@���@��;@�t�@��@̼j@��@˅@�@ʏ\@�J@�@ɩ�@�x�@ȼj@�1'@�I�@�b@�S�@�5?@�`B@��@��@��@�X@�&�@��/@���@���@ă@� �@Õ�@�t�@�dZ@�S�@��y@°!@�@�-@��@¸R@�n�@�~�@�ȴ@��y@§�@���@�?}@��D@�l�@���@�E�@��h@�%@�1'@��
@��@��@���@�-@�{@���@��@��@��u@��F@���@��\@�^5@�=q@��T@�&�@�X@���@���@�O�@��j@�I�@�1@�o@��R@�n�@�5?@��@��#@���@�x�@�G�@�?}@��`@� �@�;d@���@�{@��^@���@�z�@�Z@�Z@�(�@� �@�9X@� �@�1@��m@�ƨ@���@�l�@�M�@���@���@��-@���@�{@���@�G�@�G�@��@��u@��@�\)@�^5@�{@�J@���@��@��u@�(�@���@��;@��F@�t�@�33@�
=@�ȴ@�^5@�~�@��\@��\@�n�@�E�@��@��7@��@��`@��j@��D@��
@�t�@�C�@�C�@�+@��H@���@��\@�v�@�ff@�V@�=q@���@��7@�x�@�X@�/@���@�Ĝ@�z�@�j@�bN@� �@��
@��P@�S�@�
=@��R@�ff@�5?@�^5@�V@�@���@�X@���@��/@�Ĝ@�I�@�Q�@�9X@�9X@�(�@���@�l�@�dZ@�S�@�+@��H@�5?@��^@��7@�O�G�O�@���@���@��@��@vv�@m�T@dZ@Z�@Q��@E�@;o@4(�@/�@)x�@"�H@ȴ@�^@��@�F@b@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
-B
.B
/B
.B
.B
/B
/B
/B
1'B
0!B
/B
/B
.B
.B
0!B
33B
5?B
6FB
6FB
7LB
8RB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
;dB
;dB
<jB
=qB
N�B
�PB
��B
��B�B�B@�BVB[#B\)Bl�B��BÖB��B�B��B��B+B	7B+B%BBB��B��B�B�B�/B�B��BŢB�3B�B��B~�BgmBB�B7LB-B$�B)�B%�B�BVB
��B
�B
�yB
�;B
��B
ɺB
�LB
��B
�{B
x�B
XB
>wB
/B
�B
�B
PB	��B	�
B	ŢB	�qB	�3B	��B	��B	�7B	s�B	aHB	J�B	:^B	2-B	+B	#�B	�B	�B	�B	hB	PB		7B	B��B��B�B�B�B�B�B�B�B�B�B��B��B	  B	B	VB	hB	�B	"�B	.B	/B	/B	0!B	49B	2-B	49B	9XB	<jB	<jB	8RB	2-B	0!B	9XB	-B	bB	
=B		7B��B��B��B��B	B	%B	PB	�B	0!B	I�B	M�B	O�B	S�B	]/B	`BB	jB	x�B	�B	�\B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�\B	�=B	�B	t�B	jB	bNB	aHB	cTB	e`B	cTB	_;B	^5B	[#B	XB	T�B	Q�B	N�B	L�B	P�B	Q�B	[#B	bNB	_;B	M�B	9XB	-B	:^B	;dB	C�B	C�B	B�B	@�B	S�B	XB	XB	bNB	r�B	s�B	r�B	w�B	v�B	w�B	x�B	z�B	x�B	u�B	u�B	v�B	v�B	z�B	�B	�+B	�DB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�VB	�uB	�{B	�hB	�DB	�VB	��B	�B	�B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�'B	�FB	�jB	�jB	�dB	�RB	�RB	�LB	�?B	�-B	�'B	�!B	�3B	�9B	�LB	�XB	�qB	�jB	�jB	�jB	�qB	�qB	�^B	�RB	�LB	�FB	�LB	�RB	�qB	�qB	�jB	�wB	B	B	B	ĜB	ĜB	ŢB	ŢB	ƨB	ǮB	��B	��B	��B	��B	��B	�B	�B	�#B	�#B	�)B	�#B	�B	�B	�
B	�B	�
B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�)B	�/B	�)B	�/B	�5B	�;B	�BB	�BB	�HB	�TB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
  B
B
B
B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
	7B
DB
DB
DB
DB
DB
JB
JB
PB
\B
\B
bB
bB
\B
bB
hB
hB
hB
hB
hB
hB
hB
hB
bB
#�B
VB
�B
�B
 �B
&�B
-B
49B
<jB
B�B
J�B
R�B
[#B
`BB
e`B
iyB
n�B
s�B
w�B
y�B
}�B
~�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 B
,�B
-�B
.�B
-�B
-�B
.�B
.�B
.�B
1B
0 B
.�B
.�B
-�B
-�B
/�B
3B
5B
6(B
6$B
7+B
84B
94B
:>B
:>B
:;B
:>B
:>B
:>B
;BB
;EB
<JB
=QB
N�B
�-B
нB
��B�B�B@ZBU�BZ�B\ BlaB��B�iB��B�UB��B��B�B	BB�B�B�B��B��B�B�bB� B��B��B�tB�B��B�B~�Bg<BB`B7B,�B$�B)�B%�BqB'B
��B
�oB
�KB
�B
��B
ɇB
�B
��B
�MB
x�B
W�B
>NB
.�B
�B
]B
%B	��B	��B	�xB	�EB	�B	��B	�vB	�B	s�B	a B	J�B	:6B	2B	*�B	#�B	�B	xB	gB	AB	*B		B	�B��B��B�B�wB�qB�_B�^B�cB�oB�oB�B��B��B��B	�B	-B	=B	vB	"�B	-�B	.�B	.�B	/�B	4B	1�B	4B	9)B	<;B	<>B	8$B	1�B	/�B	9)B	,�B	5B	
B		B��B��B��B��B	�B	�B	%B	gB	/�B	I�B	M�B	O�B	S�B	\�B	`B	jOB	x�B	��B	�,B	�CB	�aB	�|B	��B	��B	��B	��B	��B	�jB	�NB	�*B	�
B	��B	t�B	jLB	bB	aB	c"B	e-B	c"B	_B	^B	Z�B	W�B	T�B	Q�B	N�B	L�B	P�B	Q�B	Z�B	bB	_B	M�B	9#B	,�B	:,B	;3B	CdB	CbB	B]B	@OB	S�B	W�B	W�B	bB	r{B	s�B	rzB	w�B	v�B	w�B	x�B	z�B	x�B	u�B	u�B	v�B	v�B	z�B	��B	��B	�B	�XB	�_B	�tB	�wB	�jB	�dB	�nB	�zB	�qB	�]B	�]B	�SB	� B	�>B	�GB	�2B	�B	�"B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�zB	��B	�xB	��B	��B	��B	��B	��B	��B	��B	�B	�hB	�hB	�iB	�]B	�hB	�bB	�[B	�aB	�\B	�QB	�OB	�{B	��B	��B	�B	�1B	�1B	�-B	�B	�B	�B	�B	��B	��B	��B	��B	� B	�B	� B	�9B	�1B	�1B	�/B	�8B	�8B	�$B	�B	�B	�B	�B	�B	�7B	�6B	�/B	�?B	�UB	�UB	�VB	�aB	�aB	�hB	�hB	�lB	�tB	ʉB	ʈB	ЭB	ҺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�B	�B	�:B	�BB	�DB	�SB	�OB	�PB	�OB	�CB	�JB	�KB	�IB	�JB	�PB	�QB	�XB	�bB	�iB	�nB	�iB	�bB	�cB	�\B	�mB	�nB	�hB	�iB	�iB	�oB	�xB	��B	��B	��B	�B	�B	�B	�B	�sB	�sB	�tB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�~B	�rB	�qB	�qB	�lB	�sB	�qB	�xB	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B	��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
B
B
	B
B
B
B
B
B
%B
%B
B
%B
)B
-B
*B
*B
+B
)B
+B
,B
%G�O�B
B
BB
bB
 �B
&�B
,�B
3�B
<+B
BPB
JB
R�B
Z�B
`B
e!B
i:B
nXB
sxB
w�B
y�B
}�B
~�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.52 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451232016080714512320160807145123  AO  ARCAADJP                                                                    20151120201719    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151120201719  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151120201719  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145123  IP                  G�O�G�O�G�O�                