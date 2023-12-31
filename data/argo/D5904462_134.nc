CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-07-20T05:01:46Z AOML 3.0 creation; 2016-08-07T21:51:31Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20160720050146  20160825183417  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287_9017_134                   2C  D   APEX                            6529                            072314                          846 @׼�t>7n1   @׼���v@0�~��"��d�vȴ9X1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BPffBX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&�C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\�C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� De��Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy� D�#3D�@ D�s3D��fD�3D�<�D�ffD��3D�3D�Y�D��3D�ٚD�� D�Y�Dڙ�D��D�fD�9�D�|�D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��
@У�AQ�A(Q�AHQ�AhQ�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B{B
{B{B{B"{B*{B2{B:{BB{BJ{BRz�BZ{Bb{Bj{Br{Bz{B�
=B�
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
=B�
=B�=pB�
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
=C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&��C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\��C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�O\C�B�C�B�C�B�D !HD �HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD	!HD	�HD
!HD
�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD !HD �HD!!HD!�HD"!HD"�HD#!HD#�HD$!HD$�HD%!HD%�HD&!HD&�HD'!HD'�HD(!HD(�HD)!HD)�HD*!HD*�HD+!HD+�HD,!HD,�HD-!HD-�HD.!HD.�HD/!HD/�HD0!HD0�HD1!HD1�HD2!HD2�HD3!HD3�HD4!HD4�HD5!HD5�HD6!HD6�HD7!HD7�HD8!HD8�HD9!HD9�HD:!HD:�HD;!HD;�HD<!HD<�HD=!HD=�HD>!HD>�HD?!HD?�HD@!HD@�HDA!HDA�HDB!HDB�HDC!HDC�HDD!HDD�HDE!HDE�HDF!HDF�HDG!HDG�HDH!HDH�HDI!HDI�HDJ!HDJ�HDK!HDK�HDL!HDL�HDM!HDM�HDN!HDN�HDO!HDO�HDP!HDP�HDQ!HDQ�HDR!HDR�HDS!HDS�HDT!HDT�HDU!HDU�HDV!HDV�HDW!HDW�HDX!HDX�HDY!HDY�HDZ!HDZ�HD[!HD[�HD\!HD\�HD]!HD]�HD^!HD^�HD_!HD_�HD`!HD`�HDa!HDa�HDb!HDb�HDc!HDc�HDd!HDd�HDe!HDe�HDf�Df�HDg!HDg�HDh!HDh�HDi!HDi�HDj!HDj�HDk!HDk�HDl!HDl�HDm!HDm�HDn!HDn�HDo!HDo�HDp!HDp�HDq!HDq�HDr!HDr�HDs!HDs�HDt!HDt��Dy�HD�3�D�P�D���D��
D��D�MqD�w
D���D�#�D�j>D���D��>D� �D�j>Dڪ>D��qD�
D�J>D�qD��
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��hA�VA�=qA��mA��/A��#A��;A��/A��
A��#A��A��
A��A���A�ƨAݥ�A�Q�A���A�oAۓuA�Aڝ�A�r�AٓuA�  A��
A�ȴAؑhA�Q�A��`AָRA�
=A�r�A�-A���A��A��TA��A�Q�A�;dA��A�1A���Aϛ�A�r�A��A� �A��`A�M�A�K�A���A��`A���A�|�AđhA�C�A�"�Aé�AÏ\A�jA�v�A�G�A��
A�K�A���A�$�A�A�z�A��A���A��A�+A��uA��A���A�?}A���A���A���A�VA��A��/A�bNA���A�7LA��yA�t�A�p�A�A�bNA�r�A�;dA�p�A���A�33A���A��7A�VA�1A��A��\A���A���A�~�A��TA�VA�{A~ȴA}�PA}�A|�+Ay�^Av�yAtJAo�PAj=qAg�;AfA�A`�A[�AXn�AU�#AS�TAO/AK��AJA�AHȴAG&�AD$�AA`BA>ȴA=/A<�9A<VA;��A9�A7ƨA6E�A4��A2v�A1t�A0��A0=qA/�A/K�A-��A,�+A,��A,�uA,r�A+XA*z�A)`BA(��A'l�A&��A&-A%�A$=qA"�A $�A�hA1'AK�An�A�-A��AI�A��A�#AVA�A�DA+A\)A�jA�A?}AjA7LA^5A�AG�A
=AZA�mA��A��AĜA=qAAA�A&�A
5?A	��A	��A	l�A	33A	�A	VA	A��AM�A��At�Az�A�TA�hAVA�\A�A/AhsAl�Ap�AVA�9A��A   A;dA%A ��A r�@���@��+@�\)@�p�@�S�@�v�@�G�@��m@�@��@�&�@��@�I�@���@�o@�ff@�x�@��@��m@�\@���@��@���@�-@�%@��@�1'@��;@�S�@��@�R@�n�@��@�  @��@�|�@��y@�$�@�X@��@�hs@���@�E�@�ff@�h@�&�@�/@�%@�/@�/@�V@���@�1@�l�@��@⟾@�M�@�X@���@��u@�A�@�l�@�+@�~�@ݡ�@��@��@�&�@���@�(�@�t�@���@�-@٩�@�`B@��`@�I�@��H@�{@�hs@�%@��`@Ԭ@ԓu@�1'@�  @�ƨ@ӥ�@�
=@ҟ�@҇+@�n�@�E�@��#@ѡ�@ѡ�@�x�@�X@���@Ь@��@϶F@υ@���@��@�X@���@̴9@���@˾w@˶F@ˍP@��@���@ʟ�@ɲ-@ȓu@�r�@�Q�@� �@��@��
@�dZ@Ɵ�@�{@��T@ũ�@őh@Ł@�X@���@ě�@�bN@��@�@�@�x�@�(�@� �@���@��w@� �@�  @��
@�C�@��y@�G�@���@�ȴ@�v�@��@�@���@��7@�`B@��@��@�b@��@�\)@��!@���@�ȴ@�ff@��7@�X@�X@��/@�I�@�(�@��w@���@�V@�E�@���@���@�/@�bN@��@���@�=q@�@��#@�/@�Ĝ@��D@�bN@�Q�@�9X@�(�@� �@�  @���@��F@���@��P@�S�@��@���@�J@���@�hs@��@��/@���@�Q�@�9X@��@��@���@�l�@�S�@�S�@�C�@�@���@���@��!@��\@�V@�$�@�@�@��#@��#@���@�`B@�?}@�V@��j@��@� �@��F@��@�33@�
=@�ff@���@��@���@�x�@�7L@���@���@���@���@�(�@���@�\)@�;d@��y@���@�-@��#@��^@���@�?}@���@���@�Ĝ@���@�bN@�1'@��w@��@�?}@�+@�@�p�@w�@j-@c@[��@R�\@J-@A�7@8bN@3o@-�@(1'@#�@��@@�T@�`11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A��hA�VA�=qA��mA��/A��#A��;A��/A��
A��#A��A��
A��A���A�ƨAݥ�A�Q�A���A�oAۓuA�Aڝ�A�r�AٓuA�  A��
A�ȴAؑhA�Q�A��`AָRA�
=A�r�A�-A���A��A��TA��A�Q�A�;dA��A�1A���Aϛ�A�r�A��A� �A��`A�M�A�K�A���A��`A���A�|�AđhA�C�A�"�Aé�AÏ\A�jA�v�A�G�A��
A�K�A���A�$�A�A�z�A��A���A��A�+A��uA��A���A�?}A���A���A���A�VA��A��/A�bNA���A�7LA��yA�t�A�p�A�A�bNA�r�A�;dA�p�A���A�33A���A��7A�VA�1A��A��\A���A���A�~�A��TA�VA�{A~ȴA}�PA}�A|�+Ay�^Av�yAtJAo�PAj=qAg�;AfA�A`�A[�AXn�AU�#AS�TAO/AK��AJA�AHȴAG&�AD$�AA`BA>ȴA=/A<�9A<VA;��A9�A7ƨA6E�A4��A2v�A1t�A0��A0=qA/�A/K�A-��A,�+A,��A,�uA,r�A+XA*z�A)`BA(��A'l�A&��A&-A%�A$=qA"�A $�A�hA1'AK�An�A�-A��AI�A��A�#AVA�A�DA+A\)A�jA�A?}AjA7LA^5A�AG�A
=AZA�mA��A��AĜA=qAAA�A&�A
5?A	��A	��A	l�A	33A	�A	VA	A��AM�A��At�Az�A�TA�hAVA�\A�A/AhsAl�Ap�AVA�9A��A   A;dA%A ��A r�@���@��+@�\)@�p�@�S�@�v�@�G�@��m@�@��@�&�@��@�I�@���@�o@�ff@�x�@��@��m@�\@���@��@���@�-@�%@��@�1'@��;@�S�@��@�R@�n�@��@�  @��@�|�@��y@�$�@�X@��@�hs@���@�E�@�ff@�h@�&�@�/@�%@�/@�/@�V@���@�1@�l�@��@⟾@�M�@�X@���@��u@�A�@�l�@�+@�~�@ݡ�@��@��@�&�@���@�(�@�t�@���@�-@٩�@�`B@��`@�I�@��H@�{@�hs@�%@��`@Ԭ@ԓu@�1'@�  @�ƨ@ӥ�@�
=@ҟ�@҇+@�n�@�E�@��#@ѡ�@ѡ�@�x�@�X@���@Ь@��@϶F@υ@���@��@�X@���@̴9@���@˾w@˶F@ˍP@��@���@ʟ�@ɲ-@ȓu@�r�@�Q�@� �@��@��
@�dZ@Ɵ�@�{@��T@ũ�@őh@Ł@�X@���@ě�@�bN@��@�@�@�x�@�(�@� �@���@��w@� �@�  @��
@�C�@��y@�G�@���@�ȴ@�v�@��@�@���@��7@�`B@��@��@�b@��@�\)@��!@���@�ȴ@�ff@��7@�X@�X@��/@�I�@�(�@��w@���@�V@�E�@���@���@�/@�bN@��@���@�=q@�@��#@�/@�Ĝ@��D@�bN@�Q�@�9X@�(�@� �@�  @���@��F@���@��P@�S�@��@���@�J@���@�hs@��@��/@���@�Q�@�9X@��@��@���@�l�@�S�@�S�@�C�@�@���@���@��!@��\@�V@�$�@�@�@��#@��#@���@�`B@�?}@�V@��j@��@� �@��F@��@�33@�
=@�ff@���@��@���@�x�@�7L@���@���@���@���@�(�@���@�\)@�;d@��y@���@�-@��#@��^@���@�?}@���@���@�Ĝ@���@�bN@�1'G�O�@��@�?}@�+@�@�p�@w�@j-@c@[��@R�\@J-@A�7@8bN@3o@-�@(1'@#�@��@@�T@�`11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�XB
�FB
�B
�B
�3B
�LB
�dB
�wB
�}B
B
ĜB
ƨB
ȴB
ɺB
ɺB
ȴB
ŢB
��B
�ZB
�yB
��BBbB�BhB\BVB1B
��B
�B
�B
�B
�mB
�ZB
�HB
�;B
�BB
�B�B"�B$�B'�B7LB:^B@�BO�BdZBiyBt�B��BƨB�BB�sB��B�B,B;dBR�Bl�B|�Bs�BjBp�Bt�Bv�By�B�B�%B�B�B�%B�7B��B��B�JBq�BR�B6FB �BB�B�)B��B�9B��BcTBD�B�B
�B
�B
�B(�B �B�B\B
�fB
ȴB
��B
�}B
ɺB
ɺB
�jB
�-B
��B
��B
��B
�%B
z�B
q�B
o�B
n�B
XB
A�B
,B

=B	�fB	��B	�qB	��B	t�B	e`B	R�B	E�B	1'B	%�B	�B	�B	oB	
=B	B��B��B�B�B�B�mB�HB�)B�
B�B�B�#B�/B�yB�B��B	1B	DB	DB		7B	1B		7B	1B		7B	
=B	
=B	JB	
=B	B��B�B�#B�
B��B�/B�/B��B	VB	7LB	@�B	<jB	?}B	@�B	>wB	<jB	;dB	9XB	8RB	33B	,B	&�B	$�B	%�B	)�B	'�B	)�B	9XB	E�B	I�B	I�B	K�B	Q�B	YB	[#B	[#B	\)B	\)B	_;B	`BB	`BB	_;B	_;B	_;B	aHB	bNB	aHB	`BB	_;B	_;B	aHB	cTB	e`B	aHB	k�B	x�B	~�B	�B	�B	z�B	r�B	�=B	�=B	�DB	�\B	�VB	�JB	�B	~�B	~�B	�B	�B	~�B	|�B	~�B	}�B	}�B	�B	�VB	�bB	�\B	�VB	�\B	�PB	�VB	�hB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�^B	�jB	�dB	�^B	�LB	�FB	�dB	�qB	ǮB	��B	��B	��B	��B	��B	��B	�B	�B	�/B	�5B	�;B	�HB	�HB	�NB	�HB	�HB	�BB	�BB	�BB	�NB	�NB	�TB	�`B	�fB	�fB	�mB	�mB	�mB	�fB	�mB	�mB	�sB	�sB	�yB	�yB	�sB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
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
%B
%B
+B
1B
1B
1B
1B
1B
1B
1B
	7B
	7B
	7B
1B
1B
1B
1B
1B
1B
1B
+B
+B
+B
1B
+B
+B
+B
+B
%B
%B
%B
B
B
B
B
B
B
B
B
B
%B
B
%B
%B
1B
1B
1B
+B
+B
+B
+B
+B
+B
1B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
DB
JB
hB
�B
�B
#�B
-B
49B
8RB
;dB
B�B
I�B
P�B
XB
\)B
`BB
e`B
iyB
m�B
p�B
u�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
�2B
�$B
��B
��B
�B
�(B
�@B
�PB
�VB
�iB
�wB
ƁB
ȑB
ɕB
ɓB
ȏB
�B
̤B
�4B
�TB
��B�B<BjB@B6B/BB
��B
�B
�B
�cB
�HB
�0B
�%B
�B
�B
�wBxB"�B$�B'�B7#B:6B@\BO�Bd2BiPBt�B��B�zB�B�IB��B�B+�B;7BR�Bl`B|�Bs�BjUBpuBt�Bv�By�B��B��B��B��B��B�B�VB�wB�Bq~BR�B6B �B �B�VB��BжB�B�VBc"BDlB�B
�B
�gB
�~B(�B �BjB)B
�5B
ȄB
�XB
�LB
ɉB
ɆB
�6B
��B
��B
��B
�sB
��B
z�B
q{B
omB
njB
W�B
AZB
+�B

B	�7B	έB	�CB	�nB	t�B	e6B	R�B	EyB	0�B	%�B	�B	sB	FB	
B	�B��B��B�B�B�hB�CB�B�B��B��B��B��B�B�OB�B��B	B	B	B		B	B		B	B			B	
B	
B	B	
B	�B��B�VB��B��B��B�B�B��B	(B	7B	@SB	<;B	?MB	@TB	>EB	<9B	;4B	9'B	8"B	3B	+�B	&�B	$�B	%�B	)�B	'�B	)�B	9&B	EpB	I�B	I�B	K�B	Q�B	X�B	Z�B	Z�B	[�B	[�B	_B	`B	`B	_	B	_	B	_	B	aB	bB	aB	`B	_B	_B	aB	c!B	e,B	aB	kSB	x�B	~�B	��B	��B	z�B	r|B	�
B	�B	�B	�'B	�B	�B	��B	~�B	~�B	��B	��B	~�B	|�B	~�B	}�B	}�B	��B	�"B	�/B	�(B	� B	�%B	�B	�#B	�2B	�2B	�?B	�JB	�WB	�aB	�|B	��B	��B	��B	��B	��B	��B	�(B	�2B	�-B	�%B	�B	�B	�,B	�:B	�wB	̓B	ЭB	ϦB	ϥB	ҼB	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�	B	�B	�B	�B	�B	�B	�)B	�.B	�.B	�3B	�6B	�4B	�/B	�4B	�4B	�;B	�<B	�?B	�?B	�<B	�3B	�=B	�KB	�RB	�XB	�XB	�VB	�WB	�UB	�XB	�VB	�[B	�^B	�\B	�\B	�dB	�cB	�cB	�iB	�dB	�kB	�eB	�`B	�_B	�UB	�WB	�bB	�gB	�nB	�oB	�vB	�uB	�oB	�oB	�nB	�jB	�kB	�eB	�cB	�cB	�dB	�gB	�hB	�kB	�oB	�sB	�{B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�{B	�eB	�cB	�hB	�oB	��B	��B	��B	�B	�{B	�eB	�]B	�UB	�RB	�VB	�WB	�]B	�^B	�]B	�\B	�bB	�jB	�nB	�nB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�G�O�B
B
+B
YB
nB
#�B
,�B
3�B
8B
;#B
BQB
IzB
P�B
W�B
[�B
`B
e"B
i;B
mRB
peB
u�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.52 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451312016080714513120160807145131  AO  ARCAADJP                                                                    20160720050146    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160720050146  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160720050146  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145131  IP                  G�O�G�O�G�O�                