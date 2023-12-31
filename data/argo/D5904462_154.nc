CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T12:59:49Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7,   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  74   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7t   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
resolution        :�o     �  qt   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �\   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �$   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20181121125949  20190405100756  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @��'�l�1   @��(��v�@/�bM���dh9XbN1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   B   @9��@�  @�  A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BO��BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B̙�Bϙ�B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C-�fC0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD�CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQfDQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy�fD� D�@ D��fD���D��D�VfD���D���D� D�C3D�i�D���D���D�I�Dډ�D�ɚD�	�D�@ D� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @Z�H@���@У�AQ�A&�RAHQ�AhQ�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B{B
{B{B{B"{B*{B2{B:{BB{BJ{BQ�BZ{Bb{Bj{Br{Bz{B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�=pB�=pB�=pB�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=Bͣ�BУ�B�
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
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.k�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD��CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�B�C�B�C�B�C�B�C�B�C�B�C�O\C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�D !HD �HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD	!HD	�HD
!HD
�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD !HD �HD!!HD!�HD"!HD"�HD#!HD#�HD$!HD$�HD%!HD%�HD&!HD&�HD'!HD'�HD(!HD(�HD)!HD)�HD*!HD*�HD+!HD+�HD,!HD,�HD-!HD-�HD.!HD.�HD/!HD/�HD0!HD0�HD1!HD1�HD2!HD2�HD3!HD3�HD4!HD4�HD5!HD5�HD6!HD6�HD7!HD7�HD8!HD8�HD9!HD9�HD:!HD:�HD;!HD;�HD<!HD<�HD=!HD=�HD>!HD>�HD?!HD?�HD@!HD@�HDA!HDA�HDB!HDB�HDC!HDC�HDD!HDD�HDE!HDE�HDF!HDF�HDG!HDG�HDH!HDH�HDI!HDI�HDJ!HDJ�HDK!HDK�HDL!HDL�HDM!HDM�HDN!HDN�HDO!HDO�HDP!HDP�HDQ'�DQ�HDR!HDR�HDS!HDS�HDT!HDT�HDU!HDU�HDV!HDV�HDW!HDW�HDX!HDX�HDY!HDY�HDZ!HDZ�HD[!HD[�HD\!HD\�HD]!HD]�HD^!HD^�HD_!HD_�HD`!HD`�HDa!HDa�HDb!HDb�HDc!HDc�HDd!HDd�HDe!HDe�HDf!HDf�HDg!HDg�HDh!HDh�HDi!HDi�HDj!HDj�HDk!HDk�HDl!HDl�HDm!HDm�HDn!HDn�HDo!HDo�HDp!HDp�HDq!HDq�HDr!HDr�HDs!HDs�HDt!HDt�{DyǮD� �D�P�D��
D��qD�*>D�g
D��>D��qD� �D�S�D�z>D��qD�qD�Z>Dښ>D��>D�>D�P�D�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��
A��HA��HA��HA��HA��TA��`A��yA��A��A��A��A��A��A��yA��yA��A��A��A���A��A��A��A��A���A�  A���A�A�A�A�A�%A�%A�1A�%A�%A�1A�%A�1A�A��`A�9XA���AЬA��A�C�A�AΉ7A�1A���A�jA�ȴAɴ9A���A�~�A�A�A��TA�z�A�A��
A�^5A���A�/A���A�9XA�S�A��+A�VA�S�A��A�r�A�z�A���A���A�;dA�`BA��A�ȴA�?}A�"�A���A��/A�1'A��^A�\)A��A��mA��PA���A��uA���A�`BA�
=A��;A�{A�
=A�ƨA�33A��hA~{A~��A��A�I�A}ƨAx�/At1An~�Ak�;AiXAfVAd��Acx�Aax�A]�wA[�hAYƨAX1'AW33AV  AR~�APZAN��AMt�AL��AJZAH��AEt�ABQ�AAA?�A=��A<  A:�uA8r�A6�HA5�A5��A4��A3dZA1�A0�A/�7A.M�A-�PA,��A*��A(�yA&ZA#�TA!�FA��Az�A  A�PAjA\)A�TAVAAt�AjA��A�A��A=qAA�A��AS�AM�A|�A
5?A�uA�;A�!AbNA\)A%A�HAQ�Ax�A1A�TA�A��A��A1A�A��A�;A-AC�A �AK�A ��A ĜA �DA {@���@�ȴ@�5?@���@��@�7L@��@�z�@�  @��
@�\)@��@�&�@�A�@��@��H@�M�@���@�@�p�@���@�ƨ@�@�^@�/@�1@�S�@�"�@�S�@���@�G�@�/@�O�@��-@�R@�C�@�@��H@�^5@�h@��@�j@���@�I�@�K�@�{@�F@��@�?}@�R@�ȴ@�F@�l�@��@�@�hs@�bN@�o@ާ�@�$�@�/@ۍP@�ȴ@��@�@���@�hs@�%@ؼj@�I�@���@׮@��@���@և+@�ff@֏\@�;d@�+@�l�@׶F@�dZ@�n�@պ^@��@Լj@�Z@Ӆ@�+@�\)@�5?@��@Ѳ-@щ7@�7L@�X@���@��@�9X@��@�ƨ@�dZ@�C�@�+@�
=@��@�ff@�J@͑h@���@��@ˮ@�t�@�
=@�n�@���@�`B@�V@�9X@ǥ�@�+@ƸR@�M�@ũ�@�?}@���@�9X@��
@�|�@�;d@�ff@�{@���@���@��@�/@��`@�I�@���@���@���@�=q@��@��@���@�@���@�`B@���@�Ĝ@�z�@�ƨ@�C�@��y@���@�^5@���@��@��D@�b@�ƨ@�\)@�K�@��@���@���@��^@��/@�1'@��;@��P@�t�@��y@��y@���@�E�@���@��T@���@�O�@��@��`@�bN@���@�ƨ@�33@��+@�E�@�$�@��@�J@���@�7L@��@� �@�b@�  @���@�dZ@�+@�ȴ@�~�@�-@�@���@�Ĝ@�Z@�  @�ƨ@���@��@�dZ@�S�@��y@���@�^5@�M�@�$�@�{@��@�p�@��9@�9X@�  @��@�K�@���@�=q@���@���@���@�x�@�7L@��@�V@��/@���@��D@�z�@�bN@��@��@���@��+@�$�@���@��@��j@��D@��@���@��@��P@�|�@�@�E�@�J@���@�7L@���@��j@��@�Z@�(�@���@�ƨ@�C�@��@��R@�ȴ@�ȴ@���@�5?@��-@�`B@�%@���@�Ĝ@��j@��D@�9X@� �@��m@�K�@���@���@���@�ff@�-@�@��@���@���@��@z=q@q&�@c�F@Yhs@Ol�@GK�@=/@6v�@.��@( �@$I�@   @x�@1@��@�@	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��
A��HA��HA��HA��HA��TA��`A��yA��A��A��A��A��A��A��yA��yA��A��A��A���A��A��A��A��A���A�  A���A�A�A�A�A�%A�%A�1A�%A�%A�1A�%A�1A�A��`A�9XA���AЬA��A�C�A�AΉ7A�1A���A�jA�ȴAɴ9A���A�~�A�A�A��TA�z�A�A��
A�^5A���A�/A���A�9XA�S�A��+A�VA�S�A��A�r�A�z�A���A���A�;dA�`BA��A�ȴA�?}A�"�A���A��/A�1'A��^A�\)A��A��mA��PA���A��uA���A�`BA�
=A��;A�{A�
=A�ƨA�33A��hA~{A~��A��A�I�A}ƨAx�/At1An~�Ak�;AiXAfVAd��Acx�Aax�A]�wA[�hAYƨAX1'AW33AV  AR~�APZAN��AMt�AL��AJZAH��AEt�ABQ�AAA?�A=��A<  A:�uA8r�A6�HA5�A5��A4��A3dZA1�A0�A/�7A.M�A-�PA,��A*��A(�yA&ZA#�TA!�FA��Az�A  A�PAjA\)A�TAVAAt�AjA��A�A��A=qAA�A��AS�AM�A|�A
5?A�uA�;A�!AbNA\)A%A�HAQ�Ax�A1A�TA�A��A��A1A�A��A�;A-AC�A �AK�A ��A ĜA �DA {@���@�ȴ@�5?@���@��@�7L@��@�z�@�  @��
@�\)@��@�&�@�A�@��@��H@�M�@���@�@�p�@���@�ƨ@�@�^@�/@�1@�S�@�"�@�S�@���@�G�@�/@�O�@��-@�R@�C�@�@��H@�^5@�h@��@�j@���@�I�@�K�@�{@�F@��@�?}@�R@�ȴ@�F@�l�@��@�@�hs@�bN@�o@ާ�@�$�@�/@ۍP@�ȴ@��@�@���@�hs@�%@ؼj@�I�@���@׮@��@���@և+@�ff@֏\@�;d@�+@�l�@׶F@�dZ@�n�@պ^@��@Լj@�Z@Ӆ@�+@�\)@�5?@��@Ѳ-@щ7@�7L@�X@���@��@�9X@��@�ƨ@�dZ@�C�@�+@�
=@��@�ff@�J@͑h@���@��@ˮ@�t�@�
=@�n�@���@�`B@�V@�9X@ǥ�@�+@ƸR@�M�@ũ�@�?}@���@�9X@��
@�|�@�;d@�ff@�{@���@���@��@�/@��`@�I�@���@���@���@�=q@��@��@���@�@���@�`B@���@�Ĝ@�z�@�ƨ@�C�@��y@���@�^5@���@��@��D@�b@�ƨ@�\)@�K�@��@���@���@��^@��/@�1'@��;@��P@�t�@��y@��y@���@�E�@���@��T@���@�O�@��@��`@�bN@���@�ƨ@�33@��+@�E�@�$�@��@�J@���@�7L@��@� �@�b@�  @���@�dZ@�+@�ȴ@�~�@�-@�@���@�Ĝ@�Z@�  @�ƨ@���@��@�dZ@�S�@��y@���@�^5@�M�@�$�@�{@��@�p�@��9@�9X@�  @��@�K�@���@�=q@���@���@���@�x�@�7L@��@�V@��/@���@��D@�z�@�bN@��@��@���@��+@�$�@���@��@��j@��D@��@���@��@��P@�|�@�@�E�@�J@���@�7L@���@��j@��@�Z@�(�@���@�ƨ@�C�@��@��R@�ȴ@�ȴ@���@�5?@��-@�`B@�%@���@�Ĝ@��j@��D@�9X@� �@��m@�K�@���@���@���@�ff@�-@�@��@���@���@��@z=q@q&�@c�F@Yhs@Ol�@GK�@=/@6v�@.��@( �@$I�@   @x�@1@��@�@	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�sB�mB�sB�sB�sB�sB�sB�sB�mB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�mB�mB�sB�sB�sB�mB�sB�sB�sB�mB�sB�sB�sB�sB�sB�sB�B	�B	}�B	�B	�%B	�DB	�PB	�hB	��B	��B	�B	�3B	��B	��B
B
	7B
hB
@�B
v�B
�bB
�B
��B
��BBuB2-B]/BiyBjBs�Bn�B_;BQ�B;dB%�B�BDBB
�B
�B
�^B
�\B
x�B
dZB
M�B
1'B
/B
7LB
<jB
@�B
Q�B
]/B
�JB
�B
n�B
P�B
<jB
�B
%B	�B
�B
33B
I�B
A�B
�B	�B	��B	��B	�-B	��B	��B	�bB	�B	q�B	ffB	^5B	S�B	J�B	A�B	2-B	)�B	"�B	�B	�B	oB	
=B��B��B�B�B�yB�`B�NB�BB�;B�;B�;B�HB�`B�yB�B�B�B�B�B�B�B�fB�B��B�wB�RB�?B�9B�FB�XB�qB�dB�LB�?B�9B�?B�LB�RB�XB�dB�^B�RB�FB�3B�3B�9B�?B�-B�?B�qB�}BǮB��B��B�TB��B		7B	DB	\B	�B	�B	�B	#�B	G�B	T�B	R�B	O�B	P�B	R�B	R�B	T�B	XB	XB	YB	ZB	_;B	`BB	bNB	cTB	dZB	hsB	n�B	r�B	t�B	y�B	}�B	}�B	{�B	z�B	{�B	~�B	�B	�B	�B	�B	�%B	�+B	�%B	�B	�7B	�\B	�uB	�{B	��B	��B	��B	�FB	��B	ŢB	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	ƨB	ÖB	�RB	��B	��B	��B	��B	��B	��B	ȴB	ŢB	ÖB	B	ÖB	ŢB	ŢB	ƨB	ɺB	ɺB	ɺB	ɺB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	�B	�B	�#B	�;B	�5B	�5B	�/B	�#B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�BB	�BB	�NB	�fB	�fB	�sB	�B	�B	�B	�B	�yB	�yB	�yB	�yB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
%B
+B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B

=B
DB
DB
JB
PB
PB
PB
PB
PB
VB
VB
VB
VB
VB
VB
VB
VB
\B
\B
bB
bB
bB
hB
oB
oB
uB
uB
{B
{B
{B
{B
{B
{B
{B
{B
uB
uB
uB
uB
uB
uB
uB
uB
uB
uB
uB
uB
{B
uB
uB
uB
uB
uB
{B
uB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
%�B
49B
:^B
:^B
B�B
H�B
N�B
VB
[#B
_;B
dZB
ffB
l�B
q�B
u�B
y�B
|�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B�KB�DB�JB�LB�JB�JB�IB�LB�EB�LB�JB�LB�JB�LB�JB�LB�LB�LB�JB�LB�LB�JB�LB�LB�EB�DB�MB�KB�KB�DB�KB�MB�KB�FB�MB�KB�MB�MB�MB�KB�UB	~B	}�B	��B	��B	�B	�(B	�?B	�^B	�pB	��B	�
B	��B	��B
 �B
	B
>B
@YB
v�B
�8B
��B
��B
��B �BHB1�B] BiJBjSBs�BnkB_BQ�B;5B%�BYBB�B
�wB
��B
�+B
�-B
x�B
d(B
M�B
0�B
.�B
7B
<8B
@PB
Q�B
\�B
�B
��B
ncB
P�B
<5B
qB
�B	�uB
rB
2�B
I�B
ASB
rB	�B	ϨB	�TB	��B	��B	�]B	�,B	��B	qqB	f.B	]�B	S�B	J�B	AOB	1�B	)�B	"�B	B	gB	4B	
B��B��B�pB�WB�>B�)B�B�B� B�B�B�B�"B�=B�NB�WB�TB�QB�PB�bB�UB�*B��BʅB�8B�B�B��B�B�B�4B�&B�B� B��B�B�B�B�B�$B� B�B�B��B��B��B�B��B�B�0B�@B�qB̍BԽB�B��B	�B	B	B	OB	nB	kB	#�B	GnB	T�B	R�B	O�B	P�B	R�B	R�B	T�B	W�B	W�B	X�B	Y�B	^�B	`B	bB	cB	dB	h2B	nYB	rnB	t{B	y�B	}�B	}�B	{�B	z�B	{�B	~�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�6B	�9B	�XB	�vB	��B	�B	�BB	�`B	�aB	�sB	ΘB	ΙB	ϜB	ӸB	ӶB	ұB	ϝB	ʀB	�gB	�UB	�B	�HB	͐B	ΗB	ΚB	˅B	�~B	�qB	�`B	�RB	�OB	�RB	�aB	�`B	�gB	�xB	�yB	�xB	�zB	�yB	�yB	�wB	�xB	ʁB	ʀB	˄B	̌B	ϜB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ԼB	ӹB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�$B	�$B	�1B	�?B	�=B	�@B	�;B	�8B	�4B	�6B	�6B	�/B	�8B	�=B	�<B	�CB	�AB	�IB	�FB	�CB	�EB	�:B	�>B	�BB	�AB	�IB	�IB	�HB	�LB	�OB	�OB	�OB	�\B	�UB	�[B	�[B	�[B	�\B	�^B	�]B	�]B	�aB	�bB	�hB	�hB	�iB	�gB	�iB	�hB	�gB	�gB	�gB	�hB	�nB	�sB	�rB	�tB	�zB	�sB	�rB	�nB	�wB	�xB	�zB	�zB	��B	�mB	�zB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B
	�B
B
 B
B

B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
 B
$B
,B
,B
1B
2B
:B
9B
9B
7B
8B
8B
7B
7B
1B
0B
0B
/B
0B
1B
.B
2B
/B
2B
1B
1B
7B
1B
3B
0B
1B
2B
7B
/B
/B
6B
9B
:B
6B
>B
=B
>B
<B
BB
CG�O�B
KB
UB
uB
%�B
3�B
:B
:B
BKB
HqB
N�B
U�B
Z�B
^�B
dB
f!B
lHB
qfB
u�B
y�B
|�B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.52 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051007562019040510075620190405100756  AO  ARCAADJP                                                                    20181121125949    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121125949  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121125949  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100756  IP                  G�O�G�O�G�O�                