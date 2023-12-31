CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-06-02T19:16:06Z AOML 3.0 creation; 2016-08-07T21:51:30Z UW 3.1 conversion     
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
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20160602191606  20160825183417  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               }A   AO  5287_9017_125                   2C  D   APEX                            6529                            072314                          846 @װ�8��1   @װ�-���@0R-V�d���E�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    }A   B   B   @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(�C*�C,  C-�fC0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ�fDK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy��D��D�6fD���D�ٚD�fD�P D�|�D��3D�fD�FfD��3D��fD�	�D�I�Dڃ3D���D��D�FfD�D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @aG�@���@У�AQ�A(Q�AHQ�AhQ�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B{B
{B{B{B"{B*{B2{B:{BB{BJ{BR{BZ{Bb{Bj{Br{Bz{B�
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
=B�=pB�=pB�
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
=B�=pB�
=B�
=C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(��C*��C,�C.k�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�O\C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�O\C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�D !HD �HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD	!HD	�HD
!HD
�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD !HD �HD!!HD!�HD"!HD"�HD#!HD#�HD$!HD$�HD%!HD%�HD&!HD&�HD'!HD'�HD(!HD(�HD)!HD)�HD*!HD*�HD+!HD+�HD,!HD,�HD-!HD-�HD.!HD.�HD/!HD/�HD0!HD0�HD1!HD1�HD2!HD2�HD3!HD3�HD4!HD4�HD5!HD5�HD6!HD6�HD7!HD7�HD8!HD8�HD9!HD9�HD:!HD:�HD;!HD;�HD<!HD<�HD=!HD=�HD>!HD>�HD?!HD?�HD@!HD@�HDA!HDA�HDB!HDB�HDC!HDC�HDD!HDD�HDE!HDE�HDF!HDF�HDG!HDG�HDH!HDH�HDI!HDI�HDJ!HDJ��DK!HDK�HDL!HDL�HDM!HDM�HDN!HDN�HDO!HDO�HDP!HDP�HDQ!HDQ�HDR!HDR�HDS!HDS�HDT!HDT�HDU!HDU�HDV!HDV�HDW!HDW�HDX!HDX�HDY!HDY�HDZ!HDZ�HD[!HD[�HD\!HD\�HD]!HD]�HD^!HD^�HD_!HD_�HD`!HD`�HDa!HDa�HDb!HDb�HDc!HDc�HDd!HDd�HDe!HDe�HDf!HDf�HDg!HDg�HDh!HDh�HDi!HDi�HDj!HDj�HDk!HDk�HDl!HDl�HDm!HDm�HDn!HDn�HDo!HDo�HDp!HDp�HDq!HDq�HDr!HDr�HDs!HDs�HDt!HDt��Dy�D�qD�G
D��qD��>D�
D�`�D��qD���D�
D�W
D���D��
D�>D�Z>Dړ�D��qD�qD�W
D�>D��
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��yA��yA��A��A��A��A��A��A��HA؝�A�Q�A�9XA��A׬A�1'A��A��`A��TA���A։7A���AՏ\A�p�A�I�A���Aԣ�Aԕ�A�XA��A�|�A�{Aҧ�A�XAћ�A�hsA��;AЗ�AГuAЁA�/Aϕ�A�;dA���A͍PA��HA�?}A�/A�bNA�(�Aˉ7A�l�A��`A�^5A�C�A���A�ȴAɩ�A���A�1A�33A���A���A�^5AîA�33A�r�A�VA�  A�9XA���A�E�A��A���A�7LA��A�z�A�ĜA�1'A�1'A���A���A�n�A�"�A�O�A���A���A�ƨA�bA�
=A��#A�E�A���A��A�A���A��A��A���A���A�(�A~�DA}x�A|��A|bNA|VA|A�A{�;A{dZAy�-Ax-Aw&�Atn�Aq|�Ao7LAn�Al{Ai��AfĜAd�A_�mAZ�!AV��AT�\AR��AO`BAI�PAG�^AC�
A>  A;dZA;�A:z�A8��A7C�A6I�A3�A2ffA1�-A0�A0n�A/��A,�yA*��A)`BA(��A&^5A$$�A#�^A#�A#hsA#O�A"��A"�DA"n�A"M�A"�A!�^Ap�A��A �A��At�Ar�A�/A�AE�A7LA�AhsAA�A$�A-A��A�mAƨAl�A��A�A5?AffA{A9XA��Av�A�-A��A�HA�`A��AG�A��AVA�An�A�A��A�;A\)A/A
�/A
�DA
I�A	��A	;dAM�A��A�;A��Ax�A�HAv�A-A  AXAoAz�A  A�
A�hAC�A�/AE�A�^A|�AG�A v�@�ƨA @�(�@��R@���@�J@���@�dZ@�G�@�\)@�!@��@��@�C�@�R@���@���@�^5@��H@��@�v�@�+@�33@��@�@�x�@���@�1@���@�1@�n�@��@��D@�K�@��H@�X@�Z@���@��@�$�@��`@�ff@��@�V@ԃ@҇+@�t�@�+@��@θR@�ff@�@��@�X@�r�@�o@ʟ�@�~�@�n�@�=q@��T@�`B@�G�@ȃ@��@�b@ǥ�@ǶF@���@�  @�C�@��@�ȴ@Ƈ+@�ff@ź^@�hs@���@�I�@�  @Õ�@��@���@��H@�ȴ@¸R@§�@�E�@�X@��@��j@��@�z�@�I�@��m@���@�C�@��@�ȴ@�v�@�M�@���@�hs@�`B@�O�@�V@��@���@�I�@��w@���@��@���@�G�@�&�@���@���@�Ĝ@��@��@��@���@���@��\@�v�@�=q@��@���@�%@�I�@��;@�"�@���@���@��9@�z�@�r�@�j@�Z@�A�@� �@�1@��m@���@�"�@���@��-@�O�@�%@�Ĝ@��@��@�C�@��@�
=@��y@�M�@��@�@�`B@�%@��`@�Ĝ@���@�r�@��@�1@�1@�1@�1@�b@�b@��w@��\@�V@�M�@�J@���@�p�@�/@��`@�Ĝ@�9X@���@�+@���@���@�ff@�-@��@��-@�7L@�Ĝ@�z�@�Q�@�9X@�1'@�  @���@��P@�K�@��@��+@���@�7L@��`@�9X@���@���@�|�@��@���@��\@�E�@�{@���@��7@�`B@�G�@���@��D@��@��@��;@���@���@�|�@�;d@��H@�~�@�{@�x�@�V@�Ĝ@�j@��@��m@��
@��@���@�E�@��@���@��#@���@��@��@��/@���@���@���@��9@��@��@���@�S�@�K�@�+@��H@���@�ȴ@��!@��D@��@�~�@|z�@q��@kƨ@c�F@W�@MO�@E?}@<�@4�/@.�+@'�w@"~�@@�@$�@G�@�@	�7111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A��A��yA��yA��A��A��A��A��A��A��HA؝�A�Q�A�9XA��A׬A�1'A��A��`A��TA���A։7A���AՏ\A�p�A�I�A���Aԣ�Aԕ�A�XA��A�|�A�{Aҧ�A�XAћ�A�hsA��;AЗ�AГuAЁA�/Aϕ�A�;dA���A͍PA��HA�?}A�/A�bNA�(�Aˉ7A�l�A��`A�^5A�C�A���A�ȴAɩ�A���A�1A�33A���A���A�^5AîA�33A�r�A�VA�  A�9XA���A�E�A��A���A�7LA��A�z�A�ĜA�1'A�1'A���A���A�n�A�"�A�O�A���A���A�ƨA�bA�
=A��#A�E�A���A��A�A���A��A��A���A���A�(�A~�DA}x�A|��A|bNA|VA|A�A{�;A{dZAy�-Ax-Aw&�Atn�Aq|�Ao7LAn�Al{Ai��AfĜAd�A_�mAZ�!AV��AT�\AR��AO`BAI�PAG�^AC�
A>  A;dZA;�A:z�A8��A7C�A6I�A3�A2ffA1�-A0�A0n�A/��A,�yA*��A)`BA(��A&^5A$$�A#�^A#�A#hsA#O�A"��A"�DA"n�A"M�A"�A!�^Ap�A��A �A��At�Ar�A�/A�AE�A7LA�AhsAA�A$�A-A��A�mAƨAl�A��A�A5?AffA{A9XA��Av�A�-A��A�HA�`A��AG�A��AVA�An�A�A��A�;A\)A/A
�/A
�DA
I�A	��A	;dAM�A��A�;A��Ax�A�HAv�A-A  AXAoAz�A  A�
A�hAC�A�/AE�A�^A|�AG�A v�@�ƨA @�(�@��R@���@�J@���@�dZ@�G�@�\)@�!@��@��@�C�@�R@���@���@�^5@��H@��@�v�@�+@�33@��@�@�x�@���@�1@���@�1@�n�@��@��D@�K�@��H@�X@�Z@���@��@�$�@��`@�ff@��@�V@ԃ@҇+@�t�@�+@��@θR@�ff@�@��@�X@�r�@�o@ʟ�@�~�@�n�@�=q@��T@�`B@�G�@ȃ@��@�b@ǥ�@ǶF@���@�  @�C�@��@�ȴ@Ƈ+@�ff@ź^@�hs@���@�I�@�  @Õ�@��@���@��H@�ȴ@¸R@§�@�E�@�X@��@��j@��@�z�@�I�@��m@���@�C�@��@�ȴ@�v�@�M�@���@�hs@�`B@�O�@�V@��@���@�I�@��w@���@��@���@�G�@�&�@���@���@�Ĝ@��@��@��@���@���@��\@�v�@�=q@��@���@�%@�I�@��;@�"�@���@���@��9@�z�@�r�@�j@�Z@�A�@� �@�1@��m@���@�"�@���@��-@�O�@�%@�Ĝ@��@��@�C�@��@�
=@��y@�M�@��@�@�`B@�%@��`@�Ĝ@���@�r�@��@�1@�1@�1@�1@�b@�b@��w@��\@�V@�M�@�J@���@�p�@�/@��`@�Ĝ@�9X@���@�+@���@���@�ff@�-@��@��-@�7L@�Ĝ@�z�@�Q�@�9X@�1'@�  @���@��P@�K�@��@��+@���@�7L@��`@�9X@���@���@�|�@��@���@��\@�E�@�{@���@��7@�`B@�G�@���@��D@��@��@��;@���@���@�|�@�;d@��H@�~�@�{@�x�@�V@�Ĝ@�j@��@��m@��
@��@���@�E�@��@���@��#@���@��@��@��/@���@���@���@��9@��@��@���@�S�@�K�@�+@��H@���@�ȴG�O�@��D@��@�~�@|z�@q��@kƨ@c�F@W�@MO�@E?}@<�@4�/@.�+@'�w@"~�@@�@$�@G�@�@	�7111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�XB
�XB
�^B
�dB
�jB
�jB
�jB
�jB
�jB
�qB
��B
��B
�}B
�jB
�XB
�RB
�FB
�FB
�FB
�FB
�FB
��B
��B
��B
��B
�;B
�ZB
�B
�B
�B
�B
�mB
�B
�B
��B  B1BJB�B/B33B-B%�B1B
��B%B  BB8RBJ�B\)BaHBr�B�%B�1B�hB��B��B�XB�5B��B��BPB8RB6FB%�BA�B`BBu�B~�BiyBVBYBdZBN�B<jB.B$�B�B�B+B�B��B�5B�'B��B��BS�B:^B �BB
�;B
��B
��B
��B
�jB
�B
��B
�+B
}�B
{�B
t�B
p�B
k�B
hsB
gmB
e`B
bNB
_;B
S�B
I�B
@�B
1'B
�B
oB
	7B	��B	�sB	��B	�wB	�DB	bNB	S�B	J�B	A�B	.B	�B		7B��B�5B�B�B��B��B��B��B��BɺBȴBƨBĜB��B�}B�qB�dB�XB�LB�dB�^B�^B�XB�RB�RB�LB�LB�FB�?B�3B�RB�^B�dBBÖBĜB�RB�!B�-B�B��B�B��B��B�B�B�FB�LB�9B�B��B��B��B��B�;B��B��B��B��B	B	B	%B	PB	uB	�B	{B	oB	�B	�B	$�B	'�B	(�B	)�B	.B	-B	-B	.B	.B	-B	,B	+B	+B	+B	,B	-B	.B	33B	5?B	9XB	<jB	=qB	=qB	=qB	=qB	>wB	?}B	?}B	@�B	A�B	K�B	T�B	N�B	W
B	_;B	gmB	m�B	o�B	p�B	r�B	v�B	|�B	~�B	�=B	�bB	��B	��B	��B	�oB	�bB	��B	�B	�!B	�'B	�-B	�LB	�RB	�FB	�FB	�FB	�RB	�RB	�RB	�LB	�FB	�LB	�LB	�LB	�LB	�LB	�?B	�FB	�^B	�qB	�jB	�LB	�9B	�LB	�LB	�FB	�LB	�LB	�LB	�XB	�jB	�}B	�}B	�}B	�}B	�}B	�wB	�wB	�qB	�^B	�^B	�jB	�qB	��B	��B	B	��B	��B	��B	��B	ĜB	ĜB	ĜB	ĜB	ĜB	ƨB	ǮB	ǮB	ǮB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�
B	�
B	�B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�)B	�;B	�;B	�;B	�;B	�5B	�5B	�5B	�5B	�/B	�5B	�5B	�;B	�;B	�;B	�BB	�BB	�HB	�TB	�ZB	�ZB	�ZB	�`B	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
+B
	7B

=B

=B

=B

=B

=B

=B
DB
DB
DB
DB
DB
DB
DB
DB
DB
DB
JB
PB
VB
VB
VB
VB
VB
VB
VB
\B
\B
\B
bB
bB
bB
bB
bB
bB
bB
hB
bB
bB
bB
bB
\B
bB
bB
bB
bB
bB
bB
bB
bB
hB
hB
oB
oB
oB
oB
oB
oB
uB
�B
%�B
,B
/B
8RB
9XB
>wB
E�B
J�B
Q�B
VB
[#B
`BB
ffB
iyB
jB
l�B
q�B
x�B
|�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
�0B
�0B
�8B
�@B
�DB
�EB
�FB
�DB
�DB
�LB
�cB
�dB
�WB
�DB
�4B
�.B
�"B
� B
�#B
�#B
�!B
�dB
ͭB
ϸB
��B
�B
�4B
�kB
�B
�~B
�VB
�GB
�cB
�B
��B
��B
B%B�B.�B3
B,�B%�B
B
��B�B
��B�B8(BJ�B[�BaBr�B��B�B�?B�gB�|B�/B�B��B��B!B8)B6B%�BA]B`Bu�B~�BiLBU�BX�Bd)BN�B<>B-�B$�BXB�B*�BVB��B�B��B��B��BS�B:-B �B�B
�B
��B
��B
��B
�9B
��B
��B
��B
}�B
{�B
t�B
pqB
kRB
hAB
g?B
e.B
b B
_
B
S�B
I�B
@VB
0�B
�B
@B
	B	��B	�EB	ѾB	�JB	�B	b%B	S�B	J�B	A`B	-�B	XB		B��B�B��B��B��BδBͩBʙBʙBɑBȊB�B�uB�aB�RB�HB�=B�,B�$B�:B�5B�5B�,B�)B�*B�#B�$B�B�B�
B�)B�7B�<B�cB�kB�tB�(B��B� B��B��B��B��B��B��B��B�B�"B�B��B��B�{B�qB��B�B�B��B��B��B	 �B	�B	�B	"B	EB	qB	LB	>B	cB	vB	$�B	'�B	(�B	)�B	-�B	,�B	,�B	-�B	-�B	,�B	+�B	*�B	*�B	*�B	+�B	,�B	-�B	3B	5B	9$B	<8B	=AB	=?B	=>B	=?B	>EB	?LB	?JB	@QB	AVB	K�B	T�B	N�B	V�B	_B	g7B	m`B	okB	poB	r|B	v�B	|�B	~�B	�B	�,B	�PB	�[B	�^B	�7B	�-B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�"B	�:B	�0B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�0B	�CB	�EB	�CB	�CB	�CB	�@B	�>B	�9B	�&B	�$B	�0B	�6B	�JB	�RB	�UB	�NB	�NB	�OB	�PB	�aB	�bB	�aB	�dB	�bB	�oB	�vB	�sB	�tB	�sB	�uB	�xB	ɀB	ʈB	ʆB	ʇB	͙B	ϢB	ШB	��B	��B	��B	��B	��B	��B	��B	ӻB	ҸB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ӽB	ӾB	ҹB	ҺB	ҹB	ѱB	ѲB	ЪB	ЪB	ѴB	ҷB	ҹB	ҷB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	� B	��B	��B	��B	��B	��B	��B	��B	� B	�B	��B	�	B	�B	�	B	�B	�B	�B	�B	�&B	�3B	�BB	�NB	�UB	�\B	�ZB	�ZB	�\B	�aB	�iB	�hB	�iB	�iB	�iB	�iB	�fB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B

 B
	�B
	�B
	�B

 B
	�B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
 B
B
&B
'B
%B
$B
%B
$B
#B
+B
$B
%B
%B
%B
B
$B
$B
$B
%B
$B
%B
$B
%B
(B
,B
/B
0B
/B
/B
1B
/G�O�B
MB
%�B
+�B
.�B
8B
9B
>8B
EbB
J�B
Q�B
U�B
Z�B
`B
f'B
i:B
j=B
lKB
qlB
x�B
|�B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.52 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451302016080714513020160807145130  AO  ARCAADJP                                                                    20160602191606    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160602191606  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160602191606  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145130  IP                  G�O�G�O�G�O�                