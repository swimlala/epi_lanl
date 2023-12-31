CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T12:59:46Z creation      
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
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20181121125946  20190405100754  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @�ȦF<�1   @�Ȧ�J�@0�+J�d�     1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B���C   C  C  C  C�C
�C  C  C  C  C  C�fC  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CY�fC\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Cs�fCv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDy,�D� D�<�D�ffD��fD��D�9�D�|�D��fD�3D�9�D�y�D�� D�3D�<�Dڌ�D��fD�fD�FfD�3D�y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�
=@У�AQ�A(Q�AHQ�AhQ�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B{B
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
=B��
C �C�C�C�C��C
��C�C�C�C�C�Ck�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZk�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ctk�Cv�Cx�Cz�C|�C~�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�O\C�O\C�B�C�5�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�B�C�5�C�B�C�B�C�B�C�B�C�B�D !HD �HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD	!HD	�HD
!HD
�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD!HD�HD�D�HD!HD�HD!HD�HD!HD�HD!HD�HD !HD �HD!!HD!�HD"!HD"�HD#!HD#�HD$!HD$�HD%!HD%�HD&!HD&�HD'!HD'�HD(!HD(�HD)!HD)�HD*!HD*�HD+!HD+�HD,!HD,�HD-!HD-�HD.!HD.�HD/!HD/�HD0!HD0�HD1!HD1�HD2!HD2�HD3!HD3�HD4!HD4�HD5!HD5�HD6!HD6�HD7!HD7�HD8!HD8�HD9!HD9�HD:!HD:�HD;!HD;�HD<!HD<�HD=!HD=�HD>!HD>�HD?!HD?�HD@!HD@�HDA!HDA�HDB!HDB�HDC!HDC�HDD!HDD�HDE!HDE�HDF!HDF�HDG!HDG�HDH!HDH�HDI!HDI�HDJ!HDJ�HDK!HDK�HDL!HDL�HDM!HDM�HDN!HDN�HDO!HDO�HDP!HDP�HDQ!HDQ�HDR!HDR�HDS!HDS�HDT!HDT�HDU!HDU�HDV!HDV�HDW!HDW�HDX!HDX�HDY!HDY�HDZ!HDZ�HD[!HD[�HD\!HD\�HD]!HD]�HD^!HD^�HD_!HD_�HD`!HD`�HDa!HDa�HDb!HDb�HDc!HDc�HDd!HDd�HDe!HDe�HDf!HDf�HDg!HDg�HDh!HDh�HDi!HDi�HDj!HDj�HDk!HDk�HDl!HDl�HDm!HDm�HDn!HDn�HDo!HDo�HDp!HDp�HDq!HDq�HDr!HDr�HDs!HDs�HDt!HDt��DyND� �D�MqD�w
D��
D�qD�J>D��qD��
D��D�J>D��>D��D��D�MqDڝqD��
D�
D�W
D��D��>11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�1'A�33A�1'A�1'A�1'A�9XA�=qA�=qA�=qA�?}A�?}A�9XA�5?A�5?A�-A��mA�jA�l�A��A���A�7A�bNA�M�A�&�A��yA�-A� �A���Aߗ�Aޙ�A܉7A��/A�VA��;Aؗ�A�O�A�t�Aة�A؏\A�"�A�;dA�bNA�z�A��;AҸRA�v�A�jA�bA��AѶFA�n�AμjA��
A�
=A��`A˲-A��;A�  A�bA�?}Aǟ�A���A�^5A��yA��A�hsA��#A�;dA��A���A��A�hsA�t�A�Q�A�$�A��A���A���A���A�bNA�Q�A���A�O�A�{A��A�ZA���A��A�A��jA���A�9XA�ȴA�I�A���A�G�A�jA��#A�jA���A��A�v�A��^A�1'A�z�A|�+Ap��AmAi%Afv�Ab~�Aa�A`��A_�TA^r�A]�A[p�AY��AV-AS�TAR��AQAOO�AM��AL��AL �AJ�AIƨAIoAH�\AG��AF=qAD��ACAB�RAA�mA?t�A<�+A<E�A;��A;�A9�PA8�A6�A3�A0$�A.�yA,r�A+�7A*��A)ƨA)C�A(5?A'�A&ȴA&�HA'�FA(ffA(��A(�DA'�7A%��A$  A#O�A!��A ~�A��A�mA��A%A  A��AK�A��AVA�;A^5A �A�hA��A�-A|�A%A�DAE�Al�A��Az�A�^AC�A�DA�At�AVA-A �AbA��A��AQ�A�#A/A~�AZA5?AA�`Av�A1A33A
�+A
A�A	�-A	S�A	%AffAx�A�/AȴA�!Az�A�#AS�A�A��A|�A\)AG�A/A��A�jAI�A��Al�A;dA ��A �/A ffA @���@�K�@��@��P@��@��@�9X@�S�@��y@�V@��@���@�A�@��^@�9X@���@�\)@�;d@�o@��@�+@��@�R@��@�"�@�+@��@�{@�7@���@�z�@��m@旍@�`B@�w@��#@�b@��H@�5?@��#@ݑh@�G�@��@ܣ�@ۥ�@�K�@��@�ȴ@�@�`B@�&�@�V@���@أ�@�1'@��@׾w@�C�@���@�G�@�/@��@���@ԃ@�1'@�1@җ�@���@Л�@�r�@�Q�@���@���@�?}@Ѳ-@�j@�\)@�E�@�X@���@� �@˥�@ˍP@�l�@��y@�^5@���@�O�@ȓu@��@��
@�|�@�C�@Ɨ�@�M�@Ł@�7L@��@�1'@�o@�ff@��#@� �@��@�$�@���@��T@��h@�x�@��@��7@��@��@�hs@��`@��@��F@�K�@�+@��@��!@�ff@��@���@�z�@��@��F@�ȴ@��+@�^5@���@��T@��T@��#@�x�@�V@��/@�Ĝ@�9X@��w@�+@���@�@�/@���@�I�@��;@�C�@�~�@�M�@�J@���@�G�@��@�Ĝ@��u@�9X@��w@�C�@���@�5?@��@���@�&�@��@�1'@���@�|�@�+@�+@��@��!@��T@��-@��-@��h@�O�@�&�@��/@��@��
@�S�@��@���@��@�v�@��@��7@�/@���@��D@�I�@��@��w@�l�@�C�@��H@�M�@��^@���@�p�@�?}@��@�%@�Ĝ@���@�r�@�1@���@�\)@�+@��@�^5@�E�@���@��`@��u@�bN@�I�@�A�@�(�@��@�1@���@�ƨ@�dZ@�;d@�33@�"�@�@��R@�~�@�n�@�M�@�$�@�@�@�O�@�V@���@���@�9X@�t�@�K�@�33@�"�@��@��!@�^5@�1'@��F@�  @�&�@y7L@o|�@dZ@\��@P�`@I�@@r�@9&�@1�^@,�@%��@!&�@(�@�y@�H@1'@o11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�1'A�33A�1'A�1'A�1'A�9XA�=qA�=qA�=qA�?}A�?}A�9XA�5?A�5?A�-A��mA�jA�l�A��A���A�7A�bNA�M�A�&�A��yA�-A� �A���Aߗ�Aޙ�A܉7A��/A�VA��;Aؗ�A�O�A�t�Aة�A؏\A�"�A�;dA�bNA�z�A��;AҸRA�v�A�jA�bA��AѶFA�n�AμjA��
A�
=A��`A˲-A��;A�  A�bA�?}Aǟ�A���A�^5A��yA��A�hsA��#A�;dA��A���A��A�hsA�t�A�Q�A�$�A��A���A���A���A�bNA�Q�A���A�O�A�{A��A�ZA���A��A�A��jA���A�9XA�ȴA�I�A���A�G�A�jA��#A�jA���A��A�v�A��^A�1'A�z�A|�+Ap��AmAi%Afv�Ab~�Aa�A`��A_�TA^r�A]�A[p�AY��AV-AS�TAR��AQAOO�AM��AL��AL �AJ�AIƨAIoAH�\AG��AF=qAD��ACAB�RAA�mA?t�A<�+A<E�A;��A;�A9�PA8�A6�A3�A0$�A.�yA,r�A+�7A*��A)ƨA)C�A(5?A'�A&ȴA&�HA'�FA(ffA(��A(�DA'�7A%��A$  A#O�A!��A ~�A��A�mA��A%A  A��AK�A��AVA�;A^5A �A�hA��A�-A|�A%A�DAE�Al�A��Az�A�^AC�A�DA�At�AVA-A �AbA��A��AQ�A�#A/A~�AZA5?AA�`Av�A1A33A
�+A
A�A	�-A	S�A	%AffAx�A�/AȴA�!Az�A�#AS�A�A��A|�A\)AG�A/A��A�jAI�A��Al�A;dA ��A �/A ffA @���@�K�@��@��P@��@��@�9X@�S�@��y@�V@��@���@�A�@��^@�9X@���@�\)@�;d@�o@��@�+@��@�R@��@�"�@�+@��@�{@�7@���@�z�@��m@旍@�`B@�w@��#@�b@��H@�5?@��#@ݑh@�G�@��@ܣ�@ۥ�@�K�@��@�ȴ@�@�`B@�&�@�V@���@أ�@�1'@��@׾w@�C�@���@�G�@�/@��@���@ԃ@�1'@�1@җ�@���@Л�@�r�@�Q�@���@���@�?}@Ѳ-@�j@�\)@�E�@�X@���@� �@˥�@ˍP@�l�@��y@�^5@���@�O�@ȓu@��@��
@�|�@�C�@Ɨ�@�M�@Ł@�7L@��@�1'@�o@�ff@��#@� �@��@�$�@���@��T@��h@�x�@��@��7@��@��@�hs@��`@��@��F@�K�@�+@��@��!@�ff@��@���@�z�@��@��F@�ȴ@��+@�^5@���@��T@��T@��#@�x�@�V@��/@�Ĝ@�9X@��w@�+@���@�@�/@���@�I�@��;@�C�@�~�@�M�@�J@���@�G�@��@�Ĝ@��u@�9X@��w@�C�@���@�5?@��@���@�&�@��@�1'@���@�|�@�+@�+@��@��!@��T@��-@��-@��h@�O�@�&�@��/@��@��
@�S�@��@���@��@�v�@��@��7@�/@���@��D@�I�@��@��w@�l�@�C�@��H@�M�@��^@���@�p�@�?}@��@�%@�Ĝ@���@�r�@�1@���@�\)@�+@��@�^5@�E�@���@��`@��u@�bN@�I�@�A�@�(�@��@�1@���@�ƨ@�dZ@�;d@�33@�"�@�@��R@�~�@�n�@�M�@�$�@�@�@�O�@�V@���@���@�9X@�t�@�K�@�33@�"�@��@��!@�^5@�1'@��F@�  @�&�@y7L@o|�@dZ@\��@P�`@I�@@r�@9&�@1�^@,�@%��@!&�@(�@�y@�H@1'@o11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�B	�B	�B	�B	�#B	�#B	�#B	�#B	�#B	�#B	�#B	�)B	�/B	�/B	�/B	�NB
B
JB
uB
�B
�B
�B
�B
�B
�B
�B
�B
#�B
&�B
%�B
JB	��B
 �B
%�B
(�B
.B
=qB
\)B
ffB
]/B
C�B
�B	��B	�fB
hB
m�B
�1B
��B
��B
��B
YB
L�B
l�B
�VB
r�B
�#BJB�B�B7LBS�B]/B^5Bo�Bo�BjBr�B��B�VB��B�!B�FB�wB��B�;B�B�B�B�
B��BƨB�B�B�B�FB�?B�'B��B��B�B��B��Bp�BM�B
��B
��B
��B
�VB
y�B
cTB
XB
M�B
A�B
6FB
�B	�3B	u�B	\)B	K�B	>wB	.B	&�B	$�B	 �B	�B	{B	PB	+B��B��B�B�B�sB�`B�TB�HB�BB�HB�BB�BB�)B�B�B�B�BB�mB�TB��B��B��B	  B	DB	hB	�B	hB	1B	B	  B��B��B��B��B��B	B	bB	#�B	:^B	I�B	O�B	P�B	O�B	K�B	H�B	D�B	=qB	6FB	5?B	49B	1'B	/B	1'B	2-B	5?B	>wB	F�B	I�B	Q�B	XB	_;B	dZB	k�B	w�B	� B	�%B	�%B	�B	�B	� B	�B	�B	�B	�1B	�JB	�hB	�uB	�uB	�oB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�?B	�XB	�dB	�jB	�jB	�wB	�wB	��B	B	ƨB	ɺB	ɺB	ȴB	ǮB	ǮB	ƨB	ŢB	B	�qB	�XB	�'B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�FB	�LB	�jB	��B	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ƨB	ĜB	ÖB	ŢB	ǮB	ǮB	ǮB	ɺB	ɺB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	��B	��B	��B	��B	�B	�/B	�;B	�HB	�`B	�`B	�ZB	�TB	�NB	�HB	�NB	�NB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�fB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
1B
1B
1B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B
1B
1B
1B
	7B
	7B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
1B
1B
	7B

=B

=B

=B

=B

=B

=B

=B
DB
DB
JB
PB
JB
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
VB
VB
VB
PB
\B
bB
bB
bB
\B
VB
VB
PB
PB
PB
PB
PB
PB
PB
PB
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
\B
\B
\B
\B
\B
\B
hB
hB
hB
hB
hB
hB
�B
{B
 �B
�B
.B
7LB
=qB
C�B
F�B
K�B
O�B
W
B
[#B
`BB
e`B
hsB
l�B
p�B
u�B
x�B
|�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�&B
�B
"B
MB
jB
�B
�B
�B
�B
�B
�B
�B
#�B
&�B
%�B
!B	��B
 �B
%�B
(�B
-�B
=KB
[�B
fAB
]B
CmB
YB	��B	�>B
>B
mgB
�B
��B
�`B
�WB
X�B
L�B
laB
�-B
r�B
��B"B[B�B7"BS�B]B^BoqBopBjSBr�B�TB�,B�cB��B�B�JB��B�B��B��B��B��B��B�yB��B��B��B�B�B��B��B�yB��B��B�YBpvBM�B
��B
ΩB
��B
�$B
y�B
c"B
W�B
M�B
AVB
6B
LB	��B	u�B	[�B	K�B	>DB	-�B	&�B	$�B	 �B	bB	DB	B	�B��B��B�pB�UB�:B�&B�B�B�B�B�	B�	B��B��B��B��B�B�4B�B��B��B��B��B	
B	/B	KB	-B	�B	�B��B��B��B��B��B��B	�B	'B	#�B	:!B	I}B	O�B	P�B	O�B	K�B	HvB	D_B	=5B	6
B	5 B	3�B	0�B	.�B	0�B	1�B	5B	>9B	FkB	I}B	Q�B	W�B	^�B	dB	kFB	w�B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	�*B	�8B	�8B	�3B	�1B	�DB	�IB	�VB	�\B	�]B	�UB	�UB	�\B	�eB	�gB	�fB	�oB	�kB	�mB	�uB	�{B	�uB	�tB	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�$B	�)B	�+B	�8B	�5B	�FB	�QB	�gB	�{B	�yB	�sB	�oB	�pB	�eB	�cB	�OB	�2B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�*B	�JB	�lB	�yB	ʂB	̌B	ΙB	ѫB	ӵB	ԾB	ԼB	ԾB	ӸB	үB	СB	˅B	�hB	�YB	�TB	�aB	�mB	�lB	�mB	�yB	�xB	�sB	�sB	ˇB	͑B	ΙB	ΕB	ϛB	ϜB	СB	ѩB	ѪB	ѧB	ѫB	ФB	УB	ФB	ѨB	ѩB	��B	��B	��B	ӷB	ѩB	ҲB	ԻB	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�$B	�0B	�2B	�0B	�1B	�9B	�CB	�HB	�MB	�XB	�^B	�_B	�\B	�`B	�VB	�NB	�NB	�KB	�bB	�hB	�lB	�mB	�sB	�rB	�zB	�B	�sB	�dB	�[B	�ZB	�[B	�jB	�mB	�|B	�tB	�sB	�sB	�lB	�sB	�zB	�xB	�rB	�zB	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B
	�B
	�B
	�B
	�B
	�B
B
 B
	B
	B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
!B
B
 B
B
B
B

B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
$B
%B
%B
$B
#B
$B
?B
9B
 �B
{B
-�B
7
B
=.B
CTB
FcB
K�B
O�B
V�B
Z�B
_�B
eB
h/B
lHB
p`B
uB
x�B
|�B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.52 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051007542019040510075420190405100754  AO  ARCAADJP                                                                    20181121125946    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121125946  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121125946  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100754  IP                  G�O�G�O�G�O�                