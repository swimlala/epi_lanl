CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-07-03T05:01:10Z AOML 3.0 creation; 2016-08-07T21:36:48Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20160703050110  20160825183352  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5286_8897_127                   2C  D   APEX                            6531                            072314                          846 @׸zt1   @׸z�/�T@6$�/���c2M���1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @���@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bo��Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz�C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX�fDYfDY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt` Dy��D�3D�FfD��3D���D���D�P D�|�D��fD��fD�)�D���D��3D��D�S3Dڀ D�ɚD� D�6fD�ffD��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��H@�{A
=A?
=A_
=A
=A��A��A��A��AυA߅A�A��BBBBB'B/B7B?BGBOBWB_BgBo\)BwBB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB�{B��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��B��HB��HC�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cz
>C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX��DY�DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt\)Dy��D�GD�DzD��GD���D���D�ND�z�D��zD��zD�'�D���D��GD�
�D�QGD�~D�ǮD�D�4zD�dzD��G11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��`A��`A��HA��TA��HA��/A���A���A���A���A�A�ĜA�ĜA���A�A���A͟�A͙�A͕�A͓uA͏\A͇+A�t�A�VA�=qA��A�z�Aˇ+A��yAǏ\A��A�5?Aţ�A�l�A�^5A�Q�A�;dA�
=A��A���AļjA�v�A�I�A�;dA� �A��/AÕ�A�;dA��TA�A�O�A��A��yA��#A��^A�I�A���A���A�;dA�/A���A�-A�VA���A���A�bA�VA���A��mA���A��A��hA���A���A�+A���A��A�^5A��A��A�~�A�~�A��A�n�A�l�A��wA�ffA��A�ȴA���A�ZA�=qA�O�A�(�A��A�t�A���A�p�A�JA���A��
A�r�A�G�A��A���A���A�I�A�{A�=qA��-A���A��mA�C�A��A��hA��A�p�A�;dA���A�bNA�&�A�-A��\A}��A}XA}33A{�^AyK�Aw/As�#Ao��Ai\)Ae�TAc��Ab=qAadZA^��A[7LAY+AWƨAU�AShsAP�`AO�AN�AM��ALM�AJJAH^5AF�RAE�hAD��AC�AB�A@ZA?7LA=p�A<�A;K�A:�`A8Q�A5"�A4=qA3�mA2�`A21'A1?}A0�+A/�A.I�A+�A)�A'�A&z�A%��A%�A$VA#;dA"  A jA��A�uA\)A�A-AJA�HA�-AZA�/A(�A��AĜA��A��A��Av�A��A�RA�A�Av�A
��A��A$�A��A�AdZA��AjA  A��AG�A�jA�DA$�A��A�A��A��AA�A��A ��@�t�@�V@�$�@�`B@��9@���@�-@��#@���@�`B@��@�`B@�1@�7L@�;d@�@��@�hs@�@�\)@�E�@�{@�p�@�&�@�u@���@��H@�@�l�@�^@�9X@߅@�o@ݩ�@�+@١�@�O�@���@ج@�z�@�Q�@��@ָR@�?}@�b@�S�@�~�@���@�
=@�ff@�%@�I�@�\)@�dZ@ϥ�@�dZ@Ͼw@� �@мj@��/@���@�%@���@�Z@�  @Ͼw@�|�@���@�E�@ͺ^@�z�@�(�@��m@˥�@�dZ@�;d@��@ʟ�@��@��@�@�`B@�G�@��@���@���@�S�@�
=@���@ċD@¸R@��7@�?}@�(�@��@��@��D@���@�I�@��P@��R@�J@��@��@�t�@�"�@��@�^5@�@��#@��^@�hs@��@��@�1'@��m@��
@��
@���@���@���@��@�`B@��/@��
@���@��@���@��\@��#@�G�@��/@���@���@�n�@�M�@��@��-@�/@���@���@��@��@���@�&�@�?}@�`B@��-@���@��7@�?}@���@��@��@�hs@��\@�G�@�G�@���@�7L@�V@�&�@���@�I�@�bN@���@�?}@�?}@�V@��@��/@�Z@�  @��@���@�{@���@��@�dZ@���@�^5@�@��T@�G�@��@���@��@��@��@�  @��@�ȴ@�ff@�5?@��@���@���@�hs@���@���@��u@��D@��D@�z�@�r�@�j@�Z@��m@�|�@�C�@�"�@���@���@��+@�V@��@��^@�`B@�&�@��@��9@��@���@�t�@�33@�
=@���@�E�@�@��@��-@�hs@���@��/@��D@� �@��;@���@��@���@�\)@�"�@�ȴ@��+@�-@��@�@�X@�&�@��@���@��j@��@��F@���@�+@��@��R@��+@�ff@�=q@��@�p�@���@���@��P@��F@���@K�@s�m@i&�@Z��@SdZ@J�!@E@AG�@<�j@7�;@1�^@,�@&ȴ@"�\@��@��@ �@"�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A��`A��`A��HA��TA��HA��/A���A���A���A���A�A�ĜA�ĜA���A�A���A͟�A͙�A͕�A͓uA͏\A͇+A�t�A�VA�=qA��A�z�Aˇ+A��yAǏ\A��A�5?Aţ�A�l�A�^5A�Q�A�;dA�
=A��A���AļjA�v�A�I�A�;dA� �A��/AÕ�A�;dA��TA�A�O�A��A��yA��#A��^A�I�A���A���A�;dA�/A���A�-A�VA���A���A�bA�VA���A��mA���A��A��hA���A���A�+A���A��A�^5A��A��A�~�A�~�A��A�n�A�l�A��wA�ffA��A�ȴA���A�ZA�=qA�O�A�(�A��A�t�A���A�p�A�JA���A��
A�r�A�G�A��A���A���A�I�A�{A�=qA��-A���A��mA�C�A��A��hA��A�p�A�;dA���A�bNA�&�A�-A��\A}��A}XA}33A{�^AyK�Aw/As�#Ao��Ai\)Ae�TAc��Ab=qAadZA^��A[7LAY+AWƨAU�AShsAP�`AO�AN�AM��ALM�AJJAH^5AF�RAE�hAD��AC�AB�A@ZA?7LA=p�A<�A;K�A:�`A8Q�A5"�A4=qA3�mA2�`A21'A1?}A0�+A/�A.I�A+�A)�A'�A&z�A%��A%�A$VA#;dA"  A jA��A�uA\)A�A-AJA�HA�-AZA�/A(�A��AĜA��A��A��Av�A��A�RA�A�Av�A
��A��A$�A��A�AdZA��AjA  A��AG�A�jA�DA$�A��A�A��A��AA�A��A ��@�t�@�V@�$�@�`B@��9@���@�-@��#@���@�`B@��@�`B@�1@�7L@�;d@�@��@�hs@�@�\)@�E�@�{@�p�@�&�@�u@���@��H@�@�l�@�^@�9X@߅@�o@ݩ�@�+@١�@�O�@���@ج@�z�@�Q�@��@ָR@�?}@�b@�S�@�~�@���@�
=@�ff@�%@�I�@�\)@�dZ@ϥ�@�dZ@Ͼw@� �@мj@��/@���@�%@���@�Z@�  @Ͼw@�|�@���@�E�@ͺ^@�z�@�(�@��m@˥�@�dZ@�;d@��@ʟ�@��@��@�@�`B@�G�@��@���@���@�S�@�
=@���@ċD@¸R@��7@�?}@�(�@��@��@��D@���@�I�@��P@��R@�J@��@��@�t�@�"�@��@�^5@�@��#@��^@�hs@��@��@�1'@��m@��
@��
@���@���@���@��@�`B@��/@��
@���@��@���@��\@��#@�G�@��/@���@���@�n�@�M�@��@��-@�/@���@���@��@��@���@�&�@�?}@�`B@��-@���@��7@�?}@���@��@��@�hs@��\@�G�@�G�@���@�7L@�V@�&�@���@�I�@�bN@���@�?}@�?}@�V@��@��/@�Z@�  @��@���@�{@���@��@�dZ@���@�^5@�@��T@�G�@��@���@��@��@��@�  @��@�ȴ@�ff@�5?@��@���@���@�hs@���@���@��u@��D@��D@�z�@�r�@�j@�Z@��m@�|�@�C�@�"�@���@���@��+@�V@��@��^@�`B@�&�@��@��9@��@���@�t�@�33@�
=@���@�E�@�@��@��-@�hs@���@��/@��D@� �@��;@���@��@���@�\)@�"�@�ȴ@��+@�-@��@�@�X@�&�@��@���@��j@��@��F@���@�+@��@��R@��+@�ff@�=q@��@�p�@���G�O�@��P@��F@���@K�@s�m@i&�@Z��@SdZ@J�!@E@AG�@<�j@7�;@1�^@,�@&ȴ@"�\@��@��@ �@"�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
)�B
)�B
(�B
(�B
(�B
(�B
)�B
(�B
(�B
oB
(�B
(�B
(�B
(�B
(�B
(�B
&�B
&�B
&�B
&�B
&�B
%�B
$�B
!�B
�B
�B
�B
�B
!�B
J�B
G�B
F�B
M�B
M�B
N�B
N�B
O�B
J�B
P�B
P�B
O�B
N�B
M�B
N�B
S�B
W
B
[#B
m�B
�1B
��B
ƨB
��BhB(�BN�BYBYBiyB�B�B��B�B'�B0!B9XB49B1'B7LBC�BO�BZBaHBdZBn�Bp�Br�Bu�Bz�B�B�+B�{B��B��B�hB�B�JB�=B�+Bw�BbNB<jB:^B/B�B�B%B\B�B'�BI�B>wB	7B�Bq�B:^B
�fB%B1B�BDB
��B
�B
�;B
ǮB
�9B
��B
�7B
^5B
,B
+B	�/B	�
B	�5B	�B	�#B	�B	��B	�?B	��B	z�B	E�B	B�TB�B��B	1B��B�B�B�NB�TB�HB�B��B��BȴBB�^B�3B�B�B��B��B��B��B��B��B�oB�\B�JB�1B�1B�+B�%B�+B�%B�+B�%B�%B�B�B�B�B�B�B� B� B~�B}�B~�B~�B}�B~�B�B� B~�B}�B}�B}�B�B�B�B�B�B�B�B�B�B�B�%B�+B�%B�B}�B|�B~�B�B�+B�7B�JB�PB�PB�PB�PB�JB�JB�PB�JB�JB�DB�=B�1B�+B�B�%B�+B�1B�7B�=B�7B�DB�JB�PB�VB�VB�PB�1B�B�B�B�B�B�B�%B�+B�=B�JB�\B�hB�uB��B��B��B��B��B��B��B�B�B�B�!B�!B�!B�!B�B�!B�?B��B��BǮB��B�
B�B�
B�B�5B�TB�mB�B��B	B	DB	JB	JB	PB	VB	\B	oB	uB	�B	�B	!�B	#�B	$�B	$�B	%�B	'�B	+B	-B	/B	33B	8RB	>wB	@�B	C�B	C�B	G�B	G�B	I�B	I�B	I�B	K�B	H�B	F�B	E�B	J�B	K�B	G�B	D�B	F�B	G�B	I�B	O�B	S�B	[#B	^5B	_;B	`BB	cTB	cTB	cTB	gmB	iyB	k�B	m�B	o�B	q�B	q�B	r�B	t�B	u�B	t�B	u�B	v�B	y�B	y�B	w�B	w�B	v�B	w�B	w�B	x�B	y�B	x�B	x�B	x�B	u�B	s�B	s�B	s�B	u�B	u�B	v�B	x�B	y�B	z�B	~�B	�B	�%B	�1B	�JB	�JB	�JB	�PB	�hB	�oB	�{B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�!B	�'B	�FB	�XB	�^B	�^B	�^B	�^B	�RB	�^B	�^B	�XB	�FB	�-B	�B	�!B	�'B	�9B	�9B	�FB	�XB	�^B	�dB	�jB	�wB	��B	��B	��B	��B	��B	��B	ÖB	ÖB	ŢB	ŢB	ǮB	ɺB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�
B	�
B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�/B	�/B	�5B	�5B	�;B	�BB	�HB	�HB	�NB	�NB	�TB	�ZB	�`B	�`B	�`B	�`B	�mB	�sB	�sB	�sB	�sB	�yB	�sB	�mB	�mB	�mB	�fB	�fB	�fB	�fB	�`B	�fB	�yB	�B	�B	�B	��B
B
PB
�B
 �B
,B
5?B
;dB
>wB
C�B
H�B
L�B
O�B
VB
ZB
_;B
`BB
cTB
ffB
k�11111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
*B
*B
)B
)B
(�B
(�B
*	B
)B
)G�O�B
)B
)B
)B
)B
)B
)B
&�B
&�B
&�B
&�B
&�B
%�B
$�B
!�B
�B
�B
�B
�B
!�B
J�B
G�B
F�B
M�B
M�B
N�B
N�B
O�B
J�B
P�B
P�B
O�B
N�B
M�B
N�B
TB
WB
[,B
m�B
�:B
��B
ưB
��BqB(�BN�BYBYBi~B�
B�B��B�B'�B0'B9^B4@B1-B7QBC�BO�BZ!BaLBd`Bn�Bp�Br�Bu�Bz�B�B�2B�B��B��B�lB�B�NB�@B�1Bw�BbSB<rB:_B/"B�B�B+B`B�B'�BI�B>{B	9B�Bq�B:fB
�nB+B:B�BIB
��B
�B
�FB
ǷB
�CB
��B
�AB
^?B
,B
:B	�?B	�B	�DB	�B	�2B	�%B	��B	�PB	��B	z�B	E�B	6B�lB�*B�B	IB�B��B�B�hB�oB�_B�/B�B��B��B¨B�xB�NB�8B�B�B��B��B��B��B��B��B�zB�hB�LB�NB�HB�@B�HB�@B�HB�CB�BB�8B�6B�6B�0B�#B�#B�B�BB~BBB~BB�#B�BB~B~B~B�(B�*B�+B�)B�:B�5B�6B�;B�;B�5B�@B�GB�BB�7B~B}BB�;B�IB�TB�hB�lB�kB�lB�mB�hB�dB�lB�eB�gB�`B�[B�MB�FB�<B�BB�HB�MB�UB�ZB�TB�aB�fB�lB�sB�qB�mB�KB�*B�-B�6B�5B�(B�4B�@B�IB�[B�gB�wB��B��B��B��B��B�B�B�	B�B�'B�6B�5B�=B�8B�;B�:B�5B�<B�[B��B��B��B�	B�!B�B�"B�5B�OB�kB�B�B�B	4B	ZB	`B	_B	hB	oB	rB	�B	�B	�B	�B	!�B	#�B	$�B	$�B	%�B	(B	+B	-$B	/1B	3GB	8hB	>�B	@�B	C�B	C�B	G�B	G�B	I�B	I�B	I�B	K�B	H�B	F�B	E�B	J�B	K�B	G�B	D�B	F�B	G�B	I�B	O�B	TB	[8B	^FB	_OB	`WB	cgB	ceB	cgB	g�B	i�B	k�B	m�B	o�B	q�B	q�B	r�B	t�B	u�B	t�B	u�B	v�B	y�B	y�B	w�B	w�B	v�B	w�B	w�B	x�B	y�B	x�B	x�B	x�B	u�B	s�B	s�B	s�B	u�B	u�B	v�B	x�B	y�B	z�B	B	�2B	�6B	�EB	�[B	�[B	�[B	�cB	�yB	��B	��B	��B	��B	��B	��B	�B	�B	�.B	�1B	�4B	�2B	�7B	�UB	�hB	�pB	�oB	�oB	�nB	�cB	�lB	�nB	�hB	�UB	�@B	�+B	�4B	�8B	�HB	�IB	�WB	�hB	�oB	�wB	�{B	��B	��B	��B	��B	��B	��B	��B	åB	åB	ųB	ŲB	ǾB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�B	�(B	�&B	�,B	�4B	�7B	�7B	�>B	�?B	�EB	�EB	�JB	�RB	�XB	�VB	�]B	�]B	�cB	�iB	�mB	�mB	�nB	�nB	�{B	�B	�B	�B	�B	�B	�B	�|B	�{B	�zB	�wB	�tB	�rB	�uB	�mB	�uB	�B	�G�O�B	��B	�	B
,B
_B
�B
 �B
,B
5LB
;sB
>�B
C�B
H�B
L�B
O�B
VB
Z)B
_FB
`NB
c`B
frB
k�11111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.06 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436482016080714364820160807143648  AO  ARCAADJP                                                                    20160703050110    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160703050110  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160703050110  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143648  IP                  G�O�G�O�G�O�                