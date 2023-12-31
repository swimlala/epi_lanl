CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-03-19T19:18:05Z AOML 3.0 creation; 2016-08-07T21:36:44Z UW 3.1 conversion     
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
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K|   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Mx   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Uh   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]X   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _T   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  gD   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �X   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �\   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �`   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �d   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20160319191805  20160807143645  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               kA   AO  5286_8897_107                   2C  D   APEX                            6531                            072314                          846 @מ`$v�1   @מ��X-@23�E����c[;dZ�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    kA   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C�C�C  C�fC  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8�C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV�CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DAy�DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy� D��D�9�D�p D��fD�3D�FfD�� D��3D�  D�I�D���D���D��3D�c3DږfD�ɚD�  D�<�D�s31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@�{@�{A
=A?
=A_
=A
=A��A��A��A��AυA߅A�A��BBBBB'B/B7B?BGBOBWB_BgBoBwBB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB�{B��HB�{B��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HC�C�C�C�C	�C
>C
>C�C�
C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C8
>C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CV
>CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DAu�DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dto\Dy�)D��D�7�D�nD��zD�GD�DzD��D��GD��D�G�D���D���D��GD�aGDڔzD�ǮD��D�:�D�qG1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��yA��`A��TA��`A��yA��mA��TA��A���A���A���A���A���A��A��TAǲ-A�r�A��mA�bA�oA��A�"�A�+A�&�A�1'A�-A�G�A�;dA�(�A�&�A�"�A��A��A�oA�bA�{A��A��A��A�{A�A���A�ƨA���A���A�ȴA�ȴA���A�ȴA���A��yAţ�A�bA�l�A� �A�1A���Aã�AËDA�hsA�ZA�%A7A�(�A���A��9A�A���A���A�VA��PA�A��A��-A��wA��FA��A���A� �A�  A��;A���A�p�A���A��A��!A���A�ȴA�"�A��A���A���A�t�A��A��+A�
=A��A�z�A�&�A�A�{A�/A���A�7LA�n�A���A�O�A~�A~��AO�A~bAy�Aw�hAu+AsAq|�An��Aj��Ail�Ag��Ad�RAa"�A_�#A_7LA]`BA\�A\VA[;dAV�AR��AP�/AO�ANn�AM�TAJ�AHn�AGAF��AFbAE/AB��A@�RA>�/A>A�A<��A:�yA:ffA8r�A6ȴA4VA1�mA.(�A+��A*5?A)t�A(�`A($�A&��A%��A#hsA"��A!�FA ��A $�A7LA%AƨA�PA�A��A�A�A��A1An�A�7A;dA�AZA?}AK�Az�A�TAx�A
��A
��A
~�A
�A	�A�`A-A��AXA��A�A��A�A�hA�!A-A�-AS�A �yA M�@��w@�v�@�@��@�r�@��@�
=@�ff@�G�@��;@���@�9@�7L@�{@���@���@�33@���@�{@�  @�=q@���@��m@��@���@ܛ�@�Z@�I�@�b@�+@ۮ@��@�o@�+@�+@�5?@�`B@�(�@�dZ@�+@��y@���@ָR@�=q@�7L@��@ԃ@���@�t�@�%@�1'@��
@��@Ο�@�=q@�@�&�@̬@���@��@���@ˍP@ɩ�@ǅ@��@�ff@�hs@���@���@��7@�p�@�p�@�hs@�`B@�O�@�%@���@�j@�Z@���@���@��@�;d@�J@�7L@��/@��u@�  @�dZ@��@���@�v�@�^5@�5?@��-@��@�r�@�1'@�1'@�(�@��@���@�;d@��@�^5@�E�@�J@��#@���@�hs@�?}@��@�%@��@��@���@�Ĝ@���@�Z@���@�K�@���@�^5@�=q@�-@���@���@��D@�bN@�b@��m@���@���@�+@���@�ff@�E�@��@��-@�X@�hs@�`B@�V@��j@�r�@��F@�\)@�"�@��y@���@���@�ff@�{@��@�X@���@���@� �@�C�@�ȴ@��-@��@�(�@��
@��w@���@�+@�ȴ@�V@��#@���@��@�l�@�S�@�;d@�o@��@��h@���@�Z@�(�@��m@��F@���@�dZ@���@�n�@��@�&�@��@���@�bN@�I�@��@�"�@��@��!@�5?@�@�p�@��@��`@�9X@��m@��m@�  @��@�1@��@��;@�ƨ@��@���@�v�@��@���@��h@�?}@��@��j@��@�j@�(�@��
@��w@��F@��F@���@�C�@�@���@���@��+@�5?@���@���@�p�@�`B@�hs@��h@�hs@��`@��@�  @��@�l�@�K�@�33@�
=@���@��\@�M�@�=q@�-@�{@��@��7@�O�@�G�@�V@�Ĝ@�Q�@�(�@��@��@��m@��m@�ƨ@�l�@�K�@��@���@���@�M�@�@��-@��@�X@�?}@�&�@��@�r�@�I�@|(�@sƨ@ix�@a�@Y�@P�9@Ix�@B��@=V@3�F@.��@(�9@"��@\)@�/@��@�j@r�@�D@	&�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111A��A��yA��`A��TA��`A��yA��mA��TA��A���A���A���A���A���A��A��TAǲ-A�r�A��mA�bA�oA��A�"�A�+A�&�A�1'A�-A�G�A�;dA�(�A�&�A�"�A��A��A�oA�bA�{A��A��A��A�{A�A���A�ƨA���A���A�ȴA�ȴA���A�ȴA���A��yAţ�A�bA�l�A� �A�1A���Aã�AËDA�hsA�ZA�%A7A�(�A���A��9A�A���A���A�VA��PA�A��A��-A��wA��FA��A���A� �A�  A��;A���A�p�A���A��A��!A���A�ȴA�"�A��A���A���A�t�A��A��+A�
=A��A�z�A�&�A�A�{A�/A���A�7LA�n�A���A�O�A~�A~��AO�A~bAy�Aw�hAu+AsAq|�An��Aj��Ail�Ag��Ad�RAa"�A_�#A_7LA]`BA\�A\VA[;dAV�AR��AP�/AO�ANn�AM�TAJ�AHn�AGAF��AFbAE/AB��A@�RA>�/A>A�A<��A:�yA:ffA8r�A6ȴA4VA1�mA.(�A+��A*5?A)t�A(�`A($�A&��A%��A#hsA"��A!�FA ��A $�A7LA%AƨA�PA�A��A�A�A��A1An�A�7A;dA�AZA?}AK�Az�A�TAx�A
��A
��A
~�A
�A	�A�`A-A��AXA��A�A��A�A�hA�!A-A�-AS�A �yA M�@��w@�v�@�@��@�r�@��@�
=@�ff@�G�@��;@���@�9@�7L@�{@���@���@�33@���@�{@�  @�=q@���@��m@��@���@ܛ�@�Z@�I�@�b@�+@ۮ@��@�o@�+@�+@�5?@�`B@�(�@�dZ@�+@��y@���@ָR@�=q@�7L@��@ԃ@���@�t�@�%@�1'@��
@��@Ο�@�=q@�@�&�@̬@���@��@���@ˍP@ɩ�@ǅ@��@�ff@�hs@���@���@��7@�p�@�p�@�hs@�`B@�O�@�%@���@�j@�Z@���@���@��@�;d@�J@�7L@��/@��u@�  @�dZ@��@���@�v�@�^5@�5?@��-@��@�r�@�1'@�1'@�(�@��@���@�;d@��@�^5@�E�@�J@��#@���@�hs@�?}@��@�%@��@��@���@�Ĝ@���@�Z@���@�K�@���@�^5@�=q@�-@���@���@��D@�bN@�b@��m@���@���@�+@���@�ff@�E�@��@��-@�X@�hs@�`B@�V@��j@�r�@��F@�\)@�"�@��y@���@���@�ff@�{@��@�X@���@���@� �@�C�@�ȴ@��-@��@�(�@��
@��w@���@�+@�ȴ@�V@��#@���@��@�l�@�S�@�;d@�o@��@��h@���@�Z@�(�@��m@��F@���@�dZ@���@�n�@��@�&�@��@���@�bN@�I�@��@�"�@��@��!@�5?@�@�p�@��@��`@�9X@��m@��m@�  @��@�1@��@��;@�ƨ@��@���@�v�@��@���@��h@�?}@��@��j@��@�j@�(�@��
@��w@��F@��F@���@�C�@�@���@���@��+@�5?@���@���@�p�@�`B@�hs@��h@�hs@��`@��@�  @��@�l�@�K�@�33@�
=@���@��\@�M�@�=q@�-@�{@��@��7@�O�@�G�@�V@�Ĝ@�Q�@�(�@��@��@��m@��m@�ƨ@�l�@�K�@��@���@���@�M�@�@��-@��@�X@�?}@�&�@��@�r�G�O�@|(�@sƨ@ix�@a�@Y�@P�9@Ix�@B��@=V@3�F@.��@(�9@"��@\)@�/@��@�j@r�@�D@	&�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�XB	�B
�B
,B
2-B
9XB
=qB
A�B
E�B
G�B
M�B
L�B
H�B
H�B
I�B
J�B
K�B
K�B
K�B
L�B
N�B
O�B
Q�B
Q�B
N�B
F�B
D�B
G�B
G�B
F�B
F�B
G�B
H�B
J�B
`BB
ŢB2-Bz�B��B��B�'B�RB�qBÖBƨB�BB�B7LB=qBn�Bu�BjBcTBQ�B:^B0!B'�B�B.BM�B-B�BbB1B%B��B�5B�-B�B�B�RB�B��BZB-B  B
�`B
��B
ɺB
�^B
��B
�JB
s�B
I�B
,B
�B	��B	�B	��B	ŢB	��B	�NB	��B	�B	��B	�wB	�!B	��B	��B	��B	�+B	�+B	|�B	k�B	]/B	VB	R�B	M�B	I�B	F�B	<jB	#�B	VB	B��B��B�B�HB�B�B��B��B��BǮB��BB��B�}B�jB�^B�RB�3B�B��B��B�B�'B�3B�?B�XB�jBŢBÖBÖB�wB�dB�LB�'B��B�oB�{B�oB�oB�oB�bB�PB�JB�PB�DB�1B�+B�B�B�B� B�B�B�B�B�B�B�B�B�B�%B�%B�%B�%B�%B�%B�+B�1B�7B�7B�7B�7B�7B�7B�DB�JB�DB�1B�B{�B|�B|�B~�B�B�B}�Bz�B{�B|�B�B�B�B�%B�%B�%B�1B�JB�PB�VB�VB�VB��B��B��B�?B�9B�^BŢB��B�
B�5B�BB�HB�NB�NB�fB�B��B��B��B��B��B��B	B	B	B	B	B	B	B	%B		7B	DB	PB	\B	hB	hB	bB	bB	bB	JB	PB	hB	uB	{B	�B	�B	�B	 �B	$�B	&�B	)�B	+B	)�B	%�B	%�B	(�B	+B	,B	,B	-B	0!B	33B	49B	6FB	6FB	6FB	9XB	@�B	D�B	F�B	G�B	G�B	J�B	N�B	R�B	W
B	]/B	_;B	aHB	bNB	dZB	ffB	ffB	gmB	iyB	iyB	iyB	jB	k�B	m�B	o�B	r�B	u�B	w�B	z�B	{�B	{�B	}�B	� B	� B	�B	�B	�%B	�+B	�7B	�7B	�JB	�PB	�PB	�PB	�PB	�\B	�bB	�hB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�-B	�3B	�9B	�?B	�FB	�RB	�^B	�jB	�}B	�}B	��B	B	��B	��B	��B	�}B	�}B	ŢB	ǮB	ȴB	ǮB	ƨB	ƨB	ƨB	ƨB	ƨB	ŢB	ĜB	ŢB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�#B	�)B	�/B	�/B	�/B	�/B	�5B	�;B	�BB	�HB	�NB	�NB	�NB	�ZB	�`B	�`B	�fB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
JB
{B
�B
$�B
-B
49B
;dB
A�B
F�B
O�B
S�B
YB
^5B
bNB
dZB
iyB
l�B
p�B
t�B
x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�kB	�+B
�B
,B
28B
9cB
=�B
A�B
E�B
G�B
M�B
L�B
H�B
H�B
I�B
J�B
K�B
K�B
K�B
L�B
N�B
O�B
Q�B
Q�B
N�B
F�B
D�B
G�B
G�B
F�B
F�B
G�B
H�B
J�B
`MB
ŪB22Bz�B��B��B�+B�UB�wBÙBƫB�$BB�B7PB=xBn�Bu�Bj�BcXBQ�B:cB0*B'�B�B.BM�B-B�BiB4B)B��B�>B�1B�B�B�WB�!B��BZ#B-B B
�gB
��B
��B
�iB
��B
�SB
s�B
I�B
,B
�B	��B	�B	��B	ŲB	��B	�\B	��B	�B	��B	��B	�2B	��B	��B	��B	�;B	�=B	}B	k�B	]CB	VB	SB	M�B	I�B	F�B	<�B	#�B	mB	(B��B��B��B�aB�0B�B�B��B��B��B��B©B��B��B��B�zB�lB�MB�B�B�B�&B�AB�NB�WB�rB��BžBñBðB��B�}B�gB�?B��B��B��B��B��B��B�}B�lB�fB�mB�bB�MB�GB�;B�.B�%B�!B�%B�#B�)B�-B�'B�)B�/B�7B�:B�BB�AB�@B�?B�AB�BB�GB�OB�SB�RB�UB�TB�UB�QB�aB�eB�`B�LB�"B|B}B}BB�*B�1B~B{ B|B}
B�"B�/B�=B�BB�AB�BB�LB�gB�nB�rB�qB�tB��B��B��B�\B�TB�zBżB��B�%B�OB�[B�_B�hB�gB�~B�B��B��B��B��B��B�B	B	B	(B	(B	"B	%B	1B	;B		NB	ZB	gB	qB	~B	B	xB	xB	xB	`B	eB	~B	�B	�B	�B	�B	�B	 �B	$�B	' B	*B	+B	*B	%�B	%�B	)B	+B	,B	,B	-!B	07B	3HB	4NB	6ZB	6ZB	6ZB	9kB	@�B	D�B	F�B	G�B	G�B	J�B	N�B	SB	WB	]AB	_NB	a^B	b_B	dmB	fyB	fzB	g�B	i�B	i�B	i�B	j�B	k�B	m�B	o�B	r�B	u�B	w�B	z�B	{�B	{�B	~B	�B	�B	�B	�*B	�7B	�:B	�IB	�IB	�[B	�`B	�bB	�aB	�cB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�>B	�EB	�JB	�PB	�TB	�cB	�nB	�{B	��B	��B	��B	 B	��B	��B	��B	��B	��B	ŲB	ǿB	��B	ǽB	ƹB	ƷB	ƶB	ƹB	ƹB	ųB	ĭB	űB	ƹB	ǾB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�%B	�+B	�2B	�:B	�?B	�>B	�=B	�>B	�CB	�LB	�PB	�VB	�]B	�\B	�[B	�jB	�qB	�nB	�tB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	��B	�B	�B	�B	�B	�B
 B
B
B
B
B
#B
%B
&B
)B
.B
,B
,B
/B
4G�O�B
XB
�B
�B
$�B
-B
4HB
;sB
A�B
F�B
O�B
TB
Y B
^@B
bXB
dfB
i�B
l�B
p�B
t�B
x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.06 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436452016080714364520160807143645  AO  ARCAADJP                                                                    20160319191805    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160319191805  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160319191805  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143645  IP                  G�O�G�O�G�O�                