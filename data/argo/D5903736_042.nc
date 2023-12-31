CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:28Z AOML 3.0 creation; 2016-05-31T19:14:31Z UW 3.1 conversion     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20140721230528  20160531121431  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               *A   AO  4051_7090_042                   2C  D   APEX                            5368                            041511                          846 @֝	{@1   @֝�A@@5>��"���d�XbM�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    *A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@y�DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtY�Dys3D��D�FfD�� D���D��D�L�D���D��fD�  D�P D�y�D�� D���D�I�Dڃ3D�� D�  D�@ D� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@|(�@�{@�{A
=A?
=A_
=A
=A��A��A��A��AυA߅A�A��BBBBB'B/B7B?BGBOBWB_Bh(�BoBwBB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HC�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@u�D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)DtU�Dyo\D��D�DzD��D���D�
�D�J�D���D��zD��D�ND�w�D��D���D�G�DځGD��D��D�>D�~1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�ƨA���AͼjAͩ�A�C�A�E�A��AɅA��A��HA���AȸRA�t�A�1'A� �A��A��A��A��AǸRA��
AǓuA��#AƲ-AƼjA�z�AŇ+AĴ9A�|�A�A�A�5?A�$�AËDA���A��HA��A���A�|�A��7A�{A�jA�;dA�
=A��A���A��+A���A�ffA�bNA�A�|�A�A��`A���A��+A�p�A�XA��jA�O�A�"�A�VA���A�%A��HA�`BA��A���A��A�K�A�33A��HA���A�(�A��#A�dZA�K�A� �A��A��A�v�A�K�A��A��A��!A�-A�\)A�ĜA���A�v�A���A�E�A�  A�z�A�5?A��A��A�A�ZA���A�|�A�?}A��
A�K�A���A� �A�E�A�ƨA��#A��A���A��A�33A���A��A��A��A�VA�1'A� �A���A��TA��+A��A��A�ƨA�9XA��A� �A}�Ay��Aw�PAt�AtVAs�ArĜAn��AkK�Aj9XAg�mAe"�Adv�Ac�TAcp�Ab�9Ab�AaO�A_�#A^n�A]%A\E�A[�;A[33AZ�yAZffAY"�AW��AT�uAS�wAR�ANM�ALbAJ�\AIS�AH��AGl�AF��AF9XAE��AE�hAE�AC��ACl�ABAAVA@E�A>�/A=p�A=oA<��A<��A<�A<^5A<  A;��A;?}A:�yA:^5A9O�A7�wA733A6ȴA6z�A6A�A5�A5dZA4��A4bNA3C�A2z�A2(�A1l�A0~�A/�A/��A/`BA/O�A.z�A-&�A+��A*�A*jA)��A(A'G�A&��A&$�A$�A#l�A"�HA"ZA!��A!�hA!dZA!33A �A�mAĜA�A`BA%A|�A�mA��A�DA��Ap�AXA`BA	��A�DA�AO�A�yA��AȴA�jA�uA�AbA
=A�AG�A bN@��@�ȴ@���@��@�G�@�(�@�z�@���@��@�t�@�5?@�V@�o@�G�@���@�u@�=q@�&�@�1@߅@�@ޏ\@���@��/@ۍP@�7L@��@Դ9@ҟ�@�/@�9X@ϝ�@�K�@�
=@Χ�@�@��@̓u@˅@��@��@�I�@�ȴ@�@�Z@���@��@�p�@�?}@�z�@�A�@�ƨ@��@�=q@�J@�@�@�V@�A�@��P@�t�@�M�@�X@���@��@���@��@�Z@�M�@�X@�|�@�33@��P@��F@�ƨ@�ƨ@��F@�33@�E�@�G�@���@��@���@���@���@�v�@���@�v�@�-@�@�X@��/@��9@�bN@��
@�o@��y@�ff@�5?@�{@�{@��-@��@�/@��`@�b@�+@��y@��H@��R@��+@�$�@��#@��-@�O�@��@���@��D@�Z@� �@��
@��P@�K�@��@��@��@��@���@�@���@���@���@��@�hs@�?}@��@��@���@��u@���@��@��@�z�@�bN@�Z@�Z@�I�@� �@���@��@��;@�ƨ@�ƨ@� �@�9X@�9X@��@��;@��@�dZ@�"�@���@�$�@�`B@�%@��/@��@��@��/@�V@���@��@�I�@�1@��w@��P@���@�C�@��@���@�M�@�-@�$�@��-@�X@�/@���@�Ĝ@� �@��m@��
@��w@��@��F@��@��P@�33@�"�@�o@�@���@��!@�~�@�V@���@�&�@���@�Q�@�A�@�Q�@�Z@�9X@� �@�  @��
@���@�S�@��@�V@��@��@��@�J@���@���@��#@���@���@���@�@�@��^@���@���@��h@�K�@���@{ƨ@q7L@h�u@]O�@U@KS�@E�T@@��@97L@3dZ@,��@'�P@"��@�@��@��@x�@�-1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ƨA���AͼjAͩ�A�C�A�E�A��AɅA��A��HA���AȸRA�t�A�1'A� �A��A��A��A��AǸRA��
AǓuA��#AƲ-AƼjA�z�AŇ+AĴ9A�|�A�A�A�5?A�$�AËDA���A��HA��A���A�|�A��7A�{A�jA�;dA�
=A��A���A��+A���A�ffA�bNA�A�|�A�A��`A���A��+A�p�A�XA��jA�O�A�"�A�VA���A�%A��HA�`BA��A���A��A�K�A�33A��HA���A�(�A��#A�dZA�K�A� �A��A��A�v�A�K�A��A��A��!A�-A�\)A�ĜA���A�v�A���A�E�A�  A�z�A�5?A��A��A�A�ZA���A�|�A�?}A��
A�K�A���A� �A�E�A�ƨA��#A��A���A��A�33A���A��A��A��A�VA�1'A� �A���A��TA��+A��A��A�ƨA�9XA��A� �A}�Ay��Aw�PAt�AtVAs�ArĜAn��AkK�Aj9XAg�mAe"�Adv�Ac�TAcp�Ab�9Ab�AaO�A_�#A^n�A]%A\E�A[�;A[33AZ�yAZffAY"�AW��AT�uAS�wAR�ANM�ALbAJ�\AIS�AH��AGl�AF��AF9XAE��AE�hAE�AC��ACl�ABAAVA@E�A>�/A=p�A=oA<��A<��A<�A<^5A<  A;��A;?}A:�yA:^5A9O�A7�wA733A6ȴA6z�A6A�A5�A5dZA4��A4bNA3C�A2z�A2(�A1l�A0~�A/�A/��A/`BA/O�A.z�A-&�A+��A*�A*jA)��A(A'G�A&��A&$�A$�A#l�A"�HA"ZA!��A!�hA!dZA!33A �A�mAĜA�A`BA%A|�A�mA��A�DA��Ap�AXA`BA	��A�DA�AO�A�yA��AȴA�jA�uA�AbA
=A�AG�A bN@��@�ȴ@���@��@�G�@�(�@�z�@���@��@�t�@�5?@�V@�o@�G�@���@�u@�=q@�&�@�1@߅@�@ޏ\@���@��/@ۍP@�7L@��@Դ9@ҟ�@�/@�9X@ϝ�@�K�@�
=@Χ�@�@��@̓u@˅@��@��@�I�@�ȴ@�@�Z@���@��@�p�@�?}@�z�@�A�@�ƨ@��@�=q@�J@�@�@�V@�A�@��P@�t�@�M�@�X@���@��@���@��@�Z@�M�@�X@�|�@�33@��P@��F@�ƨ@�ƨ@��F@�33@�E�@�G�@���@��@���@���@���@�v�@���@�v�@�-@�@�X@��/@��9@�bN@��
@�o@��y@�ff@�5?@�{@�{@��-@��@�/@��`@�b@�+@��y@��H@��R@��+@�$�@��#@��-@�O�@��@���@��D@�Z@� �@��
@��P@�K�@��@��@��@��@���@�@���@���@���@��@�hs@�?}@��@��@���@��u@���@��@��@�z�@�bN@�Z@�Z@�I�@� �@���@��@��;@�ƨ@�ƨ@� �@�9X@�9X@��@��;@��@�dZ@�"�@���@�$�@�`B@�%@��/@��@��@��/@�V@���@��@�I�@�1@��w@��P@���@�C�@��@���@�M�@�-@�$�@��-@�X@�/@���@�Ĝ@� �@��m@��
@��w@��@��F@��@��P@�33@�"�@�o@�@���@��!@�~�@�V@���@�&�@���@�Q�@�A�@�Q�@�Z@�9X@� �@�  @��
@���@�S�@��@�V@��@��@��@�J@���@���@��#@���@���@���@�@�@��^@���@���@��h@�K�@���@{ƨ@q7L@h�u@]O�@U@KS�@E�T@@��@97L@3dZ@,��@'�P@"��@�@��@��@x�@�-1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB_;B_;B_;B]/B\)BbNBk�Bo�Bx�B~�B�B�1B�+B�DB��B�B�-B�3B�-B�FB��B�?B��B�9BɺB�B�ZB�ZB�HB�NB�HB�;B�BȴB�B}�Bp�B~�B��B�-B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B��B��B��B��B��B�JB� Bx�Bt�BjBe`BbNB^5B_;B`BB^5B[#BYB@�B+BoB��B�ZB�#B��B��B�LB��B�\Bo�BiyBdZB[#BVBD�B/B!�B�B�BPB��B�`B��B��B�RB��B��B�JB^5BA�B6FB(�BoBB
��B
��B
�B
�B
�B
�yB
�fB
��B
�}B
�9B
��B
�1B
w�B
hsB
H�B
49B
&�B
�B
{B
\B
+B	�B	�NB	�#B	��B	ÖB	�}B	�jB	�^B	�FB	�3B	�B	��B	��B	��B	��B	�{B	�hB	�bB	�PB	�+B	� B	t�B	o�B	dZB	P�B	D�B	>wB	:^B	7LB	49B	2-B	0!B	/B	.B	,B	(�B	'�B	$�B	 �B	�B	�B	{B	uB	oB	hB	bB	bB	VB	PB	DB		7B	%B	B��B��B��B��B��B��B�B�B�B�B�sB�mB�ZB�HB�;B�5B�/B�#B�
B��B��B��BǮBÖB�wB�dB�XB�FB�-B�B�B��B��B��B��B��B��B��B�hB�DB�7B�%B�B}�Bx�Bs�Bo�Bk�BgmBdZBbNB`BB_;B^5B]/B]/B]/B\)B[#BXBXBVBVBT�BT�BT�BT�BW
BZBYBW
BS�BVBVBT�BVBT�BVBVBXBYBZBZB[#BZBZBZBYBXBW
BYB\)B_;B_;BaHBcTBe`Be`Be`BgmBhsBjBk�Bl�Bm�Bn�Bo�Bo�Bo�Bs�Bw�Bz�B|�B|�B~�B~�B�B�B�%B�+B�+B�=B�PB�\B�bB�oB��B��B��B��B��B�B�B�'B�?B��B��B��B�B�B�#B�;B��B��B��B	B	VB	bB	oB	�B	�B	 �B	"�B	%�B	)�B	-B	1'B	2-B	49B	5?B	7LB	;dB	=qB	>wB	?}B	?}B	C�B	G�B	H�B	I�B	I�B	N�B	Q�B	R�B	T�B	[#B	_;B	cTB	e`B	iyB	jB	k�B	m�B	o�B	p�B	r�B	s�B	u�B	v�B	w�B	w�B	x�B	z�B	{�B	�B	�B	�B	�%B	�1B	�1B	�DB	�PB	�PB	�PB	�\B	�\B	�bB	�bB	�hB	�hB	�oB	�oB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�3B	�FB	�XB	�XB	�^B	�XB	�XB	�^B	�jB	�wB	��B	��B	ÖB	ŢB	ŢB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�)B	�/B	�5B	�5B	�5B	�5B	�;B	�;B	�BB	�BB	�BB	�BB	�BB	�BB	�BB	�BB	�BB	�HB	�HB	�TB	�`B	�fB	�fB	�fB	�fB	�fB	�fB	�fB	�fB	�fB	�mB	�mB	�mB	�mB	�mB	�mB	�sB	�B
B
oB
�B
#�B
+B
2-B
=qB
D�B
I�B
M�B
R�B
XB
]/B
aHB
dZB
hsB
k�B
n�B
q�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B_DB_DB_DB]6B\3BbYBk�Bo�Bx�BB�B�;B�5B�MB��B� B�6B�@B�8B�RB��B�GB��B�EB��B�+B�cB�hB�TB�_B�YB�JB�BȽB�B}�Bp�BB��B�8B�B� B�B�B��B��B�B� B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�VB�Bx�Bt�Bj�BekBbVB^?B_GB`JB^AB[,BYB@�B+
BwB��B�dB�+B��B��B�SB��B�`Bo�Bi|Bd_B[)BVBD�B/"B!�B�B�BUB��B�cB��B��B�XB��B��B�OB^<BA�B6LB(�BwB%B
��B
��B
�B
�B
�B
�B
�nB
��B
��B
�FB
��B
�>B
w�B
h}B
H�B
4FB
&�B
�B
�B
jB
:B	��B	�\B	�2B	��B	çB	��B	�yB	�nB	�UB	�CB	�%B	��B	��B	��B	��B	��B	�yB	�uB	�bB	�;B	�B	t�B	o�B	dpB	P�B	D�B	>�B	:uB	7`B	4OB	2CB	04B	/2B	.*B	,B	)B	(B	$�B	 �B	�B	�B	�B	�B	�B	�B	wB	yB	nB	iB	\B		MB	>B	B�B��B��B��B��B��B��B��B�B�B�B�B�sB�bB�UB�OB�HB�:B�#B�B��B��B��BðB��B��B�rB�`B�HB�'B�$B�B�B�B�B�B��B��B��B�bB�VB�AB�)B~Bx�Bs�Bo�Bk�Bg�BdxBbkB``B_ZB^SB]JB]KB]LB\GB[EBX.BX.BV"BV$BUBUBUBUBW(BZ:BY6BW(BTBV#BV#BUBV!BUBV!BV!BX0BY4BZ;BZ;B[BBZ<BZ;BZ:BY4BX.BW'BY5B\HB_YB_ZBafBcsBe}BeBeBg�Bh�Bj�Bk�Bl�Bm�Bn�Bo�Bo�Bo�Bs�Bw�Bz�B}B}BBB�"B�1B�?B�GB�HB�WB�jB�wB�~B��B��B��B��B�B�B�B�)B�@B�XB��B��B�B�B�*B�;B�SB��B��B��B	*B	lB	xB	�B	�B	�B	 �B	"�B	%�B	*B	-$B	1:B	2AB	4PB	5SB	7`B	;xB	=�B	>�B	?�B	?�B	C�B	G�B	H�B	I�B	I�B	N�B	Q�B	SB	UB	[7B	_KB	ciB	esB	i�B	j�B	k�B	m�B	o�B	p�B	r�B	s�B	u�B	v�B	w�B	w�B	x�B	z�B	{�B	�B	�B	�#B	�7B	�DB	�CB	�UB	�cB	�bB	�bB	�oB	�oB	�tB	�tB	�xB	�zB	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�B	�B	�B	�B	�$B	�+B	�:B	�CB	�SB	�gB	�jB	�mB	�iB	�iB	�oB	�{B	��B	��B	��B	åB	ŲB	ůB	��B	ǿB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�&B	�0B	�:B	�>B	�FB	�FB	�GB	�DB	�JB	�IB	�PB	�TB	�OB	�TB	�RB	�TB	�RB	�PB	�QB	�WB	�WB	�eB	�nB	�tB	�tB	�tB	�tB	�uB	�sB	�uB	�uB	�uB	�}B	�|B	�|B	�~B	�~B	�}B	�B	�B
.B
|B
�B
#�B
+B
29B
={B
D�B
I�B
M�B
S B
XB
]<B
aSB
dgB
h}B
k�B
n�B
q�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.06 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214312016053112143120160531121431  AO  ARCAADJP                                                                    20140721230528    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230528  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230528  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121431  IP                  G�O�G�O�G�O�                