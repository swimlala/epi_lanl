CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:20Z AOML 3.0 creation; 2016-05-31T19:14:28Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230520  20160531121429  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  4051_7090_027                   2C  D   APEX                            5368                            041511                          846 @�v���_�1   @�v�x9�@4P ě���d�333331   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C�C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dyy�D��D�I�D�vfD��fD�	�D�@ D�vfD���D�3D�Y�D�|�D���D�3D�L�DږfD��fD�3D�33D�|�D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�z�@�{@�{A
=A?
=A_
=A
=A��A��A��A��AυA߅A�A��BBBBB'B/B7B?BGBOBWB_BgBoBwBB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HC�C�C�C�C	�C�C�C�C�C�C�C�C�C�C
>C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dyu�D�
�D�G�D�tzD��zD��D�>D�tzD���D�GD�W�D�z�D���D�GD�J�DڔzD��zD�GD�1GD�z�D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A΍PAΏ\AΏ\AΑhAΓuAΕ�AΓuAΕ�AΕ�AΗ�AΙ�AΙ�AΙ�AΛ�AΝ�AΝ�AΝ�AΟ�AΟ�AΟ�AΡ�AΡ�AΡ�AΡ�AΣ�AΣ�AΣ�AΧ�AάAάAάAάAάAΩ�AΧ�AΥ�AΝ�AΛ�AΛ�AΕ�A΍PA·+A·+A�t�A�(�A�33Aʟ�A�x�AēuA���A��mA�dZA��A�33A�A�A��RA�ȴA�`BA�XA��!A���A��A�$�A�$�A�33A���A�hsA���A��!A��
A���A�z�A�\)A��/A���A��jA��A��!A�O�A��jA��;A�(�A���A���A��RA��jA�v�A���A�O�A�ZA�?}A�"�A�ffA�VA�`BA��;A�A�A��^A��;A�XA�1'A�C�A��9A���A���A���A��A�/A��HA���A�A��A~��A|��Az�Ax9XAu�
AtȴAs;dAp�yAo+Am33Aj^5Af�!Ac��Aa33A_�FA_VA]�mA\�`A[x�AY�7AXv�AW�hAV��AU�AR1'AP��AO�AN��AMAL�!AJ�HAI?}AGƨAF�+AE��AD~�AC�-AAdZA?��A=�-A<A;�PA;\)A;33A;oA:��A9O�A7�FA6~�A4�\A3��A2��A0-A.9XA-�^A,�yA,9XA*��A(��A'�PA&ĜA&=qA%l�A$ �A#��A"�A"(�A!��A!%A��A�
A=qA?}A��A?}A�A�`Av�AA�AVAȴA~�AA��AC�A^5A�A/AAS�A��A7LA
ZA	oA9XA|�A�A�hA9XA
=A5?Ax�AA ��@�"�@�{@� �@��@���@��9@��@�!@��@��
@�33@��@�5?@��/@�"�@�ff@���@��@�@柾@�@��@�@���@��@�bN@߾w@�S�@�^5@�j@���@���@���@�b@���@��@�j@�ȴ@��T@�/@ЋD@���@�@́@̛�@�t�@ʗ�@��T@�O�@ȼj@��@�\)@���@�=q@��T@ŉ7@�/@�bN@��@\@��@��h@���@�r�@�(�@�  @��P@�dZ@��y@�M�@���@�x�@�7L@��D@�b@�S�@��!@�^5@�@�G�@���@��F@�ȴ@�J@�@���@�X@��D@��m@��@�l�@���@�^5@�=q@�hs@���@�Ĝ@���@�A�@��@�"�@���@�^5@�-@��T@���@��h@��@�%@��@�j@�A�@��@��@�K�@�n�@��^@��@�7L@��@��D@�A�@�1@��@��;@��F@�"�@�V@���@�x�@�/@�%@���@��@��D@��@��@�j@�9X@��@��;@��w@�dZ@�@���@�V@��@�@��h@�`B@�O�@��@��`@���@��D@�r�@� �@��w@��P@�dZ@�\)@�K�@�;d@�"�@��@�ȴ@��+@�J@��-@�7L@�/@�V@��`@��D@�1'@��@���@��@��\@�M�@�-@�{@�@���@��#@��@��@���@�Ĝ@�z�@�9X@� �@�b@���@�|�@���@�;d@��@�ȴ@��R@���@�=q@���@��#@���@�x�@�p�@�p�@�?}@���@��u@�1'@�ƨ@��@��@��@�ȴ@���@�~�@�E�@�-@�{@��@�@�hs@��`@��j@��@�  @���@�t�@�S�@�o@���@�~�@�-@�{@���@���@��h@�G�@��@�%@�V@���@��u@�A�@�1'@�1'@���@�
=@���@�v�@�5?@�$�@�$�@��@���@�x�@�`B@�7L@���@��m@���@���@��@�S�@��y@���@�5?@��T@�@��`@~�@sƨ@i��@a�^@[��@U�h@N��@I�#@C@:�H@3��@*�H@%��@ A�@z�@��@dZ@\)@�@�w11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A΍PAΏ\AΏ\AΑhAΓuAΕ�AΓuAΕ�AΕ�AΗ�AΙ�AΙ�AΙ�AΛ�AΝ�AΝ�AΝ�AΟ�AΟ�AΟ�AΡ�AΡ�AΡ�AΡ�AΣ�AΣ�AΣ�AΧ�AάAάAάAάAάAΩ�AΧ�AΥ�AΝ�AΛ�AΛ�AΕ�A΍PA·+A·+A�t�A�(�A�33Aʟ�A�x�AēuA���A��mA�dZA��A�33A�A�A��RA�ȴA�`BA�XA��!A���A��A�$�A�$�A�33A���A�hsA���A��!A��
A���A�z�A�\)A��/A���A��jA��A��!A�O�A��jA��;A�(�A���A���A��RA��jA�v�A���A�O�A�ZA�?}A�"�A�ffA�VA�`BA��;A�A�A��^A��;A�XA�1'A�C�A��9A���A���A���A��A�/A��HA���A�A��A~��A|��Az�Ax9XAu�
AtȴAs;dAp�yAo+Am33Aj^5Af�!Ac��Aa33A_�FA_VA]�mA\�`A[x�AY�7AXv�AW�hAV��AU�AR1'AP��AO�AN��AMAL�!AJ�HAI?}AGƨAF�+AE��AD~�AC�-AAdZA?��A=�-A<A;�PA;\)A;33A;oA:��A9O�A7�FA6~�A4�\A3��A2��A0-A.9XA-�^A,�yA,9XA*��A(��A'�PA&ĜA&=qA%l�A$ �A#��A"�A"(�A!��A!%A��A�
A=qA?}A��A?}A�A�`Av�AA�AVAȴA~�AA��AC�A^5A�A/AAS�A��A7LA
ZA	oA9XA|�A�A�hA9XA
=A5?Ax�AA ��@�"�@�{@� �@��@���@��9@��@�!@��@��
@�33@��@�5?@��/@�"�@�ff@���@��@�@柾@�@��@�@���@��@�bN@߾w@�S�@�^5@�j@���@���@���@�b@���@��@�j@�ȴ@��T@�/@ЋD@���@�@́@̛�@�t�@ʗ�@��T@�O�@ȼj@��@�\)@���@�=q@��T@ŉ7@�/@�bN@��@\@��@��h@���@�r�@�(�@�  @��P@�dZ@��y@�M�@���@�x�@�7L@��D@�b@�S�@��!@�^5@�@�G�@���@��F@�ȴ@�J@�@���@�X@��D@��m@��@�l�@���@�^5@�=q@�hs@���@�Ĝ@���@�A�@��@�"�@���@�^5@�-@��T@���@��h@��@�%@��@�j@�A�@��@��@�K�@�n�@��^@��@�7L@��@��D@�A�@�1@��@��;@��F@�"�@�V@���@�x�@�/@�%@���@��@��D@��@��@�j@�9X@��@��;@��w@�dZ@�@���@�V@��@�@��h@�`B@�O�@��@��`@���@��D@�r�@� �@��w@��P@�dZ@�\)@�K�@�;d@�"�@��@�ȴ@��+@�J@��-@�7L@�/@�V@��`@��D@�1'@��@���@��@��\@�M�@�-@�{@�@���@��#@��@��@���@�Ĝ@�z�@�9X@� �@�b@���@�|�@���@�;d@��@�ȴ@��R@���@�=q@���@��#@���@�x�@�p�@�p�@�?}@���@��u@�1'@�ƨ@��@��@��@�ȴ@���@�~�@�E�@�-@�{@��@�@�hs@��`@��j@��@�  @���@�t�@�S�@�o@���@�~�@�-@�{@���@���@��h@�G�@��@�%@�V@���@��u@�A�@�1'@�1'@���@�
=@���@�v�@�5?@�$�@�$�@��@���@�x�@�`B@�7L@���@��m@���@���@��@�S�@��y@���@�5?@��T@�@��`@~�@sƨ@i��@a�^@[��@U�h@N��@I�#@C@:�H@3��@*�H@%��@ A�@z�@��@dZ@\)@�@�w11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBcTBcTBcTBcTBcTBcTBcTBcTBcTBcTBcTBcTBcTBcTBcTBcTBcTBcTBcTBdZBcTBcTBcTBdZBcTBdZBcTBcTBdZBcTBcTBdZBdZBdZBcTBcTBcTBcTBcTBbNBbNBbNBaHB`BB^5BZBT�BL�B?}B7LB#�BDBB��B��B��B	7BB
=B	7B	7B
=B
=BJBPBPB
=B%BB��B�B�ZB�BǮB�^B�B��B��B��B�oB�1B{�Be`BS�BF�B8RB2-B&�BDB��B�fB��BŢB�!B��B��B�=BZB;dB1'B�BB
�B
�`B
�5B
��B
�B
��B
z�B
k�B
]/B
N�B
F�B
9XB
)�B
�B
B	��B	�B	�;B	��B	ǮB	�?B	��B	�PB	{�B	p�B	l�B	dZB	`BB	YB	O�B	I�B	D�B	=qB	2-B	 �B	�B	�B	oB	\B	DB	B��B	  B	B	B��B��B�B�B�mB�NB�BB�BB�5B�/B�B�B��B��BŢB��B�qB�LB�-B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B�{B�hB�VB�=B�+B�%B�B�B�B�B� B� B~�B}�B|�B{�Bz�B{�Bz�Bz�Bz�B{�Bz�By�Bz�Bz�Bx�Bv�Bs�Bq�Bp�Bt�Bv�Bw�Bu�Bv�Bv�Bv�Bv�Bt�Bs�Bs�Br�Bq�Bq�Br�Bs�Bt�Bw�Bx�Bv�Bu�Bt�Bt�Bu�Bv�Bv�Bv�Bw�By�By�B|�B}�B}�B~�B� B�B�B�B�1B�=B�DB�VB�hB�uB�uB��B��B��B��B��B��B��B�B�B�B�B�-B�9B�LB�RB�XB�XB�jB��BÖBŢBƨBȴB��B��B��B��B��B��B�B�/B�5B�;B�NB�`B�yB�B�B�B��B��B��B	B	B	B	B	%B	
=B	VB	oB	uB	�B	�B	�B	�B	�B	�B	�B	!�B	'�B	,B	,B	1'B	7LB	8RB	;dB	=qB	>wB	B�B	D�B	D�B	E�B	I�B	J�B	M�B	S�B	W
B	YB	[#B	\)B	_;B	aHB	bNB	bNB	bNB	cTB	ffB	jB	l�B	p�B	r�B	r�B	s�B	u�B	u�B	v�B	v�B	v�B	w�B	x�B	y�B	z�B	~�B	�B	�B	�%B	�7B	�7B	�=B	�JB	�JB	�VB	�\B	�bB	�bB	�bB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�3B	�?B	�FB	�RB	�^B	�dB	�jB	�jB	�jB	�jB	�qB	�qB	�}B	��B	��B	��B	��B	B	ÖB	ŢB	ƨB	ǮB	ǮB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�
B	�B	�)B	�)B	�/B	�;B	�HB	�NB	�TB	�`B	�fB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
DB
uB
�B
"�B
(�B
,B
2-B
8RB
?}B
E�B
L�B
R�B
\)B
aHB
e`B
hsB
jB
n�B
t�B
v�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Bc]Bc`Bc]Bc`Bc]BcaBc]Bc]BcZBc^Bc^BcaBc^Bc^BcaBcaBcaBc^BcaBdfBcaBcaBc^BdfBc^BdfBcaBcaBdaBc^Bc^BdaBdcBdeBc^Bc^Bc^BcaBc^BbWBbZBbZBaPB`KB^=BZ%BUBL�B?�B7TB#�BIBB��B��B��B	>B&B
BB	?B	?B
EB
FBUBVBVB
DB.B!B��B�B�bB�$BǱB�hB� B��B��B��B�vB�5B{�BefBS�BF�B8WB22B&�BHB��B�oB��BŧB�(B��B��B�DBZ#B;lB1-B�B%B
�B
�hB
�>B
��B
�$B
��B
z�B
k�B
]:B
N�B
F�B
9fB
*B
�B
,B	��B	�B	�KB	�B	ǽB	�PB	��B	�cB	{�B	p�B	l�B	dnB	`UB	Y-B	O�B	I�B	D�B	=�B	2CB	 �B	�B	�B	�B	pB	[B	8B�B	 B	2B	%B�B��B�B��B�B�hB�[B�]B�NB�HB�7B�B��B��BŻB��B��B�gB�HB�>B�0B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�sB�ZB�IB�BB�6B�1B�,B�&B�B�BB~B}B|Bz�B|Bz�B{Bz�B|Bz�By�Bz�Bz�Bx�Bv�Bs�Bq�Bp�Bt�Bv�Bw�Bu�Bv�Bv�Bv�Bv�Bt�Bs�Bs�Br�Bq�Bq�Br�Bs�Bt�Bw�Bx�Bv�Bu�Bt�Bt�Bu�Bv�Bv�Bv�Bw�By�By�B}B~B~BB�B�)B�5B�;B�LB�[B�`B�qB��B��B��B��B��B��B��B��B� B�B�B�#B�/B�7B�JB�UB�fB�iB�pB�qB��B��BðBŻBƿB��B��B��B��B��B� B�	B�)B�HB�NB�UB�hB�xB�B�B�B�B��B��B�B	#B	3B	5B	5B	;B	
PB	lB	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	(B	,B	,B	1:B	7aB	8fB	;yB	=�B	>�B	B�B	D�B	D�B	E�B	I�B	J�B	M�B	TB	WB	Y+B	[7B	\=B	_OB	a]B	b`B	baB	baB	cgB	fxB	j�B	l�B	p�B	r�B	r�B	s�B	u�B	u�B	v�B	v�B	v�B	w�B	x�B	y�B	z�B	B	�B	�0B	�7B	�KB	�GB	�MB	�]B	�[B	�kB	�mB	�vB	�vB	�uB	�yB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�B	�$B	�1B	�8B	�EB	�OB	�VB	�dB	�nB	�sB	�{B	�|B	�yB	�yB	��B	��B	��B	��B	��B	��B	��B	¢B	åB	ųB	ƸB	ǽB	ǿB	ǿB	ǿB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�$B	�8B	�7B	�<B	�JB	�XB	�\B	�dB	�mB	�vB	�|B	�B	�B	�B	�B	�B	�B	��B	�B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�
B	� B	�	B	�
B	�
B	�
B
 B
B
!B
'B
SB
�B
�B
"�B
)B
,B
2<B
8`B
?�B
E�B
L�B
R�B
\6B
aTB
ekB
h~B
j�B
n�B
t�B
v�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.06 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214292016053112142920160531121429  AO  ARCAADJP                                                                    20140721230520    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230520  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230520  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121429  IP                  G�O�G�O�G�O�                