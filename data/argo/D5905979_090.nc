CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-18T14:14:00Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         ZPRIMARY | https://orcid.org/0000-0001-7324-3159 | Matthew Alkire, University of Washington        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7(   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7,   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    70   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7@   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7P   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7`   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7h   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8    DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8$   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8D   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8H   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8L   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8l   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
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
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �T   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �d   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �h   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �x   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �|   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200618141400  20220204114420  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               ZA   AO  7662                            2C  D   APEX                            8312                            080318                          846 @�Ν�1   @�Ν�8�P@7}p��
=�c��+J1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    ZA   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBG��BO��BX  B`  Bh  Bp  Bx  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C-�fC0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cw�fCz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D4��D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dy�3D�%qD�Y�D���D��\D�!HD�]qD���D��)D��D�k�D��)D��
D��D�Q�Dڍ�D��D�)D�YHD��D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�=q@�=q@�=qA�A9�AY�Ay�A��\A��\A��\A��\Ȁ\A܏\A�\A��\BG�BG�BG�BG�B&G�B.G�B6G�B>�BE�HBM�HBVG�B^G�BfG�BnG�BvG�B~G�B�#�B�W
B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�W
B�#�B�#�C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-xRC/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��CwxRCy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��)C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D d{D �{Dd{D�{Dd{D�{Dd{D�{Dd{D�{Dd{D�{Dd{D�{Dd{D�{Dd{D�{D	d{D	�{D
d{D
�{Dd{D�{Dj�D�{Dd{D�{Dd{D�{Dd{D�{Dd{D�{Dd{D�{Dd{D�{Dd{D�{Dd{D�{D^D�{Dd{D�{Dd{D�{Dd{D�{Dd{D�{Dd{D�{Dd{D�{Dd{D�{Dd{D�{Dd{D�{Dd{D�{D d{D �{D!d{D!�{D"d{D"�{D#d{D#�{D$d{D$�{D%d{D%�{D&d{D&�{D'd{D'�{D(d{D(�{D)d{D)�{D*d{D*�{D+d{D+�{D,d{D,�{D-d{D-�{D.d{D.�{D/d{D/�{D0d{D0�{D1d{D1�{D2d{D2�{D3d{D3�{D4d{D4�D5d{D5�{D6d{D6�{D7d{D7�{D8d{D8�{D9d{D9�{D:d{D:�{D;d{D;�{D<d{D<�{D=d{D=�{D>d{D>�{D?d{D?�{D@d{D@�{DAd{DA�{DBd{DB�{DCd{DC�{DDd{DD�{DEd{DE�{DFd{DF�{DGd{DG�{DHd{DH�{DId{DI�{DJd{DJ�{DKd{DK�{DLd{DL�{DMd{DM�{DNd{DN�{DOd{DO�{DPd{DP�{DQd{DQ�{DRd{DR�{DSd{DS�{DTd{DT�{DUd{DU�{DVd{DV�{DWd{DW�{DXd{DX�{DYd{DY�{DZd{DZ�{D[d{D[�{D\d{D\�{D]d{D]�{D^d{D^�{D_d{D_�{D`d{D`�{Dad{Da�{Dbd{Db�{Dcd{Dc�{Ddd{Dd�{Ded{De�{Dfd{Df�{Dgd{Dg�{Dhd{Dh�{Did{Di�{Djd{Dj�{Dkd{Dk�{Dld{Dl�{Dmd{Dm�{Dnd{Dn�{Dod{Do�{Dpd{Dp�{Dqd{Dq�{Drd{Dr�{Dsd{Ds�{Dtd{Dt�{Dy��D��D�K�D���D���D��D�O�D�� D��fD�D�]�D��fD��GD�
D�D)Dڀ D���D�fD�K�D�3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�`BA�`BA�bNA�dZA�`BA�`BA�`BA�`BA�bNA�bNA�dZA�ffA�ffA�hsA�ffA�hsA�l�A�n�A�n�A�`BA�\)A���AԁA�~�A�+A��A�ȴA��HA̰!A���A�(�A�
=A�1'A���A��A�ffA��#A���A���A��A�E�A��-A���A�?}A��;A��A�bNA��A��A��A�{A���A�n�A�1A��mA�XA���A�\)A���A��/A���A���A��/A�1A��`A��-A��\A�ffA�(�A��A��!A�C�A�;dA�ZA��yA�n�A�A�A���A���A�\)A�{A���A��9A��A�|�A�ZA�+A���A���A�Q�A���A���A��hA�O�A��;A�5?A�%A���A��jA�bNA�-A���A�1A�M�A�VA�ȴA��`A�5?A�p�A�jA���A�S�A��A�"�A���A�v�A���A��A��-A��FA�{A{�AzȴAzE�Ax�jAu|�Ar�\Ap�An��Amt�Aj��Ag\)Ae`BAc��Aa��A`-A]�A\�yAZ^5AW�AV��AU�AT{AS/ARA�AP�AO�AL�AI&�AGhsAE��ADjAC�#ACƨAC�FAC`BAA��A@5?A?C�A=�
A=x�A=oA;hsA9��A89XA5��A4jA3&�A2�9A2 �A1O�A/�hA.M�A,�RA+p�A)ƨA)l�A)�A(�DA'�A&�uA%/A$5?A#��A"n�A!��A ��A ~�A��A��AAhsA�9A��A�A~�A��A��AC�AI�A��A��Av�AC�A?}A�DAM�Al�Az�A=qA1At�AVA~�A�A��A&�A	%A�hA"�AȴAbNA��A�A~�AE�A��AbNAJA��AS�A ��@�M�@��@��/@�o@�@��u@�
=@�~�@�=q@�`B@�\)@�5?@�I�@�"�@�J@��@�ȴ@���@���@�b@���@���@�V@��;@�\)@�V@߾w@ޟ�@���@�l�@�33@ڗ�@�?}@��;@ו�@�v�@ԋD@�t�@�@�@Ь@υ@�=q@��T@ͩ�@́@�%@���@�|�@�33@ʗ�@�ff@�=q@�O�@ț�@�I�@ǅ@��y@�ff@���@�/@�bN@�x�@��@��
@�dZ@�o@��y@�E�@���@�7L@��u@�A�@�
=@�$�@���@���@�ƨ@�"�@��@�1@��F@�ƨ@�l�@��@��R@���@���@��@��!@���@�x�@���@�I�@�ƨ@�o@�"�@�C�@�
=@���@�@���@�Z@�C�@�{@�%@�;d@�n�@��u@�;d@��@�
=@���@��+@�^5@�J@��h@�O�@���@���@���@�Ĝ@���@��`@�l�@�=q@�=q@�E�@�v�@��@�l�@��\@���@�5?@���@���@�x�@��@���@�G�@�p�@��7@�x�@��-@�V@�C�@�|�@���@�bN@�j@��m@��F@�dZ@��@���@��@���@��7@�/@���@�Ĝ@��9@�Q�@�1@���@��m@��m@���@�S�@�ȴ@��@�@�V@�E�@��@��^@�x�@�O�@�&�@�V@���@��9@�z�@�Q�@�9X@��@���@�33@�
=@��y@��H@�ȴ@�v�@�-@�G�@���@��`@���@��@�r�@�9X@�1@��
@�dZ@��y@��R@�ff@�{@��-@�?}@��9@��D@�1'@���@���@�t�@�K�@��@��@��R@��\@�V@�-@�5?@�V@�-@���@�hs@�O�@��@��@��j@���@��@�Z@�Q�@�9X@�b@���@��w@�t�@�"�@��H@��R@�v�@�^5@�E�@�5?@�J@���@�X@��j@�A�@�9X@�9X@��@���@�w�@��z@z�@te�@m�9@c�&@[�+@U5�@Nd�@D�Y@=��@68�@0r�@*��@#33@��@^5@.I@��@�@W?111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�`BA�`BA�bNA�dZA�`BA�`BA�`BA�`BA�bNA�bNA�dZA�ffA�ffA�hsA�ffA�hsA�l�A�n�A�n�A�`BA�\)A���AԁA�~�A�+A��A�ȴA��HA̰!A���A�(�A�
=A�1'A���A��A�ffA��#A���A���A��A�E�A��-A���A�?}A��;A��A�bNA��A��A��A�{A���A�n�A�1A��mA�XA���A�\)A���A��/A���A���A��/A�1A��`A��-A��\A�ffA�(�A��A��!A�C�A�;dA�ZA��yA�n�A�A�A���A���A�\)A�{A���A��9A��A�|�A�ZA�+A���A���A�Q�A���A���A��hA�O�A��;A�5?A�%A���A��jA�bNA�-A���A�1A�M�A�VA�ȴA��`A�5?A�p�A�jA���A�S�A��A�"�A���A�v�A���A��A��-A��FA�{A{�AzȴAzE�Ax�jAu|�Ar�\Ap�An��Amt�Aj��Ag\)Ae`BAc��Aa��A`-A]�A\�yAZ^5AW�AV��AU�AT{AS/ARA�AP�AO�AL�AI&�AGhsAE��ADjAC�#ACƨAC�FAC`BAA��A@5?A?C�A=�
A=x�A=oA;hsA9��A89XA5��A4jA3&�A2�9A2 �A1O�A/�hA.M�A,�RA+p�A)ƨA)l�A)�A(�DA'�A&�uA%/A$5?A#��A"n�A!��A ��A ~�A��A��AAhsA�9A��A�A~�A��A��AC�AI�A��A��Av�AC�A?}A�DAM�Al�Az�A=qA1At�AVA~�A�A��A&�A	%A�hA"�AȴAbNA��A�A~�AE�A��AbNAJA��AS�A ��@�M�@��@��/@�o@�@��u@�
=@�~�@�=q@�`B@�\)@�5?@�I�@�"�@�J@��@�ȴ@���@���@�b@���@���@�V@��;@�\)@�V@߾w@ޟ�@���@�l�@�33@ڗ�@�?}@��;@ו�@�v�@ԋD@�t�@�@�@Ь@υ@�=q@��T@ͩ�@́@�%@���@�|�@�33@ʗ�@�ff@�=q@�O�@ț�@�I�@ǅ@��y@�ff@���@�/@�bN@�x�@��@��
@�dZ@�o@��y@�E�@���@�7L@��u@�A�@�
=@�$�@���@���@�ƨ@�"�@��@�1@��F@�ƨ@�l�@��@��R@���@���@��@��!@���@�x�@���@�I�@�ƨ@�o@�"�@�C�@�
=@���@�@���@�Z@�C�@�{@�%@�;d@�n�@��u@�;d@��@�
=@���@��+@�^5@�J@��h@�O�@���@���@���@�Ĝ@���@��`@�l�@�=q@�=q@�E�@�v�@��@�l�@��\@���@�5?@���@���@�x�@��@���@�G�@�p�@��7@�x�@��-@�V@�C�@�|�@���@�bN@�j@��m@��F@�dZ@��@���@��@���@��7@�/@���@�Ĝ@��9@�Q�@�1@���@��m@��m@���@�S�@�ȴ@��@�@�V@�E�@��@��^@�x�@�O�@�&�@�V@���@��9@�z�@�Q�@�9X@��@���@�33@�
=@��y@��H@�ȴ@�v�@�-@�G�@���@��`@���@��@�r�@�9X@�1@��
@�dZ@��y@��R@�ff@�{@��-@�?}@��9@��D@�1'@���@���@�t�@�K�@��@��@��R@��\@�V@�-@�5?@�V@�-@���@�hs@�O�@��@��@��j@���@��@�Z@�Q�@�9X@�b@���@��w@�t�@�"�@��H@��R@�v�@�^5@�E�@�5?@�J@���@�X@��j@�A�@�9X@�9X@��G�O�@�w�@��z@z�@te�@m�9@c�&@[�+@U5�@Nd�@D�Y@=��@68�@0r�@*��@#33@��@^5@.I@��@�@W?111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�B	�B	�#B	�B	�#B	�#B	�sB	��B
+B
uB
B
M�B
�1B
�!B
�FB
�dB
��B
�5B
�/B
ÖB
��B
��B
ƨB
�B
�BB�B �B1'B;dB@�BE�BL�Be`Bm�B}�B�'B�^B�wB��B�BB��B��B��B��B��B��BB\B��B�B%BbB1'BJ�BW
BO�B[#Bu�B�bB�PB�7BW
BVB`BB�1B�\BVB@�B�B�B.B#�B%�B(�B#�B\B1B��B  B	7B��B�BB��B��B��B��B�JB[#BL�BD�B.B#�B%�B#�B%B
��B
�yB
�dB
��B
�B
_;B
H�B
:^B
$�B
JB
	7B
 �B
)�B
{B
  B	�B	�ZB	�B	ƨB	�B	��B	�VB	�B	x�B	n�B	hsB	ZB	L�B	C�B	=qB	6FB	2-B	.B	'�B	�B	�B	B��B�B�B�sB�mB�fB�ZB�TB�BB�5B�B�B��B��BƨB��B�RB�'B�B��B��B��B��B��B��B�uB�DB�7B�+B�B�B~�B{�Bv�Bv�Bu�Br�Bp�Bo�Bl�Bl�BiyBffBffBbNB_;B]/BZBXBW
BVBS�BR�BS�BP�BO�BK�BJ�BJ�BK�BK�BJ�BH�BG�BF�BD�BB�BA�BC�B@�B?}B?}B>wB?}B>wB>wB=qB@�B=qB<jB;dB:^B;dB:^B:^B<jB;dB9XB8RB8RB8RB8RB8RB9XB9XB9XB9XB7LB7LB6FB5?B49B49B5?B5?B5?B5?B49B7LB7LB9XB<jB;dB;dB<jB>wB>wB=qB?}B?}B@�B?}BB�BC�BC�BE�BE�BE�BE�BG�BI�BJ�BK�BM�BM�BN�BS�BVBZBaHBhsBm�Bw�Bu�Bt�Bp�BgmBl�Bo�Bq�Bq�Bt�Bu�Bx�Bx�B}�B�B�%B�7B�VB�JB�hB��B��B��B��B��B��B��B��B��B��B�B�!B�'B�FB�^B�jB��BBĜBǮB��B��B�/B�TB�TB�BB�NB�5B�BB�HB�/B�/B�;B�NB�NB�TB�fB�B�B�B�B�B�B�B��B	  B	B	%B	%B	+B	oB	�B	�B	�B	�B	!�B	!�B	$�B	'�B	)�B	/B	1'B	2-B	2-B	49B	9XB	B�B	J�B	L�B	S�B	\)B	_;B	aHB	cTB	dZB	gmB	iyB	jB	k�B	n�B	o�B	p�B	q�B	t�B	t�B	u�B	w�B	y�B	y�B	y�B	w�B	{�B	~�B	�B	�%B	�7B	�=B	�DB	�JB	�PB	�PB	�VB	�\B	�hB	�oB	�oB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�3B	�9B	�?B	�FB	�LB	�RB	�XB	�dB	�dB	�dB	�jB	�qB	�wB	�wB	�wB	�}B	��B	��B	��B	��B	B	B	ĜB	ŢB	ƨB	ǮB	ȴB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�]B	�;B	��B
VB
�B
7B
pB
)*B
1�B
:�B
G�B
H�B
N�B
TFB
YKB
^�B
bhB
f�B
k�B
n�B
qA111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�dB	��B	�B
dB	��B
E�B
�B
�B
�-B
�KB
��B
�B
�B
�~B
�kB
�lB
��B
�B
�~B
�B�B�B)
B3FB8eB=�BD�B]@BeqBu�B�B�:B�SB��B�B�B��B��B�B�B�B��B5B��B�B��B;B(�BB�BN�BG�BR�Bm�B�4B�"B�	BN�BM�BXB�B�/BM�B8[BnB�B%�B�B�B �B�B9B B��B��BB�B�eB�rB��B��B��B�sB�1BSBD�B<�B&B�B�B�B
�B
��B
�mB
�[B
��B
|B
W9B
@�B
2_B
�B
OB
<B
�B
" B
�B	�B	�B	�dB	�B	��B	�B	��B	�fB	{#B	p�B	f�B	`�B	R2B	D�B	;�B	5�B	.^B	*FB	&-B	 
B	�B	�B�#B�B��B�B��BߍBކB�zB�tB�cB�VB�9B�&B�B�B��B��B�wB�MB�4B�"B�B�B��B��B��B��B�nB�aBUB|CBz7Bw%BtBn�Bn�Bm�Bj�Bh�Bg�Bd�Bd�Ba�B^�B^�BZ}BWjBU^BRLBP@BO:BN4BL(BK#BL)BIBHBC�BB�BB�BC�BC�BB�B@�B?�B>�B<�B:�B9�B;�B8�B7�B7�B6�B7�B6�B6�B5�B8�B5�B4�B3�B2�B3�B2�B2�B4�B3�B1�B0�B0�B0�B0�B0�B1�B1�B1�B1�B/�B/�B.~B-wB,qB,rB-xB-xB-xB-xB,rB/�B/�B1�B4�B3�B3�B4�B6�B6�B5�B7�B7�B8�B7�B:�B;�B;�B=�B=�B=�B=�B?�BA�BB�BDBFBFBGBL2BN>BRVBY�B`�Be�BpBm�Bl�Bh�B_�Bd�Bg�Bi�Bi�Bl�Bm�BqBqBv-B{KB~]B�oB��B��B��B��B��B��B��B��B��B��B�B�B�3B�EB�XB�^B�|B��B��B��B��B��B��B��B�'B�cBۈBۈB�vBڂB�iB�vB�}B�dB�dB�pBڃBڃBۉBޚB�B�B��B��B��B��B��B��B�3B�LB�XB�XB�^B	
�B	�B	�B	�B	�B	�B	�B	B	 !B	"-B	'KB	)WB	*]B	*]B	,iB	1�B	:�B	B�B	D�B	L&B	TVB	WhB	YuB	[�B	\�B	_�B	a�B	b�B	c�B	f�B	g�B	h�B	i�B	l�B	l�B	m�B	o�B	rB	rB	rB	o�B	tB	w&B	|DB	~PB	�bB	�hB	�oB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	� B	�,B	�,B	�,B	�8B	�>B	�EB	�KB	�QB	�WB	�]B	�bB	�hB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�G�O�B	ԅB	�bB	��B
{B
B
\B
�B
!OB
)�B
3B
@B
@�B
F�B
LiB
QnB
V�B
Z�B
_B
c�B
gB
ic111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.43 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.008(+/-0.005) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144202022020411442020220204114420  AO  ARCAADJP                                                                    20200618141400    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200618141400  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200618141400  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114420  IP                  G�O�G�O�G�O�                