CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:07Z AOML 3.0 creation; 2016-05-31T19:14:24Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230507  20160531121424  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  4051_7090_002                   2C  D   APEX                            5368                            041511                          846 @�6�+��1   @�6�����@3l������d��x���1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�fC�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DyffD� D�L�D�� D��3D���D�S3D�|�D�ٚD�fD�FfD�l�D��3D� D�L�Dڀ D�� D��D�C3D�3D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�z�@�{@�{A
=A?
=A_
=A
=A��A��A��A��AυA߅A�A��BBBBB'B/B7B?BGBOBWB_BgBoBwBB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HC�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�
C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dyb�D�D�J�D�~D��GD���D�QGD�z�D�׮D�zD�DzD�j�D��GD�D�J�D�~D��D�
�D�AGD�GD��G11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�~�A�~�AǅAǉ7AǋDAǇ+AǇ+AǅAǃAǅAǍPAǑhAǑhAǓuAǗ�AǕ�AǕ�AǕ�AǓuAǕ�AǗ�AǙ�AǛ�Aǝ�Aǝ�Aǡ�Aǡ�Aǡ�Aǣ�Aǧ�Aǩ�AǬAǮAǮAǧ�Aǥ�Aǡ�AǕ�AǇ+A�r�A�VA�oA��A�AƃA�`BA�33A�  AŴ9A�n�A�K�A�33A�1A���A�hsA�(�A��;AöFAç�Aá�AÙ�AÏ\A�~�A�S�A�?}A�;dA�(�A�ȴA��+A���A�ȴA�ȴA�dZA�O�A�x�A��A��A�5?A�ȴA��A�1A�z�A��TA��A�n�A���A��PA�A�33A��A�=qA��9A��PA�dZA���A���A��+A�1'A�oA�r�A�I�A�l�A��jA��HA��A�`BA���A�oA���A���A�jA�ĜA���A�oA�A�v�A��A��A���A���A�E�A��A�=qA�VA��7A���A�oA�Q�A~��A}�A{�Ay�-Ax  Av��At^5As\)Aq�TAn�Ak�#Ah�Ag"�Ae�7Ab��A`r�A^��A[�
AZn�AY33AW�hAUAS\)AQt�AO��AN$�AL�AK�AH��AD�HAD-AD1AD1AC�FAAp�A@�yA?��A>��A=O�A;�A9�wA7�PA7;dA5�#A3�hA2�RA133A0bNA/?}A.1'A-%A,�jA,�A*�/A)VA(=qA'+A&�!A%��A%�hA%%A#G�A!C�A VA�A�A�\A^5A�!AXA �A��A�/AO�AZAG�AA��A�A��A(�A�A
1'A�uA�FA�An�A(�AVA�A33A�\A��A"�A ĜA �@���@�^5@�z�@�5?@���@���@�1'@�|�@��@�!@�M�@��@�7L@�Q�@�|�@�P@�P@�R@�-@�h@��@��
@�l�@�
=@�%@��;@�n�@���@�Z@���@߅@޸R@���@�j@��;@ۍP@�S�@ۥ�@۝�@�M�@�X@�b@���@�^5@�Z@�;d@���@ͺ^@�  @˅@��y@�;d@�
=@�x�@�I�@��@�  @���@�7L@��D@��@�"�@�`B@��@�j@�ȴ@�5?@��`@�A�@���@��@�E�@���@�&�@��`@�9X@�b@�1@��@���@���@���@���@��@��\@�X@�Ĝ@��
@�dZ@�C�@�+@��y@��!@���@��+@�V@�$�@��h@�G�@��/@��u@�I�@�b@�1@�9X@�(�@��m@��@�"�@��H@�M�@���@�@��@��@���@���@��@���@���@�t�@���@��!@��+@��@��T@�@���@��@�G�@�%@���@�r�@�(�@���@�+@��@�~�@�-@��#@���@�`B@�V@��`@�Ĝ@�Z@��
@��w@���@���@��P@�t�@�t�@�l�@�+@��@�ȴ@���@�ff@�E�@�{@�J@���@���@���@�7L@�%@��/@���@�r�@� �@���@�S�@�
=@��@��!@��+@�M�@���@���@�x�@�V@���@���@�r�@�Q�@�I�@�(�@�1@�  @���@�dZ@��@���@���@�ȴ@��R@��!@���@�n�@�$�@��@���@���@�x�@�hs@�7L@��@��`@�r�@�1'@���@��F@�l�@�S�@�;d@�@�ȴ@�~�@�V@��^@���@�x�@�`B@�/@�%@��9@�Z@�ƨ@���@�|�@�C�@��H@��R@��!@���@��+@�M�@���@���@��@�p�@�`B@�%@�z�@�  @��
@���@���@�S�@�"�@�
=@���@���@�~�@�^5@�5?@��@��#@���@��7@�7L@�%@��j@��\@��/@w��@p�9@f�y@_\)@Up�@KdZ@C��@<�@7�w@0�9@+o@%�@ �u@��@  @"�@v�@�@Ĝ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�~�A�~�AǅAǉ7AǋDAǇ+AǇ+AǅAǃAǅAǍPAǑhAǑhAǓuAǗ�AǕ�AǕ�AǕ�AǓuAǕ�AǗ�AǙ�AǛ�Aǝ�Aǝ�Aǡ�Aǡ�Aǡ�Aǣ�Aǧ�Aǩ�AǬAǮAǮAǧ�Aǥ�Aǡ�AǕ�AǇ+A�r�A�VA�oA��A�AƃA�`BA�33A�  AŴ9A�n�A�K�A�33A�1A���A�hsA�(�A��;AöFAç�Aá�AÙ�AÏ\A�~�A�S�A�?}A�;dA�(�A�ȴA��+A���A�ȴA�ȴA�dZA�O�A�x�A��A��A�5?A�ȴA��A�1A�z�A��TA��A�n�A���A��PA�A�33A��A�=qA��9A��PA�dZA���A���A��+A�1'A�oA�r�A�I�A�l�A��jA��HA��A�`BA���A�oA���A���A�jA�ĜA���A�oA�A�v�A��A��A���A���A�E�A��A�=qA�VA��7A���A�oA�Q�A~��A}�A{�Ay�-Ax  Av��At^5As\)Aq�TAn�Ak�#Ah�Ag"�Ae�7Ab��A`r�A^��A[�
AZn�AY33AW�hAUAS\)AQt�AO��AN$�AL�AK�AH��AD�HAD-AD1AD1AC�FAAp�A@�yA?��A>��A=O�A;�A9�wA7�PA7;dA5�#A3�hA2�RA133A0bNA/?}A.1'A-%A,�jA,�A*�/A)VA(=qA'+A&�!A%��A%�hA%%A#G�A!C�A VA�A�A�\A^5A�!AXA �A��A�/AO�AZAG�AA��A�A��A(�A�A
1'A�uA�FA�An�A(�AVA�A33A�\A��A"�A ĜA �@���@�^5@�z�@�5?@���@���@�1'@�|�@��@�!@�M�@��@�7L@�Q�@�|�@�P@�P@�R@�-@�h@��@��
@�l�@�
=@�%@��;@�n�@���@�Z@���@߅@޸R@���@�j@��;@ۍP@�S�@ۥ�@۝�@�M�@�X@�b@���@�^5@�Z@�;d@���@ͺ^@�  @˅@��y@�;d@�
=@�x�@�I�@��@�  @���@�7L@��D@��@�"�@�`B@��@�j@�ȴ@�5?@��`@�A�@���@��@�E�@���@�&�@��`@�9X@�b@�1@��@���@���@���@���@��@��\@�X@�Ĝ@��
@�dZ@�C�@�+@��y@��!@���@��+@�V@�$�@��h@�G�@��/@��u@�I�@�b@�1@�9X@�(�@��m@��@�"�@��H@�M�@���@�@��@��@���@���@��@���@���@�t�@���@��!@��+@��@��T@�@���@��@�G�@�%@���@�r�@�(�@���@�+@��@�~�@�-@��#@���@�`B@�V@��`@�Ĝ@�Z@��
@��w@���@���@��P@�t�@�t�@�l�@�+@��@�ȴ@���@�ff@�E�@�{@�J@���@���@���@�7L@�%@��/@���@�r�@� �@���@�S�@�
=@��@��!@��+@�M�@���@���@�x�@�V@���@���@�r�@�Q�@�I�@�(�@�1@�  @���@�dZ@��@���@���@�ȴ@��R@��!@���@�n�@�$�@��@���@���@�x�@�hs@�7L@��@��`@�r�@�1'@���@��F@�l�@�S�@�;d@�@�ȴ@�~�@�V@��^@���@�x�@�`B@�/@�%@��9@�Z@�ƨ@���@�|�@�C�@��H@��R@��!@���@��+@�M�@���@���@��@�p�@�`B@�%@�z�@�  @��
@���@���@�S�@�"�@�
=@���@���@�~�@�^5@�5?@��@��#@���@��7@�7L@�%@��j@��\@��/@w��@p�9@f�y@_\)@Up�@KdZ@C��@<�@7�w@0�9@+o@%�@ �u@��@  @"�@v�@�@Ĝ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B� B� B� B� B� B� B� B�B�B�B�B�B�B�B�B�B� B�B�B�B�B�B�B�B�B�B�B�1B�DB�JB�bB�{B�{B��B��B�-B�B�B��B�B#�B33B>wBC�BI�BM�BW
BZBYBXBVBT�BXBZBXBXB\)B^5B_;B_;B^5B]/B\)B]/B_;BXBN�BD�B?}B9XB/B�B�sB��B�LB�B��B�uB�B{�Bv�Bv�Bl�BZBK�BB�B:^B,B�B\BPBDBBBB��B�NB��B�wB�3B�B��B��B��B{�Bo�BYBI�B9XB.B)�B�BB
�B
�yB
�BB
��B
ǮB
��B
�FB
�'B
�B
��B
��B
�B
v�B
gmB
`BB
M�B
B�B
8RB
/B
�B
�B
DB	��B	�B	�/B	��B	ǮB	�LB	��B	��B	�hB	�1B	�B	y�B	q�B	gmB	^5B	VB	M�B	F�B	>wB	5?B	'�B	$�B	#�B	"�B	�B	�B	�B	hB	DB	%B��B��B�B�B�yB�NB�;B�/B�B�B��B��B��B��BĜB�}B�dB�LB�?B�-B�B�B��B��B��B��B�uB�oB�bB�JB�1B�+B�+B�B�B~�Bz�Bu�Bt�Bo�Bl�BjBiyBgmBe`BcTBaHB_;B^5B\)B[#BZBYBXBXBW
BXBW
BS�BQ�BO�BO�BO�BO�BO�BO�BO�BO�BN�BO�BR�B]/BaHBffBffBdZBbNBe`Br�Bw�Bx�Bt�Br�Bu�Bz�B{�B}�B� B�B�B�+B�7B�DB�\B��B��B��B��B�B�!B�!B�!B�!B��B��B��B��B�B�B�B�B�B��B�B�9B�RB�dB�qB��BĜBǮBǮBǮBǮB��B��B��B�B�5B�ZB�`B�`B�mB�yB�B�B�B�B�B�B��B��B��B��B	B	+B	DB	JB	PB	\B	bB	hB	�B	�B	�B	!�B	(�B	-B	0!B	1'B	2-B	5?B	5?B	8RB	<jB	>wB	@�B	B�B	C�B	F�B	K�B	O�B	P�B	O�B	P�B	R�B	W
B	ZB	]/B	_;B	dZB	hsB	iyB	jB	jB	l�B	n�B	p�B	q�B	t�B	u�B	y�B	{�B	}�B	� B	�B	�%B	�+B	�7B	�DB	�DB	�DB	�PB	�hB	�hB	�oB	�oB	�oB	�uB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�3B	�9B	�FB	�RB	�^B	�dB	�jB	�wB	�wB	�wB	�}B	�}B	��B	ÖB	ĜB	ŢB	ƨB	ƨB	ƨB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�)B	�)B	�5B	�;B	�;B	�5B	�5B	�5B	�5B	�5B	�;B	�BB	�HB	�NB	�ZB	�`B	�fB	�fB	�sB	�sB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
JB
�B
�B
)�B
1'B
9XB
@�B
G�B
M�B
Q�B
T�B
ZB
_;B
e`B
jB
m�B
q�B
v�B
y�B
|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�%B�B�B�B�B�(B�9B�IB�TB�gB�}B�B��B��B�4B�B�B��B�B#�B38B>�BC�BI�BM�BWBZ(BY"BXBV
BUBXBZ*BXBXB\1B^=B_CB_CB^:B]6B\3B]5B_DBXBN�BD�B?�B9`B/"B�B�wB� B�SB�B��B�{B�B{�Bv�Bv�Bl�BZ!BK�BB�B:cB,B�BaBUBHB&BBB��B�RB��B�}B�9B�B��B��B��B{�Bo�BYBI�B9\B.B*B�B"B
�B
�B
�JB
��B
ǶB
��B
�PB
�4B
�B
��B
��B
�&B
v�B
gyB
`LB
M�B
B�B
8`B
/(B
�B
�B
SB	��B	�B	�>B	�B	ǿB	�\B	�B	��B	�{B	�CB	�B	y�B	q�B	g�B	^IB	VB	M�B	F�B	>�B	5UB	(B	$�B	#�B	"�B	�B	�B	�B	}B	ZB	:B�B��B��B�B�B�fB�TB�HB�7B�B�B��B��B��BĴB��B��B�fB�YB�HB�7B�B�B��B��B��B��B��B�}B�gB�NB�HB�IB�6B�)BBz�Bu�Bt�Bo�Bl�Bj�Bi�Bg�BeBcqBaiB_XB^UB\HB[ABZ;BY8BX0BX/BW)BX0BW(BTBR
BO�BO�BO�BO�BO�BO�BO�BO�BN�BO�BSB]LBagBf�Bf�BdwBbjBe~Br�Bw�Bx�Bt�Br�Bu�Bz�B|B~B�B�6B�:B�JB�RB�`B�zB��B��B��B��B�6B�;B�>B�;B�;B�B��B��B�B�B�'B�&B�)B�B�B�7B�SB�mB�|B��B��BķB��B��B��B��B��B��B��B�B�LB�qB�uB�wB�B�B�B�B�B�B�B�B��B��B��B�B	1B	@B	YB	`B	iB	sB	zB	B	�B	�B	�B	!�B	)
B	-"B	05B	1;B	2BB	5RB	5TB	8hB	<�B	>�B	@�B	B�B	C�B	F�B	K�B	O�B	P�B	O�B	P�B	SB	WB	Z2B	]CB	_PB	dmB	h�B	i�B	j�B	j�B	l�B	n�B	p�B	q�B	t�B	u�B	y�B	{�B	~B	�B	�$B	�6B	�=B	�IB	�UB	�WB	�UB	�aB	�zB	�{B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�$B	�2B	�EB	�JB	�XB	�dB	�oB	�tB	�{B	��B	��B	��B	��B	��B	��B	çB	ĬB	ŲB	ƸB	ƹB	ƵB	ǽB	ǾB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�	B	�B	�B	�!B	�$B	�3B	�9B	�9B	�CB	�IB	�KB	�DB	�DB	�FB	�FB	�EB	�HB	�QB	�XB	�\B	�jB	�nB	�tB	�vB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B
YB
�B
�B
*B
16B
9dB
@�B
G�B
M�B
Q�B
U
B
Z)B
_FB
emB
j�B
m�B
q�B
v�B
y�B
|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.06 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214242016053112142420160531121424  AO  ARCAADJP                                                                    20140721230507    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230507  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230507  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121424  IP                  G�O�G�O�G�O�                