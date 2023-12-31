CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:06:39Z AOML 3.0 creation; 2016-05-31T21:48:58Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230639  20160531144858  5903738 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               	A   AO  4053_7107_009                   2C  D   APEX                            5370                            041511                          846 @�AA��{�1   @�ABCQ�@8�z�G��b���Q�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    	A   A   A   @�ff@�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�33A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� DtfDts3Dy��D�fD�9�D�|�D���D��D�6fD��fD�ɚD�	�D�<�D�P D�ɚD�3D�<�Dڐ D��3D���D�FfD� D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�G�@��H@��Ap�A=p�A]p�A}p�A��RA��RA��RA��RAθRA��A�RA��RB\)B\)B\)B\)B'\)B/\)B7\)B?\)BG\)BO\)BW\)B_\)Bg\)Bo\)Bw\)B\)B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BîBǮBˮBϮBӮB׮BۮB߮B�B�B�B�B�B��B��B��C�
C�
C�
C�
C	�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C!�
C#�
C%�
C'�
C)�
C+�
C-�
C/�
C1�
C3�
C5�
C7�
C9�
C;�
C=�
C?�
CA�
CC�
CE�
CG�
CI�
CK�
CM�
CO�
CQ�
CS�
CU�
CW�
CY�
C[�
C]�
C_�
Ca�
Cc�
Ce�
Cg�
Ci�
Ck�
Cm�
Co�
Cq�
Cs�
Cu�
Cw�
Cy�
C{�
C}�
C�
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D u�D ��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��D	u�D	��D
u�D
��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��D u�D ��D!u�D!��D"u�D"��D#u�D#��D$u�D$��D%u�D%��D&u�D&��D'u�D'��D(u�D(��D)u�D)��D*u�D*��D+u�D+��D,u�D,��D-u�D-��D.u�D.��D/u�D/��D0u�D0��D1u�D1��D2u�D2��D3u�D3��D4u�D4��D5u�D5��D6u�D6��D7u�D7��D8u�D8��D9u�D9��D:u�D:��D;u�D;��D<u�D<��D=u�D=��D>u�D>��D?u�D?��D@u�D@��DAu�DA��DBu�DB��DCu�DC��DDu�DD��DEu�DE��DFu�DF��DGu�DG��DHu�DH��DIu�DI��DJu�DJ��DKu�DK��DLu�DL��DMu�DM��DNu�DN��DOu�DO��DPu�DP��DQu�DQ��DRu�DR��DSu�DS��DTu�DT��DUu�DU��DVu�DV��DWu�DW��DXu�DX��DYu�DY��DZu�DZ��D[u�D[��D\u�D\��D]u�D]��D^u�D^��D_u�D_��D`u�D`��Dau�Da��Dbu�Db��Dcu�Dc��Ddu�Dd��Deu�De��Dfu�Df��Dgu�Dg��Dhu�Dh��Diu�Di��Dju�Dj��Dku�Dk��Dlu�Dl��Dmu�Dm��Dnu�Dn��Dou�Do��Dpu�Dp��Dqu�Dq��Dru�Dr��Dsu�Ds�)Dth�DyD�GD�4{D�w�D�ǮD��D�1GD��GD��{D�{D�7�D�J�D��{D��D�7�Dڊ�D�D��{D�AGD�z�D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A��A�{A�oA�oA�VA�JA�JA�%A�A�A���A�ĜA��-A���A���A��-A���A�1'A�9XA�E�A��A�;dA�5?A��#A�^5A�1A�r�A�bA�VA��A��9A��PA�^5A���A�  A�hsA�jA�n�A��A���A��9A���A���A��A���A��FA�A�A��HA�v�A�C�A���A�r�A��HA�|�A���A�ƨA�p�A��A���A�7LA�~�A��A��A���A��RA��-A���A��A�ȴA�v�A��uA��\A���A�M�A�|�A�1'A��A��A�t�A�~�A�33A���A���A�A�bNA��yA��`A�jA��\A��TA��uA��A�z�A��A�
=A��PA�O�A�33A"�A}
=Az�DAx�HAx$�AvVAt�jAs��Ar��Aq?}Ao��Al�!Aj��AidZAh�/Ag?}AfJAc�^Ab��Aa��A`v�A^��A^{A]VA[�AZQ�AY�PAX��AX1AV�/AVn�AU7LAS+AR=qAQK�AO�AN��AM?}ALVAKƨAJ�yAI�AHVAH �AG`BAF9XAE&�AC�^AB�uA@��A@�A?`BA>�9A=��A="�A<{A;/A:�uA:M�A:1'A:{A8�/A8�uA8��A8�RA8��A8�RA85?A6��A5A5oA4I�A3��A2�uA1�A0�9A0  A/p�A.A�A,bNA)��A'�wA%�-A$A�A#hsA"�HA"�DA!�^A �HA ZA��A �A�^A/AjA�AC�AjA7LA�mA&�A�A �AVA1A��A�A�A�`A  A
jA	�
A	�PA��Az�A�A�TA|�AZA�`A�\A�A�A n�@��y@�p�@��/@��m@��H@�/@�  @���@��7@�Q�@�@���@�Q�@�Ĝ@� �@�+@�A�@��@�E�@�7L@�"�@���@��m@�R@�=q@�M�@�j@��m@��@��@��@�/@�t�@�7L@�Z@�1@�ƨ@�t�@�5?@܋D@ڟ�@ٙ�@�I�@��T@�r�@҇+@�G�@ϕ�@�v�@�V@�1@�l�@�~�@ɉ7@�r�@�C�@���@�Z@��@�\)@�M�@��h@�Z@��@���@�;d@�ff@��#@� �@�dZ@�^5@�7L@�V@�%@��`@���@� �@�;d@�ȴ@�^5@�@���@�hs@��/@�1'@�
=@��@���@���@��@��@���@���@�p�@�&�@��j@�j@���@��R@���@�&�@���@�1@�C�@��\@�E�@��@�/@��u@��`@�?}@���@�1@�33@��-@��@�ƨ@�dZ@�+@��H@�ff@���@�/@�%@��j@�A�@��@�  @��;@��@��@�dZ@�+@��H@�M�@�@�@��@���@�Z@��@�ƨ@��F@�t�@�o@��R@��R@�ff@���@�@�hs@��@��D@�1@���@���@�@�O�@�Z@��m@�  @��@�t�@�;d@��+@���@���@�O�@�%@���@��9@���@��D@�z�@�Q�@�A�@�9X@�1'@��@�1@��@���@���@��@�\)@���@��!@��!@���@��+@�v�@�n�@�V@�$�@�@���@���@��@�hs@�O�@��9@�Z@�Q�@�I�@�9X@�A�@�A�@��@��@��@��P@�t�@�l�@�\)@�K�@�\)@�S�@�\)@�t�@��H@�33@�o@�"�@�33@�
=@�n�@�=q@�&�@�r�@�9X@� �@�9X@�Q�@�b@��@��@�ƨ@��w@��w@�1@�9X@�Z@�(�@��;@��w@�l�@�\)@�\)@�\)@�C�@�
=@��@��H@�ȴ@��R@���@�~�@�n�@�V@�-@��#@���@�x�@�G�@���@;d@vv�@o+@g�w@_��@Y��@T��@N5?@H1'@A��@;S�@4�@.��@+@&�y@#C�@|�@"�@
=@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��A��A�{A�oA�oA�VA�JA�JA�%A�A�A���A�ĜA��-A���A���A��-A���A�1'A�9XA�E�A��A�;dA�5?A��#A�^5A�1A�r�A�bA�VA��A��9A��PA�^5A���A�  A�hsA�jA�n�A��A���A��9A���A���A��A���A��FA�A�A��HA�v�A�C�A���A�r�A��HA�|�A���A�ƨA�p�A��A���A�7LA�~�A��A��A���A��RA��-A���A��A�ȴA�v�A��uA��\A���A�M�A�|�A�1'A��A��A�t�A�~�A�33A���A���A�A�bNA��yA��`A�jA��\A��TA��uA��A�z�A��A�
=A��PA�O�A�33A"�A}
=Az�DAx�HAx$�AvVAt�jAs��Ar��Aq?}Ao��Al�!Aj��AidZAh�/Ag?}AfJAc�^Ab��Aa��A`v�A^��A^{A]VA[�AZQ�AY�PAX��AX1AV�/AVn�AU7LAS+AR=qAQK�AO�AN��AM?}ALVAKƨAJ�yAI�AHVAH �AG`BAF9XAE&�AC�^AB�uA@��A@�A?`BA>�9A=��A="�A<{A;/A:�uA:M�A:1'A:{A8�/A8�uA8��A8�RA8��A8�RA85?A6��A5A5oA4I�A3��A2�uA1�A0�9A0  A/p�A.A�A,bNA)��A'�wA%�-A$A�A#hsA"�HA"�DA!�^A �HA ZA��A �A�^A/AjA�AC�AjA7LA�mA&�A�A �AVA1A��A�A�A�`A  A
jA	�
A	�PA��Az�A�A�TA|�AZA�`A�\A�A�A n�@��y@�p�@��/@��m@��H@�/@�  @���@��7@�Q�@�@���@�Q�@�Ĝ@� �@�+@�A�@��@�E�@�7L@�"�@���@��m@�R@�=q@�M�@�j@��m@��@��@��@�/@�t�@�7L@�Z@�1@�ƨ@�t�@�5?@܋D@ڟ�@ٙ�@�I�@��T@�r�@҇+@�G�@ϕ�@�v�@�V@�1@�l�@�~�@ɉ7@�r�@�C�@���@�Z@��@�\)@�M�@��h@�Z@��@���@�;d@�ff@��#@� �@�dZ@�^5@�7L@�V@�%@��`@���@� �@�;d@�ȴ@�^5@�@���@�hs@��/@�1'@�
=@��@���@���@��@��@���@���@�p�@�&�@��j@�j@���@��R@���@�&�@���@�1@�C�@��\@�E�@��@�/@��u@��`@�?}@���@�1@�33@��-@��@�ƨ@�dZ@�+@��H@�ff@���@�/@�%@��j@�A�@��@�  @��;@��@��@�dZ@�+@��H@�M�@�@�@��@���@�Z@��@�ƨ@��F@�t�@�o@��R@��R@�ff@���@�@�hs@��@��D@�1@���@���@�@�O�@�Z@��m@�  @��@�t�@�;d@��+@���@���@�O�@�%@���@��9@���@��D@�z�@�Q�@�A�@�9X@�1'@��@�1@��@���@���@��@�\)@���@��!@��!@���@��+@�v�@�n�@�V@�$�@�@���@���@��@�hs@�O�@��9@�Z@�Q�@�I�@�9X@�A�@�A�@��@��@��@��P@�t�@�l�@�\)@�K�@�\)@�S�@�\)@�t�@��H@�33@�o@�"�@�33@�
=@�n�@�=q@�&�@�r�@�9X@� �@�9X@�Q�@�b@��@��@�ƨ@��w@��w@�1@�9X@�Z@�(�@��;@��w@�l�@�\)@�\)@�\)@�C�@�
=@��@��H@�ȴ@��R@���@�~�@�n�@�V@�-@��#@���@�x�@�G�@���@;d@vv�@o+@g�w@_��@Y��@T��@N5?@H1'@A��@;S�@4�@.��@+@&�y@#C�@|�@"�@
=@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB/B/B0!B0!B0!B0!B0!B/B0!B0!B0!B0!B0!B[#B�B��B��B��B��B��B�!B�XB�jB�LB�?B��B��B�jB�B��B�B�?B�3B�B�B��Bw�BP�B<jB&�B�B�B�B�B�BuBVBVB	7BDBVB�B\BB��B�B�mB�ZB�#B��BȴB��B�`B7LB=qB�BŢB{�BaHBP�BN�BJ�B1'B�BB�mB�qB��B� B}�B�Bm�BG�B�BJB
��B
��B
�B
�/B
ǮB
��B
��B
�hB
�hB
�B
q�B
gmB
iyB
ZB
L�B
C�B
49B
%�B
�B
�B
PB
+B
B	��B	�B	�`B	��B	�qB	�^B	�dB	�9B	��B	��B	�uB	�bB	�DB	�1B	�B	|�B	v�B	o�B	m�B	jB	ffB	aHB	_;B	YB	N�B	H�B	F�B	@�B	;dB	6FB	33B	1'B	-B	&�B	#�B	&�B	)�B	#�B	�B	�B	VB	%B	B��B��B��B�B�B�B�B�yB�yB�B�sB�B�B�B�B�B�B�B�yB�fB�ZB�HB�/B�B�
B��B��BɺB�jB�B��B�uB�PB�DB�=B�7B�+B�%B�+B�B�B~�B|�Bx�Bp�BiyBe`BcTB^5BZBVBO�BK�BI�BG�BF�BD�BB�B?}B=qB=qB@�B@�B>wB9XB5?B33B0!B.B.B,B)�B'�B(�B)�B(�B(�B)�B)�B&�B%�B&�B&�B'�B(�B,B8RB:^B>wBN�BZBjBn�Bo�Bl�BjBgmBe`BXBS�BT�BaHBjBk�BjBhsBffBffBe`Be`BdZBcTBbNBcTBdZBbNBdZBe`BdZBe`Be`BdZBdZBdZBdZBhsBk�Bl�Bm�Bm�Bn�Bl�Bk�BjBk�Bk�Br�Bt�Bu�Bt�Bu�Bw�Bu�Bx�B}�B}�B}�B~�B~�B�B�B�B�B�+B�7B�DB�VB�bB�oB��B��B��B��B��B��B��B��B��B��B��B�B�!B�-B�9B�FB�^B�dB�jB�qB�qB�wB�wBĜBȴBȴB��B��B��B��B��B��B��B��B��B�B�/B�5B�BB�ZB�fB�sB�yB�B�B�B�B�B��B��B��B��B��B	B	B	B	B	B	B	
=B	PB	\B	bB	oB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	&�B	+B	.B	0!B	7LB	:^B	;dB	=qB	A�B	F�B	I�B	O�B	Q�B	R�B	S�B	T�B	VB	XB	ZB	]/B	`BB	`BB	dZB	hsB	iyB	k�B	m�B	m�B	n�B	o�B	r�B	t�B	v�B	y�B	{�B	}�B	�B	�B	�%B	�+B	�+B	�=B	�PB	�\B	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�9B	�?B	�FB	�LB	�LB	�LB	�FB	�?B	�9B	�FB	�LB	�^B	�jB	�qB	�}B	��B	ÖB	ĜB	ŢB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�B	�#B	�)B	�)B	�/B	�/B	�;B	�BB	�HB	�NB	�fB	��B
%B
\B
�B
#�B
,B
0!B
8RB
=qB
C�B
H�B
N�B
S�B
XB
]/B
`BB
dZB
hsB
l�B
p�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B/,B/*B03B03B03B03B00B/*B02B00B04B02B02B[4B�-B��B��B��B��B�
B�5B�nB�~B�cB�TB��B��B��B�)B��B�"B�RB�EB�2B�B��Bw�BP�B<}B&�B�B�B�B�B�B�BdBfB	IBVBdB�BlB1B��B�B�|B�gB�0B��B��B��B�qB7aB=�B�BűB{�BaWBP�BN�BJ�B16B�BB�{B��B��B�B~B�"Bm�BG�B�BZB
�B
��B
�B
�BB
��B
��B
��B
�{B
�{B
�+B
q�B
g�B
i�B
Z3B
L�B
C�B
4MB
%�B
�B
�B
gB
CB
&B	� B	��B	�yB	��B	��B	�zB	�~B	�RB	�B	��B	��B	��B	�`B	�OB	�*B	}B	v�B	o�B	m�B	j�B	f�B	afB	_ZB	Y7B	N�B	H�B	F�B	@�B	;�B	6hB	3SB	1GB	-/B	'B	#�B	'
B	*B	#�B	�B	�B	xB	IB	&B�B�	B��B��B��B�B�B�B�B�B�B�B��B��B��B��B��B�B�B�B�}B�kB�QB�AB�1B�B�B��B��B�BB��B��B�yB�nB�dB�_B�TB�NB�SB�:B�1B#B}Bx�Bp�Bi�Be�BcB^`BZEBV.BPBK�BI�BG�BF�BD�BB�B?�B=�B=�B@�B@�B>�B9�B5lB3`B0NB.@B.@B,5B*)B(B)
B**B)$B)"B*)B*)B'B&B'B'B(B)#B,5B8|B:�B>�BOBZGBj�Bn�Bo�Bl�Bj�Bg�Be�BX:BT$BU(BauBj�Bk�Bj�Bh�Bf�Bf�Be�Be�Bd�Bc~BbwBc}Bd�BbxBd�Be�Bd�Be�Be�Bd�Bd�Bd�Bd�Bh�Bk�Bl�Bm�Bm�Bn�Bl�Bk�Bj�Bk�Bk�Br�Bt�Bu�Bt�Bu�Bw�Bu�Bx�B~B~B~B$B%B�2B�6B�;B�AB�SB�aB�kB�|B��B��B��B��B��B��B��B�
B�B�B�B�B�B�1B�HB�TB�cB�lB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�%B�6B�UB�ZB�hB�B�B�B�B�B��B��B��B��B��B��B�B�B�B	+B	6B	=B	>B	=B	@B	
aB	sB	B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	'
B	+$B	.6B	0CB	7nB	:�B	;�B	=�B	A�B	F�B	I�B	P B	RB	SB	TB	UB	V%B	X/B	Z?B	]PB	`cB	`bB	dyB	h�B	i�B	k�B	m�B	m�B	n�B	o�B	r�B	t�B	v�B	y�B	|B	~B	�$B	�=B	�BB	�JB	�FB	�\B	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�*B	�4B	�UB	�^B	�dB	�lB	�hB	�kB	�dB	�[B	�UB	�cB	�jB	�{B	��B	��B	��B	��B	óB	ĻB	žB	��B	��B	��B	��B	��B	��B	�B	� B	�B	�B	�B	�B	�!B	�&B	�'B	�-B	�>B	�EB	�CB	�NB	�LB	�XB	�`B	�eB	�iB	�B	��B
?B
xB
�B
#�B
,!B
09B
8mB
=�B
C�B
H�B
N�B
TB
X,B
]FB
`[B
drB
h�B
l�B
p�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.16 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311448582016053114485820160531144858  AO  ARCAADJP                                                                    20140721230639    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230639  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230639  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531144858  IP                  G�O�G�O�G�O�                