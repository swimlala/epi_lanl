CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-02-11T08:01:13Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20180211080113  20190604094144  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @�K��P
�1   @�K����\@4䛥�S��d|��E�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C�C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN�CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D"��D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du@ Dy�\D��D�7\D�k�D��HD�
D�=�D���D�ȤD��D�5D���D��)D���D�@RDڊ�D��D�HD�C�D�r�D��H1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @|(�@��H@��HAp�A=p�A]p�A}p�A��RA��RA��RA��RAθRA޸RA�RA��RB\)B\)B\)B\)B'\)B/\)B7\)B?\)BG\)BO\)BW\)B_\)Bg\)Bo\)BwB\)B��B��GB��B��B��B��B��B��B��B��B��B��B��B��B��B��BîBǮBˮBϮBӮB׮BۮB߮B�B�B�B�B�B��B��B��C�
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
C�C�
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
CM�CO�
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
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��RC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D u�D ��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��D	u�D	��D
u�D
��D|)D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��Du�D��D u�D ��D!u�D!��D"u�D"�]D#u�D#��D$u�D$��D%u�D%��D&u�D&��D'u�D'��D(u�D(��D)u�D)��D*u�D*��D+u�D+��D,u�D,��D-u�D-��D.u�D.��D/u�D/��D0u�D0��D1u�D1��D2u�D2��D3u�D3��D4u�D4��D5u�D5��D6u�D6��D7u�D7��D8u�D8��D9u�D9��D:u�D:��D;u�D;��D<u�D<��D=u�D=��D>u�D>��D?u�D?��D@u�D@��DAu�DA��DBu�DB��DCu�DC��DDu�DD��DEu�DE��DFu�DF��DGu�DG��DHu�DH��DIu�DI��DJu�DJ��DKu�DK��DLu�DL��DMu�DM��DNu�DN��DOu�DO��DPu�DP��DQu�DQ��DRu�DR��DSu�DS��DTu�DT��DUu�DU��DVu�DV��DWu�DW��DXu�DX��DYu�DY��DZu�DZ��D[u�D[��D\u�D\��D]u�D]��D^u�D^��D_u�D_��D`u�D`��Dau�Da��Dbu�Db��Dcu�Dc��Ddu�Dd��Deu�De��Dfu�Df��Dgu�Dg��Dhu�Dh��Diu�Di��Dju�Dj��Dku�Dk��Dlu�Dl��Dmu�Dm��Dnu�Dn��Dou�Do��Dpu�Dp��Dqu�Dq��Dru�Dr��Dsu�Ds��Dtu�Dt��Du5�Dy�D��D�2=D�f�D��)D�	�D�8�D��fD�ÅD���D�0 D���D��
D��D�;3Dڅ�D���D�)D�>fD�m�D��)1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�/A�33A�7LA�;dA�7LA��A�bA�JA�%A�A���A���A���A��A��yA��/A���A�v�A��A�p�A�{A���A�ĜA�z�A�5?A��
A�p�A�`BA�1'A�A��HA���A�ȴA��A�dZA�/A��A��#A���A�&�A��A�oA�G�A�ƨA�^5A��A�G�A���A��;A�ZA��PA�1A��A�"�A�A���A�`BA�+A�n�A���A�
=A���A�E�A���A�jA�A�A� �A�x�A��+A��^A���A��A��A�ƨA�`BA�ȴA��A��mA�bNA���A�t�A�+A��A�|�A��hA�M�A�A�=qA���A�  A���A���A��A�`BA�|�A��A��HA�"�A�A�r�A�A���A�/A�/A���A��\A��+A|�Az�Ayt�Ax�RAx(�Av �As
=Aq��Ap^5An��AmS�AljAk33Aj5?Ai�
Ag;dAd��Ac\)AcAb��AbVAa��A_;dA]�hAZjAW�PAU��AS�AR�RARz�ARVAQ�TAP�yAN�9AM�wAMAL��AL�+AK�AJQ�AI"�AHv�AH5?AG�AF-AC��AB��A@�RA?��A=;dA:5?A8�A6�A4ĜA2ȴA1
=A/O�A.A,��A*jA(�A'��A'&�A&��A&$�A%S�A#�A"�A!ƨA!��A!l�A ��A`BA�7A�TAv�A��AZA\)A��A�AVAO�A  Ax�A�HAƨAS�A�uA�#AC�A�A�HA�+A �A�#A
�A	t�A	"�A��AZA��Av�A�A�A%A�TA �\A {@��y@�%@��m@��y@�ff@�$�@���@�|�@�$�@�V@�9X@�P@��@���@@�-@�&�@�r�@�!@��T@�?}@��@�r�@��;@�ff@�bN@�^5@ᙚ@�(�@޸R@ݡ�@��`@�(�@�"�@�J@�O�@�O�@ج@׾w@�+@���@�z�@��@ӥ�@��y@Ѻ^@��@�Q�@�33@�J@͡�@���@�  @˅@�ff@���@�Q�@Ǯ@�$�@ă@��@�v�@�{@��^@�hs@��/@�bN@� �@�1@��m@��w@�K�@��R@�~�@��@���@�b@��@���@��@��@�Q�@��;@�"�@��+@�$�@�J@���@���@��j@�l�@��R@�M�@�`B@��D@��@�l�@��@��\@���@���@�O�@���@�  @�33@�V@�{@���@�p�@���@���@�Q�@�1'@� �@�b@��@���@�|�@�
=@���@�v�@��\@�^5@�M�@�E�@�{@��-@���@��7@�hs@�?}@��@���@�bN@�  @��m@�ƨ@�;d@�~�@��#@��^@���@�%@��9@���@���@�r�@�(�@���@���@�l�@�dZ@�C�@���@���@��!@�^5@�M�@�@�`B@���@�z�@�(�@�  @��
@�dZ@��H@��R@���@�5?@��T@��^@��-@�`B@�?}@�7L@�&�@��@�%@���@�r�@��
@���@�|�@�l�@�\)@��@��+@��\@��\@�5?@�{@�J@���@��#@��7@�?}@�%@��@���@��D@�Q�@��@��F@��P@�;d@�+@�+@�o@���@��+@�V@��T@�p�@�%@��9@�bN@��@�ƨ@�|�@��@��!@�v�@�^5@�^5@�n�@�{@��T@��7@���@��-@��@�Ĝ@��9@��D@�Z@��m@���@�\)@�+@�
=@��y@��y@��@���@��!@���@��\@��+@�V@�$�@�{@�@��T@���@�p�@�7L@��`@�Q�@�(�@�1@���@��;@�ƨ@��w@���@�|�@�t�@�C�@�"�@���@�n�@���@���@���@}�n@uO�@l�@bd�@ZW�@U@P�_@I�3@B��@;qv@4�@-G�@(�@#e�@�b@�9@�z@Xy@��@S�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 A�/A�33A�7LA�;dA�7LA��A�bA�JA�%A�A���A���A���A��A��yA��/A���A�v�A��A�p�A�{A���A�ĜA�z�A�5?A��
A�p�A�`BA�1'A�A��HA���A�ȴA��A�dZA�/A��A��#A���A�&�A��A�oA�G�A�ƨA�^5A��A�G�A���A��;A�ZA��PA�1A��A�"�A�A���A�`BA�+A�n�A���A�
=A���A�E�A���A�jA�A�A� �A�x�A��+A��^A���A��A��A�ƨA�`BA�ȴA��A��mA�bNA���A�t�A�+A��A�|�A��hA�M�A�A�=qA���A�  A���A���A��A�`BA�|�A��A��HA�"�A�A�r�A�A���A�/A�/A���A��\A��+A|�Az�Ayt�Ax�RAx(�Av �As
=Aq��Ap^5An��AmS�AljAk33Aj5?Ai�
Ag;dAd��Ac\)AcAb��AbVAa��A_;dA]�hAZjAW�PAU��AS�AR�RARz�ARVAQ�TAP�yAN�9AM�wAMAL��AL�+AK�AJQ�AI"�AHv�AH5?AG�AF-AC��AB��A@�RA?��A=;dA:5?A8�A6�A4ĜA2ȴA1
=A/O�A.A,��A*jA(�A'��A'&�A&��A&$�A%S�A#�A"�A!ƨA!��A!l�A ��A`BA�7A�TAv�A��AZA\)A��A�AVAO�A  Ax�A�HAƨAS�A�uA�#AC�A�A�HA�+A �A�#A
�A	t�A	"�A��AZA��Av�A�A�A%A�TA �\A {@��y@�%@��m@��y@�ff@�$�@���@�|�@�$�@�V@�9X@�P@��@���@@�-@�&�@�r�@�!@��T@�?}@��@�r�@��;@�ff@�bN@�^5@ᙚ@�(�@޸R@ݡ�@��`@�(�@�"�@�J@�O�@�O�@ج@׾w@�+@���@�z�@��@ӥ�@��y@Ѻ^@��@�Q�@�33@�J@͡�@���@�  @˅@�ff@���@�Q�@Ǯ@�$�@ă@��@�v�@�{@��^@�hs@��/@�bN@� �@�1@��m@��w@�K�@��R@�~�@��@���@�b@��@���@��@��@�Q�@��;@�"�@��+@�$�@�J@���@���@��j@�l�@��R@�M�@�`B@��D@��@�l�@��@��\@���@���@�O�@���@�  @�33@�V@�{@���@�p�@���@���@�Q�@�1'@� �@�b@��@���@�|�@�
=@���@�v�@��\@�^5@�M�@�E�@�{@��-@���@��7@�hs@�?}@��@���@�bN@�  @��m@�ƨ@�;d@�~�@��#@��^@���@�%@��9@���@���@�r�@�(�@���@���@�l�@�dZ@�C�@���@���@��!@�^5@�M�@�@�`B@���@�z�@�(�@�  @��
@�dZ@��H@��R@���@�5?@��T@��^@��-@�`B@�?}@�7L@�&�@��@�%@���@�r�@��
@���@�|�@�l�@�\)@��@��+@��\@��\@�5?@�{@�J@���@��#@��7@�?}@�%@��@���@��D@�Q�@��@��F@��P@�;d@�+@�+@�o@���@��+@�V@��T@�p�@�%@��9@�bN@��@�ƨ@�|�@��@��!@�v�@�^5@�^5@�n�@�{@��T@��7@���@��-@��@�Ĝ@��9@��D@�Z@��m@���@�\)@�+@�
=@��y@��y@��@���@��!@���@��\@��+@�V@�$�@�{@�@��T@���@�p�@�7L@��`@�Q�@�(�@�1@���@��;@�ƨ@��w@���@�|�@�t�@�C�@�"�@���@�n�@���G�O�@���@}�n@uO�@l�@bd�@ZW�@U@P�_@I�3@B��@;qv@4�@-G�@(�@#e�@�b@�9@�z@Xy@��@S�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB-B-B-B-B+B)�B)�B(�B(�B(�B(�B(�B(�B(�B'�B'�B)�B0!BA�BT�B]/B]/B]/BbNBgmBm�Bv�Bw�B|�B� B�B�B�B�%B�PB�hB�uB��B��B��B��B�1Bq�BhsBffBffBgmBk�BgmBaHBhsBn�BjBhsBgmBffBffBdZB_;B^5BXBH�BI�BM�BZBW
BQ�BQ�BZB]/BbNB]/BZBT�BK�BG�BD�B=qB,B�BhB+BB��B�B�mB�TB�#B��B��B�^B��B�bB{�B[#B%�B�B
��B
�yB
�#B
�
B
��B
�B
��B
�B
v�B
e`B
<jB
-B
(�B
#�B
�B
�B
+B	��B	��B	�B	�NB	�HB	�BB	�/B	�)B	��B	�jB	�!B	�B	�B	�B	��B	��B	�=B	t�B	bNB	W
B	H�B	A�B	@�B	>wB	;dB	5?B	)�B	%�B	"�B	�B	�B	�B	�B	bB	JB		7B	1B	B��B�B�sB�HB�B��B��BƨB��B�dB�LB�-B�B��B��B��B��B��B��B��B��B�oB�bB�bB�VB�PB�=B�+B�B�B}�B{�B|�B~�B~�B}�B{�Bx�Bw�Bv�Bt�Bs�Bq�Bo�Bn�Bn�Bn�Bn�Bm�Bl�BhsBk�Bk�Bl�Bm�Bn�Bn�Bo�Bm�Bm�Bl�Bk�Bk�Bk�Bk�Bk�Bk�Bk�Bk�Bk�BiyBk�Bk�Bk�Bk�Bk�Bm�Bm�Bm�Bm�Bl�Bl�Bl�Bl�Bl�Bl�Bk�BjBjBl�Bn�Bn�Bo�Bq�Br�Bs�Bt�Bu�Bv�Bw�Bw�Bw�Bx�By�B{�B~�B� B� B�B�B�B�%B�1B�=B�=B�JB�VB�VB�bB�{B�{B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�'B�3B�9B�9B�?B�jB��BƨB��B��B��B��B�B�B�#B�)B�)B�/B�NB�sB�B�B��B��B��B	B	1B		7B	DB	JB	\B	{B	�B	�B	#�B	%�B	&�B	)�B	.B	49B	9XB	:^B	;dB	<jB	<jB	=qB	?}B	B�B	D�B	F�B	G�B	H�B	J�B	L�B	M�B	P�B	Q�B	R�B	S�B	T�B	YB	[#B	\)B	`BB	`BB	aHB	e`B	iyB	o�B	p�B	p�B	t�B	w�B	w�B	w�B	x�B	z�B	}�B	� B	�B	�B	�B	�+B	�1B	�7B	�=B	�DB	�PB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�9B	�?B	�FB	�RB	�dB	�jB	�qB	�}B	�}B	��B	��B	��B	ÖB	ŢB	ƨB	ǮB	ǮB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�/B	�5B	�;B	�BB	�HB	�NB	�NB	�TB	�ZB	�`B	�fB	�mB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
%B
+B
+B
1B
	7B

=B
)�B
NB
�B
#B
-)B
3hB
;B
>�B
EB
I�B
O�B
U�B
\CB
b�B
f�B
kQB
o�B
s�B
xB
}�B
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 B�B�B�B�B�B�B�B~B~B�B�B~B~B�BzBzB�B!�B3BF�BN�BN�BN�BS�BX�B_BhLBiSBnsBqBt�Bu�Bu�Bw�B~�B��B��B�B�+B�]B�VBy�Bc/BY�BW�BW�BX�B]BX�BR�BZ B`"B\
BY�BX�BW�BW�BU�BP�BO�BI�B:@B;EB?gBK�BH�BC{BC|BK�BN�BS�BN�BK�BF�B=XB9BB6-B/B�B:B�B��B��B�B�B�B��B��BĕB�cB�B�zB�Bm�BL�B�B	FB
�B
�>B
��B
��B
�HB
��B
�xB
u�B
h�B
W0B
.AB
�B
�B
�B
�B
	gB	�B	��B	�B	�kB	�0B	�*B	�!B	�B	�B	��B	�SB	�B	�B	�B	��B	��B	�tB	|+B	f�B	TAB	H�B	:�B	3�B	2yB	0oB	-]B	'9B	�B	�B	�B	�B	�B	�B	~B	]B�KB�6B�.B�B��B�B�wB�IB�B��B��B��B��B�pB�YB�8B�B�B��B��B��B��B��B��B��B��B�uB�uB�kBeB|QBy@Bv2BsBpBm�BoBqBqBpBn Bj�Bi�Bh�Bf�Be�Bc�Ba�B`�B`�B`�B`�B_�B^�BZ�B]�B]�B^�B_�B`�B`�Ba�B_�B_�B^�B]�B]�B]�B]�B]�B]�B]�B]�B]�B[�B]�B]�B]�B]�B]�B_�B_�B_�B_�B^�B^�B^�B^�B^�B^�B]�B\�B\�B^�B`�B`�Ba�Bc�Bd�Be�Bf�Bg�Bh�Bi�Bi�Bi�Bj�Bk�BnBqBr#Br!Bs+Bv;Bw?BxFBzQB|_B|\B~lB�vB�xB��B��B��B��B��B��B��B��B�B�B�B�B�*B�0B�3B�2B�<B�FB�OB�WB�VB�_B��B��B��B��B��B��B�B� B�+B�=B�CB�AB�JB�iBڎBޢB�B��B��B�B�5B�JB�KB�^B�cB	uB	�B	
�B	�B	�B	�B	�B	B	 +B	&OB	+nB	,rB	-sB	.|B	.}B	/�B	1�B	4�B	6�B	8�B	9�B	:�B	<�B	>�B	?�B	B�B	C�B	EB	F	B	GB	K&B	M0B	N9B	RQB	RTB	SYB	WnB	[�B	a�B	b�B	b�B	f�B	i�B	i�B	i�B	j�B	l�B	pB	rB	tB	uB	uB	y6B	z9B	{AB	|JB	}NB	]B	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�	B	�B	�*B	�7B	�BB	�EB	�MB	�XB	�kB	�tB	�yB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�5B	�:B	�AB	�EB	�NB	�TB	�OB	�YB	�^B	�bB	�gB	�nB	�wB	�vB	܅B	ߕB	ߖB	ߕB	ߓB	��B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�B	�B	�B	�"B	�(B	�&B	�-B	�-B	�5B	�7B	�<G�O�B
QB
�B

B
(B
%jB
-B
0�B
7B
;�B
A�B
G�B
N?B
TB
X�B
]NB
a�B
e�B
i�B
o�B
t�B
x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.16 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9996(+/-0.0001), vertically averaged dS =-0.014(+/-0.002) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040941442019060409414420190604094144  AO  ARCAADJP                                                                    20180211080113    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20180211080113  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20180211080113  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094144  IP                  G�O�G�O�G�O�                