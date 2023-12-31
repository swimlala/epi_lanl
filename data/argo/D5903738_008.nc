CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:06:38Z AOML 3.0 creation; 2016-05-31T21:48:58Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230638  20160531144858  5903738 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  4053_7107_008                   2C  D   APEX                            5370                            041511                          846 @�>�es��1   @�>���@8�t�j~��b�-V1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDyFfD��D�9�D�|�D��fD�3D�33D�p D�ٚD�  D�9�D�|�D��fD� D�FfD�,�D��fD��D�<�D�l�D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @6fg@|��@�ff@�ffA33A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dtc3DyC3D� D�8 D�{3D���D��D�1�D�nfD�� D��fD�8 D�{3D���D�fD�D�D�+3D���D�3D�;3D�k3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��mA��A�r�A�A�A�&�A��#A��!A���A��DA��+A�x�A�t�A�p�A�jA�dZA�\)A�E�A�1'A��A�VA�%A���A��mA��9A�x�A�bNA�VA�Q�A�33A� �A�
=A��A��A��
A�ĜA���A�ZA���A���A��uA�O�A��-A�^5A�5?A���A�t�A��A��7A�"�A��uA�^5A��A�v�A���A���A�K�A�-A���A�z�A�A�|�A�1'A�A���A�S�A���A���A��7A�v�A�jA�hsA�$�A��A�p�A� �A��DA��jA���A�  A��+A�/A�1'A�/A�&�A���A��HA�t�A��A��\A�=qA�VA�?}A�33A�`BA�&�A��hA�VA���A�A�p�A�`BA�7LA�;dA�$�A�%A�VA�A��jA�;A}\)Az�AyO�Axn�Aw�Au��Atn�Asp�AqK�Ao;dAm�^Akx�Ait�Agl�Ae/Ac��AbĜAbffA`�A_x�A^�HA^�A["�AY33AX$�AW;dAT�RAP�`AO�TAOp�AOoAN��AM��AL�\AKp�AJ�+AIAI��AI�wAI
=AFA�AFA�AD�9AB1A@~�A@bA@A?�wA?"�A>^5A=p�A<~�A;��A;�PA9�;A8��A7��A7��A6�A4��A3K�A2�9A2�A29XA2jA1;dA/�mA.~�A-�
A-+A*��A)VA(��A'��A&^5A%ƨA$�9A#��A"�A!+A ��A $�A��A��A�hA�A�A;dAv�A�Ax�A��AƨAl�A"�A��AXA1'At�A��A�A+Av�A�7A�yAl�A�-A
��A	G�A	�A~�Ax�A-A7LAQ�A?}AjA{A�A n�@�dZ@�@��`@�Q�@�x�@���@�|�@�33@�+@��@��@���@�M�@�7L@�9X@�@�ȴ@�V@�V@�ƨ@�
=@�p�@�D@�  @�$�@�Z@�n�@�@��m@�n�@���@��;@ާ�@���@��@�O�@�  @�n�@��@Ցh@�(�@�dZ@���@��`@ύP@���@�n�@���@�`B@�%@�r�@�1@ǥ�@�~�@���@���@Ł@Ĵ9@�\)@��@��T@��`@��@�o@���@���@�z�@���@�j@��@���@��@�
=@�\)@�t�@���@�J@�7L@�A�@�~�@��7@���@�Z@��@��@�=q@��-@�r�@�K�@��!@���@�ff@�p�@�z�@�ff@���@�?}@�&�@���@�Z@���@�o@��#@�x�@���@�r�@� �@���@��
@�t�@�;d@�@��R@�~�@�M�@��@���@�I�@��@�l�@�"�@�o@�+@�+@�l�@�l�@�t�@��@�33@�"�@�"�@���@��H@�M�@���@���@�p�@��/@��D@�bN@�1'@��
@�C�@�;d@�ȴ@���@�1@��w@�K�@�33@�+@�@��@�t�@��@�t�@��+@�n�@���@�n�@���@��-@�hs@���@�j@�(�@��@��m@��w@���@�l�@�33@�
=@�@�@��y@��H@��H@��y@���@���@�v�@�^5@�V@�-@�J@��T@��-@�X@��@���@���@�%@���@��u@��u@���@���@���@�r�@�bN@�9X@��m@���@��P@�\)@�S�@�K�@�C�@��H@���@�~�@�n�@�E�@�-@�{@��T@���@��h@��7@�hs@�/@��@��@�V@���@���@���@��@�z�@�Z@�A�@�(�@���@��w@��@���@���@���@���@��
@�z�@�z�@�j@�9X@���@�|�@�l�@��P@�|�@�"�@���@�E�@��@�{@�{@�ff@���@���@���@��h@�@{��@sdZ@i�7@b�\@\j@T�@N5?@HbN@A��@;ƨ@6{@.��@*��@'|�@"~�@�-@hs@p�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��mA��A�r�A�A�A�&�A��#A��!A���A��DA��+A�x�A�t�A�p�A�jA�dZA�\)A�E�A�1'A��A�VA�%A���A��mA��9A�x�A�bNA�VA�Q�A�33A� �A�
=A��A��A��
A�ĜA���A�ZA���A���A��uA�O�A��-A�^5A�5?A���A�t�A��A��7A�"�A��uA�^5A��A�v�A���A���A�K�A�-A���A�z�A�A�|�A�1'A�A���A�S�A���A���A��7A�v�A�jA�hsA�$�A��A�p�A� �A��DA��jA���A�  A��+A�/A�1'A�/A�&�A���A��HA�t�A��A��\A�=qA�VA�?}A�33A�`BA�&�A��hA�VA���A�A�p�A�`BA�7LA�;dA�$�A�%A�VA�A��jA�;A}\)Az�AyO�Axn�Aw�Au��Atn�Asp�AqK�Ao;dAm�^Akx�Ait�Agl�Ae/Ac��AbĜAbffA`�A_x�A^�HA^�A["�AY33AX$�AW;dAT�RAP�`AO�TAOp�AOoAN��AM��AL�\AKp�AJ�+AIAI��AI�wAI
=AFA�AFA�AD�9AB1A@~�A@bA@A?�wA?"�A>^5A=p�A<~�A;��A;�PA9�;A8��A7��A7��A6�A4��A3K�A2�9A2�A29XA2jA1;dA/�mA.~�A-�
A-+A*��A)VA(��A'��A&^5A%ƨA$�9A#��A"�A!+A ��A $�A��A��A�hA�A�A;dAv�A�Ax�A��AƨAl�A"�A��AXA1'At�A��A�A+Av�A�7A�yAl�A�-A
��A	G�A	�A~�Ax�A-A7LAQ�A?}AjA{A�A n�@�dZ@�@��`@�Q�@�x�@���@�|�@�33@�+@��@��@���@�M�@�7L@�9X@�@�ȴ@�V@�V@�ƨ@�
=@�p�@�D@�  @�$�@�Z@�n�@�@��m@�n�@���@��;@ާ�@���@��@�O�@�  @�n�@��@Ցh@�(�@�dZ@���@��`@ύP@���@�n�@���@�`B@�%@�r�@�1@ǥ�@�~�@���@���@Ł@Ĵ9@�\)@��@��T@��`@��@�o@���@���@�z�@���@�j@��@���@��@�
=@�\)@�t�@���@�J@�7L@�A�@�~�@��7@���@�Z@��@��@�=q@��-@�r�@�K�@��!@���@�ff@�p�@�z�@�ff@���@�?}@�&�@���@�Z@���@�o@��#@�x�@���@�r�@� �@���@��
@�t�@�;d@�@��R@�~�@�M�@��@���@�I�@��@�l�@�"�@�o@�+@�+@�l�@�l�@�t�@��@�33@�"�@�"�@���@��H@�M�@���@���@�p�@��/@��D@�bN@�1'@��
@�C�@�;d@�ȴ@���@�1@��w@�K�@�33@�+@�@��@�t�@��@�t�@��+@�n�@���@�n�@���@��-@�hs@���@�j@�(�@��@��m@��w@���@�l�@�33@�
=@�@�@��y@��H@��H@��y@���@���@�v�@�^5@�V@�-@�J@��T@��-@�X@��@���@���@�%@���@��u@��u@���@���@���@�r�@�bN@�9X@��m@���@��P@�\)@�S�@�K�@�C�@��H@���@�~�@�n�@�E�@�-@�{@��T@���@��h@��7@�hs@�/@��@��@�V@���@���@���@��@�z�@�Z@�A�@�(�@���@��w@��@���@���@���@���@��
@�z�@�z�@�j@�9X@���@�|�@�l�@��P@�|�@�"�@���@�E�@��@�{@�{@�ff@���@���@���@��h@�@{��@sdZ@i�7@b�\@\j@T�@N5?@HbN@A��@;ƨ@6{@.��@*��@'|�@"~�@�-@hs@p�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB}�Bw�Bs�Bp�Bw�B� B�B�1B�1B�DB�PB�hB��B��B��B��B��B��B��B��B��B��B��B�uB�bB�hB�hB�hB�\B�VB�VB�VB�\B�\B�\B�\B�PB�DB�oB�uB�bB�7B�1B�%B�+B�%B~�B}�B|�Bu�Br�Bl�BjBdZB[#BVBW
B[#B]/B]/BaHBcTB^5B^5BdZB^5B\)B[#BZBZBYBP�BD�B?}B�B�yB�B�wB�'B��B��B�JBx�BffBO�B<jB#�BB�B��B�FB�+BcTBB�B(�BDB
�B
��B
�^B
�9B
�'B
�B
��B
~�B
s�B
jB
gmB
e`B
_;B
M�B
=qB
6FB
33B
)�B
�B
oB
	7B	��B	�ZB	��B	�^B	��B	�hB	y�B	l�B	ffB	aHB	T�B	F�B	?}B	=qB	0!B	!�B	�B	JB��B�`B�BB�5B�)B�B�B��B��B��B�)B�B��B	  B�B��B��B��B�B�B�B�B�yB�ZB�5B�B�
B�B��B��B��B��B��BȴB�wB�^B�jBBǮBƨBÖB�wB�jB�RB��B��B��B��B��B�uB�\B�=B�B�B�B~�Bz�By�Bs�Bm�BjBl�BgmBcTB^5B\)BZB`BBhsBffBaHB\)BZBW
BT�BR�BQ�BO�BO�BL�BI�BC�B>wB=qB;dB8RB7LB49B2-B2-B2-B1'B1'B1'B0!B1'B0!B/B.B.B.B.B.B-B-B-B,B,B,B,B-B,B-B/B0!B0!B/B33B1'B.B/B.B/B.B.B.B.B.B-B-B-B0!B0!B.B0!B0!B/B0!B6FB6FB5?B:^B;dB<jB>wBA�BC�BF�BG�BG�BH�BI�BL�BK�BH�BF�BE�BG�BJ�BH�BL�BN�BM�BO�BQ�BP�BR�BVBYB\)B]/B^5B`BBdZBffBgmBgmBjBk�Bk�Bk�BjBk�Bl�Bl�Bl�Bl�Bn�Bt�Bt�Bv�Bu�Bv�Bx�B{�B|�B�B�B�%B�7B�=B�JB�JB�\B�\B�hB�oB�{B��B��B��B��B�B�'B�RB�^B�qB��BƨBȴBɺB��B��B��B�#B�5B�TB�fB�mB�sB�B�B�B	  B	B	B	B	%B	%B	B	B	%B	1B	DB	VB	bB	uB	�B	�B	�B	�B	!�B	$�B	%�B	&�B	&�B	(�B	+B	/B	2-B	6FB	7LB	8RB	:^B	<jB	A�B	E�B	F�B	F�B	G�B	I�B	L�B	N�B	R�B	W
B	YB	[#B	]/B	aHB	bNB	dZB	ffB	iyB	l�B	n�B	o�B	q�B	u�B	z�B	z�B	}�B	�B	�B	�%B	�7B	�JB	�\B	�bB	�hB	�uB	�uB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�-B	�3B	�?B	�RB	�^B	�qB	�wB	�wB	��B	ĜB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�#B	�/B	�BB	�B
B
DB
�B
"�B
(�B
1'B
7LB
<jB
C�B
H�B
M�B
S�B
XB
\)B
aHB
e`B
jB
m�B
q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B}�Bw�Bs�Bp�Bw�B�B�)B�<B�;B�LB�]B�sB��B��B��B��B��B��B��B��B��B��B��B�~B�lB�rB�qB�qB�eB�_B�aB�`B�eB�hB�dB�fB�[B�OB�wB�~B�kB�>B�=B�1B�6B�2BB}�B|�Bu�Br�Bl�Bj�BdaB[)BVBWB[*B]8B]5BaOBc\B^>B^=BdbB^>B\/B[.BZ$BZ&BYBP�BD�B?�B�B�B�B�B�/B��B��B�NBx�BflBO�B<pB#�BB�B��B�LB�0BcXBB�B(�BJB
�B
��B
�gB
�@B
�1B
�B
��B
B
s�B
j�B
gtB
ekB
_EB
M�B
=|B
6SB
3AB
*B
�B
yB
	CB	��B	�jB	��B	�mB	��B	�wB	y�B	l�B	fyB	a\B	UB	F�B	?�B	=�B	06B	!�B	�B	_B��B�uB�[B�NB�CB�6B�B�B�B��B�@B�B�B	 B��B��B�B��B�B�B�B�B�B�rB�MB�7B�"B�5B� B��B��B��B��B��B��B�zB��BªB��B��BðB��B��B�iB�B��B��B��B��B��B�yB�YB�:B�:B�-BBz�By�Bs�Bm�Bj�Bl�Bg�BcoB^TB\GBZ<B`_Bh�Bf�BadB\FBZ<BW(BUBSBRBO�BO�BL�BI�BC�B>�B=�B;�B8qB7lB4YB2MB2MB2LB1HB1GB1GB0AB1FB0AB/"B.2B.1B.3B.3B.3B-,B--B-+B,'B,,B,(B,(B-,B,'B--B/!B0CB0BB/<B3RB1DB.5B/<B.2B/;B.2B.2B.3B.4B.2B-+B--B--B0?B0BB.1B0BB0AB/;B0?B6dB6cB5_B:|B;�B<�B>�BA�BC�BF�BG�BG�BH�BI�BL�BK�BH�BF�BE�BG�BJ�BH�BL�BN�BM�BO�BR	BQBSBV"BY5B\FB]HB^QB``BduBf�Bg�Bg�Bj�Bk�Bk�Bk�Bj�Bk�Bl�Bl�Bl�Bl�Bn�Bt�Bt�Bv�Bu�Bv�Bx�B| B}B�'B�3B�@B�TB�XB�eB�gB�wB�wB��B��B��B��B��B��B�B�%B�?B�iB�uB��B��B��B��B��B��B��B�B�9B�MB�kB�}B�B�B�B��B��B	 B	(B	1B	-B	:B	9B	(B	 B	6B	GB	YB	jB	yB	�B	�B	�B	�B	�B	!�B	$�B	%�B	&�B	&�B	)B	+B	/-B	2>B	6YB	7_B	8eB	:oB	<|B	A�B	E�B	F�B	F�B	G�B	I�B	L�B	N�B	SB	WB	Y*B	[5B	]@B	aZB	b_B	dlB	fxB	i�B	l�B	n�B	o�B	q�B	u�B	z�B	z�B	~B	�B	�*B	�5B	�HB	�[B	�lB	�qB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	� B	��B	�B	�B	�#B	�+B	�)B	�6B	�=B	�BB	�MB	�bB	�nB	��B	��B	��B	��B	ĪB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	�B	�B	�B	�,B	�-B	�<B	�QB	�B
B
QB
�B
"�B
) B
13B
7VB
<vB
C�B
H�B
M�B
TB
XB
\5B
aRB
emB
j�B
m�B
q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.05 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311448582016053114485820160531144858  AO  ARCAADJP                                                                    20140721230638    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230638  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230638  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531144858  IP                  G�O�G�O�G�O�                