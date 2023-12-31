CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:43Z AOML 3.0 creation; 2016-05-31T19:14:36Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230543  20160531121436  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               GA   AO  4051_7090_071                   2C  D   APEX                            5368                            041511                          846 @��SWk@1   @��S�`@5e�S����e, ě��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    GA   A   A   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch�Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm�fDn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DyS3D�� D�)�D��3D��3D�3D�9�D�s3D���D�  D�<�D��3D�Y�D�	�D�<�D�s3D��fD�3D�C3D�|�D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @6fg@|��@�ff@�ffA33A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Ch�Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm�3Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��DyP D��fD�( D���D���D��D�8 D�q�D��3D��fD�;3D���D�X D� D�;3D�q�D���D��D�A�D�{3D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�&�A�&�A�+A�"�A��A�(�A�1'A�1'A�7LA�=qA�;dA�9XA�7LA�1'A�1'A�/A�5?A�A�A�A�A�E�A�G�A�G�A�I�A�K�A�M�A�O�A�Q�A�K�A�I�A�-A�VA���A��A��yA��/A���A��-A��uA�|�A�t�A�n�A�jA�\)A�G�A��A���A�1'A��-A�ZA�%A�ȴA�~�A��A��9A���A���A���A���A��A���A���A��hA��hA��hA�n�A��A�A��RA�ZA���A�?}A���A��uA��A�A��DA�oA���A�K�A��uA��A�ffA�n�A��uA���A��TA�-A��
A�v�A�1A���A�/A�JA�%A�x�A���A�~�A�1A���A���A��A�\)A�t�A��A�I�A��mA�7LA�p�A�C�A��jA�~�A�33A��FA�t�A�5?A�`BA�p�A��A��A�E�A��`A��HA��A���A���A�M�A�E�A�A}K�Az��Au�At�/Ar(�Ao�7Am7LAl=qAk�7Ah��Ag
=Ae33Ad1Acl�A`1A]��A[��A[�AXz�AUl�AR��AP�AP �AO�AN�!AM�AKdZAJ�yAH�AG��AF�AF�\AF1AEK�AC�AB��AA�AA��A@��A@Q�A?��A?O�A>�HA>JA< �A:��A:r�A:^5A:bA933A8�A7�A7|�A6r�A5+A2�+A1�#A1&�A0VA/VA,��A+K�A*n�A)��A(A�A'`BA%hsA$��A#�#A"$�A��AZA5?A+AQ�A�^AO�AjA(�Al�A�jA/AA�PAK�Ar�A�wA�AC�A
n�A	p�AbNA�A�A�;A��A?}A�A�uAVA��A��AA  �@�~�@��R@�A�@��+@�D@���@�  @�dZ@�@�ȴ@�~�@�$�@�J@���@�-@�-@�@�@��@�\@���@�hs@�?}@���@�Q�@�@�ȴ@��@�&�@�(�@�
=@އ+@�=q@��@�p�@�Q�@�dZ@��@�|�@�M�@���@�ƨ@ҟ�@��#@�{@�(�@�~�@��#@�{@��@�9X@���@��@Ο�@���@�?}@�|�@�hs@ț�@�A�@��;@�C�@�E�@ź^@ũ�@ŉ7@ě�@��@å�@�S�@�+@�ȴ@�v�@���@���@�Q�@���@��
@��
@��@�|�@��@���@��h@���@��9@�j@�  @��P@�;d@�@���@�ff@��-@��@��u@�9X@��@�+@�o@���@��y@�ȴ@�ff@��@���@�7L@�I�@���@��@�dZ@��y@���@�{@�hs@�G�@�7L@��@�bN@��P@�
=@��!@��!@��\@��^@�`B@��@���@���@��@�t�@�"�@��H@�M�@�hs@��@�Z@�"�@�n�@�{@�@�&�@�%@��`@��j@� �@��P@�K�@��H@��\@�n�@�V@�hs@�r�@�1'@���@��@�|�@�l�@�C�@�33@��@��@��\@�E�@�$�@�@��@��h@�O�@�/@�&�@��@�V@�V@���@���@���@�Ĝ@���@��D@�z�@�j@�bN@�bN@�A�@� �@��@��w@�"�@�ȴ@��T@�G�@���@�9X@�ƨ@���@��
@��m@��@��@��m@��;@��
@�ƨ@��F@���@���@�|�@�\)@�;d@��@�n�@���@���@��7@��7@��@�x�@�x�@�p�@�p�@�hs@�X@��/@�j@��P@�S�@��@��!@���@��\@�ff@�$�@���@��@��T@�@��7@�p�@�x�@�p�@�hs@�`B@�G�@�7L@�&�@��@��@��@��@�I�@�(�@��@�1@��;@��7@��P@��F@z��@o\)@e`B@^ff@W��@O��@JM�@?K�@:��@5�@.5?@)�7@%p�@E�@A�@��@C�@bN11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�&�A�&�A�+A�"�A��A�(�A�1'A�1'A�7LA�=qA�;dA�9XA�7LA�1'A�1'A�/A�5?A�A�A�A�A�E�A�G�A�G�A�I�A�K�A�M�A�O�A�Q�A�K�A�I�A�-A�VA���A��A��yA��/A���A��-A��uA�|�A�t�A�n�A�jA�\)A�G�A��A���A�1'A��-A�ZA�%A�ȴA�~�A��A��9A���A���A���A���A��A���A���A��hA��hA��hA�n�A��A�A��RA�ZA���A�?}A���A��uA��A�A��DA�oA���A�K�A��uA��A�ffA�n�A��uA���A��TA�-A��
A�v�A�1A���A�/A�JA�%A�x�A���A�~�A�1A���A���A��A�\)A�t�A��A�I�A��mA�7LA�p�A�C�A��jA�~�A�33A��FA�t�A�5?A�`BA�p�A��A��A�E�A��`A��HA��A���A���A�M�A�E�A�A}K�Az��Au�At�/Ar(�Ao�7Am7LAl=qAk�7Ah��Ag
=Ae33Ad1Acl�A`1A]��A[��A[�AXz�AUl�AR��AP�AP �AO�AN�!AM�AKdZAJ�yAH�AG��AF�AF�\AF1AEK�AC�AB��AA�AA��A@��A@Q�A?��A?O�A>�HA>JA< �A:��A:r�A:^5A:bA933A8�A7�A7|�A6r�A5+A2�+A1�#A1&�A0VA/VA,��A+K�A*n�A)��A(A�A'`BA%hsA$��A#�#A"$�A��AZA5?A+AQ�A�^AO�AjA(�Al�A�jA/AA�PAK�Ar�A�wA�AC�A
n�A	p�AbNA�A�A�;A��A?}A�A�uAVA��A��AA  �@�~�@��R@�A�@��+@�D@���@�  @�dZ@�@�ȴ@�~�@�$�@�J@���@�-@�-@�@�@��@�\@���@�hs@�?}@���@�Q�@�@�ȴ@��@�&�@�(�@�
=@އ+@�=q@��@�p�@�Q�@�dZ@��@�|�@�M�@���@�ƨ@ҟ�@��#@�{@�(�@�~�@��#@�{@��@�9X@���@��@Ο�@���@�?}@�|�@�hs@ț�@�A�@��;@�C�@�E�@ź^@ũ�@ŉ7@ě�@��@å�@�S�@�+@�ȴ@�v�@���@���@�Q�@���@��
@��
@��@�|�@��@���@��h@���@��9@�j@�  @��P@�;d@�@���@�ff@��-@��@��u@�9X@��@�+@�o@���@��y@�ȴ@�ff@��@���@�7L@�I�@���@��@�dZ@��y@���@�{@�hs@�G�@�7L@��@�bN@��P@�
=@��!@��!@��\@��^@�`B@��@���@���@��@�t�@�"�@��H@�M�@�hs@��@�Z@�"�@�n�@�{@�@�&�@�%@��`@��j@� �@��P@�K�@��H@��\@�n�@�V@�hs@�r�@�1'@���@��@�|�@�l�@�C�@�33@��@��@��\@�E�@�$�@�@��@��h@�O�@�/@�&�@��@�V@�V@���@���@���@�Ĝ@���@��D@�z�@�j@�bN@�bN@�A�@� �@��@��w@�"�@�ȴ@��T@�G�@���@�9X@�ƨ@���@��
@��m@��@��@��m@��;@��
@�ƨ@��F@���@���@�|�@�\)@�;d@��@�n�@���@���@��7@��7@��@�x�@�x�@�p�@�p�@�hs@�X@��/@�j@��P@�S�@��@��!@���@��\@�ff@�$�@���@��@��T@�@��7@�p�@�x�@�p�@�hs@�`B@�G�@�7L@�&�@��@��@��@��@�I�@�(�@��@�1@��;@��7@��P@��F@z��@o\)@e`B@^ff@W��@O��@JM�@?K�@:��@5�@.5?@)�7@%p�@E�@A�@��@C�@bN11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBJBJBJBJBJBJBJBJBDBDBDB
=B
=B
=B
=B
=B
=BDBDBDBJBJBJBJBJBJBJBJBDB
=B	7B	7B	7B	7B	7B	7B
=B
=B	7B
=B	7B	7B1B+B
=B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BhB�`B�B�B��B��B�bBw�BdZBI�B�BJB  B��B�sB�;B��BĜB�9B��B��B�VB�1B�By�Bq�BhsBW
BF�B=qB5?B-B%�B�B{BPB��B�yB�#BȴB��B�9B��B�{B|�B^5BYBO�B<jB)�B�BVB
��B
�BB
��B
�}B
�B
�{B
�B
l�B
hsB
ffB
\)B
N�B
;dB
�B
�B
B	�B	�ZB	�5B	�B	��B	�}B	�?B	�B	��B	��B	�7B	~�B	y�B	jB	\)B	P�B	G�B	D�B	B�B	=qB	7LB	0!B	-B	&�B	 �B	�B	�B	�B	�B	�B	�B	oB	hB	VB	JB		7B	+B	B	B��B��B��B��B��B��B�B�B�B�sB�NB�#B�B�B��BɺBB��B�jB�XB�9B�!B�B��B��B��B��B��B�bB�JB�%B�B�B~�B}�B|�B{�Bz�Bz�Bz�By�Bx�By�Bx�Bw�Bw�Bx�Bx�Bv�Bw�Bx�Bw�Bx�Bx�Bx�Bw�Bv�Bv�Bv�Bv�Bs�Bs�Br�Bq�Bq�Bq�Bt�Br�By�Bz�Bz�Bz�Bz�Bz�Bz�By�By�By�Bx�B{�B|�B}�B}�B}�B~�B�B�B�B�B�B�%B�+B�+B�+B�%B�B�B� Bz�Bz�Bz�B|�B}�B~�B�B�oB�bB�\B�uB��B��B�3B�?B�^B�dB�jB��BÖBĜBĜBŢBŢBɺB��B��B��B��B��B��B��B��B��B�B�B�B�B�#B�/B�;B�BB�HB�TB�B��B��B��B��B��B��B	  B	B	%B	%B		7B	DB	JB	VB	\B	oB	uB	{B	{B	�B	�B	�B	�B	�B	&�B	,B	-B	.B	0!B	1'B	49B	6FB	6FB	6FB	6FB	7LB	:^B	=qB	D�B	H�B	J�B	L�B	Q�B	W
B	W
B	XB	XB	YB	\)B	\)B	]/B	^5B	_;B	aHB	ffB	jB	k�B	n�B	r�B	s�B	t�B	v�B	y�B	~�B	�B	�B	�B	�B	�B	�+B	�PB	�VB	�\B	�oB	�uB	�uB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�-B	�-B	�-B	�3B	�?B	�LB	�dB	�jB	�qB	�}B	B	ĜB	ĜB	ƨB	ǮB	ǮB	ǮB	ȴB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�)B	�5B	�5B	�5B	�;B	�BB	�HB	�HB	�HB	�NB	�TB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�`B	�fB	�sB	�B	�B	�B	�B	�B	�B
B

=B
uB
�B
%�B
-B
33B
9XB
=qB
F�B
J�B
N�B
T�B
YB
\)B
dZB
jB
n�B
n�B
q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BZBZBXBZBZBZBUBYBSBTBTB
LB
JB
JB
LB
JB
JBTBRBRBUB[BUBYBUB\BZBYBRB
JB	EB	EB	BB	DB	DB	AB
JB
IB	GB
JB	FB	FB<B9B
IB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BuB�iB�B�B��B��B�lBw�BdaBI�B�BTB B��B�yB�?B�BĢB�@B��B��B�^B�5B�By�Bq�Bh{BWBF�B=xB5BB-B%�B�B�BUB��B�|B�)BȹB��B�=B��B��B|�B^:BYBO�B<pB* B�B_B
��B
�HB
��B
��B
�B
��B
�B
l�B
h~B
fpB
\4B
N�B
;pB
�B
�B
%B	�B	�gB	�EB	�B	��B	��B	�NB	�"B	��B	��B	�GB	B	y�B	j�B	\>B	P�B	G�B	D�B	B�B	=�B	7_B	05B	-"B	&�B	 �B	�B	�B	�B	�B	�B	�B	�B	~B	lB	aB		NB	AB	1B	B�B��B��B��B��B��B��B�B��B�B�gB�;B�)B�B�B��B«B��B��B�pB�RB�7B�!B�B��B��B��B��B�}B�cB�=B�0B�"BB~B}
B|Bz�Bz�Bz�By�Bx�By�Bx�Bw�Bw�Bx�Bx�Bv�Bw�Bx�Bw�Bx�Bx�Bx�Bw�Bv�Bv�Bv�Bv�Bs�Bs�Br�Bq�Bq�Bq�Bt�Br�By�Bz�Bz�Bz�Bz�Bz�Bz�By�By�By�Bx�B|B}B~B~B~BB�"B�'B�)B�2B�;B�AB�DB�EB�GB�@B�8B�5B�Bz�Bz�Bz�B}	B~BB�(B��B�|B�wB��B��B�B�KB�YB�uB�|B��B��BðBĵBĴBŹBŹB��B��B��B��B��B��B��B��B��B�B�B�4B�.B�&B�8B�FB�SB�YB�^B�lB��B��B��B��B��B��B�B	 B	*B	:B	9B		LB	ZB	_B	nB	qB	�B	�B	�B	�B	�B	�B	�B	�B	�B	&�B	,B	-!B	.*B	05B	1;B	4LB	6XB	6XB	6[B	6YB	7^B	:sB	=�B	D�B	H�B	J�B	L�B	Q�B	WB	WB	X"B	X#B	Y)B	\:B	\<B	]@B	^GB	_MB	aYB	fxB	j�B	k�B	n�B	r�B	s�B	t�B	v�B	y�B	B	�B	�B	�B	�$B	�B	�;B	�aB	�hB	�jB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�*B	�5B	�;B	�<B	�<B	�CB	�MB	�\B	�rB	�yB	��B	��B	B	ĬB	ĩB	ƶB	ǽB	ǽB	ǼB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	� B	�B	�B	�B	�B	�)B	�2B	�8B	�DB	�EB	�CB	�GB	�PB	�XB	�UB	�UB	�\B	�aB	�fB	�hB	�fB	�hB	�gB	�fB	�nB	�nB	�nB	�kB	�sB	�B	�B	�B	�B	�B	�B	�B
B

JB
�B
�B
%�B
-B
3=B
9eB
=}B
F�B
J�B
N�B
UB
Y$B
\3B
deB
j�B
n�B
n�B
q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.05 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214362016053112143620160531121436  AO  ARCAADJP                                                                    20140721230543    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230543  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230543  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121436  IP                  G�O�G�O�G�O�                