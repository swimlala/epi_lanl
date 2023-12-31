CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-12-25T03:15:15Z AOML 3.0 creation; 2016-05-31T19:14:46Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20151225031515  20160531121447  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051_7090_134                   2C  D   APEX                            5368                            041511                          846 @׈��Y�#1   @׈�c-:�@4e�Q��dc�l�C�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CM�fCP  CR  CT  CV  CX  CZ  C\  C^�C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&fD&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy` D�3D�33D�ffD�ɚD���D�<�D���D��fD�3D�P D���D���D�	�D�C3Dڙ�D���D� D�9�D�|�D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @~z@��
@��
A�A=�A]�A}�A���A���A���A���A���A���A���A���Bz�Bz�Bz�Bz�B'z�B/z�B7z�B?z�BGz�BOz�BWz�B_z�Bgz�Boz�Bwz�Bz�B��B��B��qB��qB��qB��qB��qB��qB��qB��qB��B��qB��qB��qB��qB��qBýqBǽqB˽qBϽqBӽqB׽qB۽qB߽qB�qB�qB�qB�qB�qB��qB��qB��qC޸C޸C޸C޸C	޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C!޸C#޸C%޸C'޸C)޸C+޸C-޸C/޸C1޸C3޸C5޸C7޸C9޸C;޸C=޸C?޸CA޸CC޸CE޸CG޸CI޸CK޸CM�CO޸CQ޸CS޸CU޸CW޸CY޸C[޸C]�RC_޸Ca޸Cc޸Ce޸Cg޸Ci޸Ck޸Cm޸Co޸Cq޸Cs޸Cu޸Cw޸Cy޸C{޸C}޸C޸C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\D w�D ��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D	w�D	��D
w�D
��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D w�D ��D!w�D!��D"w�D"��D#w�D#��D$w�D$��D%w�D%�D&w�D&��D'w�D'��D(w�D(��D)w�D)��D*w�D*��D+w�D+��D,w�D,��D-w�D-��D.w�D.��D/w�D/��D0w�D0��D1w�D1��D2w�D2��D3w�D3��D4w�D4��D5w�D5��D6w�D6��D7w�D7��D8w�D8��D9w�D9��D:w�D:��D;w�D;��D<w�D<��D=w�D=��D>w�D>��D?w�D?��D@w�D@��DAw�DA��DBw�DB��DCw�DC��DDw�DD��DEw�DE��DFw�DF��DGw�DG��DHw�DH��DIw�DI��DJw�DJ��DKw�DK��DLw�DL��DMw�DM��DNw�DN��DOw�DO��DPw�DP��DQw�DQ��DRw�DR��DSw�DS��DTw�DT��DUw�DU��DVw�DV��DWw�DW��DXw�DX��DYw�DY��DZw�DZ��D[w�D[��D\w�D\��D]w�D]��D^w�D^��D_w�D_��D`w�D`��Daw�Da��Dbw�Db��Dcw�Dc��Ddw�Dd��Dew�De��Dfw�Df��Dgw�Dg��Dhw�Dh��Diw�Di��Djw�Dj��Dkw�Dk��Dlw�Dl��Dmw�Dm��Dnw�Dn��Dow�Do��Dpw�Dp��Dqw�Dq��Drw�Dr��Dsw�Ds��Dtw�DyW�D�
D�/
D�b=D��qD��qD�8�D��qD��=D��
D�K�D���D�ȤD�qD�?
DڕqD�ȤD��D�5qD�x�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�
=A�VA�bA�VA�VA�
=A�JA�
=A�
=A�JA�oA�oA�bA�{A��A�1A��AѾwAѴ9Aѝ�A�x�A�`BAѰ!A���A�C�A�bNA�VA�oAѸRA�Q�A�-AжFA�5?A��/A�"�A͸RA�"�A�(�A���A�p�A�{A�l�A���A�ffA�1A��9A��DA��A���A���A���A�|�A�A�A��FA��wA�n�A�;dA�XA�A�A��+A�A�A�ĜA�ƨA�v�A���A���A��7A��A��A���A��DA�ffA�"�A��`A��A��A�JA�(�A�v�A��A�/A�bA�M�A��DA�ZA�-A�ƨA��A�
=A��A�oA�t�A�A��A���A�E�A��A�A�A�r�A��A��A�Q�A���A���A�G�A�VA�1'A�ffA|~�AyC�Aw?}Av^5Au�mAuG�Ar�Ao��AnbNAkK�AhI�Ag�Af�Ac33Ab �A`�A]�hA[hsAX�uAWƨAV  ASdZAQS�AO��AN��AM�AK��AI�AIl�AH��AHffAG�AE�;AEO�AD�!ACl�AA%A?;dA=�-A<  A9��A8(�A6ZA4VA3�wA37LA1�PA0ffA.�A,�A*�A)K�A'�
A'
=A&Q�A#�
A!��A -A&�A��AG�A�Az�AQ�A+A�A��A1AdZA��AffA �Ax�AAVA9XAG�Az�A��A�7A��A�hA��A��Ax�A
�`A
JA	��A��A�TA�A=qAK�AĜA9XA�jA�A�A �HA ��A =q@�@��@��@�V@���@��@��@�9X@�F@�$�@�A�@�"�@�n�@�G�@�j@�F@��@�z�@���@�33@�^5@�`B@�z�@���@�S�@⟾@�5?@��#@�7L@��@�V@���@�?}@ܣ�@�|�@�hs@؋D@�t�@�"�@֗�@�@���@�33@Ѻ^@�j@��y@́@́@�hs@�j@��
@�l�@�ff@�O�@���@��H@�E�@ř�@��@ċD@���@öF@�S�@�33@��@¸R@���@�z�@�  @���@�o@�v�@���@�%@��u@���@��@���@�~�@�@��h@���@��F@�ȴ@���@�{@��-@�%@��j@�j@�I�@�9X@�(�@�b@���@�|�@�o@���@��@�ȴ@��+@��@��@��`@�Ĝ@���@�Z@� �@�ƨ@�t�@�C�@��@�~�@��@��h@�&�@��/@��9@��u@�z�@�9X@��;@��@�C�@�@���@�^5@���@���@���@�X@�Ĝ@�bN@�b@��w@���@���@�t�@�S�@��@�n�@��@���@��@�Ĝ@��@�r�@�j@�I�@�9X@�1@��@���@�dZ@�C�@�@���@���@�ff@�J@�7L@���@��D@���@��@��P@�\)@��@�~�@��@��@�@���@�p�@�/@�V@���@���@��u@��@�bN@�9X@���@�|�@�33@��@�@���@�=q@�J@��@���@�p�@�X@�/@��9@��;@��F@�dZ@��y@�~�@�E�@�5?@�v�@�v�@�ff@��@�@�@��-@�x�@�?}@�V@���@�z�@�1'@�b@�  @�K�@�"�@��@��R@��R@���@���@�v�@�@��^@�hs@��@�V@�Ĝ@��@�Z@�1'@�(�@�b@�ƨ@���@��P@�|�@�|�@�t�@�S�@�33@��y@��+@�$�@��7@�p�@�%@��/@�Ĝ@�Z@��@��w@��@�dZ@���@�ȴ@���@��\@�^5@��@���@���@�x�@�p�@�G�@�Ĝ@�j@��@���@���@�|�@�dZ@��H@��+@��@��w@{�m@sdZ@i�#@b�@[�m@U?}@P �@H��@C33@;�@4�j@0�9@,�@&$�@ �u@1@�R@-@�R@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�
=A�VA�bA�VA�VA�
=A�JA�
=A�
=A�JA�oA�oA�bA�{A��A�1A��AѾwAѴ9Aѝ�A�x�A�`BAѰ!A���A�C�A�bNA�VA�oAѸRA�Q�A�-AжFA�5?A��/A�"�A͸RA�"�A�(�A���A�p�A�{A�l�A���A�ffA�1A��9A��DA��A���A���A���A�|�A�A�A��FA��wA�n�A�;dA�XA�A�A��+A�A�A�ĜA�ƨA�v�A���A���A��7A��A��A���A��DA�ffA�"�A��`A��A��A�JA�(�A�v�A��A�/A�bA�M�A��DA�ZA�-A�ƨA��A�
=A��A�oA�t�A�A��A���A�E�A��A�A�A�r�A��A��A�Q�A���A���A�G�A�VA�1'A�ffA|~�AyC�Aw?}Av^5Au�mAuG�Ar�Ao��AnbNAkK�AhI�Ag�Af�Ac33Ab �A`�A]�hA[hsAX�uAWƨAV  ASdZAQS�AO��AN��AM�AK��AI�AIl�AH��AHffAG�AE�;AEO�AD�!ACl�AA%A?;dA=�-A<  A9��A8(�A6ZA4VA3�wA37LA1�PA0ffA.�A,�A*�A)K�A'�
A'
=A&Q�A#�
A!��A -A&�A��AG�A�Az�AQ�A+A�A��A1AdZA��AffA �Ax�AAVA9XAG�Az�A��A�7A��A�hA��A��Ax�A
�`A
JA	��A��A�TA�A=qAK�AĜA9XA�jA�A�A �HA ��A =q@�@��@��@�V@���@��@��@�9X@�F@�$�@�A�@�"�@�n�@�G�@�j@�F@��@�z�@���@�33@�^5@�`B@�z�@���@�S�@⟾@�5?@��#@�7L@��@�V@���@�?}@ܣ�@�|�@�hs@؋D@�t�@�"�@֗�@�@���@�33@Ѻ^@�j@��y@́@́@�hs@�j@��
@�l�@�ff@�O�@���@��H@�E�@ř�@��@ċD@���@öF@�S�@�33@��@¸R@���@�z�@�  @���@�o@�v�@���@�%@��u@���@��@���@�~�@�@��h@���@��F@�ȴ@���@�{@��-@�%@��j@�j@�I�@�9X@�(�@�b@���@�|�@�o@���@��@�ȴ@��+@��@��@��`@�Ĝ@���@�Z@� �@�ƨ@�t�@�C�@��@�~�@��@��h@�&�@��/@��9@��u@�z�@�9X@��;@��@�C�@�@���@�^5@���@���@���@�X@�Ĝ@�bN@�b@��w@���@���@�t�@�S�@��@�n�@��@���@��@�Ĝ@��@�r�@�j@�I�@�9X@�1@��@���@�dZ@�C�@�@���@���@�ff@�J@�7L@���@��D@���@��@��P@�\)@��@�~�@��@��@�@���@�p�@�/@�V@���@���@��u@��@�bN@�9X@���@�|�@�33@��@�@���@�=q@�J@��@���@�p�@�X@�/@��9@��;@��F@�dZ@��y@�~�@�E�@�5?@�v�@�v�@�ff@��@�@�@��-@�x�@�?}@�V@���@�z�@�1'@�b@�  @�K�@�"�@��@��R@��R@���@���@�v�@�@��^@�hs@��@�V@�Ĝ@��@�Z@�1'@�(�@�b@�ƨ@���@��P@�|�@�|�@�t�@�S�@�33@��y@��+@�$�@��7@�p�@�%@��/@�Ĝ@�Z@��@��w@��@�dZ@���@�ȴ@���@��\@�^5@��@���@���@�x�@�p�@�G�@�Ĝ@�j@��@���@���@�|�@�dZ@��H@��+@��@��w@{�m@sdZ@i�#@b�@[�m@U?}@P �@H��@C33@;�@4�j@0�9@,�@&$�@ �u@1@�R@-@�R@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBL�BL�BL�BL�BL�BK�BK�BK�BK�BL�BL�BL�BL�BN�BQ�B`BBn�B�=B�B�wB��B�B  B�B=qBR�BYBdZBt�B�bB��B�!B�wBŢB�`B+B�B2-BB�B[#B_;Be`BhsBiyBhsBffB]/BK�BD�B8RB7LB7LB<jB"�BhBB�B�BB�B�
B�B��B�LB�Bn�Bx�B��B�B�Bl�BT�BL�BbNBgmBffB_;BP�B7LB�B  B�ZB��B�3B��B�1B�B�PB��B��B��B{�B�B|�BjBXBH�B)�B
��B
�B
�B
�TB
��B
ǮB
��B
��B
�VB
jB
C�B
/B
%�B
�B
�B
uB	��B	�B	�B	�#B	ƨB	�dB	�!B	��B	�JB	}�B	o�B	ffB	XB	O�B	A�B	2-B	+B	&�B	!�B	�B	uB	DB	1B	%B	B	B��B��B�B�B�fB�NB�/B�
B��B��BǮBƨBȴB��BǮBɺB��BǮBƨBĜBÖBB�}B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�oB�oB�oB�oB�bB�JB�JB�JB�PB�JB�JB�DB�=B�1B�+B�+B�%B�B�B�B�B}�B|�B|�B~�B~�B� B� B�B�B�B�B� B�B�B�B�B�B�B�%B�1B�DB�DB�DB�DB�DB�\B�bB�oB�hB�hB�hB��B��B��B�uB�{B��B��B��B��B��B��B�B�!B�FB�^B�dB�wB�wB��BƨB��B��B��B��B��B��B�B�#B�/B�;B�HB�ZB�fB�sB�B�B�B�B��B��B��B	B	+B	1B	
=B	DB	VB	\B	hB	oB	oB	uB	uB	�B	�B	�B	�B	 �B	!�B	"�B	'�B	.B	0!B	1'B	49B	5?B	:^B	=qB	?}B	?}B	A�B	D�B	G�B	K�B	N�B	P�B	Q�B	S�B	S�B	VB	YB	ZB	^5B	_;B	bNB	cTB	ffB	gmB	hsB	hsB	gmB	hsB	k�B	m�B	n�B	p�B	t�B	u�B	w�B	y�B	{�B	|�B	� B	�B	�B	�B	�B	�+B	�7B	�DB	�JB	�VB	�\B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�3B	�3B	�?B	�FB	�RB	�XB	�dB	�jB	�qB	�qB	�qB	�qB	�wB	��B	B	B	B	ÖB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�)B	�/B	�/B	�5B	�BB	�BB	�HB	�HB	�NB	�NB	�TB	�`B	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
%B
%B
+B
+B
+B
+B
+B
%B
+B
JB
�B
"�B
,B
2-B
9XB
>wB
A�B
E�B
G�B
K�B
Q�B
W
B
^5B
cTB
iyB
k�B
n�B
t�B
x�B
{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BL�BL�BL�BL�BL�BK�BK�BK�BK�BL�BL�BL�BL�BN�BQ�B`MBn�B�IB�B��B��B�B 	B�B=~BS BY$BdfBt�B�pB��B�-B��BŬB�oB9B�B29BB�B[0B_JBeqBh�Bi�Bh�BfvB]?BK�BD�B8bB7^B7]B<yB"�BuB+B�B�B+B�B�B�B��B�WB�Bn�Bx�B��B�B�Bl�BU
BL�Bb[BgzBfrB_IBP�B7ZB�B B�fB��B�=B��B�?B�'B�_B��B��B��B{�B�B|�Bj�BX BH�B*
B
�
B
�B
�B
�cB
��B
ǿB
��B
� B
�gB
j�B
C�B
/1B
%�B
�B
�B
�B	�B	�B	��B	�6B	��B	�}B	�:B	��B	�cB	~B	o�B	f�B	X+B	O�B	A�B	2JB	+!B	'B	!�B	�B	�B	bB	OB	CB	>B	%B��B��B��B�B�B�qB�QB�+B�B��B��B��B��B��B��B��B��B��B��BľB÷B°B��B�%B�B�B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�nB�nB�nB�tB�oB�mB�hB�cB�UB�QB�OB�HB�CB�?B�;B�+B~B}B}BB B�&B�(B�*B�1B�+B�,B�%B�1B�8B�:B�DB�DB�?B�KB�WB�iB�iB�jB�jB�jB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B� B� B�,B�BB�jB��B��B��B��B��B��B��B��B��B��B��B�B�1B�EB�OB�[B�kB�}B�B�B�B�B��B��B��B��B�B	,B	KB	QB	
^B	cB	tB	}B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	"�B	(B	.3B	0>B	1DB	4VB	5_B	:~B	=�B	?�B	?�B	A�B	D�B	G�B	K�B	N�B	QB	R
B	TB	TB	V B	Y5B	Z;B	^RB	_YB	bhB	cqB	f�B	g�B	h�B	h�B	g�B	h�B	k�B	m�B	n�B	p�B	t�B	u�B	w�B	y�B	|B	}	B	�B	�"B	�&B	�&B	�,B	�GB	�SB	�`B	�fB	�pB	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�+B	�;B	�FB	�NB	�OB	�XB	�`B	�lB	�sB	�~B	��B	��B	��B	��B	��B	��B	��B	¨B	«B	©B	ðB	��B	��B	��B	��B	��B	��B	��B	��B	� B	��B	�B	�B	�B	�B	�B	�.B	�6B	�:B	�BB	�HB	�IB	�NB	�ZB	�[B	�cB	�_B	�dB	�fB	�mB	�xB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	� B	�B	�B	�B	�B
B
'B
$B
$B
)B
3B
=B
<B
DB
EB
CB
CB
BB
?B
BB
aB
�B
"�B
,B
2DB
9mB
>�B
A�B
E�B
G�B
K�B
RB
W!B
^JB
cfB
i�B
k�B
n�B
t�B
x�B
{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.13 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214472016053112144720160531121447  AO  ARCAADJP                                                                    20151225031515    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151225031515  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151225031515  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121447  IP                  G�O�G�O�G�O�                