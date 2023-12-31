CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:03:18Z AOML 3.0 creation; 2016-05-26T23:45:35Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230318  20160526164535  5903726 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  4045_7089_004                   2C  D   APEX                            5372                            041511                          846 @�2.L�@1   @�2.� ?��D�`A�7@D ě��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�Dy�fD� D�33D�|�D��fD�fD�L�D��3D��3D�fD�I�D��3D���D�  D�0 Dړ3D๚D�	�D�C3D�D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @w�@��
@��
A�A=�A]�A}�A���A���A���A���A���A���A���A���Bz�Bz�Bz�Bz�B'z�B/z�B7z�B?z�BGz�BOz�BWz�B_z�Bgz�Boz�Bwz�Bz�B��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qBýqBǽqB˽qBϽqBӽqB׽qB۽qB߽qB�qB�qB�qB�qB�qB��qB��qB��qC޸C޸C޸C޸C	޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C!޸C#޸C%޸C'޸C)޸C+޸C-޸C/޸C1޸C3޸C5޸C7޸C9޸C;޸C=޸C?޸CA޸CC޸CE޸CG޸CI޸CK޸CM޸CO޸CQ޸CS޸CU޸CW޸CY޸C[޸C]޸C_޸Ca޸Cc޸Ce޸Cg޸Ci޸Ck޸Cm޸Co޸Cq޸Cs޸Cu޸Cw޸Cy޸C{޸C}޸C޸C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\D w�D ��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D	w�D	��D
w�D
��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D w�D ��D!w�D!��D"w�D"��D#w�D#��D$w�D$��D%w�D%��D&w�D&��D'w�D'��D(w�D(��D)w�D)��D*w�D*��D+w�D+��D,w�D,��D-w�D-��D.w�D.��D/w�D/��D0w�D0��D1w�D1��D2w�D2��D3w�D3��D4w�D4��D5w�D5��D6w�D6��D7w�D7��D8w�D8��D9w�D9��D:w�D:��D;w�D;��D<w�D<��D=w�D=��D>w�D>��D?w�D?��D@w�D@��DAw�DA��DBw�DB��DCw�DC��DDw�DD��DEw�DE��DFw�DF��DGw�DG��DHw�DH��DIw�DI��DJw�DJ��DKw�DK��DLw�DL��DMw�DM��DNw�DN��DOw�DO��DPw�DP��DQw�DQ��DRw�DR��DSw�DS��DTw�DT��DUw�DU��DVw�DV��DWw�DW��DXw�DX��DYw�DY��DZw�DZ��D[w�D[��D\w�D\��D]w�D]��D^w�D^��D_w�D_��D`w�D`��Daw�Da��Dbw�Db��Dcw�Dc��Ddw�Dd��Dew�De��Dfw�Df��Dgw�Dg��Dhw�Dh��Diw�Di��Djw�Dj��Dkw�Dk��Dlw�Dl��Dmw�Dm��Dnw�Dn��Dow�Do��Dpw�Dp��Dqw�Dq��Drw�Dr��Dsw�Ds��Dtd{Dy�D��D�/
D�x�D��=D�=D�H�D�
D��
D�=D�EqD��
D�ȤD���D�+�Dڏ
D�qD�qD�?
D�qD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�E�A�7LA�l�A�M�A�l�A�x�A�`BA�S�A�A�A�I�A�XA��A�oA�{A�bA�VA�bA�bA�VA�VA�VA�A�JA�A�A�A�p�A�Q�A�+A�(�A��A�JA��A���A�t�A�"�A�  A�A�A��TA�jA���A��!A��jA�A�$�A�{A� �A��7A���A���A�=qA��!A�%A�bNA��A��A��PA�Q�A�\)A��HA�=qA��A��uA�S�A�
=A��hA�/A���A���A��DA�S�A�/A�1A�ȴA���A��+A�r�A�`BA�E�A�A�A�;dA�(�A�%A���A��A���A��7A� �A���A�ȴA��FA��hA�hsA�=qA��yA��A�|�A�5?A��
A��hA�C�A��A��wA��A�hsA�oA���A��`A��FA�C�A�bA�A���A�t�A�33A�1A��9A�M�A�+A�wA~��A}�mA}&�A|ĜA|�\A{��A{
=Az��Az�DAy�^AyS�Ax��Aw�#Aw/Aw%AvĜAv-Au�7AtĜAr��Aq��Aqt�Aq|�AqVApr�Ao��Ao?}An��Am�hAl�jAlE�Ak�;Ak;dAj~�AihsAi%Ah�Agl�Af��Af��Agp�Ag`BAgS�AgO�AgC�Ag�Af�`AfbNAe��Ae?}Ad��Ad��Ad-Ac�
Ac%Ab{Aa�A`5?A_��A_A^A]�A];dA\�uA[��A[�AZI�AZ{AYO�AY�AY&�AY�AX�AX��AX�uAY?}AYK�AY?}AY?}AY/AX�AX�\AXr�AW��AW�AWK�AV��AVQ�AVZAVQ�AVAUhsAT{ASO�AR��ARbNAR=qAR�AQXAP�\AP{AO�TAO|�ANv�AN{AM��AMp�AM%AL��AL1AK�AK�AJ�AJE�AI�FAI��AIG�AHĜAH{AG�7AG33AF�AF(�AF�AF  AF  AE��AE�^AE?}AEVAEVAD��ADĜAD��AD$�ACG�AB��ABbNABbAA�^AAl�AA?}AA
=A@�HA@�A@(�A?�wA?O�A>��A>z�A=�A=�A=`BA<��A;�^A;C�A:��A:�9A:~�A9�TA9O�A8{A7�A7p�A733A7A6��A6ZA4z�A3ƨA2��A1�TA1A0{A/\)A.�HA.n�A.bA-�
A-x�A-�A,�`A,ȴA,�jA,��A,�A-VA-�A-/A-7LA-?}A-G�A-K�A,��A,VA,$�A,  A+��A+S�A*�`A*��A*9XA)|�A)p�A)G�A(�A'dZA&�`A&��A&��A&M�A$bNA$ffA$��A$��A$bNA$9XA#�TA#dZA"��A"�9A"1'A!��A �/A n�A �A�A  A��A9XA��Av�AƨA�A�A33A�A��AĜAE�Ax�A-A�`AbNA��A��A-A�FAK�AĜA�\A^5A-A�A��AC�A  A�AO�A�9A��A�uAVA�A�;AƨA��A
�A
jA	�-A	33A�/Ar�A(�A��AC�A��A�A�A�9A$�A�A�wAp�A"�An�AbA�A �uA z�A bNA I�A -A {@�|�@���@��@�X@���@���@�o@�~�@�-@�@��#@��7@��@�;d@��-@���@�I�@� �@��m@�;d@�v�@�@��m@�=q@��^@�h@�?}@�Z@���@�ff@�-@��#@�@��/@�r�@� �@�+@�7@�(�@�@�p�@�1@�V@�O�@ۥ�@ٺ^@��@�r�@�ƨ@�~�@�G�@�1@�ȴ@��@�?}@мj@�1@Χ�@�{@̃@��;@�
=@���@�&�@���@�1'@��@�^5@Ų-@��@� �@Å@�C�@��@��F@�v�@��P@��H@��@�"�@�^5@x��@n{@dj@[S�@Up�@N{@G�P@@Ĝ@=?}@;C�@9�#@8r�@5�T@2��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�E�A�7LA�l�A�M�A�l�A�x�A�`BA�S�A�A�A�I�A�XA��A�oA�{A�bA�VA�bA�bA�VA�VA�VA�A�JA�A�A�A�p�A�Q�A�+A�(�A��A�JA��A���A�t�A�"�A�  A�A�A��TA�jA���A��!A��jA�A�$�A�{A� �A��7A���A���A�=qA��!A�%A�bNA��A��A��PA�Q�A�\)A��HA�=qA��A��uA�S�A�
=A��hA�/A���A���A��DA�S�A�/A�1A�ȴA���A��+A�r�A�`BA�E�A�A�A�;dA�(�A�%A���A��A���A��7A� �A���A�ȴA��FA��hA�hsA�=qA��yA��A�|�A�5?A��
A��hA�C�A��A��wA��A�hsA�oA���A��`A��FA�C�A�bA�A���A�t�A�33A�1A��9A�M�A�+A�wA~��A}�mA}&�A|ĜA|�\A{��A{
=Az��Az�DAy�^AyS�Ax��Aw�#Aw/Aw%AvĜAv-Au�7AtĜAr��Aq��Aqt�Aq|�AqVApr�Ao��Ao?}An��Am�hAl�jAlE�Ak�;Ak;dAj~�AihsAi%Ah�Agl�Af��Af��Agp�Ag`BAgS�AgO�AgC�Ag�Af�`AfbNAe��Ae?}Ad��Ad��Ad-Ac�
Ac%Ab{Aa�A`5?A_��A_A^A]�A];dA\�uA[��A[�AZI�AZ{AYO�AY�AY&�AY�AX�AX��AX�uAY?}AYK�AY?}AY?}AY/AX�AX�\AXr�AW��AW�AWK�AV��AVQ�AVZAVQ�AVAUhsAT{ASO�AR��ARbNAR=qAR�AQXAP�\AP{AO�TAO|�ANv�AN{AM��AMp�AM%AL��AL1AK�AK�AJ�AJE�AI�FAI��AIG�AHĜAH{AG�7AG33AF�AF(�AF�AF  AF  AE��AE�^AE?}AEVAEVAD��ADĜAD��AD$�ACG�AB��ABbNABbAA�^AAl�AA?}AA
=A@�HA@�A@(�A?�wA?O�A>��A>z�A=�A=�A=`BA<��A;�^A;C�A:��A:�9A:~�A9�TA9O�A8{A7�A7p�A733A7A6��A6ZA4z�A3ƨA2��A1�TA1A0{A/\)A.�HA.n�A.bA-�
A-x�A-�A,�`A,ȴA,�jA,��A,�A-VA-�A-/A-7LA-?}A-G�A-K�A,��A,VA,$�A,  A+��A+S�A*�`A*��A*9XA)|�A)p�A)G�A(�A'dZA&�`A&��A&��A&M�A$bNA$ffA$��A$��A$bNA$9XA#�TA#dZA"��A"�9A"1'A!��A �/A n�A �A�A  A��A9XA��Av�AƨA�A�A33A�A��AĜAE�Ax�A-A�`AbNA��A��A-A�FAK�AĜA�\A^5A-A�A��AC�A  A�AO�A�9A��A�uAVA�A�;AƨA��A
�A
jA	�-A	33A�/Ar�A(�A��AC�A��A�A�A�9A$�A�A�wAp�A"�An�AbA�A �uA z�A bNA I�A -A {@�|�@���@��@�X@���@���@�o@�~�@�-@�@��#@��7@��@�;d@��-@���@�I�@� �@��m@�;d@�v�@�@��m@�=q@��^@�h@�?}@�Z@���@�ff@�-@��#@�@��/@�r�@� �@�+@�7@�(�@�@�p�@�1@�V@�O�@ۥ�@ٺ^@��@�r�@�ƨ@�~�@�G�@�1@�ȴ@��@�?}@мj@�1@Χ�@�{@̃@��;@�
=@���@�&�@���@�1'@��@�^5@Ų-@��@� �@Å@�C�G�O�@��F@�v�@��P@��H@��@�"�@�^5@x��@n{@dj@[S�@Up�@N{@G�P@@Ĝ@=?}@;C�@9�#@8r�@5�T@2��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB)�B+B)�B+B)�B)�B)�B)�B+B+B)�B+B)�B)�B)�B)�B)�B)�B(�B)�B(�B(�B)�B(�B(�B'�B%�B#�B"�B"�B!�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B �B#�B$�B&�B)�B,B.B0!B2-B33B6FB7LB7LB9XB:^B<jB=qB=qB=qB=qB=qB@�BC�BD�BD�BD�BD�BE�BD�BE�BE�BE�BE�BE�BE�BE�BE�BE�BD�BD�BB�BB�BA�BA�BA�B@�B@�B>wB=qB;dB;dB:^B7LB33B1'B/B-B'�B#�B"�B!�B �B �B�B�BoBPBVBJB	7B%BBB��B��B��B�B�B�mB�`B�HB�5B�)B�B�B��B��B��BŢBĜBB�wB�RB�'B��B��B��B��B�{B�hB�\B�JB�+B�B|�By�Bu�Bo�BgmB^5BZBT�BL�BG�BN�BffBk�Br�Bv�Bw�Bx�Bw�Br�BhsBe`BcTBcTBaHB`BBXBN�BG�BC�B@�B:^B0!B,B'�B �B�BVBB��B�B�B�B�B�B�sB�B+B1B1B	7B1BB%B1BBBB��BBBBB��B�B�B�mB�fB�`B�ZB�;B�B�B��B��BǮBĜB��B�wB�^B�FB�B��B��B��B��B��B��B��B�hB�JB�1B�B�B{�Bz�By�By�By�Bv�Bs�Bq�Bq�Bp�Bn�Bl�BhsBaHB]/BZBW
BS�BQ�BO�BM�BK�BH�BD�BA�B=qB8RB6FB1'B/B+B#�B�B�BuBbBVB1BB��B�B�B�B�mB�ZB�5BǮB�wB�3B��B��B�oB�DB�%B�B�B�B�B� B~�B�B�B�1B�VB�hB�oB�{B��B��B��B��B�uB�bB�VB�JB�=B�%B�B� B|�Bv�Bu�Br�Bm�B]/BXBT�BT�BQ�B>wBC�BH�BJ�BJ�BK�BJ�BG�BD�BC�B@�B;dB6FB2-B.B!�B�BuB	7B  B
��B
��B
�B
�B
��B
��B
��B
��B
�B
�yB
�#B
��B
��B
ŢB
��B
�qB
�^B
�RB
�FB
�?B
�9B
�3B
�-B
�!B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�oB
�\B
�PB
�DB
�7B
�+B
�B
~�B
y�B
t�B
r�B
p�B
p�B
n�B
l�B
jB
gmB
ffB
aHB
^5B
^5B
]/B
]/B
\)B
\)B
ZB
YB
W
B
VB
S�B
R�B
O�B
N�B
N�B
M�B
L�B
K�B
H�B
E�B
A�B
?}B
=qB
=qB
<jB
9XB
7LB
5?B
1'B
-B
,B
+B
)�B
&�B
"�B
!�B
!�B
 �B
�B
�B
�B
�B
�B
uB
\B
JB
1B
B
  B	��B	��B	�B	�B	�B	�B	�B	�sB	�`B	�NB	�;B	�/B	�#B	�B	�B	��B	��B	��B	��B	ȴB	ƨB	ŢB	B	�}B	�qB	�dB	�XB	�RB	�LB	�FB	�LB	�B	��B	��B	��B	��B	�3B	��B	�HB	�B
	7B
�B
1'B
J�B
T�B
gmB
�7B
��B
�B
�RB
ŢB
�#11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B*B+B*B+B*B*B*B*B+B+B*B+B*B*B*B*B*B*B)B*B)B)B*B)B)B(B%�B#�B"�B"�B!�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B �B#�B$�B&�B*B,!B.,B09B2EB3JB6\B7cB7fB9oB:xB<~B=�B=�B=�B=�B=�B@�BC�BD�BD�BD�BD�BE�BD�BE�BE�BE�BE�BE�BE�BE�BE�BE�BD�BD�BB�BB�BA�BA�BA�B@�B@�B>�B=�B;zB;|B:sB7cB3IB1>B/2B-#B(B#�B"�B!�B �B �B�B�B�BgBlB`B	MB>B)BB�B��B��B�B�B�B�uB�XB�IB�>B�2B�B�B��B��BŵBĮB¤B��B�eB�:B��B��B��B��B��B�{B�qB�[B�=B�B}By�Bu�Bo�Bg~B^DBZ1BUBL�BG�BN�BfuBk�Br�Bv�Bw�Bx�Bw�Br�Bh�BeuBchBcjBa[B`RBX#BN�BG�BC�B@�B:oB00B,B( B �B�BdBB�B��B�B�B�B�B�B�B8BBBAB	FBAB.B2BABBB-B�	BB&B/B1B�B��B�B�~B�tB�pB�lB�MB�,B�B�B��BǻBĬB��B��B�oB�UB�'B�	B��B��B��B��B��B��B�yB�ZB�AB�'B�B{�Bz�By�By�By�Bv�Bs�Bq�Bq�Bp�Bn�Bl�Bh�BaUB]=BZ,BWBTBQ�BO�BM�BK�BH�BD�BA�B=�B8_B6VB12B/*B+B#�B�B�B�BrBdB=BB��B�B�B�B�|B�hB�FBǿB��B�BB� B��B�B�TB�5B�"B� B�B�B�B	B�B�'B�@B�eB�vB�~B��B��B��B��B��B��B�sB�fB�YB�KB�3B�!B�B|�Bv�Bu�Br�Bm�B]@BXBUBUBQ�B>�BC�BH�BJ�BJ�BK�BJ�BG�BD�BC�B@�B;sB6RB2<B.#B!�B�B�B	IB B
��B
��B
��B
�B
��B
��B
��B
��B
�B
�B
�4B
��B
��B
ųB
��B
��B
�rB
�eB
�[B
�PB
�MB
�EB
�BB
�5B
� B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�pB
�cB
�UB
�IB
�>B
�'B
B
y�B
t�B
r�B
p�B
p�B
n�B
l�B
j�B
g�B
f{B
aZB
^JB
^HB
]@B
]CB
\=B
\>B
Z1B
Y)B
WB
VB
TB
SB
O�B
N�B
N�B
M�B
L�B
K�B
H�B
E�B
A�B
?�B
=�B
=�B
<B
9oB
7`B
5UB
19B
-$B
,B
+B
*B
' B
"�B
!�B
!�B
 �B
�B
�B
�B
�B
�B
�B
sB
_B
IB
2B
 B	�B	��B	��B	��B	�B	��B	�B	�B	�wB	�gB	�RB	�HB	�;B	�-B	�B	�B	��B	��B	��B	��B	��B	ŻB	¦B	��B	��B	�B	�qB	�jB	�eB	�^G�O�B	�"B	��B	��B	��B	��B	�OB	��B	�`B	��B
	KB
�B
1=B
J�B
UB
g�B
�LB
��B
�B
�cB
ųB
�611111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.13 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605261645352016052616453520160526164535  AO  ARCAADJP                                                                    20140721230318    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230318  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230318  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160526164535  IP                  G�O�G�O�G�O�                