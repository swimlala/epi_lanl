CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:22Z AOML 3.0 creation; 2016-05-31T19:14:29Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230522  20160531121429  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  4051_7090_030                   2C  D   APEX                            5368                            041511                          846 @�~MF)��1   @�~M�o��@4lI�^5?�d�`A�7L1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�DyffD�3D�@ D�vfD�� D�fD�33D�p D��fD��D�@ D��fD�ɚD���D�)�Dڙ�D๚D�  D�L�D�i�D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�=p@��
@��
A�A=�A]�A}�A���A���A���A���A���A���A���A���Bz�Bz�Bz�Bz�B'�GB/z�B7z�B?z�BGz�BOz�BWz�B_z�Bgz�Boz�Bwz�Bz�B��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qBýqBǽqB˽qBϽqBӽqB׽qB۽qB߽qB�qB�qB�qB�qB�qB��qB��qB��qC޸C޸C޸C޸C	޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C!޸C#޸C%޸C'޸C)޸C+޸C-޸C/޸C1޸C3޸C5޸C7޸C9޸C;޸C=޸C?޸CA޸CC޸CE޸CG޸CI޸CK޸CM޸CO޸CQ޸CS޸CU޸CW޸CY޸C[޸C]޸C_޸Ca޸Cc޸Ce޸Cg޸Ci޸Ck޸Cm޸Co޸Cq޸Cs޸Cu޸Cw޸Cy޸C{޸C}޸C޸C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��)C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��C��\C��\C��\C��\C��\C��\C��\C��\D w�D ��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D	w�D	��D
w�D
��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D w�D ��D!w�D!��D"w�D"��D#w�D#��D$w�D$��D%w�D%��D&w�D&��D'w�D'��D(w�D(��D)w�D)��D*w�D*��D+w�D+��D,w�D,��D-w�D-��D.w�D.��D/w�D/��D0w�D0��D1w�D1��D2w�D2��D3w�D3��D4w�D4��D5w�D5��D6w�D6��D7w�D7��D8w�D8��D9w�D9��D:w�D:��D;w�D;��D<w�D<��D=w�D=��D>w�D>��D?w�D?��D@w�D@��DAw�DA��DBw�DB��DCw�DC��DDw�DD��DEw�DE��DFw�DF��DGw�DG��DHw�DH��DIw�DI��DJw�DJ��DKw�DK��DLw�DL��DMw�DM��DNw�DN��DOw�DO��DPw�DP��DQw�DQ��DRw�DR��DSw�DS��DTw�DT��DUw�DU��DVw�DV��DWw�DW��DXw�DX��DYw�DY��DZw�DZ��D[w�D[��D\w�D\��D]w�D]��D^w�D^��D_w�D_��D`w�D`��Daw�Da��Dbw�Db��Dcw�Dc��Ddw�Dd��Dew�De��Dfw�Df��Dgw�Dg��Dhw�Dh��Diw�Di��Djw�Dj��Dkw�Dk��Dlw�Dl��Dmw�Dm��Dnw�Dn��Dow�Do��Dpw�Dp��Dqw�Dq��Drw�Dr��Dsw�Ds��DtqHDy^D��
D�;�D�r=D���D�=D�/
D�k�D��=D��D�;�D��=D��qD��qD�%qDڕqD�qD���D�H�D�eqD��
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�1A�1A�1A�1A�1A���A���A�1A�VA�VA�VA�JA�JA�VA�bA�bA�JA�JA�
=A�
=A�  A���A���A���A���A���A���A���A�  A�  A�  A�  A���A�  A�A�A�A�A�A�A�%A�%A�A�A�A�A�%A�A�  A���A��A��;A��
Aʰ!A�I�Aɕ�A�1A�^5AǛ�A��A��A�z�A��mA��A��A�K�A���A���A�K�A��\A��mA��wA��A�(�A��RA��A��A�v�A��A�(�A�n�A�"�A�jA���A�ZA���A��A�M�A��DA�A���A�A��A���A�hsA���A��A��A��A�VA���A��A�p�A��mA���A��A�  A���A��HA�z�A�A�A�ZA���A��yA�9XA�&�A��A��A��A��`A}x�Az��Ay��AxI�Av��Av�As+Ap�+An�+Ak\)AjZAit�Ah��Ag?}Ae�Ad�\AcoAa�7A`�DA_;dA]�7A\v�AZ$�AX�!AW�AW��AV��AU+AS��AS��AO�^AM�wAM�AK�;AI�;AF��AEK�ADv�AB�A?��A=|�A;�;A;�FA;"�A:v�A8��A6�RA4�A2bA0�RA.�RA,�A,VA*��A*{A)
=A&��A$ZA#33A!C�A�#A�+A5?A��AG�A��A�A�AM�A&�A1'A
=AE�A  A��A�AZA��AbNA1'A�wAl�A/AA��A=qA�A�AC�A�A  A/A	��A	?}A�!AM�A/A�yA��A�PAjAA��A��A�A?}A �y@��#@�S�@�@���@��;@�\)@��!@���@���@�P@��@�@��@�ȴ@@��^@�P@�n�@���@���@�R@�`B@�@�S�@�p�@�Q�@�~�@��@�`B@�"�@��#@ى7@���@�^5@պ^@��@�K�@ҏ\@д9@�x�@�b@�~�@�x�@���@�+@�/@� �@öF@öF@þw@�;d@�A�@�S�@�ff@��-@�p�@�hs@�hs@�%@�j@��u@���@��^@��@�1@���@��@�S�@���@�x�@��D@�j@�I�@�(�@��@�dZ@�dZ@�K�@�@���@�~�@�^5@��T@�&�@�A�@�@�{@�O�@��j@��
@�S�@���@��@��-@�X@�&�@��/@��u@�Q�@�  @�ƨ@���@�+@��@��y@���@�v�@��#@�x�@�O�@�O�@�7L@���@���@��@�"�@���@�ff@�{@��#@���@�?}@���@�Q�@�A�@��@���@�ƨ@��@�dZ@�@���@��+@�E�@�-@�@���@��@�x�@�O�@�&�@��`@���@�9X@�  @��;@���@��F@�\)@�
=@���@�@�
=@��!@���@�@�X@�Ĝ@��u@�r�@�A�@��@��w@��P@�t�@�33@��H@��!@�{@��@���@�x�@�7L@�7L@���@���@�V@���@���@�A�@���@�+@���@��y@���@�n�@��@�J@��#@�x�@�hs@�?}@�7L@�7L@��@�I�@���@��@��@�|�@���@�S�@�33@�o@��@���@�~�@�V@�V@��@���@��7@�hs@�7L@���@��9@��u@�Z@� �@���@���@�K�@�+@��y@��R@��\@�V@���@���@���@�x�@�X@�G�@��@��D@�Z@�A�@�1'@�1@��P@�33@���@��R@�~�@�ff@�-@��#@�7L@���@��`@��/@��/@���@�Ĝ@��j@��@��u@��D@�bN@�(�@���@�=q@�(�@vV@k��@co@Z=q@T9X@L9X@D�j@>@6ȴ@0�`@+33@&V@!�7@O�@x�@��@�9@$�@"�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�1A�1A�1A�1A�1A���A���A�1A�VA�VA�VA�JA�JA�VA�bA�bA�JA�JA�
=A�
=A�  A���A���A���A���A���A���A���A�  A�  A�  A�  A���A�  A�A�A�A�A�A�A�%A�%A�A�A�A�A�%A�A�  A���A��A��;A��
Aʰ!A�I�Aɕ�A�1A�^5AǛ�A��A��A�z�A��mA��A��A�K�A���A���A�K�A��\A��mA��wA��A�(�A��RA��A��A�v�A��A�(�A�n�A�"�A�jA���A�ZA���A��A�M�A��DA�A���A�A��A���A�hsA���A��A��A��A�VA���A��A�p�A��mA���A��A�  A���A��HA�z�A�A�A�ZA���A��yA�9XA�&�A��A��A��A��`A}x�Az��Ay��AxI�Av��Av�As+Ap�+An�+Ak\)AjZAit�Ah��Ag?}Ae�Ad�\AcoAa�7A`�DA_;dA]�7A\v�AZ$�AX�!AW�AW��AV��AU+AS��AS��AO�^AM�wAM�AK�;AI�;AF��AEK�ADv�AB�A?��A=|�A;�;A;�FA;"�A:v�A8��A6�RA4�A2bA0�RA.�RA,�A,VA*��A*{A)
=A&��A$ZA#33A!C�A�#A�+A5?A��AG�A��A�A�AM�A&�A1'A
=AE�A  A��A�AZA��AbNA1'A�wAl�A/AA��A=qA�A�AC�A�A  A/A	��A	?}A�!AM�A/A�yA��A�PAjAA��A��A�A?}A �y@��#@�S�@�@���@��;@�\)@��!@���@���@�P@��@�@��@�ȴ@@��^@�P@�n�@���@���@�R@�`B@�@�S�@�p�@�Q�@�~�@��@�`B@�"�@��#@ى7@���@�^5@պ^@��@�K�@ҏ\@д9@�x�@�b@�~�@�x�@���@�+@�/@� �@öF@öF@þw@�;d@�A�@�S�@�ff@��-@�p�@�hs@�hs@�%@�j@��u@���@��^@��@�1@���@��@�S�@���@�x�@��D@�j@�I�@�(�@��@�dZ@�dZ@�K�@�@���@�~�@�^5@��T@�&�@�A�@�@�{@�O�@��j@��
@�S�@���@��@��-@�X@�&�@��/@��u@�Q�@�  @�ƨ@���@�+@��@��y@���@�v�@��#@�x�@�O�@�O�@�7L@���@���@��@�"�@���@�ff@�{@��#@���@�?}@���@�Q�@�A�@��@���@�ƨ@��@�dZ@�@���@��+@�E�@�-@�@���@��@�x�@�O�@�&�@��`@���@�9X@�  @��;@���@��F@�\)@�
=@���@�@�
=@��!@���@�@�X@�Ĝ@��u@�r�@�A�@��@��w@��P@�t�@�33@��H@��!@�{@��@���@�x�@�7L@�7L@���@���@�V@���@���@�A�@���@�+@���@��y@���@�n�@��@�J@��#@�x�@�hs@�?}@�7L@�7L@��@�I�@���@��@��@�|�@���@�S�@�33@�o@��@���@�~�@�V@�V@��@���@��7@�hs@�7L@���@��9@��u@�Z@� �@���@���@�K�@�+@��y@��R@��\@�V@���@���@���@�x�@�X@�G�@��@��D@�Z@�A�@�1'@�1@��P@�33@���@��R@�~�@�ff@�-@��#@�7L@���@��`@��/@��/@���@�Ĝ@��j@��@��u@��D@�bN@�(�@���@�=q@�(�@vV@k��@co@Z=q@T9X@L9X@D�j@>@6ȴ@0�`@+33@&V@!�7@O�@x�@��@�9@$�@"�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�'B�'B�'B�'B�-B�-B�-B�-B�'B�'B�'B�'B�'B�'B�'B�'B�'B�'B�'B�'B�!B�!B�!B�!B�'B�'B�'B�'B�'B�'B�'B�'B�'B�'B�-B�-B�-B�-B�-B�-B�-B�-B�-B�-B�-B�-B�-B�-B�-B�3B�9B�RB�RBÖB�HBB�B�B �B%�B'�B&�B)�B(�B'�B%�B �B�B�BoBPB+B��B�B�yB�BɺB�RB�LB�^B�^B�LB�FB�}B�B��B��B�uB�1B�Bz�Br�Bm�BZBH�B=qB-B"�BB�sB�BB�B��BB�dB�B�\BjBL�B6FB �BB
��B
ǮB
�^B
��B
�oB
�B
r�B
R�B
!�B
VB
+B	��B	��B	�B	�mB	�BB	��B	ĜB	�qB	�XB	�?B	��B	��B	�hB	�1B	�B	y�B	p�B	jB	e`B	\)B	T�B	S�B	Q�B	M�B	F�B	B�B	=qB	,B	 �B	�B	{B		7B��B��B��B�B�mB�HB�/B�)B�B�
B��B��BŢB�}B�dB�FB�-B�B�B��B��B��B��B�{B�\B�DB�1B�%B�B�B�B� B~�B|�B|�B{�B{�Bz�Bx�Bw�Bt�Br�Bq�Bp�Bp�Bo�Bo�Bn�Bn�Bn�Bm�Bm�Bl�Bl�Bk�BjBiyBiyBiyBiyBhsBn�Bn�Bp�Bo�Bp�Bo�Bn�Bm�Bk�BjBhsBcTBcTBe`BjBk�Bk�Bk�Bl�Bl�BjBl�Bm�Bk�Bn�Bo�Bp�Br�Bu�Bw�Bx�Bx�By�By�Bz�B|�B}�Bz�B� B�B}�B� B�B~�B|�B{�B� B�B�B�B~�B�B�B�+B�VB�bB�{B��B��B��B��B��B�B�B�B�B�B�B�!B�!B�FB�qBBɺB��B��B��B�
B�B�B�B�#B�)B�/B�5B�HB�NB�TB�TB�ZB�`B�mB�yB�yB�B�B��B��B	B	B	
=B	VB	{B	�B	�B	�B	�B	�B	"�B	&�B	(�B	+B	,B	0!B	2-B	2-B	33B	5?B	:^B	>wB	?}B	@�B	@�B	E�B	J�B	M�B	Q�B	VB	YB	\)B	^5B	_;B	bNB	e`B	ffB	gmB	hsB	iyB	k�B	l�B	n�B	q�B	r�B	u�B	w�B	x�B	y�B	{�B	~�B	� B	�B	�B	�B	�%B	�=B	�JB	�PB	�PB	�VB	�bB	�oB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�'B	�-B	�-B	�3B	�9B	�FB	�LB	�dB	�jB	�qB	�}B	��B	B	B	��B	B	ĜB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�)B	�)B	�/B	�5B	�5B	�;B	�BB	�NB	�NB	�TB	�ZB	�`B	�`B	�fB	�mB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
+B
\B
�B
�B
&�B
.B
49B
<jB
D�B
G�B
O�B
T�B
[#B
`BB
cTB
hsB
k�B
p�B
s�B
w�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�5B�3B�3B�5B�9B�;B�9B�;B�3B�3B�5B�0B�3B�3B�3B�3B�0B�3B�5B�5B�0B�,B�,B�0B�2B�2B�2B�2B�2B�4B�2B�2B�2B�2B�9B�;B�;B�9B�9B�;B�;B�9B�;B�9B�9B�9B�9B�;B�9B�=B�HB�_B�aBÞB�SB/B�B�B �B%�B'�B&�B*
B)B'�B%�B �B�B�B}B]B8B��B�B�B�B��B�aB�YB�nB�kB�YB�VB��B�)B�B��B��B�?B�Bz�Br�Bm�BZ,BH�B=|B-B"�B*B�~B�PB�B��BB�oB�B�jBj�BL�B6OB �B B
��B
ǿB
�mB
��B
�~B
�#B
r�B
SB
!�B
lB
BB	�B	��B	��B	�B	�WB	�B	ĴB	��B	�pB	�UB	�B	��B	��B	�KB	�B	y�B	p�B	j�B	e{B	\EB	UB	TB	RB	M�B	F�B	B�B	=�B	,%B	 �B	�B	�B		XB�B�B��B��B�B�fB�OB�LB�?B�*B�B��B��B��B��B�iB�RB�?B�&B�B��B��B��B��B��B�iB�VB�HB�CB�7B�,B�(B!B}B}B|B|B{	Bx�Bw�Bt�Br�Bq�Bp�Bp�Bo�Bo�Bn�Bn�Bn�Bm�Bm�Bl�Bl�Bk�Bj�Bi�Bi�Bi�Bi�Bh�Bn�Bn�Bp�Bo�Bp�Bo�Bn�Bm�Bk�Bj�Bh�BczBczBe�Bj�Bk�Bk�Bk�Bl�Bl�Bj�Bl�Bm�Bk�Bn�Bo�Bp�Br�Bu�Bw�Bx�Bx�BzBzB{B}B~B{	B�%B�+B~B�&B�-B!B}B|B�&B�8B�8B�3BB�*B�>B�PB�{B��B��B��B��B��B��B��B�=B�;B�%B�6B�=B�=B�CB�GB�iB��B³B��B��B�B�B�.B�6B�8B�?B�EB�KB�QB�VB�iB�qB�uB�uB�|B�B�B�B�B�B�B��B�	B	&B	1B	
^B	uB	�B	�B	�B	�B	�B	�B	"�B	'B	)B	+B	,%B	0>B	2JB	2JB	3RB	5_B	:}B	>�B	?�B	@�B	@�B	E�B	J�B	M�B	R	B	V!B	Y4B	\EB	^QB	_WB	bkB	e{B	f�B	g�B	h�B	i�B	k�B	l�B	n�B	q�B	r�B	u�B	w�B	x�B	y�B	| B	B	�B	�"B	�(B	�5B	�AB	�XB	�eB	�lB	�lB	�qB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�-B	�5B	�>B	�AB	�GB	�FB	�LB	�SB	�`B	�eB	��B	��B	��B	��B	��B	ªB	¨B	��B	©B	ĸB	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�	B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�*B	�-B	�1B	�5B	�CB	�?B	�HB	�NB	�NB	�RB	�\B	�eB	�fB	�lB	�qB	�vB	�wB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B
 B
B
4B
EB
sB
�B
�B
&�B
.)B
4OB
<�B
D�B
G�B
O�B
UB
[8B
`VB
cjB
h�B
k�B
p�B
s�B
w�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.13 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214292016053112142920160531121429  AO  ARCAADJP                                                                    20140721230522    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230522  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230522  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121429  IP                  G�O�G�O�G�O�                