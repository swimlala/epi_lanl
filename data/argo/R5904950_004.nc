CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:04:59Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        l  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  L    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  Sl   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  mh   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �$   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �4   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �8   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �<   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181005190459  20181005190459  5904950 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6429                            2B  A   APEX                            7464                            062512                          846 @מc��1   @מd��?�@4,�C���c���l�D1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   B   B   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A���A�  A�  A���A�  A�33B  BffB  B   B(  B0  B7��B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(�C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX�CZ  C[�fC^  C`  Cb  Cd  Cf�Ch  Cj  Cl  Cm�fCp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	�fD
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fDfD�fD  D� D  D� D  D� D  D� D   D � D!  D!�fD"fD"� D#  D#� D$  D$�fD%  D%� D&  D&� D'  D'� D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:y�D;  D;� D<  D<� D=  D=� D>  D>�fD?fD?� D@  D@� DAfDA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DOfDO� DP  DP�fDQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^fD^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dqy�Dq��Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dwy�Dy�=D�:=D�_
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @1G�@w�@��
@��
A�A=�A]�A}�A���A���A�A���A���A�A���A�(�Bz�B�GBz�Bz�B'z�B/z�B7{B?z�BGz�BOz�BWz�B_z�Bgz�Boz�Bwz�Bz�B��qB��qB��qB��qB��>B��>B��qB��qB��qB��qB��qB��qB��qB��qB��qB��qBýqBǽqB˽qBϽqBӽqB׽qB۽qB߽qB��B�qB�qB�qB�qB��qB��qB��qC޸C޸C޸C޸C	޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C!޸C#޸C%޸C'�RC)޸C+޸C-޸C/޸C1޸C3޸C5޸C7޸C9޸C;޸C=޸C?޸CA޸CC޸CE޸CG޸CI޸CK޸CM޸CO޸CQ޸CS޸CU޸CW�RCY޸C[�C]޸C_޸Ca޸Cc޸Ce�RCg޸Ci޸Ck޸Cm�Co޸Cq޸Cs޸Cu޸Cw޸Cy޸C{޸C}޸C޸C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��C��\C��\C��\C��\C��\C��\C��\C��)C��\C��\C��\C��C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��)C��\C��\D w�D ��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D	~D	��D
w�D
��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��DqHD��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D~D�D~D��Dw�D��Dw�D��Dw�D��Dw�D��D w�D ��D!~D!�D"w�D"��D#w�D#��D$~D$��D%w�D%��D&w�D&��D'w�D3w�D3��D4w�D4��D5w�D5��D6w�D6��D7w�D7��D8w�D8��D9w�D9��D:qHD:��D;w�D;��D<w�D<��D=w�D=��D>~D>�D?w�D?��D@w�D@�DAw�DA��DBw�DB��DCw�DC��DDw�DD��DEw�DE��DFw�DF��DGw�DG��DHw�DH��DIw�DI��DJw�DJ��DKw�DK��DLw�DL��DMw�DM��DNw�DN�DOw�DO��DP~DP��DQw�DQ��DRw�DR��DSw�DS��DTw�DT��DUw�DU��DVw�DV��DWw�DW��DXw�DX��DYw�DY��DZw�DZ��D[w�D[��D\w�D\��D]w�D]�D^w�D^��D_w�D_��D`w�D`��Daw�Da��Dbw�Db��Dcw�Dc��Ddw�Dd��Dew�De��Dfw�Df��Dgw�Dg��Dhw�Dh��Diw�Di��Djw�Dj��Dkw�Dk��Dlw�Dl��Dmw�Dm��Dnw�Dn��Dow�Do��Dpw�Dp��DqqHDq�HDrw�Dr��Dsw�Ds��Dtw�Dt��Duw�Du��Dvw�Dv��DwqHDy��D�6D�Z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��A��A��A��A��A��A�"�A�&�A�(�A�(�A�(�A��A�$�A�&�A�&�A�"�A� �A� �A�"�A�(�A�-A�/A�33A�5?A�5?A�/A�+A�x�A�|�A���A�ffA��TA�A¥�A�A�~�A�9XA���A��+A��FA�bA�bNA���A�^5A���A��A���A�A��jA���A��hA�;dA�ZA���A�K�A���A��A��HA��A���A���A��-A�-A�A��A��-A�-A��A��;A��FA�x�A�jA�K�A���A��DA�"�A�|�A�K�A�n�A���A��RA��/A���A��A���A�33A�l�A�A�r�A��A�|�A�ƨA��A��
A�
=A��A��yA��A���A�"�A�p�A~��A|M�Ay�
Ax�AuXAqt�AmXAk|�Aj�\Ah��Af��Ae��Ad�Ac�#AcC�A`E�A]�
A[�
AZ��AU�AT�uASp�AQƨAQ�AQ��AQ�PAQVAP5?AOƨAO��AO�FAO�hANZAKAGAFz�AD�`AB1A;�A9
=A7�wA6v�A5S�A4��A3S�A0��A/�A-��A-33A-A,ȴA,�!A,�A,�jA,ȴA,�jA-�A-\)A-O�A-|�A-l�A-l�A-A,JA*�A(v�A%�A%�#A%�^A%�-A$��A!?}A ��A �A�TA��A�#A�AoA`BAbA �A�yAjA�AA��A�hA+Ar�A�FA�AA�AĜAA�A��An�AC�A�jAbNA �`@��!@�X@�Q�@���@�z�@�\)@�@�@��@�K�@�!@�X@�Z@�K�@�+@�A�@畁@�33@�+@��@�X@�7L@��H@�7L@��
@ׅ@��@�`B@�dZ@Ұ!@�A�@�+@Гu@�;d@�^5@͡�@���@�bN@��@�V@���@��@��m@��@Ə\@��@�X@�bN@�+@�{@�hs@���@�  @���@�33@�"�@�ȴ@��@��@�@��7@���@���@���@�z�@��u@�j@��@��@��F@�S�@��@��+@�&�@���@���@��@���@�ȴ@�7L@�1@�S�@�o@��@��\@��#@���@�O�@��@�z�@�9X@��F@�l�@�+@���@���@��R@�v�@�@��^@��-@�@���@�@�V@�^5@�E�@��@���@��@��D@���@��@��-@��h@���@�Q�@�Q�@�I�@�Z@�bN@�Z@�Z@�bN@�Z@�Z@��@���@�\)@�ff@�J@��P@���@���@���@�E�@���@���@��!@��H@��^@�p�@�O�@�&�@�  @�t�@�K�@�;d@�+@�S�@�|�@�{@�O�@�p�@��h@��h@��-@�?}@���@�"�@��R@�V@�=q@�E�@�V@�n�@���@���@�V@�{@���@���@�x�@���@�{@���@�p�@�%@���@��u@�Z@�(�@��w@�dZ@��R@��@�@�{@�=q@��7@�X@���@�  @��w@��w@��w@��F@��F@��@��@���@�l�@��@��@�~�@��-@�p�@�G�@��@�Z@�A�@��@���@��F@���@�A�@�  @��;@���@�;d@�ȴ@�^5@���@��R@��y@���@��\@�v�@�V@�5?@�@�@�/@��@���@��/@��@���@�j@�Z@� �@��@�dZ@�+@��@�^5@�M�@�M�@�=q@�@���@��@���@���@��^@���@���@��@�X@��@�Ĝ@���@��D@�I�@�b@���@��F@��P@��"@r� @ae,1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A��A��A��A��A��A��A�"�A�&�A�(�A�(�A�(�A��A�$�A�&�A�&�A�"�A� �A� �A�"�A�(�A�-A�/A�33A�5?A�5?A�/A�+A�x�A�|�A���A�ffA��TA�A¥�A�A�~�A�9XA���A��+A��FA�bA�bNA���A�^5A���A��A���A�A��jA���A��hA�;dA�ZA���A�K�A���A��A��HA��A���A���A��-A�-A�A��A��-A�-A��A��;A��FA�x�A�jA�K�A���A��DA�"�A�|�A�K�A�n�A���A��RA��/A���A��A���A�33A�l�A�A�r�A��A�|�A�ƨA��A��
A�
=A��A��yA��A���A�"�A�p�A~��A|M�Ay�
Ax�AuXAqt�AmXAk|�Aj�\Ah��Af��Ae��Ad�Ac�#AcC�A`E�A]�
A[�
AZ��AU�AT�uASp�AQƨAQ�AQ��AQ�PAQVAP5?AOƨAO��AO�FAO�hANZAKAGAFz�AD�`AB1A;�A9
=A7�wA6v�A5S�A4��A3S�A0��A/�A-��A-33A-A,ȴA,�!A,�A,�jA,ȴA,�jA-�A-\)A-O�A-|�A-l�A-l�A-A,JA*�A(v�A%�A%�#A%�^A%�-A$��A!?}A ��A �A�TA��A�#A�AoA`BAbA �A�yAjA�AA��A�hA+Ar�A�FA�AA�AĜAA�A��An�AC�A�jAbNA �`@��!@�X@�Q�@���@�z�@�\)@�@�@��@�K�@�!@�X@�Z@�K�@�+@�A�@畁@�33@�+@��@�X@�7L@��H@�7L@��
@ׅ@��@�`B@�dZ@Ұ!@�A�@�+@Гu@�;d@�^5@͡�@���@�bN@��@�V@���@��@��m@��@Ə\@��@�X@�bN@�+@�{@�hs@���@�  @���@�33@�"�@�ȴ@��@��@�@��7@���@���@���@�z�@��u@�j@��@��@��F@�S�@��@��+@�&�@���@���@��@���@�ȴ@�7L@�1@�S�@�o@��@��\@��#@���@�O�@��@�z�@�9X@��F@�l�@�+@���@���@��R@�v�@�@��^@��-@�@���@�@�V@�^5@�E�@��@���@��@��D@���@��@��-@��h@���@�Q�@�Q�@�I�@�Z@�bN@�Z@�Z@�bN@�Z@�Z@��@���@�\)@�ff@�J@��P@���@���@���@�E�@���@���@��!@��H@��^@�p�@�O�@�&�@�  @�t�@�K�@�;d@�+@�S�@�|�@�{@�O�@�p�@��h@��h@��-@�?}@���@�"�@��R@�V@�=q@�E�@�V@�n�@���@���@�V@�{@���@���@�x�@���@�{@���@�p�@�%@���@��u@�Z@�(�@��w@�dZ@��R@��@�@�{@�=q@��7@�X@���@�  @��w@��w@��w@��F@��F@��@��@���@�l�@��@��@�~�@��-@�p�@�G�@��@�Z@�A�@��@���@��F@���@�A�@�  @��;@���@�;d@�ȴ@�^5@���@��R@��y@���@��\@�v�@�V@�5?@�@�@�/@��@���@��/@��@���@�j@�Z@� �@��@�dZ@�+@��@�^5@�M�@�M�@�=q@�@���@��@���@���@��^@���@���@��@�X@��@�Ĝ@���@��D@�I�@�b@���@��F@��P@��"@r� @ae,1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B2-B2-B2-B2-B2-B2-B2-B2-B33B33B33B33B2-B2-B2-B2-B2-B2-B2-B2-B2-B2-B2-B2-B2-B2-B1'B2-B�%B��B��B�B�HB�fB�BBVB�B'�B<jBJ�BM�BO�BXBZB_;BgmBffBy�B�7B�bB��B��B�!B�'B�'B�B��B��B��B�hB�1B�Bs�BdZBaHB]/BYBT�BS�BQ�BP�BO�BN�BL�BH�B:^B"�B�B�fB�5B��B��B�1Bz�BgmB;dB)�B�B
�B
��B
ɺB
��B
��B
�B
��B
�hB
?}B
�B
�B
�B
\B
	7B
  B	�B	�B	��B	��B	q�B	aHB	[#B	W
B	O�B	N�B	F�B	B�B	C�B	2-B	�B	PB	%B�yB�NB�;B�NB�ZB�ZB�ZB�ZB�`B�mB�yB�yB�sB�HB��BǮBĜBƨB�9B�uB�7B�%B�B}�B|�B{�By�Bv�B�B�=B�JB�bB�uB��B��B��B��B��B��B��B�BɺB��B��B��BȴB�9B��B��B��B��B��B�hB�oB�oB�oB�7B�B��B�}B��B��BŢB�FB��B�JB�BȴB��B��B��B��B��B��BɺBĜB�}B�qB�dB�dB�^B�wBĜBǮBǮBƨB��B��B��B��B��B��B��B��B�
B�
B�B�B�)B�)B�#B�B�B��B��B��B��B��B��B��B��B�B�B�B�B�B�mB�fB�`B�`B�fB�mB�B�yB�sB�B�B�B�yB�mB�`B�B��B��B	B	%B	+B	DB	JB	PB	{B	�B	 �B	$�B	'�B	,B	1'B	33B	9XB	:^B	:^B	:^B	:^B	:^B	;dB	<jB	?}B	?}B	?}B	@�B	B�B	B�B	F�B	J�B	L�B	M�B	M�B	N�B	Q�B	Q�B	S�B	VB	XB	ZB	\)B	^5B	_;B	`BB	aHB	aHB	cTB	e`B	gmB	jB	l�B	m�B	p�B	x�B	� B	�B	�B	�1B	�7B	�1B	�+B	�%B	�%B	�+B	�VB	�hB	�hB	�hB	�oB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	J�B	�LB	�LB	�dB	�wB	��B	ƨB	ĜB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	�B	�B	�
B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�5B	�5B	�/B	�/B	�/B	�/B	�HB	�ZB	�ZB	�TB	�TB	�TB	�TB	�TB	�NB	�HB	�HB	�NB	�NB	�ZB	�mB	�yB	�B	�B	�yB	�sB	�sB	�sB	�sB	�sB	�sB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
  B
B
B
B
B
%B
%B
+B
+B
+B
1B
1B
1B
1B
	7B
	7B
	7B
	7B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B
DB
JB
JB
JB
PB
PB
VB
VB
\B
�B
1B
&L2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222224422222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B2-B2-B2-B2-B2-B2-B2-B2-B33B33B33B33B2-B2-B2-B2-B2-B2-B2-B2-B2-B2-B2-B2-B2-B2-B1'B2-B�%B��B��B�B�HB�fB�BBVB�B'�B<jBJ�BM�BO�BXBZB_;BgmBffBy�B�7B�bB��B��B�!B�'B�'B�B��B��B��B�hB�1B�Bs�BdZBaHB]/BYBT�BS�BQ�BP�BO�BN�BL�BH�B:^B"�B�B�fB�5B��B��B�1Bz�BgmB;dB)�B�B
�B
��B
ɺB
��B
��B
�B
��B
�hB
?}B
�B
�B
�B
\B
	7B
  B	�B	�B	��B	��B	q�B	aHB	[#B	W
B	O�B	N�B	F�B	B�B	C�B	2-B	�B	PB	%B�yB�NB�;B�NB�ZB�ZB�ZB�ZB�`B�mB�yB�yB�sB�HB��BǮBĜBƨB�9B�uB�7B�%B�B}�B|�B{�By�Bv�B�B�=B�JB�bB�uB��B��B��B��B��B��B��B�BɺB��B��B��BȴB�9B��B��B��B��B��B�hB�oB�oB�oB�7B�B��B�}B��B��BŢB�FB��B�JB�BȴB��B��B��B��B��B��BɺBĜB�}B�qB�dB�dB�^B�wBĜBǮBǮBƨB��B��B��B��B��B��B��B��B�
B�
B�B�B�)B�)B�#B�B�B��B��B��B��B��B��B��B��B�B�B�B�B�B�mB�fB�`B�`B�fB�mB�B�yB�sB�B�B�B�yB�mB�`B�B��B��B	B	%B	+B	DB	JB	PB	{B	�B	 �B	$�B	'�B	,B	1'B	33B	9XB	:^B	:^B	:^B	:^B	:^B	;dB	<jB	?}B	?}B	?}B	@�B	B�B	B�B	F�B	J�B	L�B	M�B	M�B	N�B	Q�B	Q�B	S�B	VB	XB	ZB	\)B	^5B	_;B	`BB	aHB	aHB	cTB	e`B	gmB	jB	l�B	m�B	p�B	x�B	� B	�B	�B	�1B	�7B	�1B	�+B	�%B	�%B	�+B	�VB	�hB	�hB	�hB	�oB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	J�B	�LB	�LB	�dB	�wB	��B	ƨB	ĜB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	�B	�B	�
B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�5B	�5B	�/B	�/B	�/B	�/B	�HB	�ZB	�ZB	�TB	�TB	�TB	�TB	�TB	�NB	�HB	�HB	�NB	�NB	�ZB	�mB	�yB	�B	�B	�yB	�sB	�sB	�sB	�sB	�sB	�sB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
  B
B
B
B
B
%B
%B
+B
+B
+B
1B
1B
1B
1B
	7B
	7B
	7B
	7B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B
DB
JB
JB
JB
PB
PB
VB
VB
\B
�B
1B
&L2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222224422222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.13 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190459                              AO  ARCAADJP                                                                    20181005190459    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190459  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190459  QCF$                G�O�G�O�G�O�C000            