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
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181005190459  20181005190459  5904950 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6429                            2B  A   APEX                            7464                            062512                          846 @םxk�:�1   @םx��fV@4"M����cîz�H1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB0ffB8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�33B�  B�33B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B���C  C  C  C  C
  C  C  C  C�fC  C  C�fC�fC  C  C   C"  C$  C&  C(  C*�C,  C.  C0  C2  C4  C6  C8  C:  C<  C>�C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CW�fCZ  C\  C^�C`  Ca�fCd  Cf  Ch  Cj  Cl�Cn  Cp  Cq�fCt  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2y�D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8y�D9  D9� D:fD:�fD;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DBy�DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DMfDM� DN  DN� DOfDO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DU��DV� DWfDW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  Dl  Dl� Dm  Dm� Dn  Dn� Do  Doy�Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dw�fDy��D�;3D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�=p@��
@��
A�A=�A]�A}�A���A���A���A���A���A���A���A���Bz�Bz�Bz�Bz�B'�GB/�GB7z�B?z�BGz�BOz�BWz�B_z�Bgz�Boz�Bwz�Bz�B��qB��qB��qB��B��B��qB��B��qB��qB��B��B��qB��qB��qB��qB��qBýqBǽqB˽qBϽqBӽqB׽qB۽qB߽qB�qB�qB�qB��B�qB��qB��qB��>C޸C޸C޸C޸C	޸C޸C޸C޸C�C޸C޸C�C�C޸C޸C޸C!޸C#޸C%޸C'޸C)�RC+޸C-޸C/޸C1޸C3޸C5޸C7޸C9޸C;޸C=�RC?޸CA޸CC޸CE޸CG޸CI޸CK޸CM޸CO޸CQ޸CS޸CU޸CW�CY޸C[޸C]�RC_޸Ca�Cc޸Ce޸Cg޸Ci޸Ck�RCm޸Co޸Cq�Cs޸Cu޸Cw޸Cy޸C{޸C}޸C޸C��\C��\C��\C��\C��\C��\C��\C��\C��)C��\C��\C��)C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��C��\C��\C��\C��)C��\C��\C��\C��\C��\C��\C��\C��\C��)C��\C��\C��\C��\C��\C��\C��\C��)C��)C��\C��\C��C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��C��C��\C��)C��)C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\D w�D ��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D	w�D	��D
w�D
��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��DqHD��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D�Dw�D��Dw�D��D w�D ��D!w�D!��D"w�D"��D#w�D#��D$w�D$��D%w�D%��D&w�D&��D'w�D'��D(w�D(��D)w�D)��D*w�D*��D+w�D+��D,w�D,��D-w�D-��D.w�D.��D/w�D/��D0w�D0��D1w�D1��D2qHD2��D3w�D3��D4w�D4��D5w�D5��D6w�D6��D7w�D7��D8qHD8��D9w�D9�D:~D:��D;w�D;��D<w�D<��D=w�D=��D>w�D>��D?w�D?��D@w�D@��DAw�DA��DBqHDB��DCw�DC��DDw�DD��DEw�DE��DFw�DF��DGw�DG��DHw�DH��DIw�DI��DJw�DJ��DKw�DK��DLw�DL�DMw�DM��DNw�DN�DOw�DO��DPw�DP��DQw�DQ��DRw�DR��DSw�DS��DTw�DT��DUw�DU�HDVw�DV�DWw�DW��DXw�DX��DYw�DY��DZw�DZ��D[w�D[��D\w�D\��D]w�D]��D^w�D^��D_w�D_��Dk��Dlw�Dl��Dmw�Dm��Dnw�Dn��DoqHDo��Dpw�Dp��Dqw�Dq��Drw�Dr��Dsw�Ds��Dtw�Dt��Duw�Du��Dvw�Dv��Dww�Dw�Dy�RD�7
D��=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�/A��A�
=A�A��yA��HA��;A�ȴAľwAĶFAĬAĥ�Aĝ�AēuAď\AēuAĕ�AčPA�bNA�C�A�5?A�/A�(�A�$�A�$�A�$�A��A��A��A�oA���A��#A��A��A��
A���A���A���A���A���Aú^Aá�AÛ�AÏ\A�|�A�jA�"�A���A��/A���A°!AhA��A�n�A�hsA�jA�l�A�jA�S�A�9XA�33A�(�A��mA��!A�`BA�C�A�l�A���A�A�A��wA��PA��A��A���A��A��A�E�A���A���A��A�K�A���A���A�ƨA�{A��-A��A���A�A���A� �A�ƨA�VA��A�S�A�ĜA�bA�
=A�+A�%A�;dA���A}��A|1'Azv�AyO�Awl�Au��As��Ar��Aq��Ap��Al1'Ai��Ah�Ag��AeG�Ac�7AbĜAb�!Ab�\Ab9XAb(�Aa&�A^�AYt�AU?}AR�9AQ�hAO�AK�AE��ACXAB��ABĜAB5?A@(�A=�7A:��A7��A5&�A4�A3�A0�A-O�A+dZA)��A'��A$$�A"9XA!�PA �uA�AZA�;AoAI�AjAv�A�!A�uAbAx�A7LA�RA-A%A�!AA��AhsAp�A"�AM�AVA�\A�
AA9XA��A�FAdZA33A
�+A
1A	x�An�A�-A`BAA�DA�A��A��A�A�hAp�@��F@��@���@��@�C�@��@@�
=@�7@��@�I�@�X@�A�@㕁@�G�@��
@�;d@�
=@߮@ߝ�@߾w@�o@�@��@�v�@�Z@�=q@�9X@�n�@��T@Ѻ^@�x�@�X@�?}@��@��@�r�@Ϯ@�/@�  @�|�@���@�E�@�O�@�A�@�1@�33@�@š�@�/@�%@ă@�ƨ@���@�E�@��@��^@�p�@�p�@�p�@�O�@���@���@�K�@���@��@��@�&�@���@��D@��@��\@�E�@��@�x�@���@��@�Z@�(�@�C�@��@���@�/@��9@��@���@��@�(�@�t�@�
=@���@�~�@�M�@�5?@��T@�O�@�%@��D@�(�@��@�t�@�ȴ@�^5@�E�@�`B@��u@��@�Z@���@�l�@�+@��@���@��!@�V@�$�@���@�O�@���@���@���@��u@�r�@�9X@� �@��@�  @��w@�K�@�-@��@���@��h@��h@��@�X@��@��@�bN@�9X@��@�b@���@��m@��w@���@�K�@�33@��@��H@���@�M�@���@�`B@�&�@���@��u@�j@�j@�Z@�A�@��P@�l�@�\)@�;d@�+@�o@���@��y@��H@��R@��+@��@�O�@��j@� �@�l�@�33@��y@�V@��#@�p�@��@�j@��
@���@�dZ@�K�@��@���@�=q@���@��@�Ĝ@��@�Z@�9X@� �@�1@�  @���@��;@���@�\)@�33@�"�@�o@�
=@��@���@�~�@�v�@�v�@�n�@�ff@�^5@�V@��#@�7L@���@��@���@���@�I�@�b@�1@�1@�b@�b@��@��@��@��@�  @���@�l�@�o@���@�~�@�ff@�^5@�^5@�E�@��@�7L@�V@�Ĝ@�Z@�ƨ@���@���@��@�x�@�`B@�G�@�?}@�7L@�&�@�Ĝ@�Z@�9X@���@��w@���@���@���@���@���@���@��P@�l�@�;d@�~�@�^5@�^5@�W�@q�@`<�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111 A�/A��A�
=A�A��yA��HA��;A�ȴAľwAĶFAĬAĥ�Aĝ�AēuAď\AēuAĕ�AčPA�bNA�C�A�5?A�/A�(�A�$�A�$�A�$�A��A��A��A�oA���A��#A��A��A��
A���A���A���A���A���Aú^Aá�AÛ�AÏ\A�|�A�jA�"�A���A��/A���A°!AhA��A�n�A�hsA�jA�l�A�jA�S�A�9XA�33A�(�A��mA��!A�`BA�C�A�l�A���A�A�A��wA��PA��A��A���A��A��A�E�A���A���A��A�K�A���A���A�ƨA�{A��-A��A���A�A���A� �A�ƨA�VA��A�S�A�ĜA�bA�
=A�+A�%A�;dA���A}��A|1'Azv�AyO�Awl�Au��As��Ar��Aq��Ap��Al1'Ai��Ah�Ag��AeG�Ac�7AbĜAb�!Ab�\Ab9XAb(�Aa&�A^�AYt�AU?}AR�9AQ�hAO�AK�AE��ACXAB��ABĜAB5?A@(�A=�7A:��A7��A5&�A4�A3�A0�A-O�A+dZA)��A'��A$$�A"9XA!�PA �uA�AZA�;AoAI�AjAv�A�!A�uAbAx�A7LA�RA-A%A�!AA��AhsAp�A"�AM�AVA�\A�
AA9XA��A�FAdZA33A
�+A
1A	x�An�A�-A`BAA�DA�A��A��A�A�hAp�@��F@��@���@��@�C�@��@@�
=@�7@��@�I�@�X@�A�@㕁@�G�@��
@�;d@�
=@߮@ߝ�@߾w@�o@�@��@�v�@�Z@�=q@�9X@�n�@��T@Ѻ^@�x�@�X@�?}@��@��@�r�@Ϯ@�/@�  @�|�@���@�E�@�O�@�A�@�1@�33@�@š�@�/@�%@ă@�ƨ@���@�E�@��@��^@�p�@�p�@�p�@�O�@���@���@�K�@���@��@��@�&�@���@��D@��@��\@�E�@��@�x�@���@��@�Z@�(�@�C�@��@���@�/@��9@��@���@��@�(�@�t�@�
=@���@�~�@�M�@�5?@��T@�O�@�%@��D@�(�@��@�t�@�ȴ@�^5@�E�@�`B@��u@��@�Z@���@�l�@�+@��@���@��!@�V@�$�@���@�O�@���@���@���@��u@�r�@�9X@� �@��@�  @��w@�K�@�-@��@���@��h@��h@��@�X@��@��@�bN@�9X@��@�b@���@��m@��w@���@�K�@�33@��@��H@���@�M�@���@�`B@�&�@���@��u@�j@�j@�Z@�A�@��P@�l�@�\)@�;d@�+@�o@���@��y@��H@��R@��+@��@�O�@��j@� �@�l�@�33@��y@�V@��#@�p�@��@�j@��
@���@�dZ@�K�@��@���@�=q@���@��@�Ĝ@��@�Z@�9X@� �@�1@�  @���@��;@���@�\)@�33@�"�@�o@�
=@��@���@�~�@�v�@�v�@�n�@�ff@�^5@�V@��#@�7L@���@��@���@���@�I�@�b@�1@�1@�b@�b@��@��@��@��@�  @���@�l�@�o@���@�~�@�ff@�^5@�^5@�E�@��@�7L@�V@�Ĝ@�Z@�ƨ@���@���@��@�x�@�`B@�G�@�?}@�7L@�&�@�Ĝ@�Z@�9X@���@��w@���@���@���@���@���@���@��P@�l�@�;d@�~�@�^5@�^5@�W�@q�@`<�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
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
��BBBB1B	7B
=BVBbB\B\B\BPBVBPBDBDB
=B
=B%BBBBBBBBBB%B%B%B1B
=BVB�B'�B+B,B.B2-BVB_;B`BBbNBbNBdZBgmBgmBk�Bn�Bo�B�B�NB�ZB��BJB�BI�Bt�B��B�3BB��B}�By�BXBI�B=qBF�B&�B\BJB  BB�)BǮB�dB�7Be`BG�B+BbB
�B
��B
��B
�7B
�B
x�B
dZB
O�B
49B
'�B
�B
uB
%B	��B	�B	�`B	�5B	��B	�qB	�B	��B	��B	�1B	|�B	{�B	�B	�B	�B	�B	x�B	ffB	G�B	,B	�B	oB	%B�B��BȴBƨBĜB�}B�?B�B��B�uB�JB�=B�+B�B� Bx�Bs�Bm�BjBhsBffBffBffBl�Bl�Bo�Bo�Bn�BjBhsBhsBjBjBjBl�Bn�Bs�Bt�Bv�Bx�B|�B� B� B�B�B�%B�DB�\B�oB�oB�oB�oB�oB�oB�oB�oB�hB�hB�bB�bB�bB�\B�PB�JB�JB�DB�=B�DB�JB�PB�PB�\B�VB�hB�uB��B��B��B��B��B��B��B��B��B��B�B�'B�9B�-B�B�?B�XB�XB�wB��BŢBǮBǮBȴBȴBȴBȴBȴBȴBȴB��B��B��B�
B�B�)B�BB�BB�NB�mB�yB�B�B�B�B�B��B��B��B��B��B��B��B��B	B	B	B	+B	1B	VB	�B	�B	�B	�B	�B	�B	$�B	(�B	)�B	+B	.B	0!B	33B	5?B	8RB	9XB	<jB	>wB	>wB	A�B	F�B	J�B	L�B	O�B	Q�B	R�B	T�B	W
B	[#B	^5B	`BB	bNB	bNB	aHB	aHB	bNB	cTB	e`B	ffB	gmB	iyB	l�B	n�B	p�B	q�B	q�B	r�B	r�B	u�B	y�B	~�B	�B	�B	�B	�B	�1B	�=B	�DB	�JB	�PB	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�-B	�9B	�FB	�RB	�XB	�XB	�XB	�^B	�wB	�}B	�}B	��B	��B	��B	��B	��B	��B	B	B	ĜB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�5B	�BB	�HB	�ZB	�`B	�fB	�mB	�mB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	G�B
DB
DB
DB
JB
PB
VB
VB
VB
\B
bB
bB
hB
oB
oB
oB
uB
uB
uB
uB
uB
uB
oB
uB
uB
uB
�B
B
'R2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222442222222222222222222222222222 B
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
��BBBB1B	7B
=BVBbB\B\B\BPBVBPBDBDB
=B
=B%BBBBBBBBBB%B%B%B1B
=BVB�B'�B+B,B.B2-BVB_;B`BBbNBbNBdZBgmBgmBk�Bn�Bo�B�B�NB�ZB��BJB�BI�Bt�B��B�3BB��B}�By�BXBI�B=qBF�B&�B\BJB  BB�)BǮB�dB�7Be`BG�B+BbB
�B
��B
��B
�7B
�B
x�B
dZB
O�B
49B
'�B
�B
uB
%B	��B	�B	�`B	�5B	��B	�qB	�B	��B	��B	�1B	|�B	{�B	�B	�B	�B	�B	x�B	ffB	G�B	,B	�B	oB	%B�B��BȴBƨBĜB�}B�?B�B��B�uB�JB�=B�+B�B� Bx�Bs�Bm�BjBhsBffBffBffBl�Bl�Bo�Bo�Bn�BjBhsBhsBjBjBjBl�Bn�Bs�Bt�Bv�Bx�B|�B� B� B�B�B�%B�DB�\B�oB�oB�oB�oB�oB�oB�oB�oB�hB�hB�bB�bB�bB�\B�PB�JB�JB�DB�=B�DB�JB�PB�PB�\B�VB�hB�uB��B��B��B��B��B��B��B��B��B��B�B�'B�9B�-B�B�?B�XB�XB�wB��BŢBǮBǮBȴBȴBȴBȴBȴBȴBȴB��B��B��B�
B�B�)B�BB�BB�NB�mB�yB�B�B�B�B�B��B��B��B��B��B��B��B��B	B	B	B	+B	1B	VB	�B	�B	�B	�B	�B	�B	$�B	(�B	)�B	+B	.B	0!B	33B	5?B	8RB	9XB	<jB	>wB	>wB	A�B	F�B	J�B	L�B	O�B	Q�B	R�B	T�B	W
B	[#B	^5B	`BB	bNB	bNB	aHB	aHB	bNB	cTB	e`B	ffB	gmB	iyB	l�B	n�B	p�B	q�B	q�B	r�B	r�B	u�B	y�B	~�B	�B	�B	�B	�B	�1B	�=B	�DB	�JB	�PB	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�-B	�9B	�FB	�RB	�XB	�XB	�XB	�^B	�wB	�}B	�}B	��B	��B	��B	��B	��B	��B	B	B	ĜB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�5B	�BB	�HB	�ZB	�`B	�fB	�mB	�mB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	G�B
DB
DB
DB
JB
PB
VB
VB
VB
\B
bB
bB
hB
oB
oB
oB
uB
uB
uB
uB
uB
uB
oB
uB
uB
uB
�B
B
'R2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222442222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.13 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190459                              AO  ARCAADJP                                                                    20181005190459    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190459  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190459  QCF$                G�O�G�O�G�O�C000            