CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-08-24T00:37:07Z creation;2018-08-24T00:38:08Z conversion to V3.1;2019-12-19T07:34:24Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ID   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pl   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tX   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �d   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ޜ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �(   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �l   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �|   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180824003707  20200115131518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              A   JA  I2_0576_273                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�{�8 1   @�{�www�@9�TɅ�o�d`s����1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DNy�DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS�fDT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do�fDp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ Dļ�D�  D�@ D�|�D�� D�  D�@ Dƀ D�� D���D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�<�D�|�D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�P D�c3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @G�@w�@��
@��
A�A=�A]�A}�A���A���A���A���A���A���A���A���Bz�Bz�Bz�Bz�B'z�B/z�B7z�B?z�BGz�BOz�BWz�B_z�Bgz�Boz�Bwz�Bz�B��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qBýqB��B˽qBϽqBӽqB׽qB۽qB߽qB��B��B�qB�qB�qB��qB��qB��qC޸C޸C޸C޸C	޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C!޸C#޸C%޸C'޸C)޸C+޸C-޸C/޸C1޸C3޸C5޸C7޸C9޸C;޸C=޸C?޸CA޸CC޸CE޸CG޸CI޸CK޸CM޸CO޸CQ޸CS޸CU޸CW޸CY޸C[޸C]޸C_޸Ca޸Cc޸Ce޸Cg޸Ci޸Ck޸Cm޸Co޸Cq޸Cs޸Cu޸Cw޸Cy޸C{޸C}޸C޸C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��)C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\D w�D ��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D	w�D	��D
w�D
��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D w�D ��D!w�D!��D"w�D"��D#w�D#��D$w�D$��D%w�D%��D&w�D&��D'w�D'��D(w�D(��D)w�D)��D*w�D*��D+w�D+��D,w�D,��D-w�D-��D.w�D.��D/w�D/��D0w�D0��D1w�D1��D2w�D2��D3w�D3��D4w�D4��D5w�D5��D6w�D6��D7w�D7��D8w�D8��D9w�D9��D:w�D:��D;w�D;��D<w�D<��D=w�D=��D>w�D>��D?w�D?��D@w�D@��DAw�DA��DBw�DB��DCw�DC��DDw�DD��DEw�DE��DFw�DF��DGw�DG��DHw�DH��DIw�DI��DJw�DJ��DKw�DK��DLw�DL��DMw�DM��DNqHDN��DOw�DO��DPw�DP��DQw�DQ��DRw�DR��DS~DS��DTw�DT��DUw�DU��DVw�DV��DWw�DW��DXw�DX��DYw�DY��DZw�DZ��D[w�D[��D\w�D\��D]w�D]��D^w�D^��D_w�D_��D`w�D`��Daw�Da��Dbw�Db��Dcw�Dc��Ddw�Dd��Dew�De��Dfw�Df��Dgw�Dg��Dhw�Dh��Diw�Di��Djw�Dj��Dkw�Dk��Dlw�Dl��Dmw�Dm��Dnw�Dn��Do~Do��Dpw�Dp��Dqw�Dq��Drw�Dr��Dsw�Ds��Dtw�Dt��Duw�Du��Dvw�Dv��Dww�Dw��Dxw�Dx��Dyw�Dy��Dzw�Dz��D{w�D{��D|w�D|��D}w�D}��D~w�D~��Dw�D��D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D��
D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D»�D���D�;�D�{�Dû�D���D�;�D�{�Dĸ�D���D�;�D�x�DŻ�D���D�;�D�{�Dƻ�D���D�;�D�{�Dǻ�D���D�;�D�{�DȻ�D���D�;�D�{�Dɻ�D���D�;�D�{�Dʻ�D���D�;�D�{�D˻�D���D�;�D�{�D̻�D���D�;�D�{�Dͻ�D���D�;�D�{�Dλ�D���D�;�D�{�Dϻ�D���D�;�D�{�Dл�D���D�;�D�{�Dѻ�D���D�;�D�{�Dһ�D���D�;�D�{�Dӻ�D���D�;�D�{�DԻ�D���D�;�D�{�Dջ�D���D�;�D�{�Dֻ�D���D�8�D�x�D׻�D���D�;�D�{�Dػ�D���D�;�D�{�Dٻ�D���D�;�D�{�Dڻ�D���D�;�D�{�Dۻ�D���D�;�D�{�Dܻ�D���D�;�D�{�Dݻ�D���D�;�D�{�D޻�D���D�;�D�{�D߻�D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D縤D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D���D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D��
D�K�D�_
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���Aֺ^AָRAִ9Aֲ-A֧�A֧�A֡�A�~�A�{A��A�I�Aҝ�A��A���A�oA�?}A�+A���A��
A�1'A���Aė�AuA��uA�  A�9XA���A�G�A�Q�A�VA��A�hsA�oA�\)A��A��wA���A��FA���A�E�A�ȴA��9A�I�A�n�A�$�A���A�{A��-A��TA���A�1'A��uA�dZA��7A���A�A�I�A��^A���A��RA���A�/A�ƨA���A��A���A��uA���A�ffA���A��\A���A�K�A���A���A��/A�ĜA���A�=qA��A���A�l�A�E�A��A�-A���A�&�A��A�~�AA}oA{|�Az��Az{Ay�7AyhsAx��Ax�Aw�hAw/Aw�Av�Av=qAt��Ar{Ap��An�!Am�Al �Ak�hAj�HAh��Ag"�AfJAd�Ac�Ac33AahsA_�-A_%A^  A\v�A[VAYVAXbAW�AW\)AV�AV��AVVAU|�AS�ARJAQ"�APANn�AN  AM��AL�!AK�PAJr�AH�AH  AGl�AF�/AFM�AE�AE�AC��AA�FA@��A?��A>�yA=�A=\)A;�A;&�A:VA9��A8�A7ƨA7\)A6�A5|�A4�uA4�A3��A2��A1��A0�`A0$�A.��A-S�A,�DA,  A+ƨA+�A++A*��A*M�A)K�A(v�A';dA%�PA$�+A#�-A#�A"�!A"=qA!�;A!�-A!O�A ^5A\)A5?A�^A�FA�-A��A;dAv�A�A{At�A�HA��A+AQ�AhsA�9A��A�AVA�`A~�AffA�A��A;dA�A�mA
�A
^5A	�A	l�Ar�A�A�yAJA�^AK�A�A33A��@���@���@���@�9X@��@�z�@�n�@�Z@�\)@��@��@��@�z�@�@��@�@�ƨ@�^5@�O�@��@�I�@�33@噚@���@�D@㕁@�-@�(�@�l�@���@�-@�&�@�5?@�V@��H@ա�@�?}@�|�@љ�@��@��;@���@�7L@ˍP@ɑh@ȃ@�|�@��@�M�@�X@��`@���@�5?@�G�@�V@�  @�l�@���@��j@�(�@�ƨ@��y@��+@�@�?}@�9X@��R@�X@���@���@�+@���@�n�@��@��-@�&�@��j@��u@��@���@�ȴ@�J@��^@�7L@���@��
@�S�@���@�~�@�`B@�bN@���@���@�|�@�O�@�j@�1@��@���@�5?@�X@��@�z�@��w@���@�J@��@���@���@���@�1@��@�=q@�O�@��@��@�Z@��@��
@��@��P@�t�@�\)@���@�ȴ@���@��+@�n�@�-@���@�&�@�Q�@�A�@�  @��@�S�@��R@�{@�{@�E�@�-@�?}@��u@��@�33@�"�@�"�@�"�@�"�@�"�@��@�~�@���@���@�7L@���@��/@���@���@�j@�1'@��@�ƨ@�t�@��@�V@���@�p�@�hs@�hs@��@���@�A�@�  @���@��@�|�@�dZ@���@���@�ff@�-@��#@��-@��7@��@�Ĝ@�Z@��;@��;@��;@��m@��@�"�@�n�@�-@��T@�@��-@��-@���@���@�?}@�j@��@�1@�P@\)@
=@~��@}�h@|�@|I�@{��@{��@z��@z=q@y&�@xb@w��@w��@w
=@v�+@vff@vV@vE�@vV@vV@v@up�@t�@t��@s�F@s33@so@r��@r�\@r^5@r-@q��@q��@q�@q��@q��@q&�@pĜ@p�u@p  @o�@o
=@n�@nV@n$�@m�@m��@m�@mp�@m`B@mO�@mO�@m?}@mV@l��@l�j@lj@l(�@k�
@k�F@k��@kt�@kC�@ko@j��@j�!@j~�@j~�@jn�@j^5@j^5@j�@i�#@i��@ihs@i&�@h1'@g�w@g��@g��@g|�@g�@f�@fv�@f5?@f5?@e�@e?}@d�/@d��@dI�@c��@c�m@c�m@c��@cdZ@cS�@co@b^5@b=q@b-@a��@a��@a��@ahs@`�9@`r�@`A�@_�w@_�P@_\)@_�@^�@^ff@]�-@\�@\�@\�@\z�@[��@[ƨ@[��@[dZ@[dZ@[dZ@[o@Z��@ZM�@ZJ@Y��@Yhs@Y7L@X��@XĜ@Xr�@X �@W�P@W
=@V��@VV@V$�@U��@UO�@T�j@TZ@T�@S��@S�m@S�
@Sƨ@S�@SS�@S33@R�H@R�\@Rn�@R=q@Q�#@Q&�@P�`@P��@P��@P�9@PQ�@O�;@O��@O\)@N�@N{@M�T@M��@M?}@M/@L��@L��@L�D@LZ@K�m@Kt�@J��@J~�@JM�@JJ@I��@Ix�@IX@I&�@H�`@HA�@G�w@G��@Gl�@G;d@G
=@F�@F�+@FV@F5?@Ep�@D��@D��@D�@CC�@B�@B�H@B�!@B�\@B~�@BM�@BJ@A�^@@��@@�@@ �@?��@?�@?;d@?
=@>�@>V@>@=?}@<��@<(�@;��@;�
@;��@;"�@:�\@9�#@9X@9&�@9�@8��@8Ĝ@8Ĝ@8��@8bN@8A�@81'@8b@8  @7�;@7�@6��@6ff@5�T@5?}@5V@4�@4��@4�@4j@4I�@4�@3ƨ@3C�@3o@2��@2~�@2=q@2�@1�7@1G�@1&�@0�`@0r�@0  @/�w@/\)@/
=@.ȴ@.��@.E�@-�@-@-�-@-��@-��@-�@-V@,��@,��@,�D@,I�@,9X@,(�@+�m@+�@+"�@*��@*��@*n�@*=q@*�@)�@)�^@)��@)�7@)G�@)�@(�`@(��@(Ĝ@(��@(�u@(A�@'�@'��@';d@'
=@&�y@&�R@&v�@&{@%�@%�T@%�h@%O�@%�@$�@$�@$�@$�@$�j@$Z@#��@#t�@"�@"�!@"��@"~�@"M�@!�@!��@!��@!G�@ �9@ �@ A�@�w@�P@l�@+@��@ȴ@��@�+@ff@E�@�T@�@/@V@�/@��@�j@z�@Z@ƨ@t�@o@�H@��@��@n�@^5@M�@J@��@��@�@�@�^@hs@7L@��@Q�@1'@ �@b@b@  @�;@\)@��@$�@{@@�@�@�@�T@�-@`B@/@V@��@z�@j@(�@��@��@ƨ@dZ@o@o@�@��@��@�!@�!@�\@J@��@�@�@�@��@��@G�@%@��@�9@bN@Q�@A�@b@��@�P@l�@�@
=@��@v�@E�@$�@@�T@�T@�T@��@��@�h@p�@O�@?}@/@/@/@V@��@j@�@��@��@�m@�m@�
@�
@ƨ@��@��@��@�@dZ@S�@33@
�@
�H@
�H@
�H@
�H@
��@
��@
~�@
M�@
�@
J@
J@	��@	��@	�7@	x�@	x�@	hs@	X@	X@	7L@	&�@	�@	%@��@�`@�9@��@�u@�@bN@  @�@�P@l�@K�@�@
=@��@�y@�@�@�@�R@v�@E�@5?@$�@{@�@�T@��@@��@�h@�@�@p�@/@�@�@�@�@�@�@�/@��@�j@�@�D@z�@j@9X@�@��@�
@ƨ@�F@��@S�@33@"�@o@o@o@�@��@~�@^5@^5@n�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���Aֺ^AָRAִ9Aֲ-A֧�A֧�A֡�A�~�A�{A��A�I�Aҝ�A��A���A�oA�?}A�+A���A��
A�1'A���Aė�AuA��uA�  A�9XA���A�G�A�Q�A�VA��A�hsA�oA�\)A��A��wA���A��FA���A�E�A�ȴA��9A�I�A�n�A�$�A���A�{A��-A��TA���A�1'A��uA�dZA��7A���A�A�I�A��^A���A��RA���A�/A�ƨA���A��A���A��uA���A�ffA���A��\A���A�K�A���A���A��/A�ĜA���A�=qA��A���A�l�A�E�A��A�-A���A�&�A��A�~�AA}oA{|�Az��Az{Ay�7AyhsAx��Ax�Aw�hAw/Aw�Av�Av=qAt��Ar{Ap��An�!Am�Al �Ak�hAj�HAh��Ag"�AfJAd�Ac�Ac33AahsA_�-A_%A^  A\v�A[VAYVAXbAW�AW\)AV�AV��AVVAU|�AS�ARJAQ"�APANn�AN  AM��AL�!AK�PAJr�AH�AH  AGl�AF�/AFM�AE�AE�AC��AA�FA@��A?��A>�yA=�A=\)A;�A;&�A:VA9��A8�A7ƨA7\)A6�A5|�A4�uA4�A3��A2��A1��A0�`A0$�A.��A-S�A,�DA,  A+ƨA+�A++A*��A*M�A)K�A(v�A';dA%�PA$�+A#�-A#�A"�!A"=qA!�;A!�-A!O�A ^5A\)A5?A�^A�FA�-A��A;dAv�A�A{At�A�HA��A+AQ�AhsA�9A��A�AVA�`A~�AffA�A��A;dA�A�mA
�A
^5A	�A	l�Ar�A�A�yAJA�^AK�A�A33A��@���@���@���@�9X@��@�z�@�n�@�Z@�\)@��@��@��@�z�@�@��@�@�ƨ@�^5@�O�@��@�I�@�33@噚@���@�D@㕁@�-@�(�@�l�@���@�-@�&�@�5?@�V@��H@ա�@�?}@�|�@љ�@��@��;@���@�7L@ˍP@ɑh@ȃ@�|�@��@�M�@�X@��`@���@�5?@�G�@�V@�  @�l�@���@��j@�(�@�ƨ@��y@��+@�@�?}@�9X@��R@�X@���@���@�+@���@�n�@��@��-@�&�@��j@��u@��@���@�ȴ@�J@��^@�7L@���@��
@�S�@���@�~�@�`B@�bN@���@���@�|�@�O�@�j@�1@��@���@�5?@�X@��@�z�@��w@���@�J@��@���@���@���@�1@��@�=q@�O�@��@��@�Z@��@��
@��@��P@�t�@�\)@���@�ȴ@���@��+@�n�@�-@���@�&�@�Q�@�A�@�  @��@�S�@��R@�{@�{@�E�@�-@�?}@��u@��@�33@�"�@�"�@�"�@�"�@�"�@��@�~�@���@���@�7L@���@��/@���@���@�j@�1'@��@�ƨ@�t�@��@�V@���@�p�@�hs@�hs@��@���@�A�@�  @���@��@�|�@�dZ@���@���@�ff@�-@��#@��-@��7@��@�Ĝ@�Z@��;@��;@��;@��m@��@�"�@�n�@�-@��T@�@��-@��-@���@���@�?}@�j@��@�1@�P@\)@
=@~��@}�h@|�@|I�@{��@{��@z��@z=q@y&�@xb@w��@w��@w
=@v�+@vff@vV@vE�@vV@vV@v@up�@t�@t��@s�F@s33@so@r��@r�\@r^5@r-@q��@q��@q�@q��@q��@q&�@pĜ@p�u@p  @o�@o
=@n�@nV@n$�@m�@m��@m�@mp�@m`B@mO�@mO�@m?}@mV@l��@l�j@lj@l(�@k�
@k�F@k��@kt�@kC�@ko@j��@j�!@j~�@j~�@jn�@j^5@j^5@j�@i�#@i��@ihs@i&�@h1'@g�w@g��@g��@g|�@g�@f�@fv�@f5?@f5?@e�@e?}@d�/@d��@dI�@c��@c�m@c�m@c��@cdZ@cS�@co@b^5@b=q@b-@a��@a��@a��@ahs@`�9@`r�@`A�@_�w@_�P@_\)@_�@^�@^ff@]�-@\�@\�@\�@\z�@[��@[ƨ@[��@[dZ@[dZ@[dZ@[o@Z��@ZM�@ZJ@Y��@Yhs@Y7L@X��@XĜ@Xr�@X �@W�P@W
=@V��@VV@V$�@U��@UO�@T�j@TZ@T�@S��@S�m@S�
@Sƨ@S�@SS�@S33@R�H@R�\@Rn�@R=q@Q�#@Q&�@P�`@P��@P��@P�9@PQ�@O�;@O��@O\)@N�@N{@M�T@M��@M?}@M/@L��@L��@L�D@LZ@K�m@Kt�@J��@J~�@JM�@JJ@I��@Ix�@IX@I&�@H�`@HA�@G�w@G��@Gl�@G;d@G
=@F�@F�+@FV@F5?@Ep�@D��@D��@D�@CC�@B�@B�H@B�!@B�\@B~�@BM�@BJ@A�^@@��@@�@@ �@?��@?�@?;d@?
=@>�@>V@>@=?}@<��@<(�@;��@;�
@;��@;"�@:�\@9�#@9X@9&�@9�@8��@8Ĝ@8Ĝ@8��@8bN@8A�@81'@8b@8  @7�;@7�@6��@6ff@5�T@5?}@5V@4�@4��@4�@4j@4I�@4�@3ƨ@3C�@3o@2��@2~�@2=q@2�@1�7@1G�@1&�@0�`@0r�@0  @/�w@/\)@/
=@.ȴ@.��@.E�@-�@-@-�-@-��@-��@-�@-V@,��@,��@,�D@,I�@,9X@,(�@+�m@+�@+"�@*��@*��@*n�@*=q@*�@)�@)�^@)��@)�7@)G�@)�@(�`@(��@(Ĝ@(��@(�u@(A�@'�@'��@';d@'
=@&�y@&�R@&v�@&{@%�@%�T@%�h@%O�@%�@$�@$�@$�@$�@$�j@$Z@#��@#t�@"�@"�!@"��@"~�@"M�@!�@!��@!��@!G�@ �9@ �@ A�@�w@�P@l�@+@��@ȴ@��@�+@ff@E�@�T@�@/@V@�/@��@�j@z�@Z@ƨ@t�@o@�H@��@��@n�@^5@M�@J@��@��@�@�@�^@hs@7L@��@Q�@1'@ �@b@b@  @�;@\)@��@$�@{@@�@�@�@�T@�-@`B@/@V@��@z�@j@(�@��@��@ƨ@dZ@o@o@�@��@��@�!@�!@�\@J@��@�@�@�@��@��@G�@%@��@�9@bN@Q�@A�@b@��@�P@l�@�@
=@��@v�@E�@$�@@�T@�T@�T@��@��@�h@p�@O�@?}@/@/@/@V@��@j@�@��@��@�m@�m@�
@�
@ƨ@��@��@��@�@dZ@S�@33@
�@
�H@
�H@
�H@
�H@
��@
��@
~�@
M�@
�@
J@
J@	��@	��@	�7@	x�@	x�@	hs@	X@	X@	7L@	&�@	�@	%@��@�`@�9@��@�u@�@bN@  @�@�P@l�@K�@�@
=@��@�y@�@�@�@�R@v�@E�@5?@$�@{@�@�T@��@@��@�h@�@�@p�@/@�@�@�@�@�@�@�/@��@�j@�@�D@z�@j@9X@�@��@�
@ƨ@�F@��@S�@33@"�@o@o@o@�@��@~�@^5@^5@n�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BoBoBoBoBhBbBVB1B��B�sB��B�fB�B��B��B��B  BJB�B-BA�B[#BaHB%�Bu�B��B��B��B��B��B�7B�uB�VB��B��B�7B|�Bz�BgmBl�Bn�B\)BbNBO�B@�B6FB6FB;dB/B!�B�BoB��B�)B�mB�
B��B��B��B��B��B�=B�Bw�BbNBO�BH�B>wB>wB6FB33B(�B{B
��B
��B
�
B
�5B
�#B
��B
��B
ȴB
��B
�dB
��B
�bB
�oB
�DB
�B
}�B
k�B
W
B
VB
T�B
Q�B
P�B
O�B
J�B
B�B
B�B
?}B
>wB
8RB
,B
�B
B
B	�B	�B	�B	�sB	�5B	��B	��B	ƨB	�jB	�FB	�'B	��B	�{B	��B	�bB	~�B	z�B	m�B	q�B	v�B	v�B	r�B	q�B	n�B	dZB	T�B	K�B	I�B	G�B	;dB	D�B	A�B	6FB	,B	&�B	�B	�B	 �B	�B	�B	�B	PB��B�B��B��B�B�B�B�NB�mB�NB�NB�/B�B�#B�B��B��B��B��BÖBŢBŢBB�XB�3B�dB�^B�wB�jB�RB�?B�!B��B��B��B�=B�hB�\B�JB�JB�7B�1B�1B�Bx�Bt�Bt�By�B� B}�Bz�Bt�Bk�BdZBcTBffBdZB\)BaHB[#BXBW
BQ�BE�BO�BW
BT�BS�BN�BB�B8RBB�B?}B<jB>wB?}B;dB5?B5?B6FB49B:^B8RB1'B&�B �B!�B,B+B%�B�B �B�B�B#�B)�B'�B"�B�B#�B"�B�B�B�B"�B%�B!�B�B�B �B"�B�B�B�B �B!�B �B�BbB�B�B�B"�B�B�B"�B!�B�B�B�B�B%�B%�B'�B'�B$�B&�B#�B�B"�B'�B'�B'�B'�B(�B1'B49B33B6FB6FB2-B1'B/B49B<jB>wB8RBA�BF�BE�BE�BE�BF�BH�BF�BF�BE�BG�BL�BL�BK�BM�BP�BR�BS�BO�BT�B[#B^5BZBR�B^5Be`Be`Be`BhsBhsBn�Bo�Bo�Br�Bw�B�B�B� B{�B{�B�B�+B�1B�bB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�'B�FB�XB�LB�3B�^B�}BĜB��B��B��B��B��B��B��B��B��B�B�)B�5B�;B�5B�5B�;B�BB�HB�BB�NB�NB�yB�B�B�B�B�B��B��B��B	B	B	B	  B	+B	1B		7B	DB	PB	VB	VB	oB	{B	�B	 �B	 �B	 �B	 �B	"�B	&�B	0!B	49B	8RB	;dB	<jB	<jB	;dB	9XB	;dB	G�B	L�B	L�B	O�B	P�B	P�B	P�B	S�B	[#B	_;B	`BB	`BB	e`B	ffB	hsB	o�B	q�B	r�B	v�B	z�B	{�B	|�B	}�B	}�B	|�B	}�B	�B	�B	�B	�%B	�7B	�=B	�DB	�JB	�JB	�VB	�\B	�\B	�bB	�hB	�hB	�uB	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�3B	�3B	�?B	�FB	�FB	�FB	�?B	�FB	�FB	�RB	�RB	�LB	�wB	��B	��B	��B	��B	B	ÖB	ĜB	ŢB	ŢB	ĜB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�)B	�BB	�NB	�HB	�HB	�TB	�ZB	�TB	�`B	�`B	�ZB	�`B	�`B	�mB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
1B
1B
	7B
	7B

=B

=B

=B

=B

=B
DB
PB
VB
VB
VB
\B
VB
\B
\B
VB
bB
hB
hB
hB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
!�B
!�B
 �B
!�B
!�B
#�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
&�B
&�B
%�B
&�B
'�B
(�B
+B
,B
,B
,B
,B
-B
-B
,B
,B
.B
.B
.B
/B
/B
.B
0!B
0!B
0!B
0!B
0!B
2-B
2-B
2-B
33B
49B
49B
49B
6FB
6FB
6FB
6FB
6FB
5?B
7LB
7LB
7LB
7LB
8RB
8RB
7LB
7LB
7LB
9XB
:^B
:^B
:^B
:^B
;dB
;dB
;dB
<jB
;dB
<jB
<jB
=qB
=qB
=qB
=qB
<jB
<jB
<jB
=qB
>wB
>wB
>wB
>wB
>wB
?}B
@�B
?}B
@�B
A�B
A�B
B�B
B�B
B�B
A�B
@�B
A�B
A�B
B�B
D�B
E�B
E�B
E�B
D�B
E�B
F�B
E�B
E�B
G�B
G�B
G�B
H�B
I�B
I�B
I�B
J�B
J�B
K�B
J�B
J�B
J�B
J�B
K�B
L�B
L�B
M�B
M�B
L�B
M�B
L�B
N�B
N�B
O�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
Q�B
Q�B
Q�B
Q�B
Q�B
S�B
T�B
T�B
T�B
T�B
S�B
Q�B
Q�B
S�B
W
B
XB
XB
XB
XB
W
B
W
B
W
B
XB
XB
W
B
YB
ZB
YB
ZB
[#B
ZB
ZB
ZB
\)B
\)B
\)B
\)B
\)B
\)B
\)B
[#B
]/B
^5B
^5B
]/B
]/B
]/B
\)B
]/B
]/B
^5B
^5B
_;B
_;B
^5B
^5B
_;B
_;B
`BB
`BB
`BB
aHB
aHB
bNB
bNB
bNB
cTB
cTB
cTB
bNB
cTB
cTB
cTB
dZB
dZB
dZB
cTB
bNB
bNB
cTB
cTB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
e`B
ffB
gmB
gmB
gmB
ffB
ffB
gmB
gmB
gmB
hsB
hsB
hsB
gmB
gmB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
hsB
iyB
jB
jB
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
k�B
k�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
m�B
m�B
m�B
o�B
o�B
o�B
o�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
q�B
q�B
q�B
p�B
q�B
q�B
r�B
s�B
t�B
s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�BoB�BoB�B}B�B�B�BB��B�gB�B��B��B�zB��B�B�B �B2�BF�B`\BgB2-Bz�B��B��B�5B�@B� B��B��B�NB�EB�_B��B�B}VBj�Bn/Bo�B^�Bc�BR BC{B8�B8RB<�B1B$B�BaB��B�'B��B�KB��B�{B��B�QB�KB��B�tBy�Bd�BR�BK)B@OB?}B7�B49B*B�B�B
�1B
�yB
ބB
ۦB
��B
̘B
�lB
�AB
�6B
�B
�[B
��B
�dB
�MB
~�B
m�B
YB
W�B
U�B
R�B
Q�B
P.B
KxB
CaB
CGB
?�B
>�B
8�B
-]B
�B
B
B	�FB	�vB	��B	�_B	߾B	�\B	�{B	��B	��B	��B	�|B	� B	��B	��B	��B	�;B	|�B	o�B	r�B	wfB	w2B	sMB	rB	oiB	e�B	W$B	NB	KB	IB	=qB	ESB	BuB	7�B	-�B	(�B	�B	�B	!�B	 �B	�B	sB	�B	 �B�UB�<B��B�%B��B�B��B�B�B�TBބB�sB��B��B̘B��B̘B�jB�9B��B��B��B�JB�%B�jB�0B��B��B�	B��B�B�sB�'B��B��B��B�}B�B��B��B��B��B��Bz^BvFBv+BzxB�4B~(B{dBu�Bl�BfLBd�BgmBe`B]�Bb4B\xBYeBX_BS�BH1BP�BWsBU�BTaBO�BDgB:DBC{B@�B=�B?cB@4B<jB6�B6`B7fB5ZB;B9$B2�B)B#B#�B,�B,"B'B!-B"B 'B!-B$�B*0B(sB#�B�B$�B#�B �B#B�B#�B&2B"�B �B�B!|B#nB�BB�B!|B"�B!�B�BoB�BB�B#nBB�B#�B"�B�BB!B!B&�B&�B(�B(�B%�B'mB$�B �B#�B(XB(�B(�B)B)�B1�B4�B3�B6�B6�B2�B2-B0UB5?B<�B?B9rBBBF�BFBF%BF%BGBH�BGEBG+BFtBHfBM6BMPBLdBN�BQhBSuBTaBP�BU�B[�B^�BZ�BT{B^�Be�Be�Be�Bh�BiDBoBp;BpoBs�BxRB�;B�;B�OB|�B|�B��B��B�B��B��B��B��B��B��B��B��B��B�'B�B��B� B�B�4B�NB�nB��B�IB�]B�iB��B��B��B�`B�rB��B��B��B� B�B��B� B�B� B�B�HB�\B�NB�MB�yB�xB�jB�pBބBބBߊB�vB�B�B��B�B�B��B��B��B��B�B�2B�0B�VB	 B	'B	AB	 �B	EB	fB		�B	�B	�B	�B	�B	�B	�B	B	 �B	 �B	 �B	!-B	#nB	'�B	0oB	4�B	8lB	;B	<�B	<�B	;�B	9�B	<B	G�B	MB	MB	PB	Q4B	Q4B	QNB	TaB	[qB	_�B	`�B	`�B	e�B	f�B	h�B	o�B	q�B	sB	v�B	z�B	|B	}"B	~B	~B	}<B	~]B	�;B	�[B	�{B	�YB	�lB	�XB	�^B	�~B	�~B	��B	�vB	�vB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�*B	�0B	�"B	�IB	�OB	�OB	�OB	�;B	�[B	�hB	�hB	�ZB	�zB	�zB	�`B	�tB	�`B	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	żB	��B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�BB	�&B	�B	�&B	�,B	�B	�&B	�@B	�9B	�?B	�9B	�KB	�KB	�7B	�QB	�kB	چB	�xB	�vB	�NB	�|B	�|B	�B	�B	�B	�`B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�	B	�$B	�B	�B	�B	�"B	�B	�6B	�(B	�(B	�(B	�BB
AB
AB
AB
MB
GB
3B
MB
MB
gB
gB
�B
KB
KB
	lB
	lB

XB

XB

XB

rB

�B
�B
�B
�B
�B
�B
vB
pB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
 B
!�B
!�B
!�B
 �B
"B
"B
$&B
&B
'B
'B
'B
'B
'B
'B
(
B
(
B
($B
(
B
'B
'B
&LB
'8B
(>B
)DB
+6B
,"B
,=B
,=B
,=B
-)B
-)B
,WB
,WB
.IB
.IB
.IB
/OB
/5B
.cB
0UB
0;B
0UB
0UB
0oB
2aB
2aB
2GB
3MB
4nB
4nB
4TB
6zB
6`B
6FB
6`B
6`B
5tB
7LB
7�B
7�B
7fB
8lB
8�B
7�B
7�B
7�B
9rB
:xB
:xB
:xB
:�B
;�B
;�B
;�B
<�B
;B
<�B
<�B
=qB
=�B
=�B
=�B
<�B
<�B
<�B
=�B
>�B
>�B
>�B
>�B
>�B
?�B
@�B
?�B
@�B
A�B
A�B
B�B
B�B
B�B
A�B
@�B
A�B
A�B
B�B
D�B
E�B
E�B
E�B
D�B
E�B
F�B
E�B
E�B
G�B
G�B
G�B
H�B
I�B
I�B
I�B
J�B
J�B
K�B
J�B
J�B
KB
J�B
K�B
MB
MB
M�B
NB
L�B
NB
MB
N�B
OB
O�B
Q B
Q B
QB
Q�B
R B
RB
SB
R�B
SB
SB
R B
RB
R B
R:B
R:B
T,B
UB
UB
UB
U2B
T,B
R B
RTB
TFB
W$B
XB
X+B
XB
XB
W?B
W?B
W?B
XEB
XEB
W?B
YKB
Z7B
Y1B
ZQB
[=B
ZQB
ZQB
ZQB
\CB
\CB
\]B
\CB
\)B
\CB
\]B
[WB
]/B
^OB
^5B
]/B
]dB
]dB
\xB
]dB
]IB
^OB
^OB
_VB
_;B
^jB
^jB
_VB
_pB
`vB
`vB
`vB
a|B
a|B
b�B
b�B
bhB
cTB
cTB
cnB
b�B
cnB
cnB
cnB
dtB
dtB
dtB
cnB
b�B
b�B
c�B
c�B
ezB
ezB
ezB
e`B
e`B
ezB
ezB
ezB
e`B
e`B
ezB
e�B
f�B
f�B
e�B
ffB
g�B
g�B
gmB
f�B
f�B
g�B
g�B
g�B
h�B
h�B
hsB
g�B
g�B
i�B
i�B
i�B
i�B
iyB
i�B
i�B
i�B
i�B
iyB
i�B
i�B
i�B
iyB
i�B
i�B
h�B
i�B
j�B
j�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
k�B
k�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
m�B
m�B
m�B
o�B
o�B
o�B
o�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
q�B
q�B
q�B
p�B
q�B
q�B
r�B
s�B
t�B
s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.13(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201808280038262018082800382620180828003826201808280200202018082802002020180828020020201808290025342018082900253420180829002534  JA  ARFMdecpA19c                                                                20180824093608  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180824003707  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180824003713  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180824003720  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180824003725  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180824003725  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180824003727  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180824003727  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180824003807  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180824003808                      G�O�G�O�G�O�                JA  ARUP                                                                        20180824010242                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180824153311  CV  JULD            G�O�G�O�F�ߕ                JM  ARCAJMQC2.0                                                                 20180827153826  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180827153826  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180827170020  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180828152534  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131518                      G�O�G�O�G�O�                