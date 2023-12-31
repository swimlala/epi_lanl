CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-09-05T09:43:18Z creation;2022-09-05T09:43:19Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tx   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �p   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220905094318  20220905100106  4902988                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               jA   JA                                  2B  A   APEX                            8600                            2.11.3                          846 @��KR��d1   @��K��kU@:S����c�333331   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�ff@�  A   A   A@  Aa��A�  A�  A�  A���A�  A�  A�  A�  B   B  B  BffB   B(  B0  B8  B@  BH  BP  BX  B`  Bg��Bp  Bx  B�33B�33B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  C   C  C�fC�fC  C
  C  C  C  C  C�fC  C  C�fC�C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Cs�fCv  Cx�Cz  C{�fC~  C��C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  D   D � D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fDfD� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D�fD  Dy�D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)�fD*fD*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/�fD0  D0� D1  D1� D2fD2� D3  D3�fD4  D4� D5fD5� D5��D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=�fD>  D>� D>��D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DDy�DD��DE� DF  DF� DG  DG� DG��DH� DIfDI� DJ  DJ� DK  DK� DK��DL� DM  DMy�DM��DNy�DO  DO� DO��DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY�fDZ  DZ� D[  D[�fD\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� DgfDg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn�fDo  Do� Dp  Dpy�Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� DvfDv� Dw  Dwy�Dw��Dxy�Dy  Dy� DzfDz�fD{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D��3D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�<�D�� D��3D�3D�@ D�� D���D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�|�D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��3D�� D�  D�@ D�|�D�� D�  D�@ D��3D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D���D�<�D�� D�� D�3D�C3D�� D�� D���D�<�D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�<�Dǀ D�� D�  D�@ Dȃ3D�� D�  D�C3Dɀ D�� D�  D�@ D�|�D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�<�D̀ D�� D�  D�@ D΀ D�� D�  D�<�Dπ D�� D�  D�@ DЀ D��3D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ Dԃ3D��3D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�<�D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ D�|�D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�3D�@ D�|�D�� D�  D�C3D� D�� D���D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�3D�C3D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�|�D��D���D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D���D���D�<�D�� D�� D�  D�C3D�� D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�=p@��
@��
A�A=�A_�A}�A���A���A�A���A���A���A���A���Bz�Bz�B�GBz�B'z�B/z�B7z�B?z�BGz�BOz�BWz�B_z�Bg{Boz�Bwz�B�GB��B��qB��qB��qB��qB��>B��>B��qB��qB��qB��qB��qB��qB��qB��qB��qBýqBǽqB˽qBϽqBӽqB׽qB۽qB߽qB�qB�qB��B�qB�qB��qB��qB��qC޸C�C�C޸C	޸C޸C޸C޸C޸C�C޸C޸C�C�RC޸C޸C!޸C#޸C%޸C'޸C)޸C+޸C-޸C/޸C1޸C3޸C5޸C7޸C9޸C;޸C=޸C?޸CA޸CC޸CE޸CG޸CI޸CK޸CM޸CO޸CQ޸CS޸CU޸CW޸CY޸C[޸C]޸C_޸Ca޸Cc޸Ce޸Cg޸Ci޸Ck޸Cm޸Co޸Cq޸Cs�Cu޸Cw�RCy޸C{�C}޸C�RC��\C��\C��C��\C��)C��\C��\C��\C��\C��\C��C��C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��C��\C��)C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��)C��\C��\C��\C��\C��\C��C��C��\C��\C��\C��\C��C��\C��\C��C��\C��\C��C��\C��\C��\C��)C��\C��\C��\C��\C��\C��\C��\C��\C��C��C��\C��C��C��\C��\C��\C��\C��\C��\C��\C��\C��)C��\C��\C��C��\C��)C��)C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��C��\C��)C��\C��\C��\C��\C��\C��\C��\C��\D w�D ��D~D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D	w�D	��D
w�D
��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D~D�Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D�HDw�D��D~D��DqHD��Dw�D��Dw�D��Dw�D��D w�D ��D!w�D!��D"w�D"��D#w�D#��D$w�D$��D%w�D%��D&w�D&��D'w�D'��D(w�D(��D)~D)�D*w�D*��D+w�D+��D,w�D,��D-w�D-��D.w�D.��D/~D/��D0w�D0��D1w�D1�D2w�D2��D3~D3��D4w�D4�D5w�D5�HD6w�D6��D7w�D7��D8w�D8��D9w�D9��D:w�D:��D;w�D;��D<w�D<��D=~D=��D>w�D>�HD?w�D?��D@w�D@��DAw�DA��DBw�DB��DCw�DC��DDqHDD�HDEw�DE��DFw�DF��DGw�DG�HDHw�DH�DIw�DI��DJw�DJ��DKw�DK�HDLw�DL��DMqHDM�HDNqHDN��DOw�DO�HDPw�DP��DQw�DQ��DRw�DR��DSw�DS��DTw�DT��DUw�DU��DVw�DV��DWw�DW��DXw�DX��DY~DY��DZw�DZ��D[~D[��D\w�D\��D]w�D]��D^w�D^��D_w�D_��D`w�D`��Daw�Da��Dbw�Db��Dcw�Dc��Ddw�Dd��Dew�De��Dfw�Df�Dgw�Dg��Dhw�Dh��Diw�Di��Djw�Dj��Dkw�Dk��Dlw�Dl��Dmw�Dm��Dn~Dn��Dow�Do��DpqHDp��Dqw�Dq��Drw�Dr��Dsw�Ds��Dtw�Dt��Duw�Du�Dvw�Dv��DwqHDw�HDxqHDx��Dyw�Dy�Dz~Dz��D{w�D{��D|w�D|��D}w�D}��D~w�D~��Dw�D��D�;�D�{�D���D���D�;�D�
D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�x�D���D��
D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�
D���D��
D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�
D���D���D�;�D�{�D��
D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�8�D�{�D���D���D�8�D�{�D��
D��
D�;�D�{�D���D���D�;�D�{�D���D���D�?
D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�
D���D���D�;�D�x�D���D���D�8�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�?
D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�?
D�
D���D���D�;�D�x�D���D���D�;�D�
D���D���D�8�D�x�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�
D���D���D�;�D�{�D���D���D�8�D�{�D���D��
D�?
D�{�D���D���D�8�D�{�D���D���D�;�D�{�D���D���D�8�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D»�D���D�;�D�{�Dû�D���D�;�D�{�DĻ�D���D�;�D�{�DŻ�D���D�;�D�{�Dƻ�D���D�8�D�{�Dǻ�D���D�;�D�
DȻ�D���D�?
D�{�Dɻ�D���D�;�D�x�Dʻ�D���D�;�D�{�D˻�D���D�;�D�{�D̻�D���D�8�D�{�Dͻ�D���D�;�D�{�Dλ�D���D�8�D�{�Dϻ�D���D�;�D�{�Dп
D���D�;�D�{�Dѻ�D���D�;�D�{�Dһ�D���D�;�D�{�Dӻ�D���D�;�D�
DԿ
D���D�;�D�{�Dջ�D���D�;�D�{�Dֻ�D���D�;�D�{�D׻�D���D�8�D�{�Dػ�D���D�;�D�{�Dٻ�D���D�;�D�{�Dڻ�D���D�;�D�{�Dۻ�D���D�;�D�{�Dܻ�D���D�;�D�{�Dݻ�D���D�;�D�x�D޻�D���D�;�D�{�D߻�D���D�;�D�{�D��D���D�8�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�x�D��D���D�;�D�{�D��D��
D�;�D�x�D��D���D�?
D�{�D��D���D�;�D�{�D��D���D�;�D�
D��D���D�;�D�{�D��D��
D�?
D�
D���D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�x�D�D���D�8�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D��
D�;�D�{�D���D���D�;�D�{�D���D���D�8�D�{�D���D���D�?
D�{�D��
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�CaA�MA�P�A�P�A�QA�Q�A�R A�Q�A�QA�O�A�Q�A�B�A�0�A�3�A�*�A�~A�JA� iA���A��A��A��A��WA��5Aס�A�)_AӀ�Aˍ�A���AþBA�IRA��rA���A�  A��6A��A���A�g8A���A�kQA�~A���A��dA���A��GA��A�8�A�.IA���A�zDA�v�A�خA���A�J�A��QA���A���A�;�A�!bA��\A���A���A��A���A���A� �A�9�A��9A��2A�#A�GA���A���A���A�c�A�I�A�@A�k�A���A�ѷA�[WA���A��mA��A��bA�!�A��KA���A��1A�5�A��$A��A��A�-wA�/�ARTA}�A|GA{�Az\)Ayu%AxFtAvp�AsqAr�4ArT�ArMjAq�fAqi�Ap��Ap��Ap=qAo��An�7Am��Al�@AlDgAjVmAe�Aa�sA_G�A\��AZ��AZ��AY�AY;AXC�AW$AVYKAV�AU�+AUB�AT��ATu�AS!-AO,�AL�AJc�AH��AF�AB�AAuA?��A>�A>�zA<�rA8e�A5��A3�PA3)�A2u%A2"�A1a�A0��A0�OA/��A.8�A-h
A,�A,�\A+�A*��A)�A)b�A(��A(��A'�A'�A&ںA&:�A%�A"��A" �A!��A!��A!�A *�A�VAbNAĜA�dA��A�A�EA��AH�A0UAoA��A��AQ�A'RA_A�AƨA�rA>BA�A4nA�hAhsA+A�A��A,�AA��A`BA�AAیA
�&A	��A�AzA \A��A��A��At�AZ�A��A�A �
@��@��k@�&@�ں@��?@���@�1'@�(�@��@�:�@�Ta@�@��@�@�m]@��'@�~�@�ں@���@�Vm@��8@�g8@��@�E9@��g@��@�m]@�@�dZ@��@߁�@�rG@�l�@ژ_@��@ֻ�@Չ7@��@�֡@�p;@��@�"h@�� @�L�@��@�[W@���@�!�@���@�$t@Ï�@���@_@�j@�� @���@���@�O�@���@�hs@�8@���@���@�kQ@�3�@���@�!�@�֡@�\�@��@� i@��}@��v@�4n@�5?@���@��@���@��.@��.@��@�q�@�ݘ@��-@���@��p@�E9@�@�>B@��"@���@��+@�X@��j@��*@��@���@��.@�Ft@��@��*@��@���@���@���@���@�z�@��@�qv@��|@�H�@�4n@��)@���@� �@���@��	@�^�@�(�@���@�u�@��@��@�@���@�s�@��S@��@�zx@�C�@���@��n@�e,@�%@��@�@@�B[@�s@��@���@��[@��n@�N�@��c@���@�xl@��@�^�@��@��E@���@�u�@���@��F@��k@�Vm@�9�@��@��@�Ov@�.�@�	@��+@���@��9@��V@�{J@�_p@��@��v@���@���@���@��?@��<@���@��9@��@�O@���@�F�@��@�?�@��@�8�@�֡@��e@�L0@��@��@���@�͟@��_@�s�@�a|@�5?@��@��@���@�v`@�hs@�RT@�L�@�T�@��@���@���@�H�@���@���@��@�3�@Z�@}��@|�f@|��@|2�@{�;@{�K@{�{@{)_@z��@z�@zں@zq�@zd�@z^5@z=q@y��@y�@y;@wƨ@w]�@w�@v6�@u�d@u�@u��@u�X@u��@u:�@t�9@s�F@s��@sH�@sC@r�8@r�@r��@r�@rGE@r@q��@q��@qx�@q�@o�m@o�}@o� @o��@oo@nM�@n�@m��@n@m�.@mk�@l6@k�	@j��@j!�@i��@i[W@i4@h��@hѷ@h�@hG@gy�@g!-@f��@f�m@f�!@f��@f^5@fJ�@f-@f@e�@e:�@dy>@d-�@c��@c��@c��@ct�@c;d@c�@b�'@b��@bl�@be@a�@arG@`�@`m�@_�W@_�@_��@_��@_.I@^�B@^q�@^1�@]�T@]�@]F@\y>@\�@\b@[��@[�
@[Mj@Z�X@Z��@Z{�@Zd�@ZH�@Z6�@Z-@Z@Y��@Y�D@Y�@Y�M@YO�@X��@W�@W��@W,�@V�H@Vz@VZ�@VO@U�3@U��@U?}@T��@S��@S�@R�'@R�F@R�+@Rq�@R5?@R+k@RJ@Qu�@P��@P[�@PS�@PN�@P:�@P'R@PM@Oخ@O��@O]�@OK�@OC@N��@N��@Nz@Ni�@NV@N�@M�N@MVm@L�@L��@K��@KH�@K�@J�H@J;�@I��@I��@I�X@IVm@H��@H�D@Hu�@H~@G�@Gݘ@G�Q@G�Q@Gخ@G��@G��@GRT@F��@F}V@F\�@E�o@E��@EVm@DɆ@D~(@D1'@C�W@C��@C{J@CZ�@CO@C i@B�1@BM�@B-@B)�@A�d@AIR@A�@@�`@@�[@@�@@��@@�e@@��@@`�@@-�@@M@?�Q@?y�@?J#@?�@>�@>��@>��@>�@>.�@>@=�@=|@=!�@<��@<��@<��@<�4@<�4@<��@<�I@<��@<��@<��@<u�@<?�@<�@;�	@;S@:J�@9��@9zx@8��@8��@8"h@7�$@7_p@7H�@6�@5��@5X@4��@3��@3��@3\)@36z@3�@3�@2�8@2�F@2
�@1�@1��@1�"@1c@1s�@1Q�@1&�@0�v@0�e@0l"@0I�@02�@/��@/��@/n/@/iD@/6z@/@.�]@.R�@.&�@.�@.J@-�.@-��@-�^@-��@-a�@-:�@-+�@-+@-�@,��@,y>@+�W@+�@+O@+"�@*��@*��@*��@*q�@*ff@*R�@*3�@*�@)�>@)�@)�-@)��@)��@)m]@)e,@)S&@)-w@)!�@(�f@(��@(��@(�U@(��@(�@(�Y@(u�@(_@(7�@(*�@'�@&��@&� @&��@&C�@&J@%�.@%��@%�j@%�N@%ϫ@%��@%F@$��@$q@$M@#n/@#Y@#S@"�@"ں@"�'@"�<@"�h@"��@"��@"��@"��@"~�@"kQ@!��@!�@ �z@ c�@s@�2@��@^5@!�@�N@�^@c@T�@0�@�@��@�O@�e@�4@�z@q@1@�k@]�@ȴ@�@�z@o @\�@Y�@B�@+�@@;@�p@�@Xy@D�@6@�@��@�@��@�F@�q@��@�{@��@|�@s@�@�m@��@H�@.�@4@�@�@��@��@�"@�7@p�@hs@Dg@��@�@�)@��@�)@ѷ@��@�E@�)@�j@��@�&@�6@��@��@�	@P�@ i@��@�H@�s@�6@z@z@xl@q�@Ta@-@�)@�@��@X@�@�@��@z�@Ft@M@�@@ߤ@��@��@v�@c @M�@8�@1�@_@�C@zx@/@u�@%�@�@G@�]@	�@M@�@	�@� @��@qv@
�@
d�@
)�@	��@	�)@	�#@	��@	�9@	�z@	��@	��@	O�@	�@	;@�@�@r�@7�@!@��@��@��@��@l�@P�@6z@;d@9�@6z@o@�"@�M@��@�!@Ov@u@��@�Z@�Z@�)@�N@�@�7@|@w2@rG@hs@X@G�@+�@	l@��@|�@?�@M@��@��@�k@��@iD@U�@/�@�@�@͟@��@{�@5?@�@��@��@�=@}�@|@s�@1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�CaA�MA�P�A�P�A�QA�Q�A�R A�Q�A�QA�O�A�Q�A�B�A�0�A�3�A�*�A�~A�JA� iA���A��A��A��A��WA��5Aס�A�)_AӀ�Aˍ�A���AþBA�IRA��rA���A�  A��6A��A���A�g8A���A�kQA�~A���A��dA���A��GA��A�8�A�.IA���A�zDA�v�A�خA���A�J�A��QA���A���A�;�A�!bA��\A���A���A��A���A���A� �A�9�A��9A��2A�#A�GA���A���A���A�c�A�I�A�@A�k�A���A�ѷA�[WA���A��mA��A��bA�!�A��KA���A��1A�5�A��$A��A��A�-wA�/�ARTA}�A|GA{�Az\)Ayu%AxFtAvp�AsqAr�4ArT�ArMjAq�fAqi�Ap��Ap��Ap=qAo��An�7Am��Al�@AlDgAjVmAe�Aa�sA_G�A\��AZ��AZ��AY�AY;AXC�AW$AVYKAV�AU�+AUB�AT��ATu�AS!-AO,�AL�AJc�AH��AF�AB�AAuA?��A>�A>�zA<�rA8e�A5��A3�PA3)�A2u%A2"�A1a�A0��A0�OA/��A.8�A-h
A,�A,�\A+�A*��A)�A)b�A(��A(��A'�A'�A&ںA&:�A%�A"��A" �A!��A!��A!�A *�A�VAbNAĜA�dA��A�A�EA��AH�A0UAoA��A��AQ�A'RA_A�AƨA�rA>BA�A4nA�hAhsA+A�A��A,�AA��A`BA�AAیA
�&A	��A�AzA \A��A��A��At�AZ�A��A�A �
@��@��k@�&@�ں@��?@���@�1'@�(�@��@�:�@�Ta@�@��@�@�m]@��'@�~�@�ں@���@�Vm@��8@�g8@��@�E9@��g@��@�m]@�@�dZ@��@߁�@�rG@�l�@ژ_@��@ֻ�@Չ7@��@�֡@�p;@��@�"h@�� @�L�@��@�[W@���@�!�@���@�$t@Ï�@���@_@�j@�� @���@���@�O�@���@�hs@�8@���@���@�kQ@�3�@���@�!�@�֡@�\�@��@� i@��}@��v@�4n@�5?@���@��@���@��.@��.@��@�q�@�ݘ@��-@���@��p@�E9@�@�>B@��"@���@��+@�X@��j@��*@��@���@��.@�Ft@��@��*@��@���@���@���@���@�z�@��@�qv@��|@�H�@�4n@��)@���@� �@���@��	@�^�@�(�@���@�u�@��@��@�@���@�s�@��S@��@�zx@�C�@���@��n@�e,@�%@��@�@@�B[@�s@��@���@��[@��n@�N�@��c@���@�xl@��@�^�@��@��E@���@�u�@���@��F@��k@�Vm@�9�@��@��@�Ov@�.�@�	@��+@���@��9@��V@�{J@�_p@��@��v@���@���@���@��?@��<@���@��9@��@�O@���@�F�@��@�?�@��@�8�@�֡@��e@�L0@��@��@���@�͟@��_@�s�@�a|@�5?@��@��@���@�v`@�hs@�RT@�L�@�T�@��@���@���@�H�@���@���@��@�3�@Z�@}��@|�f@|��@|2�@{�;@{�K@{�{@{)_@z��@z�@zں@zq�@zd�@z^5@z=q@y��@y�@y;@wƨ@w]�@w�@v6�@u�d@u�@u��@u�X@u��@u:�@t�9@s�F@s��@sH�@sC@r�8@r�@r��@r�@rGE@r@q��@q��@qx�@q�@o�m@o�}@o� @o��@oo@nM�@n�@m��@n@m�.@mk�@l6@k�	@j��@j!�@i��@i[W@i4@h��@hѷ@h�@hG@gy�@g!-@f��@f�m@f�!@f��@f^5@fJ�@f-@f@e�@e:�@dy>@d-�@c��@c��@c��@ct�@c;d@c�@b�'@b��@bl�@be@a�@arG@`�@`m�@_�W@_�@_��@_��@_.I@^�B@^q�@^1�@]�T@]�@]F@\y>@\�@\b@[��@[�
@[Mj@Z�X@Z��@Z{�@Zd�@ZH�@Z6�@Z-@Z@Y��@Y�D@Y�@Y�M@YO�@X��@W�@W��@W,�@V�H@Vz@VZ�@VO@U�3@U��@U?}@T��@S��@S�@R�'@R�F@R�+@Rq�@R5?@R+k@RJ@Qu�@P��@P[�@PS�@PN�@P:�@P'R@PM@Oخ@O��@O]�@OK�@OC@N��@N��@Nz@Ni�@NV@N�@M�N@MVm@L�@L��@K��@KH�@K�@J�H@J;�@I��@I��@I�X@IVm@H��@H�D@Hu�@H~@G�@Gݘ@G�Q@G�Q@Gخ@G��@G��@GRT@F��@F}V@F\�@E�o@E��@EVm@DɆ@D~(@D1'@C�W@C��@C{J@CZ�@CO@C i@B�1@BM�@B-@B)�@A�d@AIR@A�@@�`@@�[@@�@@��@@�e@@��@@`�@@-�@@M@?�Q@?y�@?J#@?�@>�@>��@>��@>�@>.�@>@=�@=|@=!�@<��@<��@<��@<�4@<�4@<��@<�I@<��@<��@<��@<u�@<?�@<�@;�	@;S@:J�@9��@9zx@8��@8��@8"h@7�$@7_p@7H�@6�@5��@5X@4��@3��@3��@3\)@36z@3�@3�@2�8@2�F@2
�@1�@1��@1�"@1c@1s�@1Q�@1&�@0�v@0�e@0l"@0I�@02�@/��@/��@/n/@/iD@/6z@/@.�]@.R�@.&�@.�@.J@-�.@-��@-�^@-��@-a�@-:�@-+�@-+@-�@,��@,y>@+�W@+�@+O@+"�@*��@*��@*��@*q�@*ff@*R�@*3�@*�@)�>@)�@)�-@)��@)��@)m]@)e,@)S&@)-w@)!�@(�f@(��@(��@(�U@(��@(�@(�Y@(u�@(_@(7�@(*�@'�@&��@&� @&��@&C�@&J@%�.@%��@%�j@%�N@%ϫ@%��@%F@$��@$q@$M@#n/@#Y@#S@"�@"ں@"�'@"�<@"�h@"��@"��@"��@"��@"~�@"kQ@!��@!�@ �z@ c�@s@�2@��@^5@!�@�N@�^@c@T�@0�@�@��@�O@�e@�4@�z@q@1@�k@]�@ȴ@�@�z@o @\�@Y�@B�@+�@@;@�p@�@Xy@D�@6@�@��@�@��@�F@�q@��@�{@��@|�@s@�@�m@��@H�@.�@4@�@�@��@��@�"@�7@p�@hs@Dg@��@�@�)@��@�)@ѷ@��@�E@�)@�j@��@�&@�6@��@��@�	@P�@ i@��@�H@�s@�6@z@z@xl@q�@Ta@-@�)@�@��@X@�@�@��@z�@Ft@M@�@@ߤ@��@��@v�@c @M�@8�@1�@_@�C@zx@/@u�@%�@�@G@�]@	�@M@�@	�@� @��@qv@
�@
d�@
)�@	��@	�)@	�#@	��@	�9@	�z@	��@	��@	O�@	�@	;@�@�@r�@7�@!@��@��@��@��@l�@P�@6z@;d@9�@6z@o@�"@�M@��@�!@Ov@u@��@�Z@�Z@�)@�N@�@�7@|@w2@rG@hs@X@G�@+�@	l@��@|�@?�@M@��@��@�k@��@iD@U�@/�@�@�@͟@��@{�@5?@�@��@��@�=@}�@|@s�@1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BJ	BI7BIBI7BIBH�BIBH�BH�BH�BHKBHBF�BGBF�BF�BFYBF%BFBE�BE�BESBESBEBCaB>B!�B��B��B�B��B�B�qB��B~�Bz�BrGBo Bl"Bj�BjKBk6B`B\BX_BW?BQ�BLBESB@�B6�B1�B($B"NB,B	lB�XB��B��B�BѷBƨB�9B��B�sB�?Bv�Bm]B`�BG_B+QB�}B�0B��B��B��B�B�,B��B��B��B�:B��B�JB�3B}�B{dBw�Bt�BpoBiDB_�BI�B4�B"�B�BMB
=B�B
�B
�RB
�B
�B
�B
��B
ϑB
�\B
�pB
��B
��B
āB
ªB
��B
��B
��B
��B
��B
��B
��B
n}B
Z�B
P}B
D3B
BuB
@B
;�B
6FB
2�B
,qB
+kB
)�B
&�B
$&B
�B
�B
KB	�B	�B	��B	�4B	�B	��B	�B	��B	��B	��B	��B	��B	�B	|�B	x�B	v�B	u?B	rGB	p�B	oiB	l�B	h�B	g8B	e�B	c�B	_�B	[�B	ZQB	XB	V�B	V9B	R�B	Q�B	O�B	MB	F�B	C�B	B�B	A�B	A;B	>�B	<�B	;0B	:*B	7�B	4B	3�B	,=B	(XB	&LB	%�B	$�B	$�B	"�B	!-B	 �B	 \B	 \B	�B	�B	B	�B	�B	B	�B	�B	�B	B	4B	�B	bB	BB	�B	�B		�B	�B	%B	�B	�B	�B	�B	 �B	  B��B��B�B�*B��B��B�9B�B�B�B��B��B�!B��B��B�B�=B��B�B�B�B�eB��B�yB��B�$B�*B�sB��B�8B�B��B�mB�LB�B�2B�fB�B�B�B�RB��B�6B��B��B�sB�
B�B�KB��B�B�wB��B�B�%B��B�2B�B�$B�	B��B��B��B��B�B�B�jB��B��B��B�B��B�.B	 �B	�B	EB	
�B	�B	�B	6B	0B	�B	B	�B	bB	�B	�B	@B	�B	B	!�B	"�B	&�B	(�B	,=B	/�B	1�B	2�B	6FB	7�B	8�B	9$B	:DB	:�B	<B	?�B	A�B	A�B	B'B	B[B	BAB	F%B	GEB	I7B	KxB	KDB	LB	N<B	O�B	Q�B	RTB	R�B	R�B	S@B	U2B	W�B	X�B	[�B	^�B	c�B	lqB	p�B	qvB	s�B	w�B	~�B	~�B	HB	�B	�?B	��B	�6B	�.B	�uB	�+B	�!B	�bB	��B	��B	�B	�`B	�8B	�$B	��B	��B	��B	��B	�rB	��B	��B	�XB	��B	�^B	��B	�6B	�PB	��B	��B	�<B	�]B	��B	�B	�UB	�uB	��B	��B	��B	ĶB	�9B	��B	�_B	��B	��B	�"B	�(B	��B	��B	�B	ބB	��B	�HB	�nB	�fB	�B	�B	��B	��B	�[B	�aB	��B	�zB	��B	��B
 �B
oB
'B
'B
;B
�B
�B
<B
NB
�B
�B
�B
1B
�B
!|B
$�B
&fB
'�B
)_B
)yB
*B
,B
,�B
-B
-�B
0B
1vB
1�B
2aB
5B
7LB
:�B
?HB
AoB
BuB
E�B
GEB
G_B
G_B
G+B
GzB
H1B
J=B
N�B
OvB
P�B
Q�B
RTB
R�B
R�B
T�B
VSB
X+B
Y�B
Z�B
[WB
\�B
cTB
d@B
dB
d�B
f�B
i�B
lB
nB
o�B
qvB
t�B
v�B
y$B
{�B
��B
�MB
�%B
��B
�B
�B
��B
��B
�jB
�<B
�B
��B
��B
�}B
��B
��B
�:B
��B
�&B
��B
��B
�B
�)B
��B
�B
�IB
�B
��B
��B
�'B
��B
��B
�hB
�B
��B
�`B
��B
�RB
�
B
��B
�_B
�B
��B
�qB
�CB
�IB
�B
�B
�3B
�MB
��B
�9B
��B
�2B
��B
��B
�B
�RB
��B
��B
��B
�$B
�	B
��B
��B
��B
��B
��B
�VB
��B
� B
�UB
��B
�AB
�-B
��B
�MB
żB
�KB
ɺB
�=B
��B
��B
��B
�xB
�xB
˒B
��B
�pB
�BB
�BB
�BB
ϑB
ϫB
��B
�.B
ЗB
�4B
�hB
��B
�oB
�@B
�[B
ӏB
өB
�,B
��B
յB
�SB
�
B
خB
�1B
�eB
��B
�qB
��B
�CB
�xB
�IB
��B
�VB
ߤB
�vB
�B
��B
��B
�B
�HB
�|B
�B
�B
��B
�B
�2B
�B
�B
�XB
��B
�KB
�B
�B
�=B
�qB
�B
�B
�CB
�B
�B
� B
��B
�B
�B
�vB
�B
��B
�B
�B
�B
�-B
�B
�B
�3B
��B
�nB
��B
�%B
��B
�B
�+B
�FB
�B
�fB
��B
��B
�XB
��B
�*B
�DB
�DB
�DB
�^B
�DB
�^B
�^B
�xB
��B
��B
�JB
�B
��B
�]B
��B
��B iB �B�B�BB�B�B�BtBEB�B	lB	�B	�B
=B
	B
=B
�B�B�BdB~B~B�B�BB�B�B<BVB�B(BvB�B�B.BHB}B�B�B�B�B�B�BoB�B�BB&B@B[B[BFB�B�B�B9B�B�B$BYBYBsB�B�BBEByB�B�B�B�BBKBKB�B�B�B�BBB7B7BkB�B�B�B�BB�B�B�B�BBBB�B5B�B�B�B vB!�B!�B"B"NB"NB"�B"�B"�B"�B"�B"�B"�B"�B"�B#�B$�B%,B%`B'B'�B'�B(XB(�B)B)*B)�B)�B)�B*B*0B*�B*�B*�B*�B+B+�B,B,=B-)B./B.�B/B/B/B/OB/iB/iB/�B/�B0UB0�B0�B0�B0�B0�B1AB1vB1vB1vB1�B1�B1�B1�B1�B2GB2�B2�B3�B3�B3�B3�B3�B49B4TB4nB4nB4�B4�B4�B5ZB5%B5�B5�B5tB5�B5�B5ZB5tB5�B5�B6�B6�B6�B7B72B7�B7�B8B8B8B8�B8�B8�B8�B8�B8�B9	B9rB9�B9�B:*B:�B:�B;0B;JB;�B;�B<jB=�B=�B=�B=�B>B>B>(B>B>BB>�B>�B?B@iB@�B@�B@�B@�B@�B@�B@�B@�BABA BAoBB[BB�BCGBC�BC�BC�BC�BC�BC�BC�BC�BD�BD�BD�BD�BEmBE�BE�BE�BF?BF�BF�BF�BG+BG_BGzBG_BGzBG_BG�BG�BG�BG�BG�BH�BIBI7BI7BI7BIRBI�BI�BI�BJ	BJ	BJ	BJ#BJ=BJ=BJrBJ�BKBK^BK�BK�BLJBLdBL�BL�BL�BL�BMBMBM6BM�BM�BM�BN�BN�BOBOBOBBO\BO\BOBBPH4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  BJ	BI7BIBI7BIBH�BIBH�BH�BH�BHKBHBF�BGBF�BF�BFYBF%BFBE�BE�BESBESBEBCaB>B!�B��B��B�B��B�B�qB��B~�Bz�BrGBo Bl"Bj�BjKBk6B`B\BX_BW?BQ�BLBESB@�B6�B1�B($B"NB,B	lB�XB��B��B�BѷBƨB�9B��B�sB�?Bv�Bm]B`�BG_B+QB�}B�0B��B��B��B�B�,B��B��B��B�:B��B�JB�3B}�B{dBw�Bt�BpoBiDB_�BI�B4�B"�B�BMB
=B�B
�B
�RB
�B
�B
�B
��B
ϑB
�\B
�pB
��B
��B
āB
ªB
��B
��B
��B
��B
��B
��B
��B
n}B
Z�B
P}B
D3B
BuB
@B
;�B
6FB
2�B
,qB
+kB
)�B
&�B
$&B
�B
�B
KB	�B	�B	��B	�4B	�B	��B	�B	��B	��B	��B	��B	��B	�B	|�B	x�B	v�B	u?B	rGB	p�B	oiB	l�B	h�B	g8B	e�B	c�B	_�B	[�B	ZQB	XB	V�B	V9B	R�B	Q�B	O�B	MB	F�B	C�B	B�B	A�B	A;B	>�B	<�B	;0B	:*B	7�B	4B	3�B	,=B	(XB	&LB	%�B	$�B	$�B	"�B	!-B	 �B	 \B	 \B	�B	�B	B	�B	�B	B	�B	�B	�B	B	4B	�B	bB	BB	�B	�B		�B	�B	%B	�B	�B	�B	�B	 �B	  B��B��B�B�*B��B��B�9B�B�B�B��B��B�!B��B��B�B�=B��B�B�B�B�eB��B�yB��B�$B�*B�sB��B�8B�B��B�mB�LB�B�2B�fB�B�B�B�RB��B�6B��B��B�sB�
B�B�KB��B�B�wB��B�B�%B��B�2B�B�$B�	B��B��B��B��B�B�B�jB��B��B��B�B��B�.B	 �B	�B	EB	
�B	�B	�B	6B	0B	�B	B	�B	bB	�B	�B	@B	�B	B	!�B	"�B	&�B	(�B	,=B	/�B	1�B	2�B	6FB	7�B	8�B	9$B	:DB	:�B	<B	?�B	A�B	A�B	B'B	B[B	BAB	F%B	GEB	I7B	KxB	KDB	LB	N<B	O�B	Q�B	RTB	R�B	R�B	S@B	U2B	W�B	X�B	[�B	^�B	c�B	lqB	p�B	qvB	s�B	w�B	~�B	~�B	HB	�B	�?B	��B	�6B	�.B	�uB	�+B	�!B	�bB	��B	��B	�B	�`B	�8B	�$B	��B	��B	��B	��B	�rB	��B	��B	�XB	��B	�^B	��B	�6B	�PB	��B	��B	�<B	�]B	��B	�B	�UB	�uB	��B	��B	��B	ĶB	�9B	��B	�_B	��B	��B	�"B	�(B	��B	��B	�B	ބB	��B	�HB	�nB	�fB	�B	�B	��B	��B	�[B	�aB	��B	�zB	��B	��B
 �B
oB
'B
'B
;B
�B
�B
<B
NB
�B
�B
�B
1B
�B
!|B
$�B
&fB
'�B
)_B
)yB
*B
,B
,�B
-B
-�B
0B
1vB
1�B
2aB
5B
7LB
:�B
?HB
AoB
BuB
E�B
GEB
G_B
G_B
G+B
GzB
H1B
J=B
N�B
OvB
P�B
Q�B
RTB
R�B
R�B
T�B
VSB
X+B
Y�B
Z�B
[WB
\�B
cTB
d@B
dB
d�B
f�B
i�B
lB
nB
o�B
qvB
t�B
v�B
y$B
{�B
��B
�MB
�%B
��B
�B
�B
��B
��B
�jB
�<B
�B
��B
��B
�}B
��B
��B
�:B
��B
�&B
��B
��B
�B
�)B
��B
�B
�IB
�B
��B
��B
�'B
��B
��B
�hB
�B
��B
�`B
��B
�RB
�
B
��B
�_B
�B
��B
�qB
�CB
�IB
�B
�B
�3B
�MB
��B
�9B
��B
�2B
��B
��B
�B
�RB
��B
��B
��B
�$B
�	B
��B
��B
��B
��B
��B
�VB
��B
� B
�UB
��B
�AB
�-B
��B
�MB
żB
�KB
ɺB
�=B
��B
��B
��B
�xB
�xB
˒B
��B
�pB
�BB
�BB
�BB
ϑB
ϫB
��B
�.B
ЗB
�4B
�hB
��B
�oB
�@B
�[B
ӏB
өB
�,B
��B
յB
�SB
�
B
خB
�1B
�eB
��B
�qB
��B
�CB
�xB
�IB
��B
�VB
ߤB
�vB
�B
��B
��B
�B
�HB
�|B
�B
�B
��B
�B
�2B
�B
�B
�XB
��B
�KB
�B
�B
�=B
�qB
�B
�B
�CB
�B
�B
� B
��B
�B
�B
�vB
�B
��B
�B
�B
�B
�-B
�B
�B
�3B
��B
�nB
��B
�%B
��B
�B
�+B
�FB
�B
�fB
��B
��B
�XB
��B
�*B
�DB
�DB
�DB
�^B
�DB
�^B
�^B
�xB
��B
��B
�JB
�B
��B
�]B
��B
��B iB �B�B�BB�B�B�BtBEB�B	lB	�B	�B
=B
	B
=B
�B�B�BdB~B~B�B�BB�B�B<BVB�B(BvB�B�B.BHB}B�B�B�B�B�B�BoB�B�BB&B@B[B[BFB�B�B�B9B�B�B$BYBYBsB�B�BBEByB�B�B�B�BBKBKB�B�B�B�BBB7B7BkB�B�B�B�BB�B�B�B�BBBB�B5B�B�B�B vB!�B!�B"B"NB"NB"�B"�B"�B"�B"�B"�B"�B"�B"�B#�B$�B%,B%`B'B'�B'�B(XB(�B)B)*B)�B)�B)�B*B*0B*�B*�B*�B*�B+B+�B,B,=B-)B./B.�B/B/B/B/OB/iB/iB/�B/�B0UB0�B0�B0�B0�B0�B1AB1vB1vB1vB1�B1�B1�B1�B1�B2GB2�B2�B3�B3�B3�B3�B3�B49B4TB4nB4nB4�B4�B4�B5ZB5%B5�B5�B5tB5�B5�B5ZB5tB5�B5�B6�B6�B6�B7B72B7�B7�B8B8B8B8�B8�B8�B8�B8�B8�B9	B9rB9�B9�B:*B:�B:�B;0B;JB;�B;�B<jB=�B=�B=�B=�B>B>B>(B>B>BB>�B>�B?B@iB@�B@�B@�B@�B@�B@�B@�B@�BABA BAoBB[BB�BCGBC�BC�BC�BC�BC�BC�BC�BC�BD�BD�BD�BD�BEmBE�BE�BE�BF?BF�BF�BF�BG+BG_BGzBG_BGzBG_BG�BG�BG�BG�BG�BH�BIBI7BI7BI7BIRBI�BI�BI�BJ	BJ	BJ	BJ#BJ=BJ=BJrBJ�BKBK^BK�BK�BLJBLdBL�BL�BL�BL�BMBMBM6BM�BM�BM�BN�BN�BOBOBOBBO\BO\BOBBPH4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220905094316  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220905094318  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220905094318  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220905094319                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220905184323  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220905184323  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20220905100106                      G�O�G�O�G�O�                