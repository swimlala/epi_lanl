CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-10-20T00:35:17Z creation;2018-10-20T00:35:22Z conversion to V3.1;2019-12-19T07:30:01Z update;     
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tL   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �H   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �X   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �\   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �l   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �t   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20181020003517  20200115131518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              $A   JA  I2_0576_292                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @؊3�0*�1   @؊4m�5 @9>�Mj�d7��S��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A>ffA`  A�  A�  A�  A�  A���A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cc�fCe�fCg�fCj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�FfD�l�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�=q@��
@��
A�A<Q�A]�A}�A���A���A���A�A���A���A���A���Bz�Bz�Bz�Bz�B'z�B/z�B7z�B?z�BGz�BOz�BWz�B_z�Bgz�Boz�Bwz�Bz�B��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qBýqBǽqB˽qBϽqBӽqB׽qB۽qB߽qB�qB�qB�qB�qB�qB��qB��qB��qC޸C޸C޸C޸C	޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C!޸C#޸C%޸C'޸C)޸C+޸C-޸C/޸C1޸C3޸C5޸C7޸C9޸C;޸C=޸C?޸CA޸CC޸CE޸CG޸CI޸CK޸CM޸CO޸CQ޸CS޸CU޸CW޸CY޸C[޸C]޸C_޸Ca޸Cc�Ce�Cg�Ci޸Ck޸Cm޸Co޸Cq޸Cs޸Cu޸Cw޸Cy޸C{޸C}޸C޸C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\D w�D ��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D	w�D	��D
w�D
��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D w�D ��D!w�D!��D"w�D"��D#w�D#��D$w�D$��D%w�D%��D&w�D&��D'w�D'��D(w�D(��D)w�D)��D*w�D*��D+w�D+��D,w�D,��D-w�D-��D.w�D.��D/w�D/��D0w�D0��D1w�D1��D2w�D2��D3w�D3��D4w�D4��D5w�D5��D6w�D6��D7w�D7��D8w�D8��D9w�D9��D:w�D:��D;w�D;��D<w�D<��D=w�D=��D>w�D>��D?w�D?��D@w�D@��DAw�DA��DBw�DB��DCw�DC��DDw�DD��DEw�DE��DFw�DF��DGw�DG��DHw�DH��DIw�DI��DJw�DJ��DKw�DK��DLw�DL��DMw�DM��DNw�DN��DOw�DO��DPw�DP��DQw�DQ��DRw�DR��DSw�DS��DTw�DT��DUw�DU��DVw�DV��DWw�DW��DXw�DX��DYw�DY��DZw�DZ��D[w�D[��D\w�D\��D]w�D]��D^w�D^��D_w�D_��D`w�D`��Daw�Da��Dbw�Db��Dcw�Dc��Ddw�Dd��Dew�De��Dfw�Df��Dgw�Dg��Dhw�Dh��Diw�Di��Djw�Dj��Dkw�Dk��Dlw�Dl��Dmw�Dm��Dnw�Dn��Dow�Do��Dpw�Dp��Dqw�Dq��Drw�Dr��Dsw�Ds��Dtw�Dt��Duw�Du��Dvw�Dv��Dww�Dw��Dxw�Dx��Dyw�Dy��Dzw�Dz��D{w�D{��D|w�D|��D}w�D}��D~w�D~��Dw�D��D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�
D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D��
D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D��
D�;�D�{�D���D���D�;�D�
D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D»�D���D�;�D�{�Dû�D���D�;�D�{�DĻ�D���D�;�D�{�DŻ�D���D�;�D�{�Dƻ�D���D�;�D�{�Dǻ�D���D�;�D�{�DȻ�D���D�;�D�{�Dɻ�D���D�;�D�{�Dʻ�D���D�;�D�{�D˻�D���D�;�D�{�D̻�D���D�;�D�{�Dͻ�D���D�;�D�{�Dλ�D���D�;�D�{�Dϻ�D���D�;�D�{�Dл�D���D�;�D�{�Dѻ�D���D�;�D�{�Dһ�D���D�;�D�{�Dӻ�D���D�;�D�{�DԻ�D���D�;�D�{�Dջ�D���D�;�D�{�Dֻ�D���D�;�D�{�D׻�D���D�;�D�{�Dػ�D���D�;�D�{�Dٻ�D���D�;�D�{�Dڻ�D���D�;�D�{�Dۻ�D���D�;�D�{�Dܻ�D���D�;�D�{�Dݻ�D���D�;�D�{�D޻�D���D�;�D�{�D߻�D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�8�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D���D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�B=D�h�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��TA��/A��A���AμjAκ^AΩ�AΙ�AΏ\A΅A�dZA�E�A�-A��A��`A͸RAͧ�A͗�A�\)A�-A�oA�A���A̓uA�E�A˧�A��A�5?A���Aȥ�AǕ�A�1'A���A��A���A���A���A�$�A��7A���A�z�A�hsA��/A�/A��;A�G�A�=qA���A���A�VA���A�^5A�I�A�ĜA��A�|�A��!A��#A�jA��hA�$�A�n�A��jA��yA��A�bNA�-A��HA�K�A�ƨA�9XA���A�n�A��+A���A���A��A��A�(�A��9A�VA��A��#A�n�A�"�A���A�VA��A���A��A�VA�Q�A��mA���A�+A�x�A��A�ƨA���A�M�A�A~�9A}hsA{��Ay�;Aw�7AvffAt�DAs��As+Aq�-AodZAnAl��AlZAk
=Ah�Ag�-Afv�Ad�Ab��Aa��A`��A_�A_�7A^�A^VA]O�A\z�A[C�AYAY�AX��AXVAW��AT��AS�ASARJAQ�wAQ\)AQ
=AP�+APbAM�ALjAK�#AJ�/AI�AH�AG�AFZAE��AD�yAD�AD�\ADZAC�TAB��A@�9A?+A=x�A<��A;"�A9
=A7�A5�#A5�A4VA4  A3�^A3�7A25?A0-A.��A-;dA,��A,1A+�hA+VA*ĜA*�+A*I�A)��A)��A)dZA);dA(�DA'��A'S�A&�!A%&�A$ffA#hsA"�uA!A!%A M�A`BAC�A"�AVA
=AA�A�`A�A��AƨA�AA�A�A�A �A��A;dA�/A\)A+AE�A��A��A`BAC�A��A��AA�Al�A&�A��A\)A��A�FA
M�A�A��A�uA�-A
=A��A�mA�TA �A �A -@�K�@���@�-@�hs@��m@���@���@��#@�%@��9@�A�@��
@��
@���@�@�r�@��
@�P@�"�@���@�33@�M�@���@���@�h@�@�ƨ@�P@�t�@�K�@�
=@�{@�@ޟ�@�`B@�%@ܬ@�1'@ۮ@�33@ج@Ԭ@Ӿw@�K�@�-@�G�@�V@д9@Ϯ@�E�@��@˶F@˅@�l�@�K�@�
=@ʧ�@�v�@�hs@��@ǶF@�
=@�~�@�J@�z�@�~�@��j@��y@�b@�=q@��-@���@��P@�r�@�5?@�Ĝ@�Z@�9X@� �@��@���@�o@���@�(�@�S�@��H@���@�~�@�M�@�{@���@��-@�?}@��@��F@�K�@�;d@�J@��@��@���@�=q@�-@�$�@���@�?}@���@�1'@��@�@�p�@��`@��j@�l�@�=q@�?}@�"�@��@��D@�b@�C�@�33@�o@���@��y@���@�~�@�v�@�ff@�V@�-@��T@���@�hs@�X@�X@�X@�O�@�7L@�7L@�&�@��@��@���@���@�  @�\)@��@��@�^5@�&�@��;@�v�@�$�@�$�@�5?@��@�$�@��-@�/@���@��9@�b@�ƨ@��P@�S�@�+@���@��R@�{@��#@�hs@��@���@��9@�Q�@�1@���@��m@��;@���@�|�@��@��@���@���@���@��\@�ff@�J@�@�hs@���@�z�@�(�@��@\)@K�@;d@
=@~ȴ@~ff@~@}�@}O�@|��@|��@}/@}V@}�@}?}@|j@|��@}p�@}`B@|��@|I�@{�m@{�m@|I�@|��@}O�@}��@}��@}�h@}O�@|�/@|I�@|9X@|�@{�m@{C�@z�H@z~�@z��@z�!@z��@z�\@zn�@z=q@y��@y��@xĜ@xQ�@x1'@xb@x  @w��@wl�@w\)@w�@w�@v�y@vȴ@vȴ@vv�@t�@sdZ@r^5@r=q@r=q@r=q@rM�@rM�@r^5@rM�@rJ@q�@q��@qG�@p�9@o�;@n�R@m�@m��@m�@l�j@lj@l1@kt�@j�H@j�\@j=q@ihs@hr�@hb@g��@f@d�/@d(�@c�m@c�@co@b~�@b-@ax�@a�@a%@a%@`��@`�`@`�`@`�`@`��@`Q�@` �@_�@_K�@^�y@^5?@]p�@]/@\�j@\9X@["�@Z��@Z�\@ZM�@Z-@Y��@Yx�@X��@Xr�@XA�@X �@W��@W\)@V�@V�+@V5?@U�@U��@Up�@T�@T�@T��@T�D@T�D@T�D@T��@U`B@U�-@U��@Up�@T�@T�D@Tj@T�@S�F@SS�@R��@R��@R=q@Q��@Qx�@QX@Q&�@P�u@P1'@Pb@P  @O��@O�;@P �@O��@Pb@P1'@PQ�@PQ�@PQ�@PA�@O�;@O�w@O��@O|�@OK�@O�@Nȴ@N$�@M�h@M�@Mp�@M?}@L��@Lj@K�@K@J~�@JM�@I��@I��@I��@I��@Ix�@Ix�@Ihs@IG�@I7L@H��@Hr�@Hb@G�@G
=@F�R@Fff@F$�@E�@E@E�@Ep�@EO�@E/@EV@D�D@Dz�@Dz�@DZ@C��@B�H@B-@A�#@A�^@AX@@�u@?|�@>�@>5?@=��@=�h@=/@<9X@;�F@;S�@;33@:�!@:�@9��@9G�@9�@8�9@8�@8b@7�;@7��@7�w@7�w@7�@7+@7
=@6�R@6��@6v�@6ff@5�@5?}@4�@4I�@41@3ƨ@3t�@3S�@333@2�@2�!@2n�@2M�@2=q@2-@2�@2J@1�@1��@1��@1G�@1%@0Q�@/�@/�@/�P@/;d@.�@.v�@.E�@.$�@.{@-�@-�T@-@-@-�-@-�-@-p�@,��@,��@,z�@,z�@,j@,�@+�
@+ƨ@+��@+t�@+t�@+dZ@+dZ@+S�@+S�@+33@+"�@+"�@+o@+o@+"�@+"�@+"�@+"�@+"�@+"�@+o@+@+@+@+@*�@*�@*�H@*�H@*��@*�!@*^5@*M�@*=q@*-@)��@)��@)��@)x�@)7L@)%@(��@(�9@(�u@(r�@'�@'+@'�@&��@&�R@&v�@&ff@&E�@&5?@&{@%�T@%��@%�@%O�@%?}@%/@%/@%/@%/@%�@$��@$�j@$�D@$I�@#��@#�
@#��@#S�@"�@"~�@"=q@"-@"�@!�@!��@!G�@ ��@ �u@   @�w@|�@;d@�y@�y@�@�+@ff@V@$�@�@��@@�-@��@�h@p�@`B@O�@/@/@/@�@V@��@�j@(�@�m@�
@ƨ@��@��@�@t�@dZ@S�@33@o@@�@�\@J@�^@X@7L@&�@�@�`@��@�9@�u@Q�@1'@  @�;@�;@�w@��@�P@�P@�P@\)@K�@�@ȴ@5?@{@�@@�h@/@�D@I�@�@�
@t�@S�@S�@S�@S�@S�@S�@33@�@~�@�@hs@7L@�@�@�@%@��@��@�9@�@bN@ �@ �@b@  @��@��@|�@l�@K�@+@+@+@�@�y@��@5?@�@�@�@�T@@�h@p�@V@�@�@��@j@9X@�@�
@�@�@S�@"�@@
��@
J@	��@	x�@	G�@	%@	%@��@�`@bN@A�@1'@1'@ �@  @�P@K�@
=@�y@�y@�@ȴ@��@�+@5?@{@@�@@�-@�h@?}@?}@�@�@��@9X@�@��@�m@�m11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��TA��/A��A���AμjAκ^AΩ�AΙ�AΏ\A΅A�dZA�E�A�-A��A��`A͸RAͧ�A͗�A�\)A�-A�oA�A���A̓uA�E�A˧�A��A�5?A���Aȥ�AǕ�A�1'A���A��A���A���A���A�$�A��7A���A�z�A�hsA��/A�/A��;A�G�A�=qA���A���A�VA���A�^5A�I�A�ĜA��A�|�A��!A��#A�jA��hA�$�A�n�A��jA��yA��A�bNA�-A��HA�K�A�ƨA�9XA���A�n�A��+A���A���A��A��A�(�A��9A�VA��A��#A�n�A�"�A���A�VA��A���A��A�VA�Q�A��mA���A�+A�x�A��A�ƨA���A�M�A�A~�9A}hsA{��Ay�;Aw�7AvffAt�DAs��As+Aq�-AodZAnAl��AlZAk
=Ah�Ag�-Afv�Ad�Ab��Aa��A`��A_�A_�7A^�A^VA]O�A\z�A[C�AYAY�AX��AXVAW��AT��AS�ASARJAQ�wAQ\)AQ
=AP�+APbAM�ALjAK�#AJ�/AI�AH�AG�AFZAE��AD�yAD�AD�\ADZAC�TAB��A@�9A?+A=x�A<��A;"�A9
=A7�A5�#A5�A4VA4  A3�^A3�7A25?A0-A.��A-;dA,��A,1A+�hA+VA*ĜA*�+A*I�A)��A)��A)dZA);dA(�DA'��A'S�A&�!A%&�A$ffA#hsA"�uA!A!%A M�A`BAC�A"�AVA
=AA�A�`A�A��AƨA�AA�A�A�A �A��A;dA�/A\)A+AE�A��A��A`BAC�A��A��AA�Al�A&�A��A\)A��A�FA
M�A�A��A�uA�-A
=A��A�mA�TA �A �A -@�K�@���@�-@�hs@��m@���@���@��#@�%@��9@�A�@��
@��
@���@�@�r�@��
@�P@�"�@���@�33@�M�@���@���@�h@�@�ƨ@�P@�t�@�K�@�
=@�{@�@ޟ�@�`B@�%@ܬ@�1'@ۮ@�33@ج@Ԭ@Ӿw@�K�@�-@�G�@�V@д9@Ϯ@�E�@��@˶F@˅@�l�@�K�@�
=@ʧ�@�v�@�hs@��@ǶF@�
=@�~�@�J@�z�@�~�@��j@��y@�b@�=q@��-@���@��P@�r�@�5?@�Ĝ@�Z@�9X@� �@��@���@�o@���@�(�@�S�@��H@���@�~�@�M�@�{@���@��-@�?}@��@��F@�K�@�;d@�J@��@��@���@�=q@�-@�$�@���@�?}@���@�1'@��@�@�p�@��`@��j@�l�@�=q@�?}@�"�@��@��D@�b@�C�@�33@�o@���@��y@���@�~�@�v�@�ff@�V@�-@��T@���@�hs@�X@�X@�X@�O�@�7L@�7L@�&�@��@��@���@���@�  @�\)@��@��@�^5@�&�@��;@�v�@�$�@�$�@�5?@��@�$�@��-@�/@���@��9@�b@�ƨ@��P@�S�@�+@���@��R@�{@��#@�hs@��@���@��9@�Q�@�1@���@��m@��;@���@�|�@��@��@���@���@���@��\@�ff@�J@�@�hs@���@�z�@�(�@��@\)@K�@;d@
=@~ȴ@~ff@~@}�@}O�@|��@|��@}/@}V@}�@}?}@|j@|��@}p�@}`B@|��@|I�@{�m@{�m@|I�@|��@}O�@}��@}��@}�h@}O�@|�/@|I�@|9X@|�@{�m@{C�@z�H@z~�@z��@z�!@z��@z�\@zn�@z=q@y��@y��@xĜ@xQ�@x1'@xb@x  @w��@wl�@w\)@w�@w�@v�y@vȴ@vȴ@vv�@t�@sdZ@r^5@r=q@r=q@r=q@rM�@rM�@r^5@rM�@rJ@q�@q��@qG�@p�9@o�;@n�R@m�@m��@m�@l�j@lj@l1@kt�@j�H@j�\@j=q@ihs@hr�@hb@g��@f@d�/@d(�@c�m@c�@co@b~�@b-@ax�@a�@a%@a%@`��@`�`@`�`@`�`@`��@`Q�@` �@_�@_K�@^�y@^5?@]p�@]/@\�j@\9X@["�@Z��@Z�\@ZM�@Z-@Y��@Yx�@X��@Xr�@XA�@X �@W��@W\)@V�@V�+@V5?@U�@U��@Up�@T�@T�@T��@T�D@T�D@T�D@T��@U`B@U�-@U��@Up�@T�@T�D@Tj@T�@S�F@SS�@R��@R��@R=q@Q��@Qx�@QX@Q&�@P�u@P1'@Pb@P  @O��@O�;@P �@O��@Pb@P1'@PQ�@PQ�@PQ�@PA�@O�;@O�w@O��@O|�@OK�@O�@Nȴ@N$�@M�h@M�@Mp�@M?}@L��@Lj@K�@K@J~�@JM�@I��@I��@I��@I��@Ix�@Ix�@Ihs@IG�@I7L@H��@Hr�@Hb@G�@G
=@F�R@Fff@F$�@E�@E@E�@Ep�@EO�@E/@EV@D�D@Dz�@Dz�@DZ@C��@B�H@B-@A�#@A�^@AX@@�u@?|�@>�@>5?@=��@=�h@=/@<9X@;�F@;S�@;33@:�!@:�@9��@9G�@9�@8�9@8�@8b@7�;@7��@7�w@7�w@7�@7+@7
=@6�R@6��@6v�@6ff@5�@5?}@4�@4I�@41@3ƨ@3t�@3S�@333@2�@2�!@2n�@2M�@2=q@2-@2�@2J@1�@1��@1��@1G�@1%@0Q�@/�@/�@/�P@/;d@.�@.v�@.E�@.$�@.{@-�@-�T@-@-@-�-@-�-@-p�@,��@,��@,z�@,z�@,j@,�@+�
@+ƨ@+��@+t�@+t�@+dZ@+dZ@+S�@+S�@+33@+"�@+"�@+o@+o@+"�@+"�@+"�@+"�@+"�@+"�@+o@+@+@+@+@*�@*�@*�H@*�H@*��@*�!@*^5@*M�@*=q@*-@)��@)��@)��@)x�@)7L@)%@(��@(�9@(�u@(r�@'�@'+@'�@&��@&�R@&v�@&ff@&E�@&5?@&{@%�T@%��@%�@%O�@%?}@%/@%/@%/@%/@%�@$��@$�j@$�D@$I�@#��@#�
@#��@#S�@"�@"~�@"=q@"-@"�@!�@!��@!G�@ ��@ �u@   @�w@|�@;d@�y@�y@�@�+@ff@V@$�@�@��@@�-@��@�h@p�@`B@O�@/@/@/@�@V@��@�j@(�@�m@�
@ƨ@��@��@�@t�@dZ@S�@33@o@@�@�\@J@�^@X@7L@&�@�@�`@��@�9@�u@Q�@1'@  @�;@�;@�w@��@�P@�P@�P@\)@K�@�@ȴ@5?@{@�@@�h@/@�D@I�@�@�
@t�@S�@S�@S�@S�@S�@S�@33@�@~�@�@hs@7L@�@�@�@%@��@��@�9@�@bN@ �@ �@b@  @��@��@|�@l�@K�@+@+@+@�@�y@��@5?@�@�@�@�T@@�h@p�@V@�@�@��@j@9X@�@�
@�@�@S�@"�@@
��@
J@	��@	x�@	G�@	%@	%@��@�`@bN@A�@1'@1'@ �@  @�P@K�@
=@�y@�y@�@ȴ@��@�+@5?@{@@�@@�-@�h@?}@?}@�@�@��@9X@�@��@�m@�m11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�)B�)B�B�B�B�B�
B�B�
B��B��B��B��B��B�
B�fB�B�B��B��B��B��B  B��B��BBDB�B�B �B�B�B�B(�B%�B�B+B+B0!BhBO�BbNBYBH�B<jBE�BM�BO�BJ�BD�B8RB%�B!�BJB�B�B�B�
B��B�BȴBB�9B��B�'B�B��B��B�\B�=B~�Bv�BbNB8RB9XB8RB+B�B�B�B�BuBB
�B
��B
�5B
�B
��B
�wB
�B
�B
��B
��B
��B
�oB
�DB
�oB
�\B
�%B
z�B
n�B
\)B
R�B
>wB
1'B
2-B
'�B
)�B
#�B
�B	��B	��B	��B	��B	�BB	��B	ƨB	��B	�'B	�B	�!B	�'B	�9B	�9B	�B	��B	��B	��B	�bB	�B	�\B	�+B	�B	r�B	Q�B	YB	cTB	`BB	dZB	cTB	_;B	YB	O�B	7LB	1'B	7LB	+B	�B	�B	�B	�B	�B	�B	�B	�B	�B	JB��B�fB�#B��B�BB��B�wBŢB�^B��BƨB��BȴBB�B��B��B��B�!B�B�B�B�B�B�B��B��B��B��B��B�PB�uB�7Bu�B�By�Bz�B}�Bz�B|�Bw�B�7B�1B�7B�7B�+B�B�B|�Bp�B^5BC�BN�BR�BcTBffBe`B_;BZBI�B?}BK�BYBW
BXBXBT�BO�BM�BF�BJ�BD�B6FB=qB33B&�B'�B%�B'�B-B-B-B%�B�B"�B0!B/B-B.B.B&�B�BuB�B&�B'�B.B,B,B-B'�B�BhB#�B%�B �B�BVB!�B(�B(�B&�B �B!�B'�B'�B"�B�B
=B�B��B�B#�B!�B�B�BhBB�B�B$�B�B!�B&�B"�B�B�B �B�B/B1'B0!B.B-B,B%�B$�B-B+B+B'�B�B�B�B!�B"�B(�B8RB5?B-B#�B0!B>wBL�BP�BO�BM�BK�BI�BF�BG�BP�BYB\)B\)B]/B]/B^5B\)B[#B\)B[#BbNBffB^5B_;BffBiyBt�By�Bx�Bv�Bq�Bu�Bt�Bu�Br�B~�B�B�Bz�B}�B�B�B�VB��B�B�B�XB�XB�dB�jB�jB�wBÖBĜBĜBĜBĜBȴB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�mB�B�B�fB�fB�yB�B��B��B��B��B�B�B��B��B��B��B��B	B	B	B	%B	%B	bB	hB	�B	�B	�B	�B	#�B	+B	.B	/B	/B	33B	49B	?}B	A�B	C�B	C�B	D�B	C�B	C�B	E�B	F�B	H�B	J�B	O�B	S�B	VB	YB	YB	YB	ZB	ZB	[#B	\)B	_;B	`BB	bNB	ffB	iyB	k�B	o�B	m�B	p�B	v�B	s�B	q�B	s�B	v�B	{�B	� B	�B	�7B	�=B	�=B	�=B	�=B	�DB	�JB	�\B	�\B	�VB	�VB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�3B	�-B	�-B	�3B	�!B	�B	�-B	�dB	��B	ÖB	ÖB	ÖB	ĜB	ĜB	ÖB	B	ÖB	ÖB	B	B	B	ÖB	ǮB	��B	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�/B	�;B	�BB	�BB	�BB	�BB	�BB	�;B	�;B	�BB	�;B	�5B	�BB	�BB	�HB	�`B	�fB	�fB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
1B
+B
+B
DB
DB
DB
	7B
DB
\B
bB
bB
hB
{B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
!�B
!�B
"�B
#�B
"�B
"�B
"�B
!�B
$�B
$�B
#�B
 �B
"�B
$�B
'�B
(�B
&�B
&�B
%�B
)�B
+B
,B
,B
+B
)�B
,B
-B
/B
-B
.B
/B
0!B
1'B
1'B
2-B
1'B
33B
49B
49B
49B
33B
1'B
49B
33B
49B
49B
33B
2-B
1'B
2-B
5?B
6FB
6FB
6FB
7LB
7LB
7LB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
8RB
8RB
7LB
8RB
7LB
9XB
:^B
;dB
:^B
;dB
;dB
>wB
>wB
?}B
?}B
?}B
?}B
@�B
@�B
?}B
>wB
>wB
?}B
A�B
B�B
A�B
@�B
A�B
C�B
C�B
C�B
D�B
E�B
E�B
E�B
D�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
E�B
E�B
F�B
F�B
G�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
H�B
G�B
G�B
F�B
E�B
J�B
J�B
I�B
I�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
L�B
K�B
K�B
K�B
K�B
L�B
K�B
K�B
J�B
J�B
K�B
L�B
L�B
K�B
K�B
J�B
J�B
J�B
J�B
K�B
L�B
L�B
L�B
N�B
M�B
L�B
N�B
N�B
M�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
O�B
N�B
P�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
R�B
Q�B
Q�B
S�B
S�B
VB
W
B
W
B
W
B
W
B
W
B
W
B
W
B
XB
XB
YB
YB
YB
YB
ZB
ZB
YB
YB
YB
YB
XB
XB
[#B
[#B
[#B
[#B
ZB
ZB
\)B
]/B
]/B
]/B
_;B
`BB
`BB
`BB
`BB
`BB
_;B
^5B
]/B
^5B
_;B
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
e`B
e`B
dZB
dZB
dZB
e`B
e`B
e`B
e`B
ffB
ffB
e`B
e`B
dZB
dZB
ffB
hsB
gmB
gmB
gmB
ffB
gmB
ffB
ffB
iyB
iyB
hsB
hsB
hsB
hsB
iyB
jB
jB
jB
iyB
iyB
hsB
iyB
l�B
l�B
l�B
m�B
m�B
m�B
k�B
n�B
n�B
n�B
n�B
m�B
l�B
m�B
n�B
o�B
p�B
p�B
p�B
p�B
o�B
o�B
p�B
q�B
q�B
p�B
q�B
q�B
p�B
r�B
r�B
q�B
q�B
q�B
s�B
s�B
t�B
t�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�]B�)B�KB�B�+B�+B�$B�EB�$B�gB�MB�2B�,B�aB׍B�B��B�'B�2B�"B�HB��B �B��B��B�B�B�B7B#nBVB�B%FB/B,�B 'B2�B2�B6�B1BQ�Bc�B[#BK�B?�BG�BN�BPHBK�BE�B9�B(XB#�BBB�GB�B��B�B��B�BʦB�gB�`B��B��B��B��B�B��B�xB�4Bx8Bd�B<�B;B:B-)B!�B �B�B7BaB�B
��B
өB
�!B
��B
�hB
�iB
�'B
��B
��B
��B
��B
�B
�~B
��B
��B
�+B
|B
p;B
^B
UB
AB
3�B
3�B
)�B
+B
$�B
sB	��B	��B	�B	��B	�B	�@B	ȀB	�jB	�B	��B	��B	�|B	�%B	��B	��B	��B	�;B	��B	� B	��B	�B	��B	��B	tTB	U2B	Z�B	d@B	abB	d�B	c�B	_�B	Y�B	QNB	:*B	2�B	88B	,qB	!�B	kB	�B	�B	�B	�B	�B	�B	$B	PB��B�*B�IB�
B�B�@B�;B�_B��BуB�1B�BB�7B�GB�B�WB��B��B��B�B��B��B��B��B��B��B�sB�fB�@B��B�pB�B�XBxB�MB{JB|6BB|B}�By	B�RB��B�lB�lB�_B�MB�AB}<Bq[B_�BF�BP�BT{Bc�BgBfB`BB[=BK�BBABMBY�BW�BXyBXyBU�BP�BN�BG�BKxBE�B8B>wB4�B(�B)�B'�B)�B.cB.B-�B'8BB$&B0�B/�B-�B.�B.�B'�B B�B�B'�B(�B.}B,qB,WB-)B(XB�B�B$ZB&LB!|B�B�B"hB)*B)_B'mB!�B"hB(>B($B#:BBxB�B��B+B$&B"NB \BQB:BB�`BxB%FB�B"�B'RB#nB�B�B!�B �B/OB1[B0UB.cB-wB,�B&�B%�B-�B+�B+�B(�B!-B/B!-B#nB$�B*B8�B6B.}B%�B1�B?}BMBQ BP.BN<BLJBJXBG�BIBQ�BYB\]B\�B]~B]dB^�B\xB[�B\�B[�Bb�Bf�B_VB`'Bg8BjKBu%BzBy$Bw2BraBvFBuZBvzBs�BcB��B�{B|B~�B�B��B��B�KB�qB��B�XB��B��B��B��B��BðB��B��B��B��B�B�B�B� B�B�&B�&B�B�,B�,B�B� B�hB�hBٚB�B��B�"B�mB�mB�B�B��B��B��B��B�3B�'B�B�B�ZB�6B�HB	AB	MB	SB	�B	�B	�B	�B	�B	�B	�B	!B	$&B	+6B	./B	/OB	/�B	3�B	4�B	?�B	A�B	C�B	C�B	D�B	C�B	C�B	E�B	F�B	I7B	KDB	P.B	T,B	V9B	Y1B	Y1B	Y1B	ZQB	ZQB	[WB	\xB	_VB	`vB	bhB	fLB	i�B	k�B	o�B	m�B	p�B	v�B	s�B	rB	tB	v�B	|B	� B	�B	�B	�XB	�XB	�rB	��B	�xB	��B	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�&B	�8B	�B	�B	�6B	�QB	�)B	�UB	�[B	�3B	�GB	�GB	�hB	��B	��B	��B	��B	��B	ÖB	ðB	ÖB	ĜB	ĶB	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�	B	�B	�"B	�"B	�"B	�.B	�.B	�<B	�BB	�4B	�NB	̈́B	�hB	�SB	�KB	�eB	�eB	�eB	�QB	چB	�dB	�;B	�BB	�\B	�\B	�BB	�\B	�pB	�VB	�vB	�pB	ޞB	��B	�B	�B	�zB	�B	��B	��B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	�B	�B	�	B	��B	�B	�B	��B
 �B
�B
3B
3B
MB
gB
9B
YB
?B
tB
tB
YB
KB
zB
zB
^B
xB
^B
	�B
�B
vB
}B
�B
hB
{B
�B
mB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
!�B
!�B
#B
#�B
#B
#B
#B
"B
$�B
%B
$B
!-B
# B
%,B
($B
)*B
'8B
'RB
&fB
*KB
+QB
,=B
,=B
+QB
*KB
,WB
-CB
/OB
-]B
.cB
/OB
0UB
1[B
1vB
2aB
1vB
3MB
4TB
49B
4TB
3hB
1vB
4nB
3hB
4nB
4TB
3hB
2aB
1�B
2aB
5tB
6zB
6zB
6`B
7fB
7�B
7�B
8�B
8�B
9rB
9XB
9rB
9XB
9�B
9�B
8lB
8�B
7�B
8�B
7�B
9rB
:xB
;�B
:�B
;B
;�B
>�B
>�B
?�B
?�B
?}B
?�B
@�B
@�B
?�B
>�B
>�B
?�B
A�B
B�B
A�B
@�B
A�B
C�B
C�B
C�B
D�B
E�B
E�B
E�B
D�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
E�B
E�B
F�B
F�B
G�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
H�B
G�B
G�B
F�B
FB
J�B
J�B
I�B
I�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
MB
M�B
M�B
M�B
M�B
M�B
M�B
L�B
K�B
K�B
K�B
K�B
L�B
K�B
K�B
KB
KB
K�B
L�B
L�B
K�B
K�B
KB
J�B
KB
KB
K�B
L�B
L�B
MB
N�B
NB
MB
N�B
N�B
NB
N�B
O�B
O�B
O�B
O�B
O�B
PB
O�B
P�B
Q B
Q B
Q B
Q B
Q B
QB
O�B
OB
Q B
SB
SB
S&B
SB
S�B
TB
S�B
TB
TB
T,B
TB
S&B
R:B
R:B
T,B
T,B
VB
W
B
W$B
W$B
W$B
W?B
W$B
W$B
XEB
X+B
YKB
YB
Y1B
YKB
Z7B
ZB
YB
YKB
Y1B
YKB
X_B
XEB
[#B
[=B
[WB
[WB
ZkB
ZkB
\]B
]IB
]IB
]dB
_VB
`\B
`\B
`\B
`\B
`BB
_pB
^jB
]~B
^�B
_�B
b�B
cnB
cTB
cnB
cnB
cTB
c�B
cnB
c�B
c�B
c�B
e`B
e`B
d�B
d�B
d�B
ezB
ezB
ezB
e�B
f�B
f�B
e�B
e�B
d�B
d�B
f�B
hsB
gmB
gmB
g�B
f�B
g�B
f�B
f�B
i�B
i�B
h�B
h�B
h�B
h�B
i�B
j�B
j�B
j�B
i�B
i�B
h�B
i�B
l�B
l�B
l�B
m�B
m�B
m�B
k�B
n�B
n�B
n�B
n�B
m�B
l�B
m�B
n�B
o�B
p�B
p�B
p�B
p�B
o�B
o�B
p�B
q�B
q�B
p�B
q�B
q�B
p�B
r�B
r�B
q�B
q�B
q�B
s�B
s�B
t�B
t�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<(�^<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.13(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201810240035192018102400351920181024003519201810240200152018102402001520181024020015201810250023422018102500234220181025002342  JA  ARFMdecpA19c                                                                20181020093515  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181020003517  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181020003521  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181020003521  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181020003522  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181020003522  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181020003522  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181020003522  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20181020003522  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181020003522                      G�O�G�O�G�O�                JA  ARUP                                                                        20181020005619                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181020153442  CV  JULD            G�O�G�O�F�Q�                JM  ARCAJMQC2.0                                                                 20181023153519  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181023153519  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20181023170015  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181024152342  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131518                      G�O�G�O�G�O�                