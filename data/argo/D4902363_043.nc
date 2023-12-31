CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-10-02T21:35:12Z creation;2016-10-02T21:35:14Z conversion to V3.1;2019-12-19T08:28:54Z update;     
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
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pL   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t4   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ݬ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �0   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �4   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �8   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �<Argo profile    3.1 1.2 19500101000000  20161002213512  20200115111518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               +A   JA  I2_0576_043                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @��r��Q�1   @��s�8�@;Ov_خ�d{����1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D�fDfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4�fD5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DBfDB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�<�DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D���D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�,�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�p�@��
@��
A�A=�A]�A}�A���A���A���A���A���A���A���A���Bz�Bz�Bz�Bz�B'z�B/z�B7z�B?z�BGz�BOz�BWz�B_z�Bgz�Boz�Bwz�Bz�B��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qBýqBǽqB˽qBϽqBӽqB׽qB۽qB߽qB�qB�qB�qB�qB�qB��qB��qB��qC޸C޸C޸C޸C	޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C!޸C#޸C%޸C'޸C)޸C+޸C-޸C/޸C1޸C3޸C5޸C7޸C9޸C;޸C=޸C?޸CA޸CC޸CE޸CG޸CI޸CK޸CM޸CO޸CQ޸CS޸CU޸CW޸CY޸C[޸C]޸C_޸Ca޸Cc޸Ce޸Cg޸Ci޸Ck޸Cm޸Co޸Cq޸Cs޸Cu޸Cw޸Cy޸C{޸C}޸C޸C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\D w�D ��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D	w�D	��D
w�D
��D~D�Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D w�D ��D!w�D!��D"w�D"��D#w�D#��D$w�D$��D%w�D%��D&w�D&��D'w�D'��D(w�D(��D)w�D)��D*w�D*��D+w�D+��D,w�D,��D-w�D-��D.w�D.��D/w�D/��D0w�D0��D1w�D1��D2w�D2��D3w�D3��D4~D4��D5w�D5��D6w�D6��D7w�D7��D8w�D8��D9w�D9��D:w�D:��D;w�D;��D<w�D<��D=w�D=��D>w�D>��D?w�D?��D@w�D@��DAw�DA�DBw�DB��DCw�DC��DDw�DD��DEw�DE��DFw�DF��DGw�DG��DHw�DH��DIw�DI��DJw�DJ��DKw�DK��DLw�DL��DMw�DM��DNw�DN��DOw�DO��DPw�DP��DQw�DQ��DRw�DR��DSw�DS��DTw�DT��DUw�DU��DVw�DV��DWw�DW��DXw�DX��DYw�DY��DZw�DZ��D[w�D[��D\w�D\��D]w�D]��D^w�D^��D_w�D_��D`w�D`��Daw�Da��Dbw�Db��Dcw�Dc��Ddw�Dd��Dew�De��Dfw�Df��Dgw�Dg��Dhw�Dh��Diw�Di��Djw�Dj��Dkw�Dk��Dlw�Dl��Dmw�Dm��Dnw�Dn��Dow�Do��Dpw�Dp��Dqw�Dq��Drw�Dr��Dsw�Ds��Dtw�Dt��Duw�Du��Dvw�Dv��Dww�Dw��Dxw�Dx��Dyw�Dy��Dzw�Dz��D{w�D{��D|w�D|��D}w�D}��D~w�D~��Dw�D��D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D»�D���D�;�D�{�Dû�D���D�;�D�{�DĻ�D���D�;�D�{�DŻ�D���D�;�D�{�Dƻ�D���D�;�D�{�Dǻ�D���D�;�D�{�DȻ�D���D�;�D�{�Dɻ�D���D�;�D�{�Dʻ�D���D�;�D�{�D˻�D���D�;�D�{�D̻�D���D�;�D�{�Dͻ�D���D�;�D�{�Dλ�D���D�;�D�{�Dϻ�D���D�;�D�{�Dл�D���D�;�D�{�Dѻ�D���D�;�D�{�Dһ�D���D�;�D�{�Dӻ�D���D�;�D�{�DԻ�D���D�8�D�{�Dջ�D���D�;�D�{�Dֻ�D���D�;�D�{�D׻�D���D�;�D�{�Dػ�D���D�;�D�{�Dٻ�D���D�;�D�{�Dڻ�D���D�;�D�{�Dۻ�D���D�;�D�{�Dܻ�D���D�;�D�{�Dݻ�D���D�;�D�{�D޻�D���D�;�D�{�D߻�D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�x�D縤D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D���D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D��
D�(�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A� �A�"�A�"�A� �A� �A� �A�"�A� �A�"�A�"�A�$�A�&�A�&�A�"�A�oA�33A�  A�A��-A��A��A�G�A�M�A�&�A��9A�r�A�C�A���A��9A�VA��DA�r�A��A�33A�hsA�\)A���A��A�33A�XA�x�A���A���A��A��FA���A�JA��yA���A�;dA�;dA�O�A�XA�9XA�O�A��hA��yA��+A�7LA�%A�x�A�|�A��A���A��A��A��A�A�A�A�oA�XA��yAG�A~ �A|ZAzz�Ax9XAw%Atr�At{As��Ar��Aq�Ap��Ap9XAo��AnE�Al�Ak�TAk�Ai��Ag�TAf�yAe��AdȴAc��AcG�Ac"�AcAb��AbE�AaoA_�7A^ �A]�PA\�jA[�7AY�AX�RAV��AT^5AR=qAQK�APz�AO�AN=qAM��AM��AMoAM
=AM33AMK�AM?}AL�\AL  AK��AKl�AKS�AKAI�AIAH�+AG�AG�AFE�AEx�ADĜADAC��ACO�ABr�AA�wA@��A?�#A?hsA>��A=��A<r�A;\)A:^5A8ȴA8  A7oA5/A4=qA3�A3��A3K�A2��A1��A1K�A1%A0��A0��A0^5A/�A/7LA.��A.v�A.�A,��A+K�A*M�A)�hA);dA(ȴA(ZA'�A%�A%�A#��A#G�A"�DA"{A!t�A �jA $�A��A��AbNAJA�PA\)A?}An�A�-A�Al�A"�A��AȴA�DA^5A�FA9XA��AhsAv�A;dA��AI�A�-AQ�AG�A�Ar�AVA  At�A��A�wAE�AA��A
��A
�A	�A�;AhsA�RA�A��A�\A��AI�A�A ~�A J@�
=@�%@�+@�ff@�@�&�@�A�@��@�{@���@��@��9@��u@�1'@�;d@�E�@�V@�F@���@�5?@�?}@���@�(�@�-@���@�bN@�w@���@��`@���@���@�1@ݺ^@�dZ@�M�@٩�@�hs@���@ו�@�V@Չ7@�/@ҸR@�x�@���@θR@ͩ�@�dZ@�=q@��@��@��@���@ă@���@Ý�@�C�@�@�=q@�5?@��T@�bN@���@��@�1@�1@��@�C�@�5?@��/@��
@�V@��9@� �@�V@�V@� �@�5?@��h@�hs@�%@�1'@�l�@��@�@�ȴ@�-@�x�@�  @�;d@��!@�~�@�=q@��@�z�@��@���@�^5@�E�@�$�@��@��D@�t�@�o@�ȴ@��+@���@�%@��@�1'@��w@�"�@�n�@�x�@�X@��@�Z@��F@�dZ@��@�=q@��T@���@�A�@���@�;d@��H@�v�@��7@�%@���@��@�;d@�"�@�ȴ@�=q@��h@�7L@��@�=q@��@���@���@���@��7@��@�/@��7@��h@�x�@�O�@��-@�hs@�?}@��@���@�Z@�Z@��w@���@�=q@�J@���@�@�7L@��9@��@�9X@��@�l�@�;d@�"�@��@��@��@��R@��+@�M�@�-@�{@���@�?}@�Ĝ@���@��u@��u@��D@��@��@�j@��@��m@��;@��w@��@���@�l�@��!@�ff@�^5@�^5@�^5@�^5@�~�@�V@��#@��-@���@���@���@�hs@�V@���@���@�A�@�@�P@K�@�@~��@~�y@~�@~$�@}�@}p�@}/@|��@|�@|1@{t�@{"�@{@z�H@z�!@z=q@y��@y��@yX@yG�@y&�@x��@x��@x�9@x�u@xb@w�;@w��@w�P@w;d@v�y@v�R@v��@v5?@uV@t�j@t��@tz�@t9X@t1@s��@st�@r�@rn�@r�@q��@q�7@qX@p�`@p�@pbN@p  @o;d@n��@nff@mO�@l�j@l�@m/@m�@m�@mV@mV@mV@l�j@lz�@lz�@lz�@lj@lZ@lI�@l1@k��@k�@kC�@ko@j��@j=q@ix�@i�@h�9@h�@g��@g
=@fȴ@f�+@f$�@e@eV@dj@c�m@c�
@cS�@b�!@b-@a��@a��@aX@`Ĝ@`r�@`bN@_�@_�@_��@_;d@_�@^ȴ@^v�@^V@^5?@]��@]�@\�/@\�D@\z�@\j@\Z@\Z@\I�@[��@[C�@Z�!@Z=q@ZJ@ZJ@Y��@Y��@Y&�@XQ�@W|�@W;d@V�@V�+@V$�@Up�@U�@UV@UV@T�@T��@T�j@Tz�@S��@Sƨ@S��@R��@R^5@R=q@R-@RJ@Q��@Q��@Q7L@P�`@P�u@PA�@P  @O�w@O\)@O
=@N�R@NE�@M��@M?}@L��@L(�@K��@K@J�@J�H@J�!@J�@IX@I�@H�`@H�9@H�u@H�@Hr�@HQ�@Hb@Hb@Hb@G�@G�;@G�@G+@F�R@FV@F$�@F$�@F$�@F{@E��@E��@E`B@D��@D�/@D�/@D�/@D�j@D�D@Dj@D9X@D(�@D(�@C��@C��@B�H@A��@Ahs@Ahs@AX@AG�@AG�@AG�@A&�@A%@@��@@Ĝ@@��@@�@@Q�@@  @?�;@?�w@?�@?�P@?;d@>�@>�+@>V@>$�@>$�@=�@=��@=�h@=�@=`B@=?}@<�j@<j@<Z@<Z@<I�@<I�@<(�@<�@;��@;ƨ@;��@;dZ@;@:�!@:~�@:M�@:=q@:-@:J@9x�@9&�@9%@8��@8 �@8  @7��@7|�@7;d@6��@5�T@5�h@5�@5`B@4��@4�D@4z�@4j@4(�@3�F@3��@3��@3�@3dZ@333@3@2~�@2J@2J@1��@1&�@1%@0�`@0��@0��@0�@0bN@0Q�@0A�@0 �@0 �@0b@0b@0  @/�;@/;d@.��@.v�@.5?@.{@-�T@-��@-�-@-�h@-O�@-/@-V@,��@,�@,j@,9X@,�@+��@+�@+"�@*��@*~�@*n�@*^5@*M�@)��@)hs@)G�@)�@(�`@(1'@'�w@'�@'
=@'
=@&�y@&�@&�@&�R@&��@&V@&$�@&@%��@%�h@%/@$�@$�@#��@#33@"��@"��@"~�@"~�@"M�@"J@!��@!7L@!�@ Ĝ@ bN@ A�@ A�@ b@�@�@�P@K�@�@
=@��@�@ȴ@��@ff@@��@O�@?}@�@�@�j@��@j@�m@t�@33@�H@�\@^5@�@�@hs@��@�9@bN@Q�@A�@ �@�@�;@��@��@�w@�w@�@��@;d@�y@��@V@V@5?@5?@$�@�@�h@O�@/@�@��@�@�@��@j@I�@�@1@1@1@�m@S�@�H@��@�\@^5@-@�@�@��@X@%@�9@bN@1'@  @��@�P@|�@l�@\)@K�@K�@;d@;d@+@�y@�@ȴ@�R@v�@$�@p�@/@�/@�@z�@I�@(�@�@��@�m@�m@��@t�@C�@"�@o@
��@
��@
�\@
M�@
�@	��@	�@	�^@	��@	x�@	hs@	�@�`@��@��@Ĝ@Ĝ@�9@�9@��@r�@A�@ �@�@�w@��@�P@|�@+@�R@ff@5?@@@��@�@`B@?}@V@��@��@�D@z�@z�@j@(�@��@�m@�
@�
@ƨ@��@t�@dZ@C�@o@�@��@�!@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A� �A�"�A�"�A� �A� �A� �A�"�A� �A�"�A�"�A�$�A�&�A�&�A�"�A�oA�33A�  A�A��-A��A��A�G�A�M�A�&�A��9A�r�A�C�A���A��9A�VA��DA�r�A��A�33A�hsA�\)A���A��A�33A�XA�x�A���A���A��A��FA���A�JA��yA���A�;dA�;dA�O�A�XA�9XA�O�A��hA��yA��+A�7LA�%A�x�A�|�A��A���A��A��A��A�A�A�A�oA�XA��yAG�A~ �A|ZAzz�Ax9XAw%Atr�At{As��Ar��Aq�Ap��Ap9XAo��AnE�Al�Ak�TAk�Ai��Ag�TAf�yAe��AdȴAc��AcG�Ac"�AcAb��AbE�AaoA_�7A^ �A]�PA\�jA[�7AY�AX�RAV��AT^5AR=qAQK�APz�AO�AN=qAM��AM��AMoAM
=AM33AMK�AM?}AL�\AL  AK��AKl�AKS�AKAI�AIAH�+AG�AG�AFE�AEx�ADĜADAC��ACO�ABr�AA�wA@��A?�#A?hsA>��A=��A<r�A;\)A:^5A8ȴA8  A7oA5/A4=qA3�A3��A3K�A2��A1��A1K�A1%A0��A0��A0^5A/�A/7LA.��A.v�A.�A,��A+K�A*M�A)�hA);dA(ȴA(ZA'�A%�A%�A#��A#G�A"�DA"{A!t�A �jA $�A��A��AbNAJA�PA\)A?}An�A�-A�Al�A"�A��AȴA�DA^5A�FA9XA��AhsAv�A;dA��AI�A�-AQ�AG�A�Ar�AVA  At�A��A�wAE�AA��A
��A
�A	�A�;AhsA�RA�A��A�\A��AI�A�A ~�A J@�
=@�%@�+@�ff@�@�&�@�A�@��@�{@���@��@��9@��u@�1'@�;d@�E�@�V@�F@���@�5?@�?}@���@�(�@�-@���@�bN@�w@���@��`@���@���@�1@ݺ^@�dZ@�M�@٩�@�hs@���@ו�@�V@Չ7@�/@ҸR@�x�@���@θR@ͩ�@�dZ@�=q@��@��@��@���@ă@���@Ý�@�C�@�@�=q@�5?@��T@�bN@���@��@�1@�1@��@�C�@�5?@��/@��
@�V@��9@� �@�V@�V@� �@�5?@��h@�hs@�%@�1'@�l�@��@�@�ȴ@�-@�x�@�  @�;d@��!@�~�@�=q@��@�z�@��@���@�^5@�E�@�$�@��@��D@�t�@�o@�ȴ@��+@���@�%@��@�1'@��w@�"�@�n�@�x�@�X@��@�Z@��F@�dZ@��@�=q@��T@���@�A�@���@�;d@��H@�v�@��7@�%@���@��@�;d@�"�@�ȴ@�=q@��h@�7L@��@�=q@��@���@���@���@��7@��@�/@��7@��h@�x�@�O�@��-@�hs@�?}@��@���@�Z@�Z@��w@���@�=q@�J@���@�@�7L@��9@��@�9X@��@�l�@�;d@�"�@��@��@��@��R@��+@�M�@�-@�{@���@�?}@�Ĝ@���@��u@��u@��D@��@��@�j@��@��m@��;@��w@��@���@�l�@��!@�ff@�^5@�^5@�^5@�^5@�~�@�V@��#@��-@���@���@���@�hs@�V@���@���@�A�@�@�P@K�@�@~��@~�y@~�@~$�@}�@}p�@}/@|��@|�@|1@{t�@{"�@{@z�H@z�!@z=q@y��@y��@yX@yG�@y&�@x��@x��@x�9@x�u@xb@w�;@w��@w�P@w;d@v�y@v�R@v��@v5?@uV@t�j@t��@tz�@t9X@t1@s��@st�@r�@rn�@r�@q��@q�7@qX@p�`@p�@pbN@p  @o;d@n��@nff@mO�@l�j@l�@m/@m�@m�@mV@mV@mV@l�j@lz�@lz�@lz�@lj@lZ@lI�@l1@k��@k�@kC�@ko@j��@j=q@ix�@i�@h�9@h�@g��@g
=@fȴ@f�+@f$�@e@eV@dj@c�m@c�
@cS�@b�!@b-@a��@a��@aX@`Ĝ@`r�@`bN@_�@_�@_��@_;d@_�@^ȴ@^v�@^V@^5?@]��@]�@\�/@\�D@\z�@\j@\Z@\Z@\I�@[��@[C�@Z�!@Z=q@ZJ@ZJ@Y��@Y��@Y&�@XQ�@W|�@W;d@V�@V�+@V$�@Up�@U�@UV@UV@T�@T��@T�j@Tz�@S��@Sƨ@S��@R��@R^5@R=q@R-@RJ@Q��@Q��@Q7L@P�`@P�u@PA�@P  @O�w@O\)@O
=@N�R@NE�@M��@M?}@L��@L(�@K��@K@J�@J�H@J�!@J�@IX@I�@H�`@H�9@H�u@H�@Hr�@HQ�@Hb@Hb@Hb@G�@G�;@G�@G+@F�R@FV@F$�@F$�@F$�@F{@E��@E��@E`B@D��@D�/@D�/@D�/@D�j@D�D@Dj@D9X@D(�@D(�@C��@C��@B�H@A��@Ahs@Ahs@AX@AG�@AG�@AG�@A&�@A%@@��@@Ĝ@@��@@�@@Q�@@  @?�;@?�w@?�@?�P@?;d@>�@>�+@>V@>$�@>$�@=�@=��@=�h@=�@=`B@=?}@<�j@<j@<Z@<Z@<I�@<I�@<(�@<�@;��@;ƨ@;��@;dZ@;@:�!@:~�@:M�@:=q@:-@:J@9x�@9&�@9%@8��@8 �@8  @7��@7|�@7;d@6��@5�T@5�h@5�@5`B@4��@4�D@4z�@4j@4(�@3�F@3��@3��@3�@3dZ@333@3@2~�@2J@2J@1��@1&�@1%@0�`@0��@0��@0�@0bN@0Q�@0A�@0 �@0 �@0b@0b@0  @/�;@/;d@.��@.v�@.5?@.{@-�T@-��@-�-@-�h@-O�@-/@-V@,��@,�@,j@,9X@,�@+��@+�@+"�@*��@*~�@*n�@*^5@*M�@)��@)hs@)G�@)�@(�`@(1'@'�w@'�@'
=@'
=@&�y@&�@&�@&�R@&��@&V@&$�@&@%��@%�h@%/@$�@$�@#��@#33@"��@"��@"~�@"~�@"M�@"J@!��@!7L@!�@ Ĝ@ bN@ A�@ A�@ b@�@�@�P@K�@�@
=@��@�@ȴ@��@ff@@��@O�@?}@�@�@�j@��@j@�m@t�@33@�H@�\@^5@�@�@hs@��@�9@bN@Q�@A�@ �@�@�;@��@��@�w@�w@�@��@;d@�y@��@V@V@5?@5?@$�@�@�h@O�@/@�@��@�@�@��@j@I�@�@1@1@1@�m@S�@�H@��@�\@^5@-@�@�@��@X@%@�9@bN@1'@  @��@�P@|�@l�@\)@K�@K�@;d@;d@+@�y@�@ȴ@�R@v�@$�@p�@/@�/@�@z�@I�@(�@�@��@�m@�m@��@t�@C�@"�@o@
��@
��@
�\@
M�@
�@	��@	�@	�^@	��@	x�@	hs@	�@�`@��@��@Ĝ@Ĝ@�9@�9@��@r�@A�@ �@�@�w@��@�P@|�@+@�R@ff@5?@@@��@�@`B@?}@V@��@��@�D@z�@z�@j@(�@��@�m@�
@�
@ƨ@��@t�@dZ@C�@o@�@��@�!@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�%B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�By�BQ�BE�B<jB,B"�BuB��B�fB�HB�B�BB�B��B��BŢB��B�!B��B��B��B�JBz�Bs�BcTBVBI�B=qB+B �BPB	7B�B�)BŢB�FB��B��B�DB{�Bm�BffBVBK�BH�B@�B'�B%B
�B
�;B
��B
ǮB
�RB
�3B
��B
��B
�uB
}�B
w�B
iyB
aHB
M�B
I�B
>wB
9XB
7LB
33B
1'B
/B
1'B
0!B
%�B
�B
{B
oB
DB	��B	�B	�fB	�5B	�
B	��B	��B	��B	��B	��B	B	�^B	�!B	�B	��B	��B	��B	�uB	�7B	r�B	^5B	S�B	J�B	B�B	9XB	49B	33B	5?B	A�B	H�B	M�B	]/B	\)B	ZB	YB	W
B	VB	S�B	M�B	F�B	B�B	?}B	9XB	5?B	0!B	-B	&�B	&�B	%�B	!�B	!�B	�B	�B	�B	�B	�B	VB	%B	B��B��B��B�B�sB�mB�fB�`B�TB�HB�5B�/B�)B�B�B�B�B��B��B��B��BĜB�}B�^B�XB�FB�9B�B��B��B��B��B��B��B��B�oB�oB�hB�VB�DB�DB�1B�%B�B�B~�B}�B}�B}�B|�B{�Bz�By�By�Bu�Br�Bq�Bp�Bk�Bk�BiyBgmBe`B_;B]/B[#B[#BZBXBW
BVBP�BO�BN�BM�BJ�BI�BF�BC�BC�B@�B?}B>wB;dB7LB8RB5?B33B1'B1'B-B-B,B+B)�B(�B'�B'�B&�B&�B%�B%�B%�B#�B#�B!�B �B�B�B�B�B�B�B�B�B�B�B�BuBoB{B\BVBVBPBPBPBPBJBDBPB
=BJB
=BDBbB\B\BoBuBoBoBoBoBuB�B{B{BuB{B�B�B�B�B�B�B�B�B�B#�B!�B!�B#�B#�B%�B+B-B,B,B.B/B/B/B0!B1'B2-B7LB9XB:^B:^B:^B<jB>wB>wB?}B@�B@�B@�BD�BG�BG�BI�BK�BL�BP�BQ�BS�BVBW
BZB]/B`BBaHBbNBffBhsBjBl�Bo�Bp�Bt�Bx�B{�B}�B�B�B�1B�=B�PB�bB�{B�{B��B��B��B��B��B�B�-B�qBɺBĜBŢBƨB��B��B��B��B�
B�BB�`B�`B�fB�B�B�B�B�B�B�B�B��B��B��B��B��B	  B	B	B	B	%B	1B		7B	
=B	DB	PB	VB	bB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	!�B	!�B	#�B	$�B	%�B	&�B	,B	-B	.B	1'B	49B	7LB	;dB	>wB	=qB	=qB	@�B	A�B	C�B	D�B	G�B	I�B	L�B	P�B	S�B	VB	W
B	XB	[#B	[#B	[#B	]/B	`BB	aHB	bNB	dZB	e`B	hsB	l�B	p�B	r�B	u�B	w�B	z�B	{�B	|�B	}�B	}�B	}�B	~�B	�B	�B	�B	�+B	�1B	�7B	�=B	�DB	�DB	�JB	�PB	�\B	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�-B	�'B	�!B	�'B	�3B	�LB	�XB	�qB	�wB	�}B	��B	��B	��B	��B	��B	��B	B	B	ÖB	ĜB	ƨB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�#B	�)B	�/B	�/B	�;B	�BB	�HB	�NB	�TB	�ZB	�fB	�fB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
%B
%B
%B
+B
1B
1B
	7B
	7B

=B

=B
DB
JB
JB
PB
PB
VB
\B
bB
hB
oB
oB
oB
oB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
+B
+B
+B
,B
,B
,B
,B
,B
,B
-B
.B
.B
/B
/B
/B
0!B
0!B
0!B
1'B
2-B
2-B
2-B
2-B
33B
33B
33B
33B
49B
49B
49B
49B
49B
5?B
5?B
5?B
6FB
6FB
6FB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
F�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
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
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
VB
VB
W
B
W
B
W
B
XB
XB
XB
XB
XB
XB
YB
YB
YB
YB
YB
YB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
\)B
\)B
\)B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
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
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
jB
jB
jB
jB
k�B
jB
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�%B�B�9B�B�B�9B�B�9B�9B�9B�B�SB�mB��B��B��B�fBY�BL�B?}B/�B'RB�B�B�B��BܬB��B�#BϑB̘B��B�B�GB�0B�B�jB��B}�BwLBe�BX_BL�BA;B-�B# B\BB��B�;B�B��B�qB�!B��B}�Bo�Bh�BV�BL�BJ�BD�B+B	RB
�B
�HB
�sB
�RB
��B
�tB
��B
�qB
�B
�B
zB
k�B
c�B
O�B
LJB
?.B
:*B
8lB
5%B
1�B
/�B
2-B
1�B
'�B
�B
�B
,B
jB	�PB	�'B	�B	�!B	��B	�4B	�BB	�\B	��B	�~B	āB	��B	�'B	�=B	��B	��B	��B	�B	�0B	u%B	_�B	UMB	LB	C�B	9�B	4�B	3�B	5tB	AoB	H�B	NVB	^B	\�B	Z�B	YeB	W�B	V�B	U�B	N�B	GzB	C{B	@�B	:xB	6FB	1B	-�B	'�B	'�B	'B	"�B	# B	�B	B	�B	�B	sB	�B	�B	�B�B�lB��B�B�B��B�8B�fB�tB��BޞBݲBܬBںB��B�BּBөB��B��B�jB��B�iB��B�B�fB�B��B�_B�RB��B��B�yB��B��B�[B�[B��B��B��B��B��B��B�?B��BcB~BB~wB~]B}qB|�B{B{0B{�Bv�BshBs3BrBl�Bl=Bj�Bi*Bf�B`'B]�B[�B[�B[	BYKBX�BW�BQhBP�BPHBN�BL0BKDBGzBD�BD�BA�BAUB@�B<B8lB9�B6+B49B2�B2GB-�B-�B,�B+�B+B)�B(sB(sB'RB'RB&fB&�B&�B$�B$�B"hB!|B �BVB�BB�B7B1B_B�B�B�B{BB�BB�B�B�B<B<B�B�B�BVB^B6B)B�BBB�B�BFB�B�B�B�BB�B�B2B�B�B�BB�B�BkB�B�B�B!B$�B"�B# B$�B$�B'8B+�B-wB,�B,�B.�B/�B/iB/�B0�B1�B3MB7�B9�B:�B:�B;B=VB?HB?B?�B@�B@�BA BEmBHfBHBJ	BLJBM�BQNBRoBT{BV�BW�BZ�B]�B`�Ba�Bb�Bf�Bh�BkBmBp!Bq[ButByrB|jB~wB��B��B��B��B��B��B��B��B��B�B��B��B�tB�KB��B�(B��BňB�B��B��B� B�&B�B�$B��B�B�B��B��B�B�CB�iB��B��B�B�B�tB�RB�0B�PB�qB	 4B	AB	MB	SB	YB	fB		lB	
rB	�B	�B	�B	�B	
B	�B	�B	�B	�B	�B	�B	�B	B	 B	"B	!�B	!�B	$B	%,B	&LB	'�B	,=B	-CB	.B	1'B	4TB	7fB	;�B	>�B	=�B	=�B	@�B	A�B	C�B	EB	G�B	J	B	M6B	Q4B	T,B	V9B	W?B	X+B	[WB	[WB	[qB	]~B	`vB	a�B	b�B	d�B	e�B	h�B	l�B	p�B	r�B	u�B	xB	z�B	|B	}<B	~(B	~(B	~(B	B	�;B	�GB	�mB	�_B	�KB	�RB	�XB	�^B	�xB	�dB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�$B	�KB	�=B	�]B	�}B	�oB	�|B	�|B	�vB	�!B	�'B	�MB	�fB	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ðB	��B	��B	��B	��B	�B	��B	�B	�B	�BB	�4B	�2B	�9B	�SB	�YB	�B	�qB	�xB	�IB	�~B	ߊB	��B	�bB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�2B	�>B	�B	�0B	�B	�B	�<B	�(B	�B	��B	�.B
 4B
 4B
 4B
UB
AB
[B
aB
SB
SB
SB
YB
YB
YB
_B
KB
fB
	lB
	RB

rB

rB
^B
~B
�B
�B
�B
�B
�B
�B
�B
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
B
 'B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
#B
#B
"�B
"�B
"�B
#B
#�B
$B
$B
$B
$�B
$�B
%,B
&B
'B
'B
'B
'B
'B
(
B
($B
(
B
($B
(>B
)B
)B
)B
)B
(�B
*0B
*0B
*0B
*B
*B
+6B
+6B
+6B
,"B
,"B
,"B
,=B
,=B
,=B
-)B
.IB
.cB
/iB
/OB
/OB
0UB
0oB
0oB
1vB
2GB
2aB
2GB
2aB
3MB
3MB
3hB
3MB
4nB
4TB
4TB
4nB
4nB
5tB
5tB
5�B
6`B
6`B
6�B
7�B
7�B
7�B
7�B
7fB
7fB
8lB
8RB
8lB
8lB
8lB
8RB
8lB
8lB
8�B
9�B
:�B
:�B
;�B
;�B
;�B
;�B
;B
;�B
<�B
<�B
<�B
=�B
=�B
=�B
=�B
>�B
>�B
>�B
?�B
?�B
?�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
F�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
MB
NB
N"B
N"B
OB
N�B
OB
OB
OB
OB
N�B
P.B
P.B
PB
QB
QB
QB
RB
RB
R B
S&B
S&B
SB
TB
TB
T,B
T,B
TB
S�B
TB
TB
S�B
TB
U2B
U2B
UB
UB
VB
VB
V9B
VB
V9B
VB
V9B
W$B
W?B
W$B
X+B
XEB
X+B
XEB
XEB
XEB
YKB
Y1B
YB
YB
YKB
YKB
Z7B
Z7B
ZQB
[=B
[=B
[=B
[WB
\]B
\]B
\]B
]IB
]dB
^jB
^jB
^jB
^OB
^OB
^5B
^OB
^5B
_VB
_VB
_;B
_VB
_pB
_VB
_;B
_pB
_VB
`�B
`vB
abB
a|B
b�B
b�B
b�B
c�B
cnB
c�B
cnB
cnB
dtB
d�B
d�B
d�B
dtB
d�B
e�B
e�B
ezB
e�B
e�B
ezB
e�B
f�B
f�B
f�B
f�B
f�B
f�B
ffB
f�B
f�B
f�B
gmB
g�B
g�B
g�B
g�B
h�B
h�B
h�B
hsB
h�B
i�B
i�B
j�B
j�B
j�B
j�B
k�B
j�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<g,<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.13(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201610070034212016100700342120161007003421201806221214492018062212144920180622121449201804050407332018040504073320180405040733  JA  ARFMdecpA19c                                                                20161003063504  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20161002213512  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20161002213512  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20161002213513  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20161002213513  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20161002213513  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20161002213513  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20161002213513  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20161002213514  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20161002213514                      G�O�G�O�G�O�                JA  ARUP                                                                        20161002223010                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20161003153508  CV  JULD            G�O�G�O�F�{�                JM  ARCAJMQC2.0                                                                 20161006153421  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20161006153421  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404190733  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031449  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115111518                      G�O�G�O�G�O�                