CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2017-07-02T15:36:04Z creation;2017-07-02T15:36:06Z conversion to V3.1;2019-12-18T07:29:48Z update;2022-11-21T05:32:22Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7H   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7X   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7\   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7`   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7p   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  8   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8H   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8L   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8P   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8T   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8t   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8x   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8|   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    9   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :    PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]$   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t|   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �d   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �h   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ݈   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20170702153604  20221123111507  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               iA   JA  I1_0397_105                     2C  DdL�NAVIS_A                         0397                            ARGO 011514                     863 @��z� 1   @����Q�@;�h	ԕ�dL�m\��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A���A�  A�  A�33A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BPffBX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�33C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�Ff111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�=q@��
@��
A�A=�A]�A}�A�A���A���A�(�A���A���A���A���Bz�Bz�Bz�Bz�B'z�B/z�B7z�B?z�BGz�BO�HBWz�B_z�Bgz�Boz�Bwz�Bz�B��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qBýqBǽqB˽qBϽqBӽqB׽qB۽qB߽qB�qB�qB��B�qB�qB��qB��B��qC޸C޸C޸C޸C	޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C!޸C#޸C%޸C'޸C)޸C+޸C-޸C/޸C1޸C3޸C5޸C7޸C9޸C;޸C=޸C?޸CA޸CC޸CE޸CG޸CI޸CK޸CM޸CO޸CQ޸CS޸CU޸CW޸CY޸C[޸C]޸C_޸Ca޸Cc޸Ce޸Cg޸Ci޸Ck޸Cm޸Co޸Cq޸Cs޸Cu޸Cw޸Cy޸C{޸C}޸C޸C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\D w�D ��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D	w�D	��D
w�D
��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D w�D ��D!w�D!��D"w�D"��D#w�D#��D$w�D$��D%w�D%��D&w�D&��D'w�D'��D(w�D(��D)w�D)��D*w�D*��D+w�D+��D,w�D,��D-w�D-��D.w�D.��D/w�D/��D0w�D0��D1w�D1��D2w�D2��D3w�D3��D4w�D4��D5w�D5��D6w�D6��D7w�D7��D8w�D8��D9w�D9��D:w�D:��D;w�D;��D<w�D<��D=w�D=��D>w�D>��D?w�D?��D@w�D@��DAw�DA��DBw�DB��DCw�DC��DDw�DD��DEw�DE��DFw�DF��DGw�DG��DHw�DH��DIw�DI��DJw�DJ��DKw�DK��DLw�DL��DMw�DM��DNw�DN��DOw�DO��DPw�DP��DQw�DQ��DRw�DR��DSw�DS��DTw�DT��DUw�DU��DVw�DV��DWw�DW��DXw�DX��DYw�DY��DZw�DZ��D[w�D[��D\w�D\��D]w�D]��D^w�D^��D_w�D_��D`w�D`��Daw�Da��Dbw�Db��Dcw�Dc��Ddw�Dd��Dew�De��Dfw�Df��Dgw�Dg��Dhw�Dh��Diw�Di��Djw�Dj��Dkw�Dk��Dlw�Dl��Dmw�Dm��Dnw�Dn��Dow�Do��Dpw�Dp��Dqw�Dq��Drw�Dr��Dsw�Ds��Dtw�Dt��Duw�Du��Dvw�Dv��Dww�Dw��Dxw�Dx��Dyw�Dy��Dzw�Dz��D{w�D{��D|w�D|��D}w�D}��D~w�D~��Dw�D��D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D»�D���D�;�D�{�Dû�D���D�;�D�{�DĻ�D���D�;�D�{�DŻ�D���D�;�D�{�Dƻ�D���D�;�D�{�Dǻ�D���D�;�D�{�DȻ�D���D�;�D�{�Dɻ�D���D�;�D�{�Dʻ�D���D�;�D�{�D˻�D���D�;�D�{�D̻�D���D�;�D�{�Dͻ�D���D�;�D�{�Dλ�D���D�;�D�{�Dϻ�D���D�;�D�{�Dл�D���D�;�D�{�Dѻ�D���D�;�D�{�Dһ�D���D�;�D�{�Dӻ�D���D�;�D�{�DԻ�D���D�;�D�{�Dջ�D���D�;�D�{�Dֻ�D���D�;�D�{�D׻�D���D�;�D�{�Dػ�D���D�;�D�{�Dٻ�D���D�;�D�{�Dڻ�D���D�;�D�{�Dۻ�D���D�;�D�{�Dܻ�D���D�;�D�{�Dݻ�D���D�;�D�{�D޻�D���D�;�D�{�D߻�D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D���D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�B=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AʅAʃA�~�A�~�AʃAʉ7A�|�A�I�A�5?A�|�Aǟ�A��uA��jA�VA���A��A��A�
=A���A�33A��DA��A��-A���A�n�A��A��FA��yA�7LA�1A��A�=qA��A�ĜA�Q�A��/A�M�A���A�t�A�hsA�?}A��`A�G�A�`BA�VA��A�ĜA�  A��TA�-A�-A��A�hsA��HA�=qA��DA��A�
=A�33A���A�bNA��A33A|�Ay�#Aw"�AvAu+As��Ap�RAo�^An�`Am`BAj��Ai��Ah-Ag�^Agx�AgoAe��Ae33Ad-AcO�Ab=qA`�/A`ZA_�#A_/A^ffA]�A\ZA[�A[x�A[7LAZ$�AY
=AX9XAWl�AU��AU\)AUO�AU�AT5?ASt�AQ��AQAP��APȴAP�AOt�AN�yAN�AN  AM�AM�AL�AL1AK�#AKS�AK
=AJbNAI33AHAG�AFZAE�AE�hAE;dAD��AD�jAD5?AC�AB��AAK�A@$�A?t�A>�A>M�A=x�A<�+A;��A:��A:VA: �A97LA8�9A8�A7C�A5��A4�HA4=qA4{A41A3hsA3
=A2�HA2��A2-A1�;A1�hA0��A/�A/�A/XA.�A.-A-K�A,�`A,��A,r�A,9XA+�A)��A(n�A'|�A&��A&M�A%�mA%�A%"�A$�DA$1A#&�A"bA �/A��A��A�A�\A$�A�^A`BA�HAM�A��A��A�FAoA��Ar�A  Al�A��A~�AA�A��A�+AE�A�A�7A�A�RAI�A�;A`BA&�A�yA(�AȴA�+A
��A	�^A	;dA��A�AAS�AVA�A�FA��AVA �u@��w@���@�/@��H@���@�9X@��y@�ȴ@�R@���@�~�@�=q@�O�@��@홚@��@�%@�r�@��@��@�b@�/@�1'@�ff@ӕ�@��T@Л�@�  @�dZ@�ff@��@�(�@˕�@�;d@�o@�ȴ@�E�@�x�@��@���@��`@°!@�@�p�@�
=@��@���@���@���@�X@��@���@��@�@��@�1'@��@�K�@���@���@�ƨ@�C�@��\@���@��@��D@�b@�C�@��-@�x�@�G�@�%@�Z@��;@���@�
=@��!@�v�@��-@�`B@���@���@�C�@���@�V@���@���@�/@��9@��@��H@��-@��@� �@��
@���@�S�@��@���@�9X@��P@�@�M�@�X@��j@�z�@�bN@�Z@� �@���@��P@�l�@�\)@�+@���@��@���@�(�@��@�|�@���@�O�@��`@�Ĝ@��j@���@��@�A�@�(�@��@���@�33@���@�E�@�{@�@�G�@�%@��/@��9@�j@�1@�dZ@���@��\@�-@�x�@�7L@�&�@��@��@�V@���@���@��/@��u@��m@�;d@��@�ȴ@���@��\@�E�@��#@�@���@���@���@�x�@���@��u@�j@���@�\)@�S�@�K�@�;d@�o@�@��@�ȴ@��+@�M�@�@���@��D@� �@�;@\)@~��@~�R@~V@}p�@}?}@|�@|��@|��@|�j@|�@|j@{��@z=q@y�^@y��@yX@x�9@xQ�@w�@w�@wK�@v�y@v{@u��@u�h@t�D@t(�@so@r��@r�\@r~�@rM�@q��@q�#@q��@rM�@rn�@rJ@o;d@n�R@n��@n�+@nv�@nV@n@m��@mO�@l��@l�@l�D@lz�@l(�@kS�@j-@i��@i�#@j-@j-@k@j�@jJ@hQ�@hQ�@g��@g�@e�T@e��@e��@e�@e`B@e/@d�@dj@c��@co@c"�@c"�@c33@c33@cC�@co@b~�@aG�@`�`@`��@`�9@a%@a&�@a�@`��@`1'@` �@`1'@`b@`b@` �@`1'@`bN@`��@b=q@b�\@b��@c"�@c"�@c33@cC�@cS�@cS�@cS�@cS�@cdZ@c�@c��@c�m@c��@cS�@b�H@b�\@b~�@bn�@b=q@a��@a&�@`bN@_��@_|�@_K�@^�y@^{@]O�@]�@\1@[S�@Z��@Y��@Y�#@X�@W�P@U�-@T��@T�@S�
@Sƨ@S�@R�@Rn�@R-@Q�^@QX@QX@QG�@Q7L@Q7L@Q7L@QG�@Qhs@Q�7@Q7L@Q�@PĜ@PA�@P  @O�w@Ol�@O�@N�+@Nff@NV@NV@NE�@M�@M@M��@M��@M��@M��@M@M��@L�/@Kƨ@J��@J=q@J-@J�@J�@J�@JJ@JJ@I��@IX@I&�@I&�@I�@H��@H�u@Hr�@G;d@F��@G
=@G�P@H1'@F��@F{@E��@E�-@E�T@E@E��@E�T@Ep�@D�@D��@D�j@D��@D�D@Dz�@Dj@D(�@D(�@D9X@C��@C@B��@B�\@B�\@B�!@B~�@BM�@A��@A�#@A��@A�^@A��@A��@A��@Ax�@Ahs@AX@A&�@@��@@�9@@�9@@r�@@1'@@1'@@b@?��@>��@=�@=V@;��@;o@;@:��@:��@:n�@:M�@:-@9��@9G�@8  @7|�@7K�@6��@6$�@5�h@5p�@5O�@4��@4I�@41@3�@2�!@2n�@2n�@2^5@2��@2��@2n�@1�#@1��@1��@1G�@1%@0��@0��@0�@1%@1�@0��@0bN@0A�@0A�@0b@/�@/
=@.�+@.V@-�T@-@-�h@-?}@,��@,��@,�@,z�@,�@+�F@+��@+dZ@+@*�!@*�!@*M�@)�@)��@)��@)��@)��@)%@(  @'|�@'l�@'|�@'�P@'l�@';d@&��@&��@&��@&�+@&v�@&ff@&5?@%�T@%p�@%�@$�@$�j@$��@$�D@$z�@$j@$Z@$9X@$1@#�m@#��@#t�@#t�@#"�@"��@"��@"��@"��@"�\@"^5@"=q@"�@"J@!��@!�#@!��@!�^@!��@!G�@ Q�@��@��@|�@l�@l�@\)@
=@��@�@�R@v�@ff@E�@5?@{@@�@��@��@��@p�@/@�/@��@I�@1@�
@�F@�F@��@dZ@C�@"�@@��@~�@=q@�@�@��@hs@&�@�@%@�9@Q�@A�@  @K�@�@��@$�@�-@�h@�h@�@p�@V@�@z�@j@I�@9X@(�@1@�m@��@S�@o@�H@�!@��@�\@�\@~�@�^@x�@G�@7L@�@�`@��@Ĝ@��@�u@Q�@ �@b@�@��@�w@�w@��@\)@;d@+@+@
=@�y@�@ȴ@��@�+@@p�@/@�@��@�j@��@z�@9X@�
@ƨ@ƨ@�F@��@t�@C�@"�@o@
��@
�\@
M�@
=q@
-@
-@
�@
�@
J@	�@	�@	�@	�@	�^@	x�@	X@	7L@	&�@��@��@Ĝ@�u@�@r�@bN@A�@�@��@��@|�@|�@l�@K�@;d@+@�@
=@��@�R@�+@5?@�T@��@�h@O�@?}@�@�@��@�j@�j@�@�@�@�@��@��@z�@Z@I�@(�@��@�F@�@t�@t�@t�@33@@@@@o@o@@@�@��@-@J@��@�#@��@�^@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AʅAʃA�~�A�~�AʃAʉ7A�|�A�I�A�5?A�|�Aǟ�A��uA��jA�VA���A��A��A�
=A���A�33A��DA��A��-A���A�n�A��A��FA��yA�7LA�1A��A�=qA��A�ĜA�Q�A��/A�M�A���A�t�A�hsA�?}A��`A�G�A�`BA�VA��A�ĜA�  A��TA�-A�-A��A�hsA��HA�=qA��DA��A�
=A�33A���A�bNA��A33A|�Ay�#Aw"�AvAu+As��Ap�RAo�^An�`Am`BAj��Ai��Ah-Ag�^Agx�AgoAe��Ae33Ad-AcO�Ab=qA`�/A`ZA_�#A_/A^ffA]�A\ZA[�A[x�A[7LAZ$�AY
=AX9XAWl�AU��AU\)AUO�AU�AT5?ASt�AQ��AQAP��APȴAP�AOt�AN�yAN�AN  AM�AM�AL�AL1AK�#AKS�AK
=AJbNAI33AHAG�AFZAE�AE�hAE;dAD��AD�jAD5?AC�AB��AAK�A@$�A?t�A>�A>M�A=x�A<�+A;��A:��A:VA: �A97LA8�9A8�A7C�A5��A4�HA4=qA4{A41A3hsA3
=A2�HA2��A2-A1�;A1�hA0��A/�A/�A/XA.�A.-A-K�A,�`A,��A,r�A,9XA+�A)��A(n�A'|�A&��A&M�A%�mA%�A%"�A$�DA$1A#&�A"bA �/A��A��A�A�\A$�A�^A`BA�HAM�A��A��A�FAoA��Ar�A  Al�A��A~�AA�A��A�+AE�A�A�7A�A�RAI�A�;A`BA&�A�yA(�AȴA�+A
��A	�^A	;dA��A�AAS�AVA�A�FA��AVA �u@��w@���@�/@��H@���@�9X@��y@�ȴ@�R@���@�~�@�=q@�O�@��@홚@��@�%@�r�@��@��@�b@�/@�1'@�ff@ӕ�@��T@Л�@�  @�dZ@�ff@��@�(�@˕�@�;d@�o@�ȴ@�E�@�x�@��@���@��`@°!@�@�p�@�
=@��@���@���@���@�X@��@���@��@�@��@�1'@��@�K�@���@���@�ƨ@�C�@��\@���@��@��D@�b@�C�@��-@�x�@�G�@�%@�Z@��;@���@�
=@��!@�v�@��-@�`B@���@���@�C�@���@�V@���@���@�/@��9@��@��H@��-@��@� �@��
@���@�S�@��@���@�9X@��P@�@�M�@�X@��j@�z�@�bN@�Z@� �@���@��P@�l�@�\)@�+@���@��@���@�(�@��@�|�@���@�O�@��`@�Ĝ@��j@���@��@�A�@�(�@��@���@�33@���@�E�@�{@�@�G�@�%@��/@��9@�j@�1@�dZ@���@��\@�-@�x�@�7L@�&�@��@��@�V@���@���@��/@��u@��m@�;d@��@�ȴ@���@��\@�E�@��#@�@���@���@���@�x�@���@��u@�j@���@�\)@�S�@�K�@�;d@�o@�@��@�ȴ@��+@�M�@�@���@��D@� �@�;@\)@~��@~�R@~V@}p�@}?}@|�@|��@|��@|�j@|�@|j@{��@z=q@y�^@y��@yX@x�9@xQ�@w�@w�@wK�@v�y@v{@u��@u�h@t�D@t(�@so@r��@r�\@r~�@rM�@q��@q�#@q��@rM�@rn�@rJ@o;d@n�R@n��@n�+@nv�@nV@n@m��@mO�@l��@l�@l�D@lz�@l(�@kS�@j-@i��@i�#@j-@j-@k@j�@jJ@hQ�@hQ�@g��@g�@e�T@e��@e��@e�@e`B@e/@d�@dj@c��@co@c"�@c"�@c33@c33@cC�@co@b~�@aG�@`�`@`��@`�9@a%@a&�@a�@`��@`1'@` �@`1'@`b@`b@` �@`1'@`bN@`��@b=q@b�\@b��@c"�@c"�@c33@cC�@cS�@cS�@cS�@cS�@cdZ@c�@c��@c�m@c��@cS�@b�H@b�\@b~�@bn�@b=q@a��@a&�@`bN@_��@_|�@_K�@^�y@^{@]O�@]�@\1@[S�@Z��@Y��@Y�#@X�@W�P@U�-@T��@T�@S�
@Sƨ@S�@R�@Rn�@R-@Q�^@QX@QX@QG�@Q7L@Q7L@Q7L@QG�@Qhs@Q�7@Q7L@Q�@PĜ@PA�@P  @O�w@Ol�@O�@N�+@Nff@NV@NV@NE�@M�@M@M��@M��@M��@M��@M@M��@L�/@Kƨ@J��@J=q@J-@J�@J�@J�@JJ@JJ@I��@IX@I&�@I&�@I�@H��@H�u@Hr�@G;d@F��@G
=@G�P@H1'@F��@F{@E��@E�-@E�T@E@E��@E�T@Ep�@D�@D��@D�j@D��@D�D@Dz�@Dj@D(�@D(�@D9X@C��@C@B��@B�\@B�\@B�!@B~�@BM�@A��@A�#@A��@A�^@A��@A��@A��@Ax�@Ahs@AX@A&�@@��@@�9@@�9@@r�@@1'@@1'@@b@?��@>��@=�@=V@;��@;o@;@:��@:��@:n�@:M�@:-@9��@9G�@8  @7|�@7K�@6��@6$�@5�h@5p�@5O�@4��@4I�@41@3�@2�!@2n�@2n�@2^5@2��@2��@2n�@1�#@1��@1��@1G�@1%@0��@0��@0�@1%@1�@0��@0bN@0A�@0A�@0b@/�@/
=@.�+@.V@-�T@-@-�h@-?}@,��@,��@,�@,z�@,�@+�F@+��@+dZ@+@*�!@*�!@*M�@)�@)��@)��@)��@)��@)%@(  @'|�@'l�@'|�@'�P@'l�@';d@&��@&��@&��@&�+@&v�@&ff@&5?@%�T@%p�@%�@$�@$�j@$��@$�D@$z�@$j@$Z@$9X@$1@#�m@#��@#t�@#t�@#"�@"��@"��@"��@"��@"�\@"^5@"=q@"�@"J@!��@!�#@!��@!�^@!��@!G�@ Q�@��@��@|�@l�@l�@\)@
=@��@�@�R@v�@ff@E�@5?@{@@�@��@��@��@p�@/@�/@��@I�@1@�
@�F@�F@��@dZ@C�@"�@@��@~�@=q@�@�@��@hs@&�@�@%@�9@Q�@A�@  @K�@�@��@$�@�-@�h@�h@�@p�@V@�@z�@j@I�@9X@(�@1@�m@��@S�@o@�H@�!@��@�\@�\@~�@�^@x�@G�@7L@�@�`@��@Ĝ@��@�u@Q�@ �@b@�@��@�w@�w@��@\)@;d@+@+@
=@�y@�@ȴ@��@�+@@p�@/@�@��@�j@��@z�@9X@�
@ƨ@ƨ@�F@��@t�@C�@"�@o@
��@
�\@
M�@
=q@
-@
-@
�@
�@
J@	�@	�@	�@	�@	�^@	x�@	X@	7L@	&�@��@��@Ĝ@�u@�@r�@bN@A�@�@��@��@|�@|�@l�@K�@;d@+@�@
=@��@�R@�+@5?@�T@��@�h@O�@?}@�@�@��@�j@�j@�@�@�@�@��@��@z�@Z@I�@(�@��@�F@�@t�@t�@t�@33@@@@@o@o@@@�@��@-@J@��@�#@��@�^@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�NB�NB�TB�TB�TB�NB�NB�HB�BB�)B��B�}B�3B��B�DB�By�Bq�BjBcTB]/BVBN�BJ�BF�B=qB-B�BDBB��B��B�B�fB�NB�;B�#B�
B��B��B�}B�3B��B�7Bx�Br�Bn�BcTBQ�BE�B6FB%�B�BhB%B
��B
�B
�HB
��B
��B
ŢB
�jB
�9B
��B
�B
jB
aHB
YB
M�B
9XB
0!B
(�B
�B
DB
B	��B	��B	�B	�B	�B	�ZB	�/B	�
B	��B	ȴB	ŢB	��B	�qB	�RB	�3B	�B	��B	��B	��B	��B	��B	�uB	�VB	�%B	�B	�B	� B	z�B	v�B	o�B	jB	jB	iyB	e`B	bNB	_;B	^5B	ZB	XB	T�B	R�B	O�B	N�B	K�B	I�B	F�B	@�B	:^B	5?B	2-B	0!B	/B	-B	,B	)�B	'�B	%�B	 �B	�B	�B	hB	VB	DB	1B	B	  B��B��B��B��B�B�B�B�mB�ZB�NB�HB�BB�5B�/B�)B�B�B�
B�B��B��B��B��B��BȴBŢBÖBB��B�}B�jB�LB�-B�B�B�B��B��B��B��B��B��B��B��B�uB�\B�DB�1B�%B�B�B�B� B|�Bz�Bv�Bt�Bs�Br�Bp�Bo�Bm�Bk�BiyBhsBgmBffBe`BcTBbNBaHB_;B^5B]/B\)BZBYBVBR�BP�BM�BI�BH�BF�BE�BC�BA�B?}B;dB8RB5?B2-B1'B/B.B+B(�B%�B#�B#�B"�B"�B"�B!�B �B�B�B�B�B�B�BuBhB\B\BVBPBPBPBVBPBPBPBPBVBVBVBVBVBVBVBVBJB\BbBhBbBoBuBuBuBuBoBuBuB{B{B�B�B�B�B�B�B�B�B�B �B!�B"�B#�B$�B'�B(�B(�B(�B+B,B,B.B/B/B1'B2-B49B7LB8RB9XB:^B;dB<jB<jB>wB?}BC�BG�BK�BL�BM�BN�BO�BO�BT�BZB\)B^5B`BBdZBgmBhsBhsBhsBiyBl�Bl�Bm�Bm�Bn�Bo�Br�Bz�B|�B}�B� B�B�=B�JB�JB�PB�PB�PB�\B�\B�bB�oB�{B��B��B��B��B��B��B��B��B��B��B�B�3B�3B�LB�wB��BBBBBÖBÖBĜBǮB��B��B��B�B�
B�
B�B�/B�5B�5B�;B�;B�;B�ZB�mB�sB�B�B�B�B�B��B��B��B��B��B��B��B	B	B	+B		7B	DB	PB	VB	\B	oB	uB	{B	�B	�B	�B	�B	�B	�B	�B	 �B	 �B	"�B	$�B	&�B	(�B	+B	,B	-B	0!B	0!B	1'B	5?B	6FB	:^B	=qB	=qB	=qB	>wB	?}B	?}B	B�B	G�B	H�B	H�B	K�B	M�B	N�B	N�B	N�B	O�B	P�B	R�B	S�B	T�B	VB	VB	VB	VB	VB	XB	XB	YB	\)B	_;B	e`B	hsB	gmB	hsB	hsB	hsB	iyB	k�B	m�B	n�B	o�B	r�B	u�B	v�B	y�B	}�B	�B	�B	�B	�B	�B	�B	�%B	�1B	�=B	�=B	�=B	�DB	�JB	�PB	�\B	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�?B	�FB	�FB	�LB	�LB	�LB	�LB	�RB	�^B	�}B	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�#B	�)B	�)B	�/B	�5B	�5B	�;B	�HB	�NB	�TB	�ZB	�mB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
+B
1B
	7B

=B
DB
JB
VB
VB
VB
VB
VB
VB
\B
VB
\B
bB
hB
oB
uB
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
 �B
 �B
 �B
"�B
#�B
#�B
$�B
#�B
#�B
"�B
!�B
!�B
!�B
 �B
 �B
"�B
#�B
#�B
#�B
#�B
#�B
$�B
%�B
%�B
&�B
)�B
+B
,B
/B
0!B
0!B
1'B
1'B
1'B
2-B
49B
6FB
6FB
7LB
7LB
7LB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
:^B
:^B
;dB
;dB
;dB
;dB
;dB
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
;dB
;dB
<jB
<jB
<jB
=qB
<jB
?}B
@�B
A�B
A�B
A�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
M�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
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
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
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
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
cTB
cTB
cTB
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
dZB
dZB
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
gmB
gmB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
k�B
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
l�B
l�B
l�B
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
p�B
p�B
p�B
p�B
p�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�NB�NB�nB�TB�nB�B�B�B�B�B�WB��B��B�fB��B�uB|�Bu%Bm�Bg8Ba-BZ7BQBL�BK�BE�B6�B!BpBB�6B�B��B�B�B�B�]BؓB՛B��B�B�B��B�xBzBs�Bp�Be�BTBHB8�B'�B	BBB
��B
�CB
�B
�B
��B
��B
��B
�2B
��B
�B
l"B
b�B
[=B
Q B
:�B
1�B
+6B
 �B
�B
�B	�lB	�`B	�B	��B	�B	�B	�jB	�yB	�oB	ɠB	�YB	�uB	��B	��B	�nB	��B	��B	��B	�LB	�'B	��B	��B	��B	��B	�aB	��B	�;B	|B	x�B	poB	j�B	j�B	jKB	fLB	c B	_�B	_!B	Z�B	X�B	U�B	S�B	PbB	O�B	LdB	J�B	HKB	A�B	;B	6FB	2�B	0�B	/�B	-wB	,�B	*�B	(�B	'mB	"�B		B	�B	oB	(B	~B		lB	9B	;B��B�B��B��B��B��B�cB�sB�B�B�B�B��BݘB��BںBٴB��B��B�,B�}B�vB̈́B��B��B�YB�B�-B�[B� B�B�>B��B�;B��B��B��B��B��B��B� B�\B�CB��B�B�4B�~B��B��B��B��B��B�B~(B|PBw�BuZBtBshBq�Bp�Bn/Bl�BjeBh�Bh
BgBfBdBc Ba�B_�B_B]�B\�BZ�BZkBW�BS�BR�BO\BJ�BI�BG�BFtBD�BC-BAoB=qB9�B72B33B2GB0!B/�B,�B*�B(XB$�B$B# B#B#:B"hB!�B!bB�BIB�BkBYB�B,B4BbB�BBpB<B�B�BB<B�B�B�B�B�B�B�B�BBB�B�BB:B�B&B�B�B�B�B@BFB,BMBMB?B+B+B�BWBIBOBpB �B!bB"NB#nB$�B%�B(>B)_B)DB)�B+�B,WB,�B.}B/�B/�B1�B2�B5B7�B8�B9�B:�B;�B<�B=B?B@�BD�BH�BLBMBN"BOBBPbBQ BVBZ�B\�B^�BaBd�Bg�Bh�Bh�Bh�Bi�Bl�Bl�Bm�Bm�Bo5BpUBs�B{0B}VB~wB��B�B��B�~B�~B�jB��B��B��B��B��B��B��B��B��B�	B�)B��B��B�B�:B�FB�mB�wB��B��B��B��B��B��B��B��B��B��B��B�B�KB�jB�@B�2B�SB�YB�sB�eB�IB�OB�OB�pBߊB��B�B�B�B� B��B��B��B�B��B��B��B�B�8B�rB��B	oB	gB	zB		�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	�B	 �B	!B	#:B	%B	'B	)DB	+6B	,WB	-wB	0UB	0oB	1�B	5�B	6�B	:�B	=�B	=�B	=�B	>�B	?�B	?�B	B�B	G�B	IB	I�B	K�B	M�B	OB	OB	N�B	P.B	Q4B	S&B	TFB	U2B	V9B	V9B	VSB	VmB	V�B	XEB	X+B	YB	\)B	_;B	ezB	h�B	h
B	h�B	h�B	h�B	i�B	k�B	m�B	n�B	o�B	r�B	vB	wB	z*B	~(B	�B	�'B	�-B	�GB	�MB	�mB	��B	��B	�rB	�rB	�XB	�^B	�dB	�jB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�kB	��B	��B	�B	�B	�GB	�ZB	�FB	�`B	�LB	�fB	�LB	�fB	�lB	�xB	��B	��B	��B	�B	��B	��B	�B	�B	�.B	�NB	�TB	�FB	�B	�B	�9B	�sB	�yB	�_B	�yB	�B	�kB	چB	�kB	ںB	ٴB	��B	�kB	�kB	�QB	�=B	�WB	�qB	�qB	�]B	�xB	�IB	�OB	�jB	�VB	�HB	�NB	�nB	�tB	�B	�B	�B	��B	��B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	��B	�B	�3B	�%B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	�B	�B	�0B	�6B	�VB	�B	�B	��B
  B
�B
[B
'B
AB
B
9B
EB
KB
	�B

rB
^B
dB
VB
pB
�B
pB
�B
VB
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 'B
B
;B
;B
 B
 �B
 �B
 �B
#B
#�B
$B
$�B
$@B
$ZB
# B
!�B
!�B
"4B
!B
 �B
#B
$&B
#�B
$B
$B
$@B
$�B
%�B
%�B
&�B
)�B
+B
,=B
/5B
0UB
0;B
1AB
1AB
1AB
2aB
49B
6FB
6�B
7�B
7fB
7�B
8�B
8�B
8�B
9�B
9�B
9�B
9rB
:xB
:xB
;B
;�B
;�B
;B
;�B
:xB
:�B
:�B
:�B
:�B
:xB
:�B
:�B
:xB
;B
;B
<�B
<�B
<�B
=�B
<�B
?}B
@�B
A�B
A�B
A�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
LB
N"B
OB
OB
O�B
O�B
O�B
PB
Q B
Q B
QB
QB
Q�B
R B
SB
SB
R�B
SB
SB
SB
SB
SB
T,B
T,B
TB
U2B
U2B
U2B
U2B
VB
VB
V9B
V9B
V9B
W?B
W$B
W$B
X+B
XEB
X+B
X+B
Y1B
Y1B
Z7B
Z7B
ZQB
Z7B
Z7B
ZkB
[WB
[WB
\]B
\xB
\]B
]IB
]/B
]IB
]dB
]~B
^jB
^OB
^OB
_;B
_VB
_;B
_pB
_pB
_VB
_pB
`\B
`vB
`vB
abB
aHB
abB
a|B
a�B
bhB
b�B
cnB
cnB
c�B
cnB
cnB
c�B
c�B
cnB
d�B
dtB
d�B
d�B
dtB
dtB
dtB
d�B
ezB
e`B
ezB
e�B
ezB
e`B
ezB
e�B
e�B
f�B
f�B
g�B
g�B
h�B
h�B
h�B
h�B
h�B
i�B
i�B
iyB
i�B
j�B
j�B
j�B
j�B
j�B
j�B
k�B
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
l�B
l�B
l�B
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
p�B
p�B
p�B
p�B
p�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
xB
x�B
x�B
x�B
x�B
x�B
x�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<h�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.13(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201707130033562017071300335620170713003356202211182130572022111821305720221118213057201804031936202018040319362020180403193620  JA  ARFMdecpA19c                                                                20170703003514  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170702153604  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170702153605  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170702153605  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170702153606  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170702153606  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170702153606  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170702153606  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170702153606  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170702153606                      G�O�G�O�G�O�                JA  ARUP                                                                        20170702160932                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170702153239  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20170712153356  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170712153356  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180403103620  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114171527                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123057  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111507                      G�O�G�O�G�O�                