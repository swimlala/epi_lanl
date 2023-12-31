CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-03-01T10:01:39Z creation      
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
resolution        =���   axis      Z        l  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  `    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  ol   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  �h   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ۔   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ޔ   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
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
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20200301100139  20200301100139  5906101 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               A   AO  7907                            2B  A   NAVIS_A                         1015                            170425                          863 @��\�/1   @���O�P@)BI�^5�c�C��%1   GPS     Primary sampling: averaged []                                                                                                                                                                                                                                      A   A   A   @���@�33A   A   A@  Aa��A�  A���A�33A�  A�33A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�ffB�33B�ffB�33B���B�ffB���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C#�fC&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'fD'�fD(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ DҼ�D���D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @\)@�{@��HAp�A9p�A[
>Ayp�A��A��A��RA��A̸RAܸRA�RA��RB\)B\)B\)B\)B&\)B.\)B6\)B>\)BF\)BN\)BV\)B^\)Bf\)Bn\)Bv\)B~\)B�.B�.B�.B�.B�.B�.B�aGB�aGB�.B��zB�aGB��zB�aGB���B��zB�ǮB���B�.B�.B�.B�.B�.B�.B�.B�.B�.B���B�.B�.B�.B�.B�.C�
C�
C�
C�
C	�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C!�
C#}pC%�
C'�
C)�
C+�
C-�
C/�
C1�
C3�
C5�
C7�
C9�
C;�
C=�
C?�
CA�
CC�
CE�
CG�
CI�
CK�
CM�
CO�
CQ�
CS�
CU�
CW�
CY�
C[�
C]�
C_�
Ca�
Cc�
Ce�
Cg�
Ci�
Ck�
Cm�
Co�
Cq�
Cs�
Cu�
Cw�
Cy�
C{�
C}�
C�
C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C���C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅D e�D ��De�D��De�D��De�D��De�D��De�D��De�D��De�D��De�D��D	e�D	��D
e�D
��De�D��De�D��De�D��De�D��De�D��De�D��De�D��De�D��De�D��De�D��De�D��De�D��De�D��De�D��De�D��De�D��De�D��De�D��De�D��De�D��De�D��D e�D ��D!e�D!��D"e�D"��D#e�D#��D$e�D$��D%e�D%��D&e�D&�)D'l)D'��D(e�D(��D)e�D)��D*e�D*��D+e�D+��D,e�D,��D-e�D-��D.e�D.��D/e�D/��D0e�D0��D1e�D1��D2e�D2��D3e�D3��D4e�D4��D5e�D5��D6e�D6��D7e�D7��D8e�D8��D9e�D9��D:e�D:��D;e�D;��D<e�D<��D=e�D=��D>e�D>��D?e�D?��D@e�D@��DAe�DA��DBe�DB��DCe�DC��DDe�DD��DEe�DE��DFe�DF��DGe�DG��DHe�DH��DIe�DI��DJe�DJ��DKe�DK��DLe�DL��DMe�DM��DNe�DN��DOe�DO��DPe�DP��DQe�DQ��DRe�DR��DSe�DS��DTe�DT��DUe�DU��DVe�DV��DWe�DW��DXe�DX��DYe�DY��DZe�DZ��D[e�D[��D\e�D\��D]e�D]��D^e�D^��D_e�D_��D`e�D`��Dae�Da��Dbe�Db��Dce�Dc��Dde�Dd��Dee�De��Dfe�Df��Dge�Dg��Dhe�Dh��Die�Di��Dje�Dj��Dke�Dk��Dle�Dl��Dme�Dm��Dne�Dn��Doe�Do��Dpe�Dp��Dqe�Dq��Dre�Dr��Dse�Ds��Dte�Dt��Due�Du��Dve�Dv��Dwe�Dw��Dxe�Dx��Dye�Dy��Dze�Dz��D{e�D{��D|e�D|��D}e�D}��D~e�D~��De�D��D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D²�D���D�2�D�r�Dò�D���D�2�D�r�DĲ�D���D�2�D�r�DŲ�D���D�2�D�r�DƲ�D���D�2�D�r�Dǲ�D���D�2�D�r�DȲ�D���D�2�D�r�Dɲ�D���D�2�D�r�Dʲ�D���D�2�D�r�D˲�D���D�2�D�r�D̲�D���D�2�D�r�DͲ�D���D�2�D�r�Dβ�D���D�2�D�r�Dϲ�D���D�2�D�r�Dв�D���D�2�D�r�DѲ�D���D�2�D�r�Dү�D��D�2�D�r�DӲ�D���D�2�D�r�DԲ�D���D�2�D�r�Dղ�D���D�2�D�r�Dֲ�D���D�2�D�r�Dײ�D���D�2�D�r�Dز�D���D�2�D�r�Dٲ�D���D�2�D�r�Dڲ�D���D�2�D�r�D۲�D���D�2�D�r�Dܲ�D���D�2�D�r�Dݲ�D���D�2�D�r�D޲�D���D�2�D�r�D߲�D���D�2�D�r�D��D���D�2�D�r�D��D���D�2�D�r�D��D���D�2�D�r�D��D���D�2�D�r�D��D���D�2�D�r�D��D���D�2�D�r�D��D���D�2�D�r�D��D���D�2�D�r�D��D���D�2�D�r�D��D���D�2�D�r�D��D���D�2�D�vD��D���D�2�D�r�D��D���D�2�D�r�D���D���D�2�D�r�D��D���D�2�D�r�D��D���D�2�D�r�D��D���D�2�D�r�D��D���D�2�D�r�D��D���D�2�D�r�D��D���D�2�D�r�D���D���D�2�D�r�D���D���D�2�D�r�D��D�߮111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AΝ�AΡ�AΡ�AΟ�AΡ�AΩ�AΧ�AΩ�AΥ�Aδ9A���A�A�ĜA�ĜA�A�ĜA�ƨA�A���A�A���A���A���A���A���A���A���A���A���A���A���A���A��
A��
A��A��A��A��
A���AξwAΡ�A�^5A�XAȲ-A�ĜA��A���A��A�&�A��jA��A�dZA�S�A�VA�O�A�p�A��A���A���A���A�Q�A�VA�r�A�VA���A�x�A���A}%AxM�At~�ApĜAl�Ai+Af�HAb$�A_ƨA]��A\��A[
=AVz�AR�AJ��AHbAF��AF1AB�A@  A>�A<M�A:E�A8bA69XA3��A2ĜA2��A1��A0��A-��A,�A+|�A*9XA(n�A'�;A&��A&A&�9A'K�A'��A&(�A$��A"9XA!�^A �yA �A v�A ^5A r�A r�A M�A�mA�A�Av�AM�A$�A�A\)AĜA^5A�
A;dA�\Ar�Az�A��AoA��A�/A�HA��A��AS�A+A
=A��A�A��AI�A�AC�A��A��A�jA�AM�A�A�AƨA�PA\)A�A$�A�A��A�^A�PA�PA�hA�7A7LA�A�`A�DA�TAC�A��AM�A5?A=qA��AVA&�A�`AM�A�wAl�A7LA"�AoA
��A
�/A
��A
�+A	��A	ƨA	�A	`BA	O�A	�AȴA�AM�A{A��A|�A�A�DA�mA��Ap�A\)A;dA
=A�A{A��Ax�A&�AȴA�!A�A �A��A�#A��A\)A ��A �9@�ƨ@��@�~�@�@�%@�A�@�\)@���@�~�@��@�7L@��`@�bN@� �@�\)@���@���@�1@��@�Z@�(�@�dZ@�V@��@�x�@� �@��@��
@@�t�@���@�ff@��^@��/@�@�\)@�+@��H@���@�Ĝ@��@��;@�K�@��@�V@�@��@�bN@㕁@◍@�h@��@���@�1'@�
=@�v�@�E�@��T@���@ݡ�@���@���@�j@��@�"�@�J@��m@�~�@��@�?}@�V@�Q�@ӕ�@�K�@��@�E�@���@щ7@�X@���@϶F@θR@�n�@��#@�G�@̋D@�  @�\)@�l�@˕�@ʸR@Ɂ@�z�@�9X@ǅ@�J@�5?@�@��`@�r�@��
@��@¸R@�=q@�@�?}@���@��@�A�@���@�K�@��!@��@��h@���@�b@���@�
=@���@�^5@�$�@��h@�&�@��`@���@�Q�@��m@��P@�o@��R@��@���@��h@�hs@��`@�bN@�b@��@�|�@�;d@��@�v�@�=q@��T@���@���@�G�@��@��D@��@�  @��m@��w@�"�@���@�~�@�E�@���@�O�@�V@��j@�z�@�1@���@���@�S�@�
=@��R@�n�@��@��^@���@�p�@�hs@��9@�r�@��m@��P@�\)@�+@��y@�^5@��@��-@�`B@�X@�O�@���@��D@��@��w@��P@�
=@�n�@�E�@���@�@���@�hs@�7L@���@��@��D@�z�@�Z@���@��@�l�@�+@���@�@���@���@��@�1'@���@�C�@��@��\@�ff@�-@��T@��@�/@��u@�9X@�  @���@�;d@��@�~�@�E�@���@�p�@���@�bN@��;@���@��P@�K�@��+@��@���@�p�@�7L@��@�%@��`@��@��u@�r�@�Z@�9X@��@���@���@��@��!@���@�x�@�O�@�G�@�7L@��@��/@���@�I�@�  @��;@�dZ@�33@�+@�o@�@��H@�ȴ@�ȴ@��\@�p�@�G�@�7L@�&�@��@�%@��@���@��9@�I�@���@��@�|�@�|�@��@��@�|�@�"�@���@���@�=q@���@��7@�x�@�G�@���@��9@��u@�bN@�bN@�Z@�(�@���@�dZ@�@���@���@�^5@��@��T@��#@��7@�X@�?}@�&�@��`@��u@�r�@�j@�bN@�9X@l�@~v�@~@}�@}�T@}��@}?}@|I�@{ƨ@{��@{S�@z��@zM�@y�@y�7@yx�@y7L@y�@x�@w�;@w;d@v�y@v�R@vff@u@t�/@tI�@s��@s�@sdZ@sC�@s@r�@r��@r^5@q��@qx�@q�@pr�@p  @o�@o|�@n�@n�@n��@mp�@l��@lz�@l9X@l(�@l(�@l�@l�@l�@k��@kC�@j�H@j~�@j=q@i��@i�@i�#@i�#@i��@i��@i�@hA�@h  @gl�@f�@f��@f��@f��@f�+@fE�@f@e��@e�@d�@c�m@ct�@co@c@b�@b��@bn�@bM�@a��@`��@`�@`1'@_�@_�w@_��@_l�@^��@^��@^E�@]��@]�@]�@\j@\I�@\I�@[�m@[C�@Z�H@Z��@Z��@Z�!@Z�\@ZM�@Z=q@Z�@Y��@Y�@Y��@Y�@X�u@XbN@XA�@W�;@W�@W��@W|�@V�@U�@U�@UO�@U/@U�@T��@T�/@T�D@T(�@S�
@S�F@S��@S�@SS�@S33@S33@S33@R�@R��@R��@RM�@RJ@Q�#@Q��@Q��@Qhs@Q&�@P�9@P�@PbN@P1'@O�@O��@O�@O��@O��@O|�@O\)@N�@N��@Nv�@NV@N5?@M�@M��@M/@Lz�@L�@Kƨ@K��@KdZ@Ko@J�H@J��@J��@J=q@JJ@I��@I�7@Ix�@IX@I�@H��@Hr�@G�@G�;@G��@G�@G|�@F�@FE�@E�@E�@E�h@E?}@E�@EV@D�D@D(�@C��@C�@CS�@Co@B��@B=q@B�@A��@A��@AX@A7L@A%@@��@@�`@@ �@?|�@?l�@?\)@?+@?�@?
=@>�@>E�@=�h@=p�@=O�@=V@<�@<�@<��@<j@<9X@;�
@;�@;"�@:�H@:��@:M�@:�@9��@9�7@97L@8�u@81'@7�@7��@7�@7K�@7;d@7+@7
=@6��@6v�@6V@6$�@5�@5�h@5�@4�j@4z�@49X@3�
@3S�@2�@2��@2=q@1x�@1�@0�@0Q�@01'@0  @0  @0  @/�;@/�P@/�P@.ȴ@.@-�h@-/@,��@,�j@,��@,�D@,(�@+�m@+ƨ@+��@+S�@*�H@*M�@)��@)�^@)hs@)7L@(Ĝ@(�@(r�@(Q�@( �@'�@'�w@'K�@&�@&��@&�+@&�+@&�+@&ff@&{@%�-@%?}@$��@$��@$�@$��@$�@#ƨ@#ƨ@#�@#33@#o@#@"�@"��@"n�@"�@!�@!�^@!hs@!G�@!G�@!&�@ ��@ �@ bN@ 1'@ b@�@��@�w@�P@\)@��@ȴ@��@v�@$�@�@@��@p�@/@��@�@z�@z�@j@Z@9X@�@��@�
@��@dZ@33@o@@�@�H@�H@��@��@��@��@^5@=q@�@��@�@��@��@�^@�7@G�@�@�`@Ĝ@��@�@1'@��@��@��@|�@\)@K�@;d@+@
=@ȴ@�+@V@V@V@E�@@�-@��@��@�h@�h@�@`B@?}@�@�@z�@I�@I�@I�@1@��@�m111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AΝ�AΡ�AΡ�AΟ�AΡ�AΩ�AΧ�AΩ�AΥ�Aδ9A���A�A�ĜA�ĜA�A�ĜA�ƨA�A���A�A���A���A���A���A���A���A���A���A���A���A���A���A��
A��
A��A��A��A��
A���AξwAΡ�A�^5A�XAȲ-A�ĜA��A���A��A�&�A��jA��A�dZA�S�A�VA�O�A�p�A��A���A���A���A�Q�A�VA�r�A�VA���A�x�A���A}%AxM�At~�ApĜAl�Ai+Af�HAb$�A_ƨA]��A\��A[
=AVz�AR�AJ��AHbAF��AF1AB�A@  A>�A<M�A:E�A8bA69XA3��A2ĜA2��A1��A0��A-��A,�A+|�A*9XA(n�A'�;A&��A&A&�9A'K�A'��A&(�A$��A"9XA!�^A �yA �A v�A ^5A r�A r�A M�A�mA�A�Av�AM�A$�A�A\)AĜA^5A�
A;dA�\Ar�Az�A��AoA��A�/A�HA��A��AS�A+A
=A��A�A��AI�A�AC�A��A��A�jA�AM�A�A�AƨA�PA\)A�A$�A�A��A�^A�PA�PA�hA�7A7LA�A�`A�DA�TAC�A��AM�A5?A=qA��AVA&�A�`AM�A�wAl�A7LA"�AoA
��A
�/A
��A
�+A	��A	ƨA	�A	`BA	O�A	�AȴA�AM�A{A��A|�A�A�DA�mA��Ap�A\)A;dA
=A�A{A��Ax�A&�AȴA�!A�A �A��A�#A��A\)A ��A �9@�ƨ@��@�~�@�@�%@�A�@�\)@���@�~�@��@�7L@��`@�bN@� �@�\)@���@���@�1@��@�Z@�(�@�dZ@�V@��@�x�@� �@��@��
@@�t�@���@�ff@��^@��/@�@�\)@�+@��H@���@�Ĝ@��@��;@�K�@��@�V@�@��@�bN@㕁@◍@�h@��@���@�1'@�
=@�v�@�E�@��T@���@ݡ�@���@���@�j@��@�"�@�J@��m@�~�@��@�?}@�V@�Q�@ӕ�@�K�@��@�E�@���@щ7@�X@���@϶F@θR@�n�@��#@�G�@̋D@�  @�\)@�l�@˕�@ʸR@Ɂ@�z�@�9X@ǅ@�J@�5?@�@��`@�r�@��
@��@¸R@�=q@�@�?}@���@��@�A�@���@�K�@��!@��@��h@���@�b@���@�
=@���@�^5@�$�@��h@�&�@��`@���@�Q�@��m@��P@�o@��R@��@���@��h@�hs@��`@�bN@�b@��@�|�@�;d@��@�v�@�=q@��T@���@���@�G�@��@��D@��@�  @��m@��w@�"�@���@�~�@�E�@���@�O�@�V@��j@�z�@�1@���@���@�S�@�
=@��R@�n�@��@��^@���@�p�@�hs@��9@�r�@��m@��P@�\)@�+@��y@�^5@��@��-@�`B@�X@�O�@���@��D@��@��w@��P@�
=@�n�@�E�@���@�@���@�hs@�7L@���@��@��D@�z�@�Z@���@��@�l�@�+@���@�@���@���@��@�1'@���@�C�@��@��\@�ff@�-@��T@��@�/@��u@�9X@�  @���@�;d@��@�~�@�E�@���@�p�@���@�bN@��;@���@��P@�K�@��+@��@���@�p�@�7L@��@�%@��`@��@��u@�r�@�Z@�9X@��@���@���@��@��!@���@�x�@�O�@�G�@�7L@��@��/@���@�I�@�  @��;@�dZ@�33@�+@�o@�@��H@�ȴ@�ȴ@��\@�p�@�G�@�7L@�&�@��@�%@��@���@��9@�I�@���@��@�|�@�|�@��@��@�|�@�"�@���@���@�=q@���@��7@�x�@�G�@���@��9@��u@�bN@�bN@�Z@�(�@���@�dZ@�@���@���@�^5@��@��T@��#@��7@�X@�?}@�&�@��`@��u@�r�@�j@�bN@�9X@l�@~v�@~@}�@}�T@}��@}?}@|I�@{ƨ@{��@{S�@z��@zM�@y�@y�7@yx�@y7L@y�@x�@w�;@w;d@v�y@v�R@vff@u@t�/@tI�@s��@s�@sdZ@sC�@s@r�@r��@r^5@q��@qx�@q�@pr�@p  @o�@o|�@n�@n�@n��@mp�@l��@lz�@l9X@l(�@l(�@l�@l�@l�@k��@kC�@j�H@j~�@j=q@i��@i�@i�#@i�#@i��@i��@i�@hA�@h  @gl�@f�@f��@f��@f��@f�+@fE�@f@e��@e�@d�@c�m@ct�@co@c@b�@b��@bn�@bM�@a��@`��@`�@`1'@_�@_�w@_��@_l�@^��@^��@^E�@]��@]�@]�@\j@\I�@\I�@[�m@[C�@Z�H@Z��@Z��@Z�!@Z�\@ZM�@Z=q@Z�@Y��@Y�@Y��@Y�@X�u@XbN@XA�@W�;@W�@W��@W|�@V�@U�@U�@UO�@U/@U�@T��@T�/@T�D@T(�@S�
@S�F@S��@S�@SS�@S33@S33@S33@R�@R��@R��@RM�@RJ@Q�#@Q��@Q��@Qhs@Q&�@P�9@P�@PbN@P1'@O�@O��@O�@O��@O��@O|�@O\)@N�@N��@Nv�@NV@N5?@M�@M��@M/@Lz�@L�@Kƨ@K��@KdZ@Ko@J�H@J��@J��@J=q@JJ@I��@I�7@Ix�@IX@I�@H��@Hr�@G�@G�;@G��@G�@G|�@F�@FE�@E�@E�@E�h@E?}@E�@EV@D�D@D(�@C��@C�@CS�@Co@B��@B=q@B�@A��@A��@AX@A7L@A%@@��@@�`@@ �@?|�@?l�@?\)@?+@?�@?
=@>�@>E�@=�h@=p�@=O�@=V@<�@<�@<��@<j@<9X@;�
@;�@;"�@:�H@:��@:M�@:�@9��@9�7@97L@8�u@81'@7�@7��@7�@7K�@7;d@7+@7
=@6��@6v�@6V@6$�@5�@5�h@5�@4�j@4z�@49X@3�
@3S�@2�@2��@2=q@1x�@1�@0�@0Q�@01'@0  @0  @0  @/�;@/�P@/�P@.ȴ@.@-�h@-/@,��@,�j@,��@,�D@,(�@+�m@+ƨ@+��@+S�@*�H@*M�@)��@)�^@)hs@)7L@(Ĝ@(�@(r�@(Q�@( �@'�@'�w@'K�@&�@&��@&�+@&�+@&�+@&ff@&{@%�-@%?}@$��@$��@$�@$��@$�@#ƨ@#ƨ@#�@#33@#o@#@"�@"��@"n�@"�@!�@!�^@!hs@!G�@!G�@!&�@ ��@ �@ bN@ 1'@ b@�@��@�w@�P@\)@��@ȴ@��@v�@$�@�@@��@p�@/@��@�@z�@z�@j@Z@9X@�@��@�
@��@dZ@33@o@@�@�H@�H@��@��@��@��@^5@=q@�@��@�@��@��@�^@�7@G�@�@�`@Ĝ@��@�@1'@��@��@��@|�@\)@K�@;d@+@
=@ȴ@�+@V@V@V@E�@@�-@��@��@�h@�h@�@`B@?}@�@�@z�@I�@I�@I�@1@��@�m111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B33B2-B2-B33B33B1'B2-B2-B33B2-B1'B1'B1'B2-B33B2-B33B49B2-B33B33B33B33B49B49B33B33B33B33B33B49B49B2-B49B33B33B49B6FB9XB9XB9XB;dBhsBB	DB	��B
�B
33B
8RB
;dB
J�B
T�B
W
B
Q�B
M�B
O�B
@�B
;dB
,B
�B
�B
VB	��B	��B
+B
�B
+B	�)B	�}B	�B	��B	�VB	{�B	q�B	_;B	R�B	H�B	>wB	33B	�B	1B�B�sB�TB�BB�fB�;B�HB�NB�5B�B�B	1B	PB	PB	{B	�B	:^B	=qB	>wB	7LB	'�B	,B	)�B	)�B	G�B	S�B	\)B	jB	cTB	\)B	ZB	dZB	}�B	�!B	��B	��B	�B	�)B	�BB	�fB	�B	�B	�B	�B	��B
B
B
B
B
JB
hB
{B
�B
�B
�B
�B
�B
�B
�B
!�B
!�B
!�B
#�B
'�B
,B
33B
49B
49B
6FB
8RB
9XB
:^B
<jB
<jB
<jB
=qB
=qB
>wB
?}B
A�B
B�B
E�B
K�B
L�B
L�B
M�B
M�B
N�B
N�B
M�B
M�B
N�B
R�B
O�B
L�B
I�B
H�B
I�B
J�B
E�B
G�B
F�B
C�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
C�B
B�B
A�B
?}B
=qB
:^B
9XB
9XB
9XB
;dB
:^B
9XB
9XB
8RB
8RB
8RB
8RB
7LB
8RB
7LB
6FB
6FB
5?B
49B
2-B
1'B
-B
+B
+B
)�B
(�B
'�B
%�B
$�B
$�B
#�B
"�B
"�B
"�B
$�B
"�B
!�B
!�B
�B
�B
%�B
&�B
&�B
%�B
$�B
$�B
#�B
#�B
"�B
"�B
"�B
"�B
!�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
{B
uB
uB
uB
oB
oB
oB
hB
bB
bB
bB
\B
\B
PB
PB
PB
JB
DB
DB
DB
DB
DB
DB
DB
DB
DB
DB
DB
DB
DB
DB

=B
DB
PB
VB
DB
JB
DB
JB
DB
VB
PB
DB

=B

=B

=B
	7B
	7B

=B
	7B
	7B

=B
	7B
	7B
	7B

=B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B

=B

=B

=B
DB
DB
DB
JB
JB
JB
DB
DB
DB
DB
JB
JB
PB
PB
PB
\B
\B
\B
bB
oB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
"�B
"�B
"�B
!�B
"�B
"�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
&�B
%�B
&�B
&�B
&�B
&�B
'�B
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
,B
,B
,B
,B
,B
-B
-B
.B
.B
.B
.B
.B
.B
/B
/B
/B
/B
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
2-B
2-B
2-B
33B
33B
33B
49B
49B
49B
49B
49B
49B
49B
49B
5?B
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
?}B
?}B
?}B
?}B
@�B
?}B
?}B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
C�B
C�B
C�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
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
N�B
O�B
O�B
O�B
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
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
S�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
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
ZB
ZB
ZB
YB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
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
^5B
^5B
^5B
_;B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
aHB
`BB
aHB
aHB
aHB
aHB
bNB
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
dZB
dZB
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
hsB
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
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
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
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
q�B
q�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
r�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
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
t�B
t�B
t�B
t�B
u�B
u�B
u�B
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
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
� B
~�B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B33B2-B2-B33B33B1'B2-B2-B33B2-B1'B1'B1'B2-B33B2-B33B49B2-B33B33B33B33B49B49B33B33B33B33B33B49B49B2-B49B33B33B49B6FB9XB9XB9XB;dBhsBB	DB	��B
�B
33B
8RB
;dB
J�B
T�B
W
B
Q�B
M�B
O�B
@�B
;dB
,B
�B
�B
VB	��B	��B
+B
�B
+B	�)B	�}B	�B	��B	�VB	{�B	q�B	_;B	R�B	H�B	>wB	33B	�B	1B�B�sB�TB�BB�fB�;B�HB�NB�5B�B�B	1B	PB	PB	{B	�B	:^B	=qB	>wB	7LB	'�B	,B	)�B	)�B	G�B	S�B	\)B	jB	cTB	\)B	ZB	dZB	}�B	�!B	��B	��B	�B	�)B	�BB	�fB	�B	�B	�B	�B	��B
B
B
B
B
JB
hB
{B
�B
�B
�B
�B
�B
�B
�B
!�B
!�B
!�B
#�B
'�B
,B
33B
49B
49B
6FB
8RB
9XB
:^B
<jB
<jB
<jB
=qB
=qB
>wB
?}B
A�B
B�B
E�B
K�B
L�B
L�B
M�B
M�B
N�B
N�B
M�B
M�B
N�B
R�B
O�B
L�B
I�B
H�B
I�B
J�B
E�B
G�B
F�B
C�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
C�B
B�B
A�B
?}B
=qB
:^B
9XB
9XB
9XB
;dB
:^B
9XB
9XB
8RB
8RB
8RB
8RB
7LB
8RB
7LB
6FB
6FB
5?B
49B
2-B
1'B
-B
+B
+B
)�B
(�B
'�B
%�B
$�B
$�B
#�B
"�B
"�B
"�B
$�B
"�B
!�B
!�B
�B
�B
%�B
&�B
&�B
%�B
$�B
$�B
#�B
#�B
"�B
"�B
"�B
"�B
!�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
{B
uB
uB
uB
oB
oB
oB
hB
bB
bB
bB
\B
\B
PB
PB
PB
JB
DB
DB
DB
DB
DB
DB
DB
DB
DB
DB
DB
DB
DB
DB

=B
DB
PB
VB
DB
JB
DB
JB
DB
VB
PB
DB

=B

=B

=B
	7B
	7B

=B
	7B
	7B

=B
	7B
	7B
	7B

=B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B

=B

=B

=B
DB
DB
DB
JB
JB
JB
DB
DB
DB
DB
JB
JB
PB
PB
PB
\B
\B
\B
bB
oB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
"�B
"�B
"�B
!�B
"�B
"�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
&�B
%�B
&�B
&�B
&�B
&�B
'�B
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
,B
,B
,B
,B
,B
-B
-B
.B
.B
.B
.B
.B
.B
/B
/B
/B
/B
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
2-B
2-B
2-B
33B
33B
33B
49B
49B
49B
49B
49B
49B
49B
49B
5?B
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
?}B
?}B
?}B
?}B
@�B
?}B
?}B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
C�B
C�B
C�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
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
N�B
O�B
O�B
O�B
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
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
S�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
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
ZB
ZB
ZB
YB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
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
^5B
^5B
^5B
_;B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
aHB
`BB
aHB
aHB
aHB
aHB
bNB
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
dZB
dZB
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
hsB
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
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
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
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
q�B
q�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
r�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
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
t�B
t�B
t�B
t�B
u�B
u�B
u�B
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
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
� B
~�B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.41 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20200301100139                              AO  ARCAADJP                                                                    20200301100139    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200301100139  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200301100139  QCF$                G�O�G�O�G�O�0               