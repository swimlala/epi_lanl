CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2021-06-20T11:00:34Z creation      
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
resolution        =���   axis      Z        l  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  G�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  Kx   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Y�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  ]�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  k�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  zX   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  }�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �`   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  �h   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  �p   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  �x   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �@   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �D   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �H   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �L   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �P   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ڐ   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ڠ   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ڤ   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ڴ   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ڸ   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ڼ   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210620110034  20210620110034  5906585 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               A   AO  8354                            2B  A   NAVIS_A                         1272                            170425                          863 @�}�V�3�1   @�}�����@5���n��d�S���1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         A   A   A   @���@�  A   A   AA��A^ffA�  A�  A�  A�  A���A�  A�  A�  B   B  B  B��B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cq�fCt  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&�fD'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>�fD?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D��3D�3D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @~{@�=q@�=qA�A:�RAW�Ay�A��\A��\A��\A�\)Ȁ\A܏\A�\A��\BG�BG�B�HBG�B&G�B.G�B6G�B>G�BFG�BNG�BVG�B^G�BfG�BnG�BvG�B~G�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��CqxRCs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D d{D �{Dd{D�{Dd{D�{Dd{D�{Dd{D�{Dd{D�{Dd{D�{Dd{D�{Dd{D�{D	d{D	�{D
d{D
�{Dd{D�{Dd{D�{Dd{D�{Dd{D�{Dd{D�{Dd{D�{Dd{D�{Dd{D�{Dd{D�{Dd{D�{Dd{D�{Dd{D�{Dd{D�{Dd{D�{Dd{D�{Dd{D�{Dd{D�{Dd{D�{Dd{D�{Dd{D�{Dd{D�{D d{D �{D!d{D!�{D"d{D"�{D#d{D#�{D$d{D$�{D%d{D%�{D&j�D&�{D'd{D'�{D(d{D(�{D)d{D)�{D*d{D*�{D+d{D+�{D,d{D,�{D-d{D-�{D.d{D.�{D/d{D/�{D0d{D0�{D1d{D1�{D2d{D2�{D3d{D3�{D4d{D4�{D5d{D5�{D6d{D6�{D7d{D7�{D8d{D8�{D9d{D9�{D:d{D:�{D;d{D;�{D<d{D<�{D=d{D=�{D>j�D>�{D?d{D?�{D@d{D@�{DAd{DA�{DBd{DB�{DCd{DC�{DDd{DD�{DEd{DE�{DFd{DF�{DGd{DG�{DHd{DH�{DId{DI�{DJd{DJ�{DKd{DK�{DLd{DL�{DMd{DM�{DNd{DN�{DOd{DO�{DPd{DP�{DQd{DQ�{DRd{DR�{DSd{DS�{DTd{DT�{DUd{DU�{DVd{DV�{DWd{DW�{DXd{DX�{DYd{DY�{DZd{DZ�{D[d{D[�{D\d{D\�{D]d{D]�{D^d{D^�{D_d{D_�{D`d{D`�{Dad{Da�{Dbd{Db�{Dcd{Dc�{Ddd{Dd�{Ded{De�{Dfd{Df�{Dgd{Dg�{Dhd{Dh�{Did{Di�{Djd{Dj�{Dkd{Dk�{Dld{Dl�{Dmd{Dm�{Dnd{Dn�{Dod{Do�{Dpd{Dp�{Dqd{Dq�{Drd{Dr�{Dsd{Ds�{Dtd{Dt�{Dud{Du�{Dvd{Dv�{Dwd{Dw�{Dxd{Dx�{Dyd{Dy�{Dzd{Dz�{D{d{D{�{D|d{D|�{D}d{D}�{D~d{D~�{Dd{D�{D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��pD��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D��=D��=D�2=D�r=D²=D��=D�2=D�r=Dò=D��=D�2=D�r=DĲ=D��=D�2=D�r=DŲ=D��=D�2=D�r=DƲ=D��=D�2=D�r=Dǲ=D��=D�2=D�r=DȲ=D��=D�2=D�r=Dɲ=D��=D�2=D�r=Dʲ=D��=D�2=D�r=D˲=D��=D�2=D�r=D̲=D��=D�2=D�r=DͲ=D��=D�2=D�r=Dβ=D��=D�2=D�r=DϵpD��pD�2=D�r=Dв=D��=D�2=D�r=DѲ=D��=D�2=D�r=DҲ=D��=D�2=D�r=DӲ=D��=D�2=D�r=DԲ=D��=D�2=D�r=Dղ=D��=D�2=D�r=Dֲ=D��=D�2=D�r=Dײ=D��=D�2=D�r=Dز=D��=D�2=D�r=Dٲ=D��=D�2=D�r=Dڲ=D��=D�2=D�r=D۲=D��=D�2=D�r=Dܲ=D��=D�2=D�r=Dݲ=D��=D�2=D�r=D޲=D��=D�2=D�r=D߲=D��=D�2=D�r=D�=D��=D�2=D�r=D�=D��=D�2=D�r=D�=D��=D�2=D�r=D�=D��=D�2=D�r=D�=D��=D�2=D�r=D�=D��=D�2=D�r=D�=D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aח�Aי�Aף�A׬A׉7A�
=A�XAӋDA� �A��;A�VA�p�A�A�A�1AȋDA�+A��TAżjA�Q�A�XA��A�K�A���A�hsA�~�A�+A���A� �A�z�A�dZA�jA��FA�"�A���A���A��A�jA�$�A�x�A��A��A��FA��+A�ZA�ĜA�1'A���A�l�A���A���A���A�^5A���A�=qA��A�A�bNA��HA�bA��A�t�A���A��A��9A��-A��A�"�A��A�7LA���A��A��;A��A��A���A��FA�A�=qA�ƨA�z�A��A�VA�5?A���A��jA�7LA�/A�A�E�A�VA��\A�r�A�ZA��+A��A�?}A���A�VA�S�A�ĜAA~�DA}?}Az�9Ay��Ay"�AwO�Au|�Ar��Ar�uAr�\Ar5?Ap=qAm7LAl �AjA�AhbAf^5Ae��Ad�AbM�AaA_��A]��A[`BAY��AW�TAV  AT��AR�HAQ�hAO�TAM�AL�HAKoAI\)AGhsAF(�AD��AC��ACK�AB�RAAC�A@^5A@bA?\)A=�TA<��A;dZA9�PA7��A5�wA5�A4��A3��A3\)A2{A1oA/�wA/G�A.��A.JA-�^A-p�A-�A,��A,bNA+��A*�A*=qA)�;A)33A(^5A(=qA'�A'?}A&v�A%
=A#��A!�
A��A�+A  A�At�A�9A�/A�mA~�A"�A1'AhsA�;A&�AS�A��A�
A�hA?}A�`A�uA^5A$�A�-A
��A
bA	��A	��A	�AAK�A�jA5?A�A5?A��A��A�
A"�A �HA �A v�A �@��@���@�~�@�$�@���@��
@�"�@�S�@�K�@�V@�x�@�?}@���@�Z@���@�5?@��#@�O�@�V@��/@��9@��@��m@�w@�@�5?@�X@�j@�b@��@���@�%@��@�b@��@�ƨ@�t�@�7@�  @�dZ@���@���@��@�-@��`@�A�@�(�@�1'@߾w@݉7@�b@�K�@�^5@��@١�@�1@�X@�Z@��H@�G�@��@�`B@��@�o@���@ȴ9@Ȭ@���@�ff@�`B@�z�@�|�@�33@��@�@�V@���@�`B@�G�@��/@�z�@��
@�@��^@���@��@��#@�J@�5?@�=q@�$�@���@�O�@��D@��@��@�@���@��\@��\@���@��;@�\)@�V@�%@��@� �@��@���@�I�@�O�@��7@���@���@��@�o@�S�@�V@�I�@�1'@�(�@��9@���@��@�x�@��F@�S�@��@�~�@��h@��j@�z�@�Q�@��w@���@��@�C�@�K�@�\)@�t�@�  @���@��@���@��D@�%@���@��@��\@�O�@�r�@��F@���@���@���@�t�@�S�@��P@��w@�t�@�o@�~�@���@��7@�hs@�X@�X@���@��@�z�@��@�~�@��R@��@�@�hs@�/@��`@�I�@�Z@�Z@�b@�l�@�V@��@��@��@���@��j@��@�  @��@�dZ@�;d@���@���@���@�5?@���@��h@�hs@�V@�Ĝ@��@��@�K�@�ȴ@��\@�V@�$�@���@���@��h@�hs@�G�@�&�@���@�bN@�  @��
@��@��P@�S�@�o@��y@��R@���@��+@�^5@�M�@�=q@��@�hs@�G�@�/@�V@���@�Ĝ@��@�(�@��@��P@�dZ@��@��@���@�ff@��@���@��7@��@���@���@��P@�;d@�@��@�n�@�5?@��@���@��-@�7L@�j@�bN@�bN@�z�@��@�9X@�b@��
@��w@�t�@�K�@��@��\@�^5@��@��#@��^@���@��h@�`B@�?}@���@���@��@��u@��D@�Z@� �@��
@��
@��@�l�@�
=@��y@���@���@�ff@�-@��@��#@��#@�@�`B@���@��9@�1@l�@~��@~$�@}`B@|j@|�@{��@{C�@z��@y��@yG�@x��@x�`@x��@xA�@x1'@w��@wK�@w�@w
=@w
=@v�y@v�y@v�y@v�@v�@v�@vȴ@v��@vv�@vff@u�-@u/@tI�@s�F@s@rn�@q�#@qhs@qhs@qhs@qX@qX@p��@p  @o�@o��@o�P@ol�@ol�@ol�@ol�@ol�@o|�@ol�@o;d@n��@n��@nff@n5?@n$�@m�T@m�@k�
@j��@jn�@j^5@jM�@jM�@j-@i�#@h��@h�u@hbN@g�;@g�P@g
=@f��@f5?@e�@e��@d��@dz�@c�m@c��@ct�@cS�@co@b�H@b��@b�\@b~�@b~�@b~�@bn�@bM�@b-@b�@a�@a��@a��@a7L@`��@`�`@`��@`�9@`�@`bN@` �@_\)@^��@]��@]?}@]V@\�/@\�@\�D@\z�@\Z@\9X@\1@[��@[�
@[dZ@Z^5@Y�#@Y��@Y�7@Yhs@YX@Y7L@X�u@X1'@W��@W�@V�R@V�+@VV@V$�@U@U��@U�@Up�@Up�@UO�@T�@S�F@SC�@R�@R��@Rn�@RM�@R�@Q�7@P��@Pr�@O�;@O�@O;d@O�@N�y@N��@Nv�@NV@N@MV@L��@L��@Lj@L9X@L1@K�F@K33@K@J^5@JJ@I7L@I%@H�`@H��@HĜ@H�@H1'@G�P@F�@F$�@E�@E�T@E�-@Ep�@E?}@EV@D�@D�/@D�j@D�@D��@D9X@C@B��@B�!@B-@A%@@��@@Q�@@b@?�;@?��@?�P@?+@>�@>��@>�+@>v�@>5?@=�-@=�-@=�h@=`B@=O�@=/@<�/@<j@;��@;�
@;��@;��@;t�@;C�@;@:��@:��@:~�@:n�@:M�@:�@9�@9��@9G�@8r�@7��@7�P@7\)@7+@6�R@6�+@6�+@6v�@5�T@5?}@4�/@4Z@4(�@4�@3�m@3��@3dZ@3o@2�\@2-@2J@2J@1�@1�^@1X@0��@0�9@0�@0r�@01'@0b@/��@/;d@.v�@.V@.$�@.@-@-/@,�j@,�D@,Z@,Z@,(�@+ƨ@+S�@+"�@+o@*�@*�\@*=q@)�^@)x�@)X@)X@(�`@(bN@(b@'�@'�@&E�@%�T@%�h@%�@$�D@$�@#�m@#�
@#�F@#S�@#@"��@"��@"^5@!��@!��@!�7@ ��@ �9@ �9@ �@ r�@ Q�@ 1'@�@�P@�@�R@��@ff@5?@$�@�@�@`B@`B@O�@/@�@��@��@�D@z�@�@��@S�@33@"�@�@��@��@�!@�\@n�@M�@-@��@�^@�7@hs@G�@&�@��@Ĝ@�9@��@r�@A�@�@��@\)@��@ȴ@V@E�@5?@�@��@�-@��@p�@V@�j@z�@(�@�
@�
@�
@ƨ@ƨ@�F11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aח�Aי�Aף�A׬A׉7A�
=A�XAӋDA� �A��;A�VA�p�A�A�A�1AȋDA�+A��TAżjA�Q�A�XA��A�K�A���A�hsA�~�A�+A���A� �A�z�A�dZA�jA��FA�"�A���A���A��A�jA�$�A�x�A��A��A��FA��+A�ZA�ĜA�1'A���A�l�A���A���A���A�^5A���A�=qA��A�A�bNA��HA�bA��A�t�A���A��A��9A��-A��A�"�A��A�7LA���A��A��;A��A��A���A��FA�A�=qA�ƨA�z�A��A�VA�5?A���A��jA�7LA�/A�A�E�A�VA��\A�r�A�ZA��+A��A�?}A���A�VA�S�A�ĜAA~�DA}?}Az�9Ay��Ay"�AwO�Au|�Ar��Ar�uAr�\Ar5?Ap=qAm7LAl �AjA�AhbAf^5Ae��Ad�AbM�AaA_��A]��A[`BAY��AW�TAV  AT��AR�HAQ�hAO�TAM�AL�HAKoAI\)AGhsAF(�AD��AC��ACK�AB�RAAC�A@^5A@bA?\)A=�TA<��A;dZA9�PA7��A5�wA5�A4��A3��A3\)A2{A1oA/�wA/G�A.��A.JA-�^A-p�A-�A,��A,bNA+��A*�A*=qA)�;A)33A(^5A(=qA'�A'?}A&v�A%
=A#��A!�
A��A�+A  A�At�A�9A�/A�mA~�A"�A1'AhsA�;A&�AS�A��A�
A�hA?}A�`A�uA^5A$�A�-A
��A
bA	��A	��A	�AAK�A�jA5?A�A5?A��A��A�
A"�A �HA �A v�A �@��@���@�~�@�$�@���@��
@�"�@�S�@�K�@�V@�x�@�?}@���@�Z@���@�5?@��#@�O�@�V@��/@��9@��@��m@�w@�@�5?@�X@�j@�b@��@���@�%@��@�b@��@�ƨ@�t�@�7@�  @�dZ@���@���@��@�-@��`@�A�@�(�@�1'@߾w@݉7@�b@�K�@�^5@��@١�@�1@�X@�Z@��H@�G�@��@�`B@��@�o@���@ȴ9@Ȭ@���@�ff@�`B@�z�@�|�@�33@��@�@�V@���@�`B@�G�@��/@�z�@��
@�@��^@���@��@��#@�J@�5?@�=q@�$�@���@�O�@��D@��@��@�@���@��\@��\@���@��;@�\)@�V@�%@��@� �@��@���@�I�@�O�@��7@���@���@��@�o@�S�@�V@�I�@�1'@�(�@��9@���@��@�x�@��F@�S�@��@�~�@��h@��j@�z�@�Q�@��w@���@��@�C�@�K�@�\)@�t�@�  @���@��@���@��D@�%@���@��@��\@�O�@�r�@��F@���@���@���@�t�@�S�@��P@��w@�t�@�o@�~�@���@��7@�hs@�X@�X@���@��@�z�@��@�~�@��R@��@�@�hs@�/@��`@�I�@�Z@�Z@�b@�l�@�V@��@��@��@���@��j@��@�  @��@�dZ@�;d@���@���@���@�5?@���@��h@�hs@�V@�Ĝ@��@��@�K�@�ȴ@��\@�V@�$�@���@���@��h@�hs@�G�@�&�@���@�bN@�  @��
@��@��P@�S�@�o@��y@��R@���@��+@�^5@�M�@�=q@��@�hs@�G�@�/@�V@���@�Ĝ@��@�(�@��@��P@�dZ@��@��@���@�ff@��@���@��7@��@���@���@��P@�;d@�@��@�n�@�5?@��@���@��-@�7L@�j@�bN@�bN@�z�@��@�9X@�b@��
@��w@�t�@�K�@��@��\@�^5@��@��#@��^@���@��h@�`B@�?}@���@���@��@��u@��D@�Z@� �@��
@��
@��@�l�@�
=@��y@���@���@�ff@�-@��@��#@��#@�@�`B@���@��9@�1@l�@~��@~$�@}`B@|j@|�@{��@{C�@z��@y��@yG�@x��@x�`@x��@xA�@x1'@w��@wK�@w�@w
=@w
=@v�y@v�y@v�y@v�@v�@v�@vȴ@v��@vv�@vff@u�-@u/@tI�@s�F@s@rn�@q�#@qhs@qhs@qhs@qX@qX@p��@p  @o�@o��@o�P@ol�@ol�@ol�@ol�@ol�@o|�@ol�@o;d@n��@n��@nff@n5?@n$�@m�T@m�@k�
@j��@jn�@j^5@jM�@jM�@j-@i�#@h��@h�u@hbN@g�;@g�P@g
=@f��@f5?@e�@e��@d��@dz�@c�m@c��@ct�@cS�@co@b�H@b��@b�\@b~�@b~�@b~�@bn�@bM�@b-@b�@a�@a��@a��@a7L@`��@`�`@`��@`�9@`�@`bN@` �@_\)@^��@]��@]?}@]V@\�/@\�@\�D@\z�@\Z@\9X@\1@[��@[�
@[dZ@Z^5@Y�#@Y��@Y�7@Yhs@YX@Y7L@X�u@X1'@W��@W�@V�R@V�+@VV@V$�@U@U��@U�@Up�@Up�@UO�@T�@S�F@SC�@R�@R��@Rn�@RM�@R�@Q�7@P��@Pr�@O�;@O�@O;d@O�@N�y@N��@Nv�@NV@N@MV@L��@L��@Lj@L9X@L1@K�F@K33@K@J^5@JJ@I7L@I%@H�`@H��@HĜ@H�@H1'@G�P@F�@F$�@E�@E�T@E�-@Ep�@E?}@EV@D�@D�/@D�j@D�@D��@D9X@C@B��@B�!@B-@A%@@��@@Q�@@b@?�;@?��@?�P@?+@>�@>��@>�+@>v�@>5?@=�-@=�-@=�h@=`B@=O�@=/@<�/@<j@;��@;�
@;��@;��@;t�@;C�@;@:��@:��@:~�@:n�@:M�@:�@9�@9��@9G�@8r�@7��@7�P@7\)@7+@6�R@6�+@6�+@6v�@5�T@5?}@4�/@4Z@4(�@4�@3�m@3��@3dZ@3o@2�\@2-@2J@2J@1�@1�^@1X@0��@0�9@0�@0r�@01'@0b@/��@/;d@.v�@.V@.$�@.@-@-/@,�j@,�D@,Z@,Z@,(�@+ƨ@+S�@+"�@+o@*�@*�\@*=q@)�^@)x�@)X@)X@(�`@(bN@(b@'�@'�@&E�@%�T@%�h@%�@$�D@$�@#�m@#�
@#�F@#S�@#@"��@"��@"^5@!��@!��@!�7@ ��@ �9@ �9@ �@ r�@ Q�@ 1'@�@�P@�@�R@��@ff@5?@$�@�@�@`B@`B@O�@/@�@��@��@�D@z�@�@��@S�@33@"�@�@��@��@�!@�\@n�@M�@-@��@�^@�7@hs@G�@&�@��@Ĝ@�9@��@r�@A�@�@��@\)@��@ȴ@V@E�@5?@�@��@�-@��@p�@V@�j@z�@(�@�
@�
@�
@ƨ@ƨ@�F11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B$�B;dBO�Bs�B��B�B�dB��B�B�B��B	7B�B-B>wBA�BL�BT�BZBe`Br�Bw�B~�B�B�B�\B�uB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�!B�!B�!B�'B�B��B��B��B��B��B�oB�PB� B{�Br�BhsBdZB_;BXBW
BN�BH�BF�B?}B+B�B1B�B�B��B�B�uB�PB�B|�Bt�Bl�B\)BH�B/B�B\BB
��B
�B
�)B
��B
��B
��B
�9B
��B
�\B
x�B
n�B
bNB
]/B
N�B
F�B
C�B
6FB
-B
�B
�B
�B
�B
	7B	��B	�B	�ZB	�B	��B	ŢB	��B	�3B	�B	��B	��B	�PB	�B	w�B	m�B	gmB	^5B	T�B	K�B	@�B	:^B	0!B	'�B	�B	�B	bB	DB	+B	B	  B��B��B��B�B�B�B�`B�5B��B��B��BɺBƨBŢB��B�dB�XB�LB�9B�-B�'B�B�B�B��B��B��B��B��B��B��B��B��B��B��B�VB�1B�Bz�Bx�Bw�Bv�Bt�Br�Bp�Bm�BhsBdZBbNB_;B]/B[#B[#B_;Bo�Bp�Bo�Bn�Bn�Bm�Bm�Bk�BjBjBjBl�Bm�Bo�Br�Bx�B~�B�B�B�7B�DB�\B�bB�hB�oB�uB��B��B��B��B��B��B��B��B�B�-B�XB�dB�qB��B��BƨB��B��B��B�B�B�;B�ZB�sB�B��B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B��B��B�B��B��B��B�B�B�ZB�TB�NB�fB�HB�fB�yB�B�B�B�B�B�B��B��B��B��B	  B	%B	DB		7B	
=B	JB	\B	oB	�B	�B	�B	�B	�B	 �B	$�B	&�B	(�B	)�B	,B	/B	33B	33B	49B	2-B	2-B	33B	7LB	7LB	49B	6FB	J�B	K�B	N�B	R�B	R�B	R�B	O�B	T�B	ZB	YB	YB	ZB	XB	\)B	bNB	ZB	[#B	^5B	e`B	jB	m�B	p�B	l�B	iyB	hsB	gmB	ffB	gmB	jB	jB	l�B	n�B	p�B	p�B	s�B	y�B	}�B	�B	�B	�B	�B	�PB	��B	��B	��B	�{B	�bB	�\B	�PB	�JB	�JB	�PB	�PB	�VB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�-B	�9B	�^B	�qB	�qB	�}B	��B	��B	��B	�wB	�}B	��B	�}B	��B	��B	ÖB	ĜB	ƨB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�#B	�/B	�/B	�5B	�5B	�;B	�;B	�HB	�TB	�`B	�`B	�fB	�fB	�mB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
1B
1B
1B
1B
1B
+B
+B
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
JB
JB
JB
DB
JB
PB
\B
bB
hB
oB
oB
oB
uB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
#�B
#�B
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
%�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
(�B
(�B
)�B
)�B
)�B
+B
+B
+B
+B
+B
+B
+B
+B
+B
,B
,B
,B
,B
,B
,B
,B
-B
-B
-B
-B
-B
-B
-B
-B
-B
-B
-B
-B
-B
-B
.B
/B
/B
/B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
33B
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
8RB
9XB
8RB
8RB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
:^B
<jB
<jB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
B�B
B�B
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
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
H�B
H�B
G�B
H�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
P�B
Q�B
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
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
VB
VB
VB
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
YB
ZB
ZB
ZB
[#B
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
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
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
cTB
cTB
cTB
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
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hs11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�B�B�B�B$�B;dBO�Bs�B��B�B�dB��B�B�B��B	7B�B-B>wBA�BL�BT�BZBe`Br�Bw�B~�B�B�B�\B�uB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�!B�!B�!B�'B�B��B��B��B��B��B�oB�PB� B{�Br�BhsBdZB_;BXBW
BN�BH�BF�B?}B+B�B1B�B�B��B�B�uB�PB�B|�Bt�Bl�B\)BH�B/B�B\BB
��B
�B
�)B
��B
��B
��B
�9B
��B
�\B
x�B
n�B
bNB
]/B
N�B
F�B
C�B
6FB
-B
�B
�B
�B
�B
	7B	��B	�B	�ZB	�B	��B	ŢB	��B	�3B	�B	��B	��B	�PB	�B	w�B	m�B	gmB	^5B	T�B	K�B	@�B	:^B	0!B	'�B	�B	�B	bB	DB	+B	B	  B��B��B��B�B�B�B�`B�5B��B��B��BɺBƨBŢB��B�dB�XB�LB�9B�-B�'B�B�B�B��B��B��B��B��B��B��B��B��B��B��B�VB�1B�Bz�Bx�Bw�Bv�Bt�Br�Bp�Bm�BhsBdZBbNB_;B]/B[#B[#B_;Bo�Bp�Bo�Bn�Bn�Bm�Bm�Bk�BjBjBjBl�Bm�Bo�Br�Bx�B~�B�B�B�7B�DB�\B�bB�hB�oB�uB��B��B��B��B��B��B��B��B�B�-B�XB�dB�qB��B��BƨB��B��B��B�B�B�;B�ZB�sB�B��B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B��B��B�B��B��B��B�B�B�ZB�TB�NB�fB�HB�fB�yB�B�B�B�B�B�B��B��B��B��B	  B	%B	DB		7B	
=B	JB	\B	oB	�B	�B	�B	�B	�B	 �B	$�B	&�B	(�B	)�B	,B	/B	33B	33B	49B	2-B	2-B	33B	7LB	7LB	49B	6FB	J�B	K�B	N�B	R�B	R�B	R�B	O�B	T�B	ZB	YB	YB	ZB	XB	\)B	bNB	ZB	[#B	^5B	e`B	jB	m�B	p�B	l�B	iyB	hsB	gmB	ffB	gmB	jB	jB	l�B	n�B	p�B	p�B	s�B	y�B	}�B	�B	�B	�B	�B	�PB	��B	��B	��B	�{B	�bB	�\B	�PB	�JB	�JB	�PB	�PB	�VB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�-B	�9B	�^B	�qB	�qB	�}B	��B	��B	��B	�wB	�}B	��B	�}B	��B	��B	ÖB	ĜB	ƨB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�#B	�/B	�/B	�5B	�5B	�;B	�;B	�HB	�TB	�`B	�`B	�fB	�fB	�mB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
1B
1B
1B
1B
1B
+B
+B
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
JB
JB
JB
DB
JB
PB
\B
bB
hB
oB
oB
oB
uB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
#�B
#�B
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
%�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
(�B
(�B
)�B
)�B
)�B
+B
+B
+B
+B
+B
+B
+B
+B
+B
,B
,B
,B
,B
,B
,B
,B
-B
-B
-B
-B
-B
-B
-B
-B
-B
-B
-B
-B
-B
-B
.B
/B
/B
/B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
33B
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
8RB
9XB
8RB
8RB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
:^B
<jB
<jB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
B�B
B�B
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
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
H�B
H�B
G�B
H�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
P�B
Q�B
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
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
VB
VB
VB
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
YB
ZB
ZB
ZB
[#B
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
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
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
cTB
cTB
cTB
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
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hs11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.43 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20210620110034                              AO  ARCAADJP                                                                    20210620110034    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20210620110034  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20210620110034  QCF$                G�O�G�O�G�O�0               